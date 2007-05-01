
(module reductions-engine mzscheme
  (require (lib "list.ss")
           "deriv.ss"
           "stx-util.ss"
           "steps.ss")
  (provide (all-from "steps.ss"))

  (provide context
           big-context
           current-derivation
           current-definites
           learn-definites
           current-frontier
           add-frontier
           blaze-frontier
           rename-frontier
           with-context
           with-derivation
           with-new-local-context
           CC
           R
           revappend)
  (provide walk
           walk/foci
           stumble
           stumble/E)

  ;; context: parameter of Context
  (define context (make-parameter null))

  ;; big-context: parameter of BigContext
  (define big-context (make-parameter null))

  ;; current-derivation : parameter of Derivation
  (define current-derivation (make-parameter #f))

  ;; current-definites : parameter of (list-of identifier)
  (define current-definites (make-parameter null))

  ;; current-frontier : parameter of (list-of syntax)
  (define current-frontier (make-parameter null))

  (define-syntax with-context
    (syntax-rules ()
      [(with-context f . body)
       (let ([c (context)])
         (parameterize ([context (cons f c)])
           (let () . body)))]))

  (define-syntax with-derivation
    (syntax-rules ()
      [(with-derivation d . body)
       (parameterize ((current-derivation d)) . body)]))
  
  (define-syntax with-new-local-context
    (syntax-rules ()
      [(with-new-local-context e . body)
       (parameterize ([big-context
                       (cons (make-bigframe (current-derivation) (context) (list e) e)
                             (big-context))]
                      [context null])
         . body)]))

  (define (learn-definites ids)
    (current-definites (append ids (current-definites))))

  (define (add-frontier stxs)
    (current-frontier (append stxs (current-frontier)))
    #;(printf "new frontier: ~s~n" (current-frontier)))

  (define (blaze-frontier stx)
    #;(unless (memq stx (current-frontier))
      (fprintf (current-error-port) "frontier does not contain term: ~s~n" stx)
      (error 'blaze-frontier))
    (current-frontier (remq stx (current-frontier)))
    #;(printf "new frontier (blazed): ~s~n" (current-frontier)))

  ;; -----------------------------------

  ;; CC
  ;; the context constructor
  (define-syntax (CC stx)
    (syntax-case stx ()
      [(CC HOLE expr pattern)
       #'(syntax-copier HOLE expr pattern)]))

  ;; R
  ;; the threaded reductions engine
  (define-syntax R
    (syntax-rules ()
      [(R form . clauses)
       (R** #f _ [#:set-syntax form] [#:pattern pattern] . clauses)]))

  (define-syntax (R** stx)
    (syntax-case stx (! @ List Block =>)
      [(R** form-var pattern)
       #'null]
      
      [(R** f p => k)
       #'(k f)]
      
      ;; Change patterns
      [(R** f p [#:pattern p2] . more)
       #'(R** f p2 . more)]
      ;; Bind pattern variables
      [(R** f p [#:bind pattern rhs] . more)
       #'(with-syntax ([pattern (with-syntax ([p f]) rhs)])
           (R** f p . more))]
      ;; Change syntax
      [(R** f p [#:set-syntax form] . more)
       #'(let ([form-variable form])
           (R** form-variable p . more))]
      ;; Change syntax with step
      [(R** f p [#:walk form2 foci1 foci2 description] . more)
       #'(let-values ([(form2-var foci1-var foci2-var description-var)
                       (with-syntax ([p f])
                         (values form2 foci1 foci2 description))])
           (cons (walk/foci foci1-var foci2-var f form2-var description-var)
                 (R** form2-var p . more)))]
      [(R** f p [#:rename form2 foci1 foci2 description] . more)
       #'(let-values ([(form2-var foci1-var foci2-var description-var)
                       (with-syntax ([p f])
                         (values form2 foci1 foci2 description))])
           (rename-frontier f form2-var)
           (with-context (make-renames foci1-var foci2-var)
             (cons (walk/foci foci1-var foci2-var
                              f form2-var
                              description-var)
                   (R** form2-var p . more))))]
      [(R** f p [#:walk form2 description] . more)
       #'(let-values ([(form2-var description-var)
                       (with-syntax ([p f])
                         (values form2 description))])
           (cons (walk f form2-var description-var)
                 (R** form2-var p . more)))]
      [(R** f p [#:learn ids] . more)
       #'(begin (learn-definites ids)
                (R** f p . more))]
      [(R** f p [#:frontier stxs] . more)
       #'(begin (add-frontier (with-syntax ([p f]) stxs))
                (R** f p . more))]

      ;; Conditional
      [(R** f p [#:if test consequent ...] . more)
       #'(if (with-syntax ([p f]) test)
             (R** f p consequent ... . more)
             (R** f p . more))]

      ;; Error-point case
      [(R** f p [! info] . more)
       #'(R** f p [! info #f] . more)]
      [(R** f p [! info key] . more)
       #'(let ([continue (lambda () (R** f p . more))])
           (cond [(and (pair? info) (car info))
                  ;; error-wrap
                  ;; If this is the key, then insert the misstep here and stop.
                  ;; This stops processing *within* an error-wrapped prim.
                  (if (or (eq? key #f) (eq? key (cdr info)))
                      (list (stumble f (car info)))
                      (continue))]
                 [else
                  (continue)]))]
      
      [(R** f p [Generator hole0 fill0] . more)
       #'(let-values ([(reducer get-e1 get-e2) Generator])
           (R** f p [reducer get-e1 get-e2 hole0 fill0] . more))]
      
      ;; Implementation for (hole ...) sequences
      [(R** form-var pattern
            [f0 get-e1 get-e2 (hole0 :::) fill0s] . more)
       (and (identifier? #':::)
            (module-identifier=? #'::: (quote-syntax ...)))
       #'(let ([ctx0 (CC (hole0 :::) form-var pattern)])
           (let ([e1s (with-syntax ([pattern form-var]) (syntax->list #'(hole0 :::)))])
             (let loop ([fills fill0s] [prefix null] [suffix e1s])
               (cond
                 [(pair? fills)
                  (append 
                   (with-context ctx0
                     (with-context (lambda (x) (revappend prefix (cons x (cdr suffix))))
                       (f0 (car fills))))
                   (cond [(interrupted-wrap? (car fills))
                          null]
                         [(error-wrap? (car fills))
                          null]
                         [else
                          (loop (cdr fills)
                                (cons (get-e2 (car fills)) prefix)
                                (cdr suffix))]))]
                 [(null? fills)
                  (let ([form-var (ctx0 (reverse prefix))])
                    (R** form-var pattern . more))]))))]
      ;; Implementation
      [(R** form-var pattern
            [f0 get-e1 get-e2 hole0 fill0] . more)
       #'(let ([ctx0 (CC hole0 form-var pattern)])
           (append (with-context ctx0
                     (f0 fill0))
                   ;; If the last thing we ran through was interrupted,
                   ;; then there's nothing left to do.
                   ;; This stops processing *after* an error-wrapped deriv.
                   (cond [(interrupted-wrap? fill0) null]
                         [(error-wrap? fill0) null]
                         [else 
                          (let ([form-var (ctx0 (get-e2 fill0))])
                            (R** form-var pattern . more))])))]))
  

  ;; Rename mapping

  (define (rename-frontier from to)
    (current-frontier
     (apply append (map (make-rename-mapping from to) (current-frontier)))))

  (define (make-rename-mapping from to)
    (define table (make-hash-table))
    (let loop ([from from] [to to])
      (cond [(syntax? from)
             (hash-table-put! table from (flatten-syntaxes to))
             (loop (syntax-e from) to)]
            [(syntax? to)
             (loop from (syntax-e to))]
            [(pair? from)
             (loop (car from) (car to))
             (loop (cdr from) (cdr to))]
            [(vector? from)
             (loop (vector->list from) (vector->list to))]
            [(box? from)
             (loop (unbox from) (unbox to))]
            [else (void)]))
    (lambda (stx)
      (let ([replacement (hash-table-get table stx #f)])
        (if replacement
            (begin #;(printf "  replacing ~s with ~s~n" stx replacement)
                   replacement)
            (begin #;(printf "  not replacing ~s~n" stx)
                   (list stx))))))

  (define (flatten-syntaxes x)
    (cond [(syntax? x)
           (list x)]
          [(pair? x)
           (append (flatten-syntaxes (car x))
                   (flatten-syntaxes (cdr x)))]
          [(vector? x)
           (flatten-syntaxes (vector->list x))]
          [(box? x)
           (flatten-syntaxes (unbox x))]
          [else null]))

  ;; -----------------------------------

  ;; walk : syntax(es) syntax(es) StepType -> Reduction
  ;; Lifts a local step into a term step.
  (define (walk e1 e2 type)
    (make-step (current-derivation) (big-context) type (context)
               (current-definites) (current-frontier)
               (foci e1) (foci e2) e1 e2))
  
  ;; walk/foci : syntaxes syntaxes syntax syntax StepType -> Reduction
  (define (walk/foci foci1 foci2 Ee1 Ee2 type)
    (make-step (current-derivation) (big-context) type (context)
               (current-definites) (current-frontier)
               (foci foci1) (foci foci2) Ee1 Ee2))

  ;; stumble : syntax exception -> Reduction
  (define (stumble stx exn)
    (make-misstep (current-derivation) (big-context) 'error (context)
                  (current-definites) (current-frontier)
                  (foci stx) stx exn))
  
  ;; stumble/E : syntax(s) syntax exn -> Reduction
  (define (stumble/E focus Ee1 exn)
    (make-misstep (current-derivation) (big-context) 'error (context)
                  (current-definites) (current-frontier)
                  (foci focus) Ee1 exn))
  
  ;; ------------------------------------
  
  (define (revappend a b)
    (cond [(pair? a) (revappend (cdr a) (cons (car a) b))]
          [(null? a) b]))

  (define (foci x)
    (if (list? x)
        x
        (list x)))
  )
