
(module reductions-engine mzscheme
  (require "deriv.ss"
           "stx-util.ss"
           "steps.ss")
  (provide (all-defined)
           (all-from "steps.ss"))
  
  ;; A Context is (syntax -> syntax)
  ;; A BigContext is (list-of (cons Syntaxes Syntax))
  ;;   local expansion contexts: pairs of foci, term
  
  ;; context: parameter of Context
  (define context (make-parameter (lambda (x) x)))

  ;; big-context: parameter of BigContext
  (define big-context (make-parameter null))
  
  (define-syntax with-context
    (syntax-rules ()
      [(with-context f . body)
       (let ([E (context)])
         (parameterize ([context (lambda (x) (E (f x)))])
           . body))]))
  
  (define-syntax with-new-local-context
    (syntax-rules ()
      [(with-new-local-context e . body)
       (parameterize ([big-context (cons (cons (list e) (E e)) (big-context))]
                      [context (lambda (x) x)])
         . body)]))
  
  ;; E : syntax -> syntax
  (define (E stx) ((context) stx))

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
      [(R form pattern . clauses)
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
       #'(with-syntax ([pattern rhs])
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
           (cons (walk/foci/E foci1-var foci2-var f form2-var description-var)
                 (R** form2-var p . more)))]
      [(R** f p [#:rename form2 foci1 foci2 description] . more)
       #'(let-values ([(form2-var foci1-var foci2-var description-var)
                       (with-syntax ([p f])
                         (values form2 foci1 foci2 description))])
           (cons (walk-rename/foci/E foci1-var foci2-var
                                     f form2-var
                                     description-var)
                 (R** form2-var p . more)))]
      [(R** f p [#:walk form2 description] . more)
       #'(let-values ([(form2-var description-var)
                       (with-syntax ([p f])
                         (values form2 description))])
           (cons (walk f form2-var description-var)
                 (R** form2-var p . more)))]

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
                      (list (make-misstep f (E f) (car info)))
                      (continue))]
                 [else
                  (continue)]))]
      
      [(R** f p [Generator hole0 fill0] . more)
       #'(let-values ([(reducer get-e1 get-e2) Generator])
           (R** f p [reducer get-e1 get-e2 hole0 fill0] . more))]
      
;      ;; Expression case
;      [(R** f p [hole0 fill0] . more)
;       #'(R** f p [reductions deriv-e1 deriv-e2 hole0 fill0] . more)]
;      ;; List case
;      [(R** f p [List hole0 fill0] . more)
;       #'(R** f p [list-reductions lderiv-es1 lderiv-es2 hole0 fill0] . more)]
;      ;; Block case
;      [(R** f p [Block hole0 fill0] . more)
;       #'(R** f p [block-reductions bderiv-es1 bderiv-es2 hole0 fill0] . more)]

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
  
  
  ;; -----------------------------------
  
  ;; walk : syntax(s) syntax(s) [string] -> Reduction
  ;; Lifts a local step into a term step.
  (define walk
    (case-lambda
      [(e1 e2) (walk e1 e2 #f)]
      [(e1 e2 note) (make-rewrite-step e1 e2 (E e1) (E e2) note (big-context))]))
  
  ;; walk/foci/E : syntax(s) syntax(s) syntax syntax string -> Reduction
  (define (walk/foci/E focus1 focus2 e1 e2 note)
    (walk/foci focus1 focus2 (E e1) (E e2) note))

  ;; walk-rename/foci/E : syntax(s) syntax(s) syntax syntax string -> Reduction
  (define (walk-rename/foci/E focus1 focus2 e1 e2 note)
    (make-rename-step focus1 focus2 (E e1) (E e2) note (big-context)))

  ;; walk/foci : syntax(s) syntax(s) syntax syntax string -> Reduction
  (define (walk/foci focus1 focus2 Ee1 Ee2 note)
    (make-rewrite-step focus1 focus2 Ee1 Ee2 note (big-context)))

  ;; stumble : syntax exception -> Reduction
  (define (stumble stx exn)
    (make-misstep stx (E stx) exn))
  ;; ------------------------------------
  
  (define (revappend a b)
    (cond [(pair? a) (revappend (cdr a) (cons (car a) b))]
          [(null? a) b]))
  

  )