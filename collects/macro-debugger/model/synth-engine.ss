
(module synth-engine mzscheme
  (require "deriv.ss"
           "deriv-util.ss"
           "stx-util.ss"
           (lib "plt-match.ss"))
  
  (provide recv
           >>P
           >>Pn
           >>Prim
           >>Seek
           macro-policy
           phase
           force-letrec-transformation
           subterms-table
           lifts-available
           lifts-retained
           )
  
  ;; macro-policy : parameter of (identifier -> boolean)
  (define macro-policy (make-parameter (lambda (id) #t)))

  ;; phase : parameter of number
  (define phase (make-parameter 0))

  ;; force-letrec-transformation : parameter of boolean
  (define force-letrec-transformation (make-parameter #f))

  ;; subterms-table : parameter of hashtable[syntax => (list-of Path)]
  (define subterms-table (make-parameter #f))

  ;; lifts-available : parameter of (listof (cons syntax Derivation))
  (define lifts-available (make-parameter 'uninitialized))

  ;; lifts-retained : parameter of (listof (cons syntax Derivation))
  ;; Ordered reverse-chronologically, ie same order as definition sequence
  (define lifts-retained (make-parameter 'uninitialized))
  
  ;; Macros

  (define-syntax recv
    (syntax-rules ()
      [(recv body)
       (begin body)]
      [(recv [(var ...) expr] . more)
       (let-values ([(var ...) expr]) (recv . more))]))

  (define-syntax (>>P stx)
    (syntax-case stx ()
      [(>>P pr (constructor var ...) pattern . clauses)
       #'(>>PrimI pr #t (constructor var ...) pattern pattern . clauses)]))

  ;; >>P with no restamping
  (define-syntax (>>Pn stx)
    (syntax-case stx ()
      [(>>Pn pr (constructor var ...) pattern . clauses)
       #'(>>PrimI pr #f (constructor var ...) pattern pattern . clauses)]))

  (define-syntax (>>PrimI stx)
    (syntax-case stx ()
      [(>>PrimI pr restamp? cons+vars inp outp . clauses)
       #'(let ([prvar pr])
           (>>Prim prvar (deriv-e1 prvar) restamp? cons+vars inp outp . clauses))]))

  (define-syntax (>>Prim stx)
    (syntax-case stx ()
      [(>>Prim pr e1 restamp? cons+vars inp outp clauses)
       #'(>>Prim pr e1 restamp? cons+vars inp outp clauses #:with values)]
      [(>>Prim pr e1 restamp? cons+vars inp outp clauses #:with transform)
       #'(>>Prim pr e1 restamp? cons+vars inp outp clauses
                 #:with transform #:with2 values)]
      [(>>Prim pr given-e1 restamp? cons+vars inp outp clauses #:with2 transform)
       #'(>>Prim pr given-e1 restamp? cons+vars inp outp clauses #:with values #:with2 transform)]
      [(>>Prim pr given-e1 restamp? (constructor var ...)
               in-pattern
               out-pattern
               ([recur hole fill/bind] ...)
               #:with stransform
               #:with2 transform)
       (let ([restamp? (syntax-e #'restamp?)])
         (with-syntax ([(s-tmp ...) (generate-temporaries #'(fill/bind ...))])
           #`(let ([prule-var pr])
               (let-values ([(fill/bind s-tmp)
                             (let ([fbvar fill/bind])
                               (if fbvar (recur fbvar) (values fbvar #f)))] ...)
                 (let ([new-e2
                        (stransform
                         (if (or (interrupted-wrap? prule-var) (error-wrap? prule-var))
                             #f
                             (with-syntax ([in-pattern given-e1])
                               (with-syntax ([hole s-tmp] ...)
                                 #,(if restamp?
                                       #'(syntax/restamp out-pattern #'out-pattern
                                                         (deriv-e2 prule-var))
                                       #'#'out-pattern)))))])
                   (let ([new-pr
                          (match prule-var
                            [(AnyQ prule (e1 _ rs))
                             (constructor e1 new-e2 rs var ...)])])
                     (let-values ([(new-pr new-e2) (transform new-pr new-e2)])
                       (values (rewrap prule-var new-pr)
                               new-e2))))))))]))

  (define-syntax >>Seek
    (syntax-rules (! =>)
      [(>>Seek) null]
      [(>>Seek [! tag exni] . more)
       (if (and (pair? exni) (eq? tag (car exni)))
           null
           (>>Seek . more))]
      [(>>Seek [! exni] . more)
       (if (pair? exni) null (>>Seek . more))]
      [(>>Seek [#:append expr] . more)
       (append (apply append expr) (>>Seek . more))]
      [(>>Seek [#:table t] . more)
       (parameterize ((subterms-table t)) (>>Seek . more))]
      [(>>Seek [#:rename expr] . more)
       (let-values ([(subterms new-table) expr])
         (parameterize ((subterms-table new-table))
           (append subterms (>>Seek . more))))]
      [(>>Seek expr . more)
       (append expr (>>Seek . more))]))

  )
