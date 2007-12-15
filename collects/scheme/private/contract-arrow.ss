(module contract-arrow mzscheme
  (require (lib "etc.ss")
           "contract-guts.ss"
           "contract-arr-checks.ss"
           "contract-opt.ss")
  (require-for-syntax "contract-opt-guts.ss"
                      "contract-helpers.ss"
                      "contract-arr-obj-helpers.ss"
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))

  (provide ->
           ->d
           ->*
           ->d*
           ->r
           ->pp
           ->pp-rest
           case->
	   opt->
           opt->*
           unconstrained-domain->
           check-procedure)
  
  (define-syntax (unconstrained-domain-> stx)
    (syntax-case stx ()
      [(_ rngs ...)
       (with-syntax ([(rngs-x ...) (generate-temporaries #'(rngs ...))]
                     [(proj-x ...) (generate-temporaries #'(rngs ...))]
                     [(p-app-x ...) (generate-temporaries #'(rngs ...))]
                     [(res-x ...) (generate-temporaries #'(rngs ...))])
         #'(let ([rngs-x (coerce-contract 'unconstrained-domain-> rngs)] ...)
             (let ([proj-x ((proj-get rngs-x) rngs-x)] ...)
               (make-proj-contract
                (build-compound-type-name 'unconstrained-domain-> ((name-get rngs-x) rngs-x) ...)
                (λ (pos-blame neg-blame src-info orig-str)
                  (let ([p-app-x (proj-x pos-blame neg-blame src-info orig-str)] ...)
                    (λ (val)
                      (if (procedure? val)
                          (λ args
                            (let-values ([(res-x ...) (apply val args)])
                              (values (p-app-x res-x) ...)))
                          (raise-contract-error val
                                                src-info
                                                pos-blame
                                                orig-str
                                                "expected a procedure")))))
                procedure?))))]))
  
  ;; FIXME: need to pass in the name of the contract combinator.
  (define (build--> name doms doms-rest rngs rng-any? func)
    (let ([doms/c (map (λ (dom) (coerce-contract name dom)) doms)]
          [rngs/c (map (λ (rng) (coerce-contract name rng)) rngs)]
          [doms-rest/c (and doms-rest (coerce-contract name doms-rest))])
      (make--> rng-any? doms/c doms-rest/c rngs/c func)))
  
  (define-struct/prop -> (rng-any? doms dom-rest rngs func)
    ((proj-prop (λ (ctc) 
                  (let* ([doms/c (map (λ (x) ((proj-get x) x)) 
                                      (if (->-dom-rest ctc)
                                          (append (->-doms ctc) (list (->-dom-rest ctc)))
                                          (->-doms ctc)))]
                         [rngs/c (map (λ (x) ((proj-get x) x)) (->-rngs ctc))]
                         [func (->-func ctc)]
                         [dom-length (length (->-doms ctc))]
                         [check-proc
                          (if (->-dom-rest ctc)
                              check-procedure/more
                              check-procedure)])
                    (lambda (pos-blame neg-blame src-info orig-str)
                      (let ([partial-doms (map (λ (dom) (dom neg-blame pos-blame src-info orig-str))
                                               doms/c)]
                            [partial-ranges (map (λ (rng) (rng pos-blame neg-blame src-info orig-str))
                                                 rngs/c)])
                        (apply func
                               (λ (val) (check-proc val dom-length src-info pos-blame orig-str))
                               (append partial-doms partial-ranges)))))))
     (name-prop (λ (ctc) (single-arrow-name-maker 
                          (->-doms ctc)
                          (->-dom-rest ctc)
                          (->-rng-any? ctc)
                          (->-rngs ctc))))
     (first-order-prop
      (λ (ctc)
        (let ([l (length (->-doms ctc))])
          (if (->-dom-rest ctc)
              (λ (x)
                (and (procedure? x) 
                     (procedure-accepts-and-more? x l)))
              (λ (x)
                (and (procedure? x) 
                     (procedure-arity-includes? x l)))))))
     (stronger-prop
      (λ (this that)
        (and (->? that)
             (= (length (->-doms that))
                (length (->-doms this)))
             (andmap contract-stronger?
                     (->-doms that)
                     (->-doms this))
             (= (length (->-rngs that))
                (length (->-rngs this)))
             (andmap contract-stronger?
                     (->-rngs this) 
                     (->-rngs that)))))))
  
  (define (single-arrow-name-maker doms/c doms-rest rng-any? rngs)
    (cond
      [doms-rest
       (build-compound-type-name 
        '->*
        (apply build-compound-type-name doms/c)
        doms-rest
        (cond
          [rng-any? 'any]
          [else (apply build-compound-type-name rngs)]))]
      [else
       (let ([rng-name
              (cond
                [rng-any? 'any]
                [(null? rngs) '(values)]
                [(null? (cdr rngs)) (car rngs)]
                [else (apply build-compound-type-name 'values rngs)])])
         (apply build-compound-type-name '-> (append doms/c (list rng-name))))]))
  
  (define arity-one-wrapper
    (lambda (chk a3 c5) (lambda (val) (chk val) (lambda (a1) (c5 (val (a3 a1)))))))
  
  (define arity-two-wrapper
    (lambda (chk a3 b4 c5) (lambda (val) (chk val) (lambda (a1 b2) (c5 (val (a3 a1) (b4 b2)))))))
  
  (define arity-three-wrapper
    (lambda (chk a9 b10 c11 r12) (lambda (val) (chk val) (lambda (a6 b7 c8) (r12 (val (a9 a6) (b10 b7) (c11 c8)))))))
  
  (define arity-four-wrapper
    (lambda (chk a17 b18 c19 d20 r21) (lambda (val) (chk val) (lambda (a13 b14 c15 d16) (r21 (val (a17 a13) (b18 b14) (c19 c15) (d20 d16)))))))
  
  (define arity-five-wrapper
      (lambda (chk a27 b28 c29 d30 e31 r32)
        (lambda (val) (chk val) (lambda (a22 b23 c24 d25 e26) (r32 (val (a27 a22) (b28 b23) (c29 c24) (d30 d25) (e31 e26)))))))
  
  (define arity-six-wrapper
    (lambda (chk a39 b40 c41 d42 e43 f44 r45)
      (lambda (val) (chk val) (lambda (a33 b34 c35 d36 e37 f38) (r45 (val (a39 a33) (b40 b34) (c41 c35) (d42 d36) (e43 e37) (f44 f38)))))))
  
  (define arity-seven-wrapper
    (lambda (chk a53 b54 c55 d56 e57 f58 g59 r60)
      (lambda (val) (chk val) (lambda (a46 b47 c48 d49 e50 f51 g52) (r60 (val (a53 a46) (b54 b47) (c55 c48) (d56 d49) (e57 e50) (f58 f51) (g59 g52)))))))
  
  (define-syntax-set (-> ->*)
    (define (->/proc stx) 
      (let-values ([(stx _1 _2) (->/proc/main stx)])
        stx))
    
    ;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
    (define (->/proc/main stx)
      (let-values ([(dom-names rng-names dom-ctcs rng-ctcs inner-args/body use-any?) (->-helper stx)])
        (with-syntax ([(args body) inner-args/body])
          (with-syntax ([(dom-names ...) dom-names]
                        [(rng-names ...) rng-names]
                        [(dom-ctcs ...) dom-ctcs]
                        [(rng-ctcs ...) rng-ctcs]
                        [inner-lambda 
                         (add-name-prop
                          (syntax-local-infer-name stx)
                          (syntax (lambda args body)))]
                        [use-any? use-any?])
            (with-syntax ([outer-lambda
                           (let* ([lst (syntax->list #'args)]
                                  [len (and lst (length lst))])
                             (if (and #f ;; this optimization disables the names so is turned off for now
				      lst
                                      (not (syntax-e #'use-any?))
                                      (= len (length (syntax->list #'(dom-names ...))))
                                      (= 1 (length (syntax->list #'(rng-names ...))))
                                      (<= 1 len 7))
                                 (case len
                                   [(1) #'arity-one-wrapper]
                                   [(2) #'arity-two-wrapper]
                                   [(3) #'arity-three-wrapper]
                                   [(4) #'arity-four-wrapper]
                                   [(5) #'arity-five-wrapper]
                                   [(6) #'arity-six-wrapper]
                                   [(7) #'arity-seven-wrapper])
                                 (syntax
                                  (lambda (chk dom-names ... rng-names ...)
                                    (lambda (val)
                                      (chk val)
                                      inner-lambda)))))])
              (values
               (syntax (build--> '->
                                 (list dom-ctcs ...)
                                 #f
                                 (list rng-ctcs ...)
                                 use-any?
                                 outer-lambda))
               inner-args/body
               (syntax (dom-names ... rng-names ...))))))))
    
    (define (->-helper stx)
      (syntax-case* stx (-> any values) module-or-top-identifier=?
        [(-> doms ... any)
         (with-syntax ([(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))]
                       [(ignored) (generate-temporaries (syntax (rng)))])
           (values (syntax (dom-ctc ...))
                   (syntax (ignored))
                   (syntax (doms ...))
                   (syntax (any/c))
                   (syntax ((args ...) (val (dom-ctc args) ...)))
                   #t))]
        [(-> doms ... (values rngs ...))
         (with-syntax ([(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))]
                       [(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                       [(rng-ctc ...) (generate-temporaries (syntax (rngs ...)))])
           (values (syntax (dom-ctc ...))
                   (syntax (rng-ctc ...))
                   (syntax (doms ...))
                   (syntax (rngs ...))
                   (syntax ((args ...) 
                            (let-values ([(rng-x ...) (val (dom-ctc args) ...)])
                              (values (rng-ctc rng-x) ...))))
                   #f))]
        [(_ doms ... rng)
         (with-syntax ([(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))]
                       [(rng-ctc) (generate-temporaries (syntax (rng)))])
           (values (syntax (dom-ctc ...))
                   (syntax (rng-ctc))
                   (syntax (doms ...))
                   (syntax (rng))
                   (syntax ((args ...) (rng-ctc (val (dom-ctc args) ...))))
                   #f))]))
    
    (define (->*/proc stx) 
      (let-values ([(stx _1 _2) (->*/proc/main stx)])
        stx))
    
    ;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
    (define (->*/proc/main stx)
      (syntax-case* stx (->* any) module-or-top-identifier=?
        [(->* (doms ...) any)
         (->/proc/main (syntax (-> doms ... any)))]
        [(->* (doms ...) (rngs ...))
         (->/proc/main (syntax (-> doms ... (values rngs ...))))]
        [(->* (doms ...) rst (rngs ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (doms ...)))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(rst-x) (generate-temporaries (syntax (rst)))]
                       [(rest-arg) (generate-temporaries (syntax (rst)))]
                       [(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                       [(rng-args ...) (generate-temporaries (syntax (rngs ...)))])
           (let ([inner-args/body 
                  (syntax ((args ... . rest-arg)
                           (let-values ([(rng-args ...) (apply val (dom-x args) ... (rst-x rest-arg))])
                             (values (rng-x rng-args) ...))))])
             (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                           (add-name-prop
                                            (syntax-local-infer-name stx)
                                            (syntax (lambda args body))))])
               (with-syntax ([outer-lambda 
                              (syntax
                               (lambda (chk dom-x ... rst-x rng-x ...)
                                 (lambda (val)
                                   (chk val)
                                   inner-lambda)))])
                 (values (syntax (build--> '->*
                                           (list doms ...)
                                           rst
                                           (list rngs ...)
                                           #f 
                                           outer-lambda))
                         inner-args/body
                         (syntax (dom-x ... rst-x rng-x ...)))))))]
        [(->* (doms ...) rst any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (doms ...)))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(rst-x) (generate-temporaries (syntax (rst)))]
                       [(rest-arg) (generate-temporaries (syntax (rst)))])
           (let ([inner-args/body 
                  (syntax ((args ... . rest-arg) 
                           (apply val (dom-x args) ... (rst-x rest-arg))))])
             (with-syntax ([inner-lambda (with-syntax ([(args body) inner-args/body])
                                           (add-name-prop
                                            (syntax-local-infer-name stx)
                                            (syntax (lambda args body))))])
               (with-syntax ([outer-lambda 
                              (syntax
                               (lambda (chk dom-x ... rst-x ignored)
                                 (lambda (val)
                                   (chk val)
                                   inner-lambda)))])
                 (values (syntax (build--> '->*
                                           (list doms ...)
                                           rst
                                           (list any/c)
                                           #t
                                           outer-lambda))
                         inner-args/body
                         (syntax (dom-x ... rst-x)))))))])))
  
  (define-for-syntax (select/h stx err-name ctxt-stx)
    (syntax-case stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
      [(-> . args) ->/h]
      [(->* . args) ->*/h]
      [(->d . args) ->d/h]
      [(->d* . args) ->d*/h]
      [(->r . args) ->r/h]
      [(->pp . args) ->pp/h]
      [(->pp-rest . args) ->pp-rest/h]
      [(xxx . args) (raise-syntax-error err-name "unknown arrow constructor" ctxt-stx (syntax xxx))]
      [_ (raise-syntax-error err-name "malformed arrow clause" ctxt-stx stx)]))
  
  (define-syntax (->d stx) (make-/proc #f ->d/h stx))
  (define-syntax (->d* stx) (make-/proc #f ->d*/h stx))
  (define-syntax (->r stx) (make-/proc #f ->r/h stx))
  (define-syntax (->pp stx) (make-/proc #f ->pp/h stx))
  (define-syntax (->pp-rest stx) (make-/proc #f ->pp-rest/h stx))
  (define-syntax (case-> stx) (make-case->/proc #f stx stx select/h))
  (define-syntax (opt-> stx) (make-opt->/proc #f stx select/h #'case-> #'->))
  (define-syntax (opt->* stx) (make-opt->*/proc #f stx stx select/h #'case-> #'->))

  ;;
  ;; arrow opter
  ;;
  (define/opter (-> opt/i opt/info stx)
    (define (opt/arrow-ctc doms rngs)
      (let*-values ([(dom-vars rng-vars) (values (generate-temporaries doms)
                                                 (generate-temporaries rngs))]
                    [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                     (let loop ([vars dom-vars]
                                [doms doms]
                                [next-doms null]
                                [lifts-doms null]
                                [superlifts-doms null]
                                [partials-doms null]
                                [stronger-ribs null])
                       (cond
                         [(null? doms) (values (reverse next-doms)
                                               lifts-doms
                                               superlifts-doms
                                               partials-doms
                                               stronger-ribs)]
                         [else
                          (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                        (opt/i (opt/info-swap-blame opt/info) (car doms))])
                            (loop (cdr vars)
                                  (cdr doms)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-doms)
                                  (append lifts-doms lift)
                                  (append superlifts-doms superlift)
                                  (append partials-doms partial)
                                  (append this-stronger-ribs stronger-ribs)))]))]
                    [(next-rngs lifts-rngs superlifts-rngs partials-rngs stronger-ribs-rng)
                     (let loop ([vars rng-vars]
                                [rngs rngs]
                                [next-rngs null]
                                [lifts-rngs null]
                                [superlifts-rngs null]
                                [partials-rngs null]
                                [stronger-ribs null])
                       (cond
                         [(null? rngs) (values (reverse next-rngs)
                                               lifts-rngs
                                               superlifts-rngs
                                               partials-rngs
                                               stronger-ribs)]
                         [else
                          (let-values ([(next lift superlift partial _ __ this-stronger-ribs)
                                        (opt/i opt/info (car rngs))])
                            (loop (cdr vars)
                                  (cdr rngs)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-rngs)
                                  (append lifts-rngs lift)
                                  (append superlifts-rngs superlift)
                                  (append partials-rngs partial)
                                  (append this-stronger-ribs stronger-ribs)))]))])
        (values
         (with-syntax ((pos (opt/info-pos opt/info))
                       (src-info (opt/info-src-info opt/info))
                       (orig-str (opt/info-orig-str opt/info))
                       ((dom-arg ...) dom-vars)
                       ((rng-arg ...) rng-vars)
                       ((next-dom ...) next-doms)
                       (dom-len (length dom-vars))
                       ((next-rng ...) next-rngs))
           (syntax (begin
                     (check-procedure val dom-len src-info pos orig-str)
                     (λ (dom-arg ...)
                       (let-values ([(rng-arg ...) (val next-dom ...)])
                         (values next-rng ...))))))
         (append lifts-doms lifts-rngs)
         (append superlifts-doms superlifts-rngs)
         (append partials-doms partials-rngs)
         #f
         #f
         (append stronger-ribs-dom stronger-ribs-rng))))
    
    (define (opt/arrow-any-ctc doms)
      (let*-values ([(dom-vars) (generate-temporaries doms)]
                    [(next-doms lifts-doms superlifts-doms partials-doms stronger-ribs-dom)
                     (let loop ([vars dom-vars]
                                [doms doms]
                                [next-doms null]
                                [lifts-doms null]
                                [superlifts-doms null]
                                [partials-doms null]
                                [stronger-ribs null])
                       (cond
                         [(null? doms) (values (reverse next-doms)
                                               lifts-doms
                                               superlifts-doms
                                               partials-doms
                                               stronger-ribs)]
                         [else
                          (let-values ([(next lift superlift partial flat _ this-stronger-ribs)
                                        (opt/i (opt/info-swap-blame opt/info) (car doms))])
                            (loop (cdr vars)
                                  (cdr doms)
                                  (cons (with-syntax ((next next)
                                                      (car-vars (car vars)))
                                          (syntax (let ((val car-vars)) next)))
                                        next-doms)
                                  (append lifts-doms lift)
                                  (append superlifts-doms superlift)
                                  (append partials-doms partial)
                                  (append this-stronger-ribs stronger-ribs)))]))])
        (values
         (with-syntax ((pos (opt/info-pos opt/info))
                       (src-info (opt/info-src-info opt/info))
                       (orig-str (opt/info-orig-str opt/info))
                       ((dom-arg ...) dom-vars)
                       ((next-dom ...) next-doms)
                       (dom-len (length dom-vars)))
           (syntax (begin
                     (check-procedure val dom-len src-info pos orig-str)
                     (λ (dom-arg ...)
                       (val next-dom ...)))))
         lifts-doms
         superlifts-doms
         partials-doms
         #f
         #f
         stronger-ribs-dom)))
    
    (syntax-case* stx (-> values any) module-or-top-identifier=?
      [(-> dom ... (values rng ...))
       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                      (syntax->list (syntax (rng ...))))]
      [(-> dom ... any)
       (opt/arrow-any-ctc (syntax->list (syntax (dom ...))))]
      [(-> dom ... rng)
       (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                      (list #'rng))])))
