#lang racket/base
(require syntax/stx 
         syntax/name)

(require (for-syntax racket/base))
(require (for-template racket/base)
         (for-template racket/contract/private/guts
                       racket/contract/private/misc
                       racket/contract/private/prop
                       racket/contract/private/blame)
         (for-template "contract-arr-checks.rkt"))

(provide make-/proc ->/h ->*/h ->d/h ->d*/h ->r/h 
         ->pp/h ->pp-rest/h 
         make-case->/proc 
         make-opt->/proc make-opt->*/proc)

;; make-/proc : boolean
;;              (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
;;              syntax
;;           -> (syntax -> syntax)
(define (make-/proc method-proc? /h stx)
  (let-values ([(arguments-check build-proj check-val first-order-check wrapper)
                (/h method-proc? stx)])
    (let ([outer-args (syntax (val blame name-id))])
      (with-syntax ([inner-check (check-val outer-args)]
                    [(val blame name-id) outer-args]
                    [(val-args body) (wrapper outer-args)])
        (with-syntax ([inner-lambda
                       (set-inferred-name-from
                        stx
                        (syntax/loc stx (lambda val-args body)))])
          (let ([inner-lambda
                 (syntax
                  (lambda (val)
                    inner-check
                    inner-lambda))])
            (with-syntax ([proj-code (build-proj outer-args inner-lambda)]
                          [first-order-check first-order-check])
              (arguments-check
               outer-args
               (syntax/loc stx
                 (make-contract
                  #:name name-id
                  #:projection (lambda (blame) proj-code)
                  #:first-order first-order-check))))))))))

(define (make-case->/proc method-proc? stx inferred-name-stx select/h)
  (syntax-case stx ()
    
    ;; if there are no cases, this contract should only accept the "empty" case-lambda.
    [(_) (syntax empty-case-lambda/c)]
    
    ;; if there is only a single case, just skip it.
    [(_ case) (syntax case)]
    
    [(_ cases ...)
     (let-values ([(arguments-check build-projs check-val first-order-check wrapper) 
                   (case->/h method-proc? stx (syntax->list (syntax (cases ...))) select/h)])
       (let ([outer-args (syntax (val blame name-id))])
         (with-syntax ([(inner-check ...) (check-val outer-args)]
                       [(val blame name-id) outer-args]
                       [(body ...) (wrapper outer-args)])
           (with-syntax ([inner-lambda 
                          (set-inferred-name-from
                           inferred-name-stx
                           (syntax/loc stx (case-lambda body ...)))])
             (let ([inner-lambda
                    (syntax
                     (lambda (val)
                       inner-check ...
                       inner-lambda))])
               (with-syntax ([proj-code (build-projs outer-args inner-lambda)]
                             [first-order-check first-order-check])
                 (arguments-check
                  outer-args
                  (syntax/loc stx
                    (make-contract
                     #:name (apply build-compound-type-name 'case-> name-id)
                     #:projection (lambda (blame) proj-code)
                     #:first-order first-order-check)))))))))]))

(define (make-opt->/proc method-proc? stx select/h case-arr-stx arr-stx)
  (syntax-case stx (any)
    [(_ (reqs ...) (opts ...) any)
     (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) any)) stx select/h case-arr-stx arr-stx)]
    [(_ (reqs ...) (opts ...) res)
     (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) (res))) stx select/h case-arr-stx arr-stx)]))

(define (make-opt->*/proc method-proc? stx inferred-name-stx select/h case-arr-stx arr-stx)
  (syntax-case stx (any)
    [(_ (reqs ...) (opts ...) any)
     (let* ([req-vs (generate-temporaries (syntax->list (syntax (reqs ...))))]
            [opt-vs (generate-temporaries (syntax->list (syntax (opts ...))))]
            [cses
             (reverse
              (let loop ([opt-vs (reverse opt-vs)])
                (cond
                  [(null? opt-vs) (list req-vs)]
                  [else (cons (append req-vs (reverse opt-vs))
                              (loop (cdr opt-vs)))])))])
       (with-syntax ([(req-vs ...) req-vs]
                     [(opt-vs ...) opt-vs]
                     [((case-doms ...) ...) cses])
         (with-syntax ([expanded-case->
                        (make-case->/proc
                         method-proc?
                         (with-syntax ([case-> case-arr-stx]
                                       [-> arr-stx])
                           (syntax (case-> (-> case-doms ... any) ...)))
                         inferred-name-stx
                         select/h)])
           (syntax/loc stx
             (let ([req-vs reqs] ...
                   [opt-vs opts] ...)
               expanded-case->)))))]
    [(_ (reqs ...) (opts ...) (ress ...))
     (let* ([res-vs (generate-temporaries (syntax->list (syntax (ress ...))))]
            [req-vs (generate-temporaries (syntax->list (syntax (reqs ...))))]
            [opt-vs (generate-temporaries (syntax->list (syntax (opts ...))))]
            [cses
             (reverse
              (let loop ([opt-vs (reverse opt-vs)])
                (cond
                  [(null? opt-vs) (list req-vs)]
                  [else (cons (append req-vs (reverse opt-vs))
                              (loop (cdr opt-vs)))])))])
       (with-syntax ([(res-vs ...) res-vs]
                     [(req-vs ...) req-vs]
                     [(opt-vs ...) opt-vs]
                     [((case-doms ...) ...) cses])
         (with-syntax ([(single-case-result ...)
                        (let* ([ress-lst (syntax->list (syntax (ress ...)))]
                               [only-one?
                                (and (pair? ress-lst)
                                     (null? (cdr ress-lst)))])
                          (map
                           (if only-one?
                               (lambda (x) (car (syntax->list (syntax (res-vs ...)))))
                               (lambda (x) (syntax (values res-vs ...))))
                           cses))])
           (with-syntax ([expanded-case->
                          (make-case->/proc
                           method-proc?
                           (with-syntax ([case-> case-arr-stx]
                                         [-> arr-stx])
                             (syntax (case-> (-> case-doms ... single-case-result) ...)))
                           inferred-name-stx
                           select/h)])
             (set-inferred-name-from
              stx
              (syntax/loc stx
                (let ([res-vs ress] 
                      ...
                      [req-vs reqs]
                      ...
                      [opt-vs opts]
                      ...)
                  expanded-case->)))))))]))

;; exactract-argument-lists : syntax -> (listof syntax)
(define (extract-argument-lists stx)
  (map (lambda (x)
         (syntax-case x ()
           [(arg-list body) (syntax arg-list)]))
       (syntax->list stx)))

;; ensure-cases-disjoint : syntax syntax[list] -> void
(define (ensure-cases-disjoint stx cases)
  (let ([individual-cases null]
        [dot-min #f])
    (for-each (lambda (case)
                (let ([this-case (get-case case)])
                  (cond
                    [(number? this-case) 
                     (cond
                       [(member this-case individual-cases)
                        (raise-syntax-error
                         'case-> 
                         (format "found multiple cases with ~a arguments" this-case)
                         stx)]
                       [(and dot-min (dot-min . <= . this-case))
                        (raise-syntax-error 
                         'case-> 
                         (format "found overlapping cases (~a+ followed by ~a)" dot-min this-case)
                         stx)]
                       [else (set! individual-cases (cons this-case individual-cases))])]
                    [(pair? this-case)
                     (let ([new-dot-min (car this-case)])
                       (cond
                         [dot-min
                          (if (dot-min . <= . new-dot-min)
                              (raise-syntax-error
                               'case->
                               (format "found overlapping cases (~a+ followed by ~a+)" dot-min new-dot-min)
                               stx)
                              (set! dot-min new-dot-min))]
                         [else
                          (set! dot-min new-dot-min)]))])))
              cases)))

;; get-case : syntax -> (union number (cons number 'more))
(define (get-case stx)
  (let ([ilist (syntax->datum stx)])
    (if (list? ilist)
        (length ilist)
        (cons 
         (let loop ([i ilist])
           (cond
             [(pair? i) (+ 1 (loop (cdr i)))]
             [else 0]))
         'more))))


;; case->/h : boolean
;;            syntax
;;            (listof syntax) 
;;            select/h
;;         -> (values (syntax -> syntax)
;;                    (syntax -> syntax)
;;                    (syntax -> syntax)
;;                    (syntax syntax -> syntax) 
;;                    (syntax -> syntax)
;;                    (syntax -> syntax))
;; like the other /h functions, but composes the wrapper functions
;; together and combines the cases of the case-lambda into a single list.
(define (case->/h method-proc? orig-stx cases select/h)
  (let loop ([cases cases]
             [name-ids '()])
    (cond
      [(null? cases) 
       (values 
        (lambda (outer-args body)
          (with-syntax ([(val blame name-id) outer-args]
                        [body body]
                        [(name-ids ...) (reverse name-ids)])
            (syntax
             (let ([name-id (list name-ids ...)])
               body))))
        (lambda (x y) y)
        (lambda (args) (syntax ()))
        (syntax (lambda (x) #t))
        (lambda (args) (syntax ())))]
      [else
       (let ([/h (select/h (car cases) 'case-> orig-stx)]
             [new-id (car (generate-temporaries (syntax (case->name-id))))])
         (let-values ([(arguments-checks build-projs check-vals first-order-checks wrappers)
                       (loop (cdr cases) (cons new-id name-ids))]
                      [(arguments-check build-proj check-val first-order-check wrapper)
                       (/h method-proc? (car cases))])
           (values
            (lambda (outer-args x) 
              (with-syntax ([(val blame name-id) outer-args]
                            [new-id new-id])
                (arguments-check 
                 (syntax (val blame new-id)) 
                 (arguments-checks 
                  outer-args
                  x))))
            (lambda (args inner) (build-projs args (build-proj args inner)))
            (lambda (args)
              (with-syntax ([checks (check-vals args)]
                            [check (check-val args)])
                (syntax (check . checks))))
            (with-syntax ([checks first-order-checks]
                          [check first-order-check])
              (syntax (lambda (x) (and (checks x) (check x)))))
            (lambda (args)
              (with-syntax ([case (wrapper args)]
                            [cases (wrappers args)])
                (syntax (case . cases)))))))])))

;; ensure-no-duplicates : syntax (listof syntax[identifier]) -> void
(define (ensure-no-duplicates stx form-name names)
  (let ([ht (make-hasheq)])
    (for-each (lambda (name)
                (let ([key (syntax-e name)])
                  (when (hash-ref ht key (lambda () #f))
                    (raise-syntax-error form-name
                                        "duplicate method name"
                                        stx
                                        name))
                  (hash-set! ht key #t)))
              names)))

;; method-specifier? : syntax -> boolean
;; returns #t if x is the syntax for a valid method specifier
(define (method-specifier? x)
  (or (eq? 'public (syntax-e x))
      (eq? 'override (syntax-e x))))

;; prefix-super : syntax[identifier] -> syntax[identifier]
;; adds super- to the front of the identifier
(define (prefix-super stx)
  (datum->syntax
   #'here
   (string->symbol
    (format 
     "super-~a"
     (syntax->datum
      stx)))))

;; method-name->contract-method-name : syntax[identifier] -> syntax[identifier]
;; given the syntax for a method name, constructs the name of a method
;; that returns the super's contract for the original method.
(define (method-name->contract-method-name stx)
  (datum->syntax
   #'here
   (string->symbol
    (format 
     "ACK_DONT_GUESS_ME-super-contract-~a"
     (syntax->datum
      stx)))))

;; Each of the /h functions builds six pieces of syntax:
;;  - [arguments-check]
;;    code that binds the contract values to names and
;;    does error checking for the contract specs
;;    (were the arguments all contracts?)
;;  - [build-proj]
;;    code that partially applies the input contracts to build the projection
;;  - [check-val]
;;    code that does error checking on the contract'd value itself
;;    (is it a function of the right arity?)
;;  - [first-order-check]
;;    predicate function that does the first order check and returns a boolean
;;    (is it a function of the right arity?)
;;  - [pos-wrapper]
;;    a piece of syntax that has the arguments to the wrapper
;;    and the body of the wrapper.
;;  - [neg-wrapper]
;;    a piece of syntax that has the arguments to the wrapper
;;    and the body of the wrapper.
;; the first function accepts a body expression and wraps
;;    the body expression with checks. In addition, it
;;    adds a let that binds the contract exprssions to names
;;    the results of the other functions mention these names.
;; the second and third function's input syntax should be five
;;    names: val, blame, src-info, orig-str, name-id
;; the fourth function returns a syntax list with two elements,
;;    the argument list (to be used as the first arg to lambda,
;;    or as a case-lambda clause) and the body of the function.
;; They are combined into a lambda for the -> ->* ->d ->d* macros,
;; and combined into a case-lambda for the case-> macro.

;; ->/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->/h method-proc? stx)
  (syntax-case stx ()
    [(_) (raise-syntax-error '-> "expected at least one argument" stx)]
    [(_ dom ... rng)
     (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-length dom-index ...) (generate-indices (syntax (dom ...)))]
                   [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
       (with-syntax ([(name-dom-contract-x ...)
                      (if method-proc?
                          (cdr
                           (syntax->list
                            (syntax (dom-contract-x ...))))
                          (syntax (dom-contract-x ...)))])
         (syntax-case* (syntax rng) (any values) module-or-top-identifier=?
           [any 
            (values
             (lambda (outer-args body)
               (with-syntax ([body body]
                             [(val blame name-id) outer-args])
                 (syntax
                  (let ([dom-contract-x (coerce-contract '-> dom)] ...)
                    (let ([dom-x (contract-projection dom-contract-x)] ...)
                      (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... 'any)])
                        body))))))
             
             ;; proj
             (lambda (outer-args inner-lambda)
               (with-syntax ([(val blame name-id) outer-args]
                             [inner-lambda inner-lambda])
                 (syntax
                  (let ([dom-projection-x (dom-x (blame-swap blame))] ...)
                    inner-lambda))))
             
             (lambda (outer-args)
               (with-syntax ([(val blame name-id) outer-args])
                 (syntax
                  (check-procedure val dom-length 0 '() '() #|keywords|# blame))))
             (syntax (check-procedure? dom-length))
             (lambda (outer-args)
               (with-syntax ([(val blame name-id) outer-args])
                 (syntax
                  ((arg-x ...)
                   (val (dom-projection-x arg-x) ...))))))]
           [(values rng ...)
            (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                          [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                          [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                          [(rng-length rng-index ...) (generate-indices (syntax (rng ...)))]
                          [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                          [(res-x ...) (generate-temporaries (syntax (rng ...)))])
              (values
               (lambda (outer-args body)
                 (with-syntax ([body body]
                               [(val blame name-id) outer-args])
                   (syntax
                    (let ([dom-contract-x (coerce-contract '-> dom)] 
                          ...
                          [rng-contract-x (coerce-contract '-> rng)] ...)
                      (let ([dom-x (contract-projection dom-contract-x)] 
                            ...
                            [rng-x (contract-projection rng-contract-x)]
                            ...)
                        (let ([name-id 
                               (build-compound-type-name 
                                '-> 
                                name-dom-contract-x ...
                                (build-compound-type-name 'values rng-contract-x ...))])
                          body))))))
               
               ;; proj
               (lambda (outer-args inner-lambda)
                 (with-syntax ([(val blame name-id) outer-args]
                               [inner-lambda inner-lambda])
                   (syntax
                    (let ([dom-projection-x (dom-x (blame-swap blame))] 
                          ...
                          [rng-projection-x (rng-x blame)] ...)
                      inner-lambda))))
               
               (lambda (outer-args)
                 (with-syntax ([(val blame name-id) outer-args])
                   (syntax
                    (check-procedure val dom-length 0 '() '() #|keywords|# blame))))
               (syntax (check-procedure? dom-length))
               
               (lambda (outer-args)
                 (with-syntax ([(val blame name-id) outer-args])
                   (syntax
                    ((arg-x ...)
                     (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                       (values (rng-projection-x
                                res-x)
                               ...))))))))]
           [rng
            (with-syntax ([(rng-x) (generate-temporaries (syntax (rng)))]
                          [(rng-contact-x) (generate-temporaries (syntax (rng)))]
                          [(rng-projection-x) (generate-temporaries (syntax (rng)))]
                          [(rng-ant-x) (generate-temporaries (syntax (rng)))]
                          [(res-x) (generate-temporaries (syntax (rng)))])
              (values
               (lambda (outer-args body)
                 (with-syntax ([body body]
                               [(val blame name-id) outer-args])
                   (syntax
                    (let ([dom-contract-x (coerce-contract '-> dom)] 
                          ...
                          [rng-contract-x (coerce-contract '-> rng)])
                      (let ([dom-x (contract-projection dom-contract-x)] 
                            ...
                            [rng-x (contract-projection rng-contract-x)])
                        (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... rng-contract-x)])
                          body))))))
               
               ;; proj
               (lambda (outer-args inner-lambda)
                 (with-syntax ([(val blame name-id) outer-args]
                               [inner-lambda inner-lambda])
                   (syntax
                    (let ([dom-projection-x (dom-x (blame-swap blame))] 
                          ...
                          [rng-projection-x (rng-x blame)])
                      inner-lambda))))
               
               (lambda (outer-args)
                 (with-syntax ([(val blame name-id) outer-args])
                   (syntax
                    (check-procedure val dom-length 0 '() '() #|keywords|# blame))))
               (syntax (check-procedure? dom-length))
               (lambda (outer-args)
                 (with-syntax ([(val blame name-id) outer-args])
                   (syntax
                    ((arg-x ...)
                     (let ([res-x (val (dom-projection-x arg-x) ...)])
                       (rng-projection-x res-x))))))))])))]))

;; ->*/h : boolean stx -> (values (syntax -> syntax) (syntax syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->*/h method-proc? stx)
  (syntax-case stx (any)
    [(_ (dom ...) (rng ...))
     (->/h method-proc? (syntax (-> dom ... (values rng ...))))]
    [(_ (dom ...) any)
     (->/h method-proc? (syntax (-> dom ... any)))]
    [(_ (dom ...) rest (rng ...))
     (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-length dom-index ...) (generate-indices (syntax (dom ...)))]
                   [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                   [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                   [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                   [dom-rest-projection-x (car (generate-temporaries (list (syntax rest))))]
                   [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                   
                   [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                   [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                   [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                   [(rng-length rng-index ...) (generate-indices (syntax (rng ...)))]
                   [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                   [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                   [arity (length (syntax->list (syntax (dom ...))))])
       (values
        (lambda (outer-args body)
          (with-syntax ([(val blame name-id) outer-args]
                        [body body]
                        [(name-dom-contract-x ...)
                         (if method-proc?
                             (cdr
                              (syntax->list
                               (syntax (dom-contract-x ...))))
                             (syntax (dom-contract-x ...)))])
            (syntax
             (let ([dom-contract-x (coerce-contract '->* dom)] 
                   ...
                   [dom-rest-contract-x (coerce-contract '->* rest)]
                   [rng-contract-x (coerce-contract '->* rng)] ...)
               (let ([dom-x (contract-projection dom-contract-x)]
                     ...
                     [dom-rest-x (contract-projection dom-rest-contract-x)]
                     [rng-x (contract-projection rng-contract-x)] 
                     ...)
                 (let ([name-id 
                        (build-compound-type-name 
                         '->*
                         (build-compound-type-name dom-contract-x ...)
                         dom-rest-contract-x
                         (build-compound-type-name rng-contract-x ...))])
                   body))))))
        ;; proj
        (lambda (outer-args inner-lambda)
          (with-syntax ([(val blame name-id) outer-args]
                        [inner-lambda inner-lambda])
            (syntax
             (let ([dom-projection-x (dom-x (blame-swap blame))] 
                   ...
                   [dom-rest-projection-x (dom-rest-x (blame-swap blame))]
                   [rng-projection-x (rng-x blame)] ...)
               inner-lambda))))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             (check-procedure/more val dom-length '() '() #|keywords|# blame))))
        (syntax (check-procedure/more? dom-length))
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             ((arg-x ... . arg-rest-x)
              (let-values ([(res-x ...)
                            (apply
                             val
                             (dom-projection-x arg-x)
                             ...
                             (dom-rest-projection-x arg-rest-x))])
                (values (rng-projection-x res-x) ...))))))))]
    [(_ (dom ...) rest any)
     (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-length dom-index ...) (generate-indices (syntax (dom ...)))]
                   [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                   [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                   [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                   [dom-projection-rest-x (car (generate-temporaries (list (syntax rest))))]
                   [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                   
                   [arity (length (syntax->list (syntax (dom ...))))])
       (values
        (lambda (outer-args body)
          (with-syntax ([body body]
                        [(val blame name-id) outer-args]
                        [(name-dom-contract-x ...)
                         (if method-proc?
                             (cdr
                              (syntax->list
                               (syntax (dom-contract-x ...))))
                             (syntax (dom-contract-x ...)))])
            (syntax
             (let ([dom-contract-x (coerce-contract '->* dom)] 
                   ...
                   [dom-rest-contract-x (coerce-contract '->* rest)])
               (let ([dom-x (contract-projection dom-contract-x)]
                     ...
                     [dom-rest-x (contract-projection dom-rest-contract-x)])
                 (let ([name-id (build-compound-type-name
                                 '->* 
                                 (build-compound-type-name name-dom-contract-x ...)
                                 dom-rest-contract-x
                                 'any)])
                   body))))))
        ;; proj
        (lambda (outer-args inner-lambda)
          (with-syntax ([(val blame name-id) outer-args]
                        [inner-lambda inner-lambda])
            (syntax
             (let ([dom-projection-x (dom-x (blame-swap blame))] 
                   ...
                   [dom-projection-rest-x (dom-rest-x (blame-swap blame))])
               inner-lambda))))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             (check-procedure/more val dom-length '() '() #|keywords|# blame))))
        (syntax (check-procedure/more? dom-length))
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             ((arg-x ... . arg-rest-x)
              (apply
               val
               (dom-projection-x arg-x)
               ...
               (dom-projection-rest-x arg-rest-x))))))))]))

;; ->d/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->d/h method-proc? stx)
  (syntax-case stx ()
    [(_) (raise-syntax-error '->d "expected at least one argument" stx)]
    [(_ dom ... rng)
     (with-syntax ([(rng-x) (generate-temporaries (syntax (rng)))]
                   [(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                   [arity (length (syntax->list (syntax (dom ...))))])
       (values
        (lambda (outer-args body)
          (with-syntax ([body body]
                        [(val blame name-id) outer-args]
                        [(name-dom-contract-x ...)
                         (if method-proc?
                             (cdr
                              (syntax->list
                               (syntax (dom-contract-x ...))))
                             (syntax (dom-contract-x ...)))])
            (syntax
             (let ([dom-contract-x (coerce-contract '->d dom)] ...)
               (let ([dom-x (contract-projection dom-contract-x)] 
                     ...
                     [rng-x rng])
                 (check-rng-procedure '->d rng-x arity)
                 (let ([name-id (build-compound-type-name '->d name-dom-contract-x ... '(... ...))])
                   body))))))
        
        ;; proj
        (lambda (outer-args inner-lambda)
          (with-syntax ([(val blame name-id) outer-args]
                        [inner-lambda inner-lambda])
            (syntax
             (let ([dom-projection-x (dom-x (blame-swap blame))] ...)
               inner-lambda))))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             (check-procedure val arity 0 '() '() #|keywords|# blame))))
        
        (syntax (check-procedure? arity))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             ((arg-x ...)
              (let ([arg-x (dom-projection-x arg-x)] ...) 
                (let ([rng-contract (rng-x arg-x ...)])
                  (((contract-projection (coerce-contract '->d rng-contract))
                    blame)
                   (val arg-x ...))))))))))]))

;; ->d*/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->d*/h method-proc? stx)
  (syntax-case stx ()
    [(_ (dom ...) rng-mk)
     (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-length dom-index ...) (generate-indices (syntax (dom ...)))]
                   [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
       (values
        (lambda (outer-args body)
          (with-syntax ([body body]
                        [(val blame name-id) outer-args]
                        [(name-dom-contract-x ...)
                         (if method-proc?
                             (cdr
                              (syntax->list
                               (syntax (dom-contract-x ...))))
                             (syntax (dom-contract-x ...)))])
            (syntax
             (let ([dom-contract-x (coerce-contract '->d* dom)] ...)
               (let ([dom-x (contract-projection dom-contract-x)] 
                     ...
                     [rng-mk-x rng-mk])
                 (check-rng-procedure '->d* rng-mk-x dom-length)
                 (let ([name-id (build-compound-type-name
                                 '->d* 
                                 (build-compound-type-name name-dom-contract-x ...)
                                 '(... ...))])
                   body))))))
        
        ;; proj
        (lambda (outer-args inner-lambda)
          (with-syntax ([(val blame name-id) outer-args]
                        [inner-lambda inner-lambda])
            (syntax
             (let ([dom-projection-x (dom-x (blame-swap blame))] ...)
               inner-lambda))))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             (check-procedure val dom-length 0 '() '() #|keywords|# blame))))
        (syntax (check-procedure? dom-length))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             ((arg-x ...)
              (call-with-values
               (lambda () (rng-mk-x arg-x ...))
               (lambda rng-contracts
                 (call-with-values
                  (lambda ()
                    (val (dom-projection-x arg-x) ...))
                  (lambda results
                    (check-rng-lengths results rng-contracts)
                    (apply 
                     values
                     (map (lambda (rng-contract result)
                            (((contract-projection (coerce-contract '->d* rng-contract))
                              blame)
                             result))
                          rng-contracts
                          results))))))))))))]
    [(_ (dom ...) rest rng-mk)
     (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                   [(dom-rest-x) (generate-temporaries (syntax (rest)))]
                   [(dom-rest-contract-x) (generate-temporaries (syntax (rest)))]
                   [(dom-rest-projection-x) (generate-temporaries (syntax (rest)))]
                   [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                   [arity (length (syntax->list (syntax (dom ...))))])
       (values
        (lambda (outer-args body)
          (with-syntax ([body body]
                        [(val blame name-id) outer-args]
                        [(name-dom-contract-x ...)
                         (if method-proc?
                             (cdr
                              (syntax->list
                               (syntax (dom-contract-x ...))))
                             (syntax (dom-contract-x ...)))])
            (syntax
             (let ([dom-contract-x (coerce-contract '->d* dom)] 
                   ...
                   [dom-rest-contract-x (coerce-contract '->d* rest)])
               (let ([dom-x (contract-projection dom-contract-x)] 
                     ...
                     [dom-rest-x (contract-projection dom-rest-contract-x)]
                     [rng-mk-x rng-mk])
                 (check-rng-procedure/more rng-mk-x arity)
                 (let ([name-id (build-compound-type-name 
                                 '->d*
                                 (build-compound-type-name name-dom-contract-x ...)
                                 dom-rest-contract-x
                                 '(... ...))])
                   body))))))
        
        ;; proj
        (lambda (outer-args inner-lambda)
          (with-syntax ([(val blame name-id) outer-args]
                        [inner-lambda inner-lambda])
            (syntax
             (let ([dom-projection-x (dom-x (blame-swap blame))] 
                   ...
                   [dom-rest-projection-x (dom-rest-x (blame-swap blame))])
               inner-lambda))))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             (check-procedure/more val arity '() '() #|keywords|# blame))))
        (syntax (check-procedure/more? arity))
        
        (lambda (outer-args)
          (with-syntax ([(val blame name-id) outer-args])
            (syntax
             ((arg-x ... . rest-arg-x)
              (call-with-values
               (lambda ()
                 (apply rng-mk-x arg-x ... rest-arg-x))
               (lambda rng-contracts
                 (call-with-values
                  (lambda ()
                    (apply 
                     val
                     (dom-projection-x arg-x)
                     ...
                     (dom-rest-projection-x rest-arg-x)))
                  (lambda results
                    (check-rng-lengths results rng-contracts)
                    (apply 
                     values
                     (map (lambda (rng-contract result)
                            (((contract-projection (coerce-contract '->d* rng-contract))
                              blame)
                             result))
                          rng-contracts
                          results))))))))))))]))

;; ->r/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->r/h method-proc? stx)
  (syntax-case stx ()
    [(_ ([x dom] ...) rng)
     (syntax-case* (syntax rng) (any values) module-or-top-identifier=?
       [any 
        (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t any)))]
       [(values . args)
        (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t rng #t)))]
       [rng
        (->r-pp/h method-proc? '->r (syntax (->r ([x dom] ...) #t rng unused-id #t)))]
       [_
        (raise-syntax-error '->r "unknown result contract spec" stx (syntax rng))])]
    
    [(_ ([x dom] ...) rest-x rest-dom rng)
     (syntax-case* (syntax rng) (values any) module-or-top-identifier=?
       [any 
        (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t any)))]
       [(values . whatever)
        (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t rng #t)))]
       [_
        (->r-pp-rest/h method-proc? '->r (syntax (->r ([x dom] ...) rest-x rest-dom #t rng unused-id #t)))])]))

;; ->pp/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->pp/h method-proc? stx) (->r-pp/h method-proc? '->pp stx))

;; ->pp/h : boolean symbol stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->r-pp/h method-proc? name stx)
  (syntax-case stx ()
    [(_ ([x dom] ...) pre-expr . result-stuff)
     (and (andmap identifier? (syntax->list (syntax (x ...))))
          (not (check-duplicate-identifier (syntax->list (syntax (x ...))))))
     (with-syntax ([stx-name name])
       (with-syntax ([(dom-id ...) (generate-temporaries (syntax (dom ...)))]
                     [arity (length (syntax->list (syntax (dom ...))))]
                     [name-stx
                      (with-syntax ([(name-xs ...) (if method-proc?
                                                       (cdr (syntax->list (syntax (x ...))))
                                                       (syntax (x ...)))])
                        (syntax 
                         (build-compound-type-name 'stx-name
                                                   (build-compound-type-name
                                                    (build-compound-type-name 'name-xs '(... ...))
                                                    ...)
                                                   '(... ...))))])
         (values
          (lambda (outer-args body)
            (with-syntax ([body body]
                          [(val blame name-id) outer-args])
              (syntax
               (let ([name-id name-stx])
                 body))))
          (lambda (outer-args inner-lambda) inner-lambda)
          
          (lambda (outer-args)
            (with-syntax ([(val blame name-id) outer-args]
                          [kind-of-thing (if method-proc? 'method 'procedure)])
              (syntax
               (begin 
                 (check-procedure/kind val arity 'kind-of-thing blame)))))
          
          (syntax (check-procedure? arity))
          
          (lambda (outer-args)
            (with-syntax ([(val blame name-id) outer-args])
              (syntax-case* (syntax result-stuff) (any values) module-or-top-identifier=?
                [(any)
                 (syntax
                  ((x ...)
                   (begin
                     (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                     (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom)) 
                                    (blame-swap blame))]
                           ...)
                       (val (dom-id x) ...)))))]
                [((values (rng-ids rng-ctc) ...) post-expr)
                 (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                      (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                 (with-syntax ([(rng-ids-x ...) (generate-temporaries (syntax (rng-ids ...)))])
                   (syntax
                    ((x ...)
                     (begin
                       (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                       (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom))
                                      (blame-swap blame))]
                             ...)
                         (let-values ([(rng-ids ...) (val (dom-id x) ...)])
                           (check-post-expr->pp/h val post-expr blame)
                           (let ([rng-ids-x ((contract-projection (coerce-contract 'stx-name rng-ctc))
                                             blame)] ...)
                             (values (rng-ids-x rng-ids) ...))))))))]
                [((values (rng-ids rng-ctc) ...) post-expr)
                 (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                 (let ([dup (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))])
                   (raise-syntax-error name "duplicate identifier" stx dup))]
                [((values (rng-ids rng-ctc) ...) post-expr)
                 (for-each (lambda (rng-id) 
                             (unless (identifier? rng-id)
                               (raise-syntax-error name "expected identifier" stx rng-id)))
                           (syntax->list (syntax (rng-ids ...))))]
                [((values . x) . junk)
                 (raise-syntax-error name "malformed multiple values result" stx (syntax (values . x)))]
                [(rng res-id post-expr)
                 (syntax
                  ((x ...)
                   (begin
                     (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                     (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom))
                                    (blame-swap blame))]
                           ...
                           [rng-id ((contract-projection (coerce-contract 'stx-name rng)) 
                                    blame)])
                       (let ([res-id (rng-id (val (dom-id x) ...))])
                         (check-post-expr->pp/h val post-expr blame)
                         res-id)))))]
                [_ 
                 (raise-syntax-error name "unknown result specification" stx (syntax result-stuff))]))))))]
    
    [(_ ([x dom] ...) pre-expr . result-stuff)
     (andmap identifier? (syntax->list (syntax (x ...))))
     (raise-syntax-error 
      name
      "duplicate identifier"
      stx
      (check-duplicate-identifier (syntax->list (syntax (x ...)))))]
    [(_ ([x dom] ...) pre-expr . result-stuff)
     (for-each (lambda (x) (unless (identifier? x) (raise-syntax-error name "expected identifier" stx x)))
               (syntax->list (syntax (x ...))))]
    [(_ (x ...) pre-expr . result-stuff)
     (for-each (lambda (x)
                 (syntax-case x ()
                   [(x y) (identifier? (syntax x)) (void)]
                   [bad (raise-syntax-error name "expected identifier and contract" stx (syntax bad))]))
               (syntax->list (syntax (x ...))))]
    [(_ x dom pre-expr . result-stuff)
     (raise-syntax-error name "expected list of identifiers and expression pairs" stx (syntax x))]))

;; ->pp-rest/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->pp-rest/h method-proc? stx) (->r-pp-rest/h method-proc? '->pp-rest stx))

;; ->r-pp-rest/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
(define (->r-pp-rest/h method-proc? name stx)
  (syntax-case stx ()
    [(_ ([x dom] ...) rest-x rest-dom pre-expr . result-stuff)
     (and (identifier? (syntax rest-x))
          (andmap identifier? (syntax->list (syntax (x ...))))
          (not (check-duplicate-identifier (cons (syntax rest-x) (syntax->list (syntax (x ...)))))))
     (with-syntax ([stx-name name])
       (with-syntax ([(dom-id ...) (generate-temporaries (syntax (dom ...)))]
                     [arity (length (syntax->list (syntax (dom ...))))]
                     [name-stx 
                      (with-syntax ([(name-xs ...) (if method-proc?
                                                       (cdr (syntax->list (syntax (x ...))))
                                                       (syntax (x ...)))])
                        (syntax 
                         (build-compound-type-name 'stx-name
                                                   `(,(build-compound-type-name 'name-xs '(... ...)) ...)
                                                   'rest-x
                                                   '(... ...)
                                                   '(... ...))))])
         (values
          (lambda (outer-args body)
            (with-syntax ([body body]
                          [(val blame name-id) outer-args])
              (syntax
               (let ([name-id name-stx])
                 body))))
          (lambda (outer-args inner-lambda) inner-lambda)
          
          (lambda (outer-args)
            (with-syntax ([(val blame name-id) outer-args]
                          [kind-of-thing (if method-proc? 'method 'procedure)])
              (syntax
               (begin 
                 (check-procedure/more/kind val arity 'kind-of-thing blame)))))
          (syntax (check-procedure/more? arity))
          
          (lambda (outer-args)
            (with-syntax ([(val blame name-id) outer-args])
              (syntax-case* (syntax result-stuff) (values any) module-or-top-identifier=?
                [(any)
                 (syntax
                  ((x ... . rest-x)
                   (begin
                     (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                     (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom))
                                    (blame-swap blame))]
                           ...
                           [rest-id ((contract-projection (coerce-contract 'stx-name rest-dom))
                                     (blame-swap blame))])
                       (apply val (dom-id x) ... (rest-id rest-x))))))]
                [(any . x)
                 (raise-syntax-error name "cannot have anything after any" stx (syntax result-stuff))]
                [((values (rng-ids rng-ctc) ...) post-expr)
                 (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                      (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                 (with-syntax ([(rng-ids-x ...) (generate-temporaries (syntax (rng-ids ...)))])
                   (syntax
                    ((x ... . rest-x)
                     (begin
                       (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                       (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom))
                                      (blame-swap blame))]
                             ...
                             [rest-id ((contract-projection (coerce-contract 'stx-name rest-dom))
                                       (blame-swap blame))])
                         (let-values ([(rng-ids ...) (apply val (dom-id x) ... (rest-id rest-x))])
                           (check-post-expr->pp/h val post-expr blame)
                           (let ([rng-ids-x ((contract-projection (coerce-contract 'stx-name rng-ctc))
                                             blame)] ...)
                             (values (rng-ids-x rng-ids) ...))))))))]
                [((values (rng-ids rng-ctc) ...) . whatever)
                 (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                      (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                 (raise-syntax-error name "expected exactly on post-expression at the end" stx)]
                [((values (rng-ids rng-ctc) ...) . whatever)
                 (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                 (let ([dup (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))])
                   (raise-syntax-error name "duplicate identifier" stx dup))]
                [((values (rng-ids rng-ctc) ...) . whatever)
                 (for-each (lambda (rng-id) 
                             (unless (identifier? rng-id)
                               (raise-syntax-error name "expected identifier" stx rng-id)))
                           (syntax->list (syntax (rng-ids ...))))]
                [((values . x) . whatever)
                 (raise-syntax-error name "malformed multiple values result" stx (syntax (values . x)))]
                [(rng res-id post-expr)
                 (identifier? (syntax res-id))
                 (syntax
                  ((x ... . rest-x)
                   (begin
                     (check-pre-expr->pp/h val pre-expr (blame-swap blame))
                     (let ([dom-id ((contract-projection (coerce-contract 'stx-name dom))
                                    (blame-swap blame))]
                           ...
                           [rest-id ((contract-projection (coerce-contract 'stx-name rest-dom))
                                     (blame-swap blame))]
                           [rng-id ((contract-projection (coerce-contract 'stx-name rng))
                                    blame)])
                       (let ([res-id (rng-id (apply val (dom-id x) ... (rest-id rest-x)))])
                         (check-post-expr->pp/h val post-expr blame)
                         res-id)))))]
                [(rng res-id post-expr)
                 (not (identifier? (syntax res-id)))
                 (raise-syntax-error name "expected an identifier" stx (syntax res-id))]
                [_
                 (raise-syntax-error name "malformed result sepecification" stx (syntax result-stuff))]))))))]
    [(_ ([x dom] ...) rest-x rest-dom pre-expr . result-stuff)
     (not (identifier? (syntax rest-x)))
     (raise-syntax-error name "expected identifier" stx (syntax rest-x))]
    [(_ ([x dom] ...) rest-x rest-dom rng . result-stuff)
     (and (identifier? (syntax rest-x))
          (andmap identifier? (cons (syntax rest-x) (syntax->list (syntax (x ...))))))
     (raise-syntax-error 
      name
      "duplicate identifier"
      stx
      (check-duplicate-identifier (syntax->list (syntax (x ...)))))]
    
    [(_ ([x dom] ...) rest-x rest-dom rng . result-stuff)
     (for-each (lambda (x) (unless (identifier? x) (raise-syntax-error name "expected identifier" stx x)))
               (cons
                (syntax rest-x)
                (syntax->list (syntax (x ...)))))]
    [(_ x dom rest-x rest-dom rng . result-stuff)
     (raise-syntax-error name "expected list of identifiers and expression pairs" stx (syntax x))]))

;; set-inferred-name-from : syntax syntax -> syntax
(define (set-inferred-name-from with-name to-be-named)
  (let ([name (syntax-local-infer-name with-name)])
    (cond
      [(identifier? name)
       (with-syntax ([rhs (syntax-property to-be-named 'inferred-name (syntax-e name))]
                     [name (syntax-e name)])
         (syntax (let ([name rhs]) name)))]
      [(symbol? name)
       (with-syntax ([rhs (syntax-property to-be-named 'inferred-name name)]
                     [name name])
         (syntax (let ([name rhs]) name)))]
      [else to-be-named])))

;; generate-indices : syntax[list] -> (cons number (listof number))
;; given a syntax list of length `n', returns a list containing
;; the number n followed by th numbers from 0 to n-1
(define (generate-indices stx)
  (let ([n (length (syntax->list stx))])
    (cons n
          (let loop ([i n])
            (cond
              [(zero? i) null]
              [else (cons (- n i)
                          (loop (- i 1)))])))))
