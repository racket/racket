(module contract-arrow mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           "contract-util.ss"
           "class-internal.ss")
  
  (require-for-syntax (lib "list.ss")
                      (lib "stx.ss" "syntax")
                      (lib "name.ss" "syntax"))

  (provide any
           ->
           ->d
           ->*
           ->d*
           ->r
           ->pp
           ->pp-rest
           case->
	   opt->
           opt->*
           object-contract
           mixin-contract
           make-mixin-contract
           is-a?/c 
           subclass?/c 
           implementation?/c)
  
  (define-syntax (any stx)
    (raise-syntax-error 'any "Use any out of an arrow contract" stx))

  (define-syntax (-> stx)
    (syntax-case* stx (any values) module-or-top-identifier=?
      [(_ doms ... (values v ...))
       (syntax (->/real doms ... (values v ...)))]
      [(_ doms ... rng)
       (with-syntax ([(args ...) (generate-temporaries (syntax (doms ...)))]
                     [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))])
         (let* ([name (syntax-local-infer-name stx)]
                [any-range? (syntax-case* (syntax rng) (any values) module-or-top-identifier=?
                              [any #t]
                              [_ #f])]
                [lambda-stx
                 (if any-range?
                     (syntax (lambda (args ...) (val (dom-ctc args) ...)))
                     (syntax (lambda (args ...) (rng-ctc (val (dom-ctc args) ...)))))])
           (with-syntax ([inner-lambda (cond
                                         [(identifier? name)
                                          (syntax-property lambda-stx 'inferred-name (syntax-e name))]
                                         [(symbol? name)
                                          (syntax-property lambda-stx 'inferred-name name)]
                                         [else lambda-stx])])
             (with-syntax ([outer-lambda
                            (syntax
                             (lambda (chk rng-ctc dom-ctc ...)
                               (lambda (val)
                                 (chk val)
                                 inner-lambda)))])
               (if any-range?
                   (syntax (->/proc/any (list doms ...) outer-lambda))
                   (syntax (->/proc (list doms ...) rng outer-lambda)))))))]))
  
  (define (->/proc doms rng func)
    (->/proc/any-or-not doms rng (coerce-contract -> rng) func))
  
  (define (->/proc/any doms func)
    (->/proc/any-or-not doms any/c 'any func))
  
  (define (->/proc/any-or-not doms rng rng-name func)
    (let ([doms/c (map (λ (dom) (coerce-contract -> dom)) doms)]
          [rng/c (coerce-contract -> rng)]
          [dom-length (length doms)])
      (make-->
       (lambda (pos-blame neg-blame src-info orig-str)
         (let ([partial-doms (map (λ (dom) 
                                    ((contract-proc dom)
                                     neg-blame pos-blame src-info orig-str))
                                  doms/c)]
               [partial-range ((contract-proc rng/c)
                               pos-blame neg-blame src-info orig-str)])
           (apply func
                  (λ (val)
                    (check-procedure val dom-length src-info pos-blame neg-blame orig-str))
                  partial-range
                  partial-doms)))
       (apply build-compound-type-name '-> (append doms/c (list rng-name)))
       doms/c
       rng/c)))
  
  (define-struct/prop -> (proj-proc name doms rng)
    ((proj-prop (λ (ctc) (->-proj-proc ctc)))
     (name-prop (λ (ctc) (->-name ctc)))
     (stronger-prop
      (λ (this that)
        (and (->? that)
             (andmap contract-stronger?
                     (->-doms that)
                     (->-doms this))
             (contract-stronger? (->-rng this) 
                                 (->-rng that)))))))
  
  (define-syntax-set (->/real ->* ->d ->d* ->r ->pp ->pp-rest case-> object-contract opt-> opt->*)
    
    (define (->/real/proc stx) (make-/proc #f ->/h stx))
    (define (->*/proc stx) (make-/proc #f ->*/h stx))
    (define (->d/proc stx) (make-/proc #f ->d/h stx))
    (define (->d*/proc stx) (make-/proc #f ->d*/h stx))
    (define (->r/proc stx) (make-/proc #f ->r/h stx))
    (define (->pp/proc stx) (make-/proc #f ->pp/h stx))
    (define (->pp-rest/proc stx) (make-/proc #f ->pp-rest/h stx))
        
    (define (obj->/proc stx) (make-/proc #t ->/h stx))
    (define (obj->*/proc stx) (make-/proc #t ->*/h stx))
    (define (obj->d/proc stx) (make-/proc #t ->d/h stx))
    (define (obj->d*/proc stx) (make-/proc #t ->d*/h stx))
    (define (obj->r/proc stx) (make-/proc #t ->r/h stx))
    (define (obj->pp/proc stx) (make-/proc #t ->pp/h stx))
    (define (obj->pp-rest/proc stx) (make-/proc #t ->pp-rest/h stx))
    
    (define (case->/proc stx) (make-case->/proc #f stx stx))
    (define (obj-case->/proc stx) (make-case->/proc #t stx stx))

    (define (obj-opt->/proc stx) (make-opt->/proc #t stx))
    (define (obj-opt->*/proc stx) (make-opt->*/proc #t stx stx))
    (define (opt->/proc stx) (make-opt->/proc #f stx))
    (define (opt->*/proc stx) (make-opt->*/proc #f stx stx))
  
    ;; make-/proc : boolean
    ;;              (syntax -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))) 
    ;;              syntax
    ;;           -> (syntax -> syntax)
    (define (make-/proc method-proc? /h stx)
      (let-values ([(arguments-check build-proj check-val wrapper) (/h method-proc? stx)])
        (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
          (with-syntax ([inner-check (check-val outer-args)]
                        [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                        [(val-args body) (wrapper outer-args)])
            (with-syntax ([inner-lambda
                           (set-inferred-name-from
                            stx
                            (syntax/loc stx (lambda val-args body)))])
              (let ([inner-lambda-w/err-check
                     (syntax
                      (lambda (val)
                        inner-check
                        inner-lambda))])
                (with-syntax ([proj-code (build-proj outer-args inner-lambda-w/err-check)])
                  (arguments-check
                   outer-args
                   (syntax/loc stx
                     (make-contract
                      name-id
                      (lambda (pos-blame neg-blame src-info orig-str)
                        proj-code)))))))))))
    
    (define (make-case->/proc method-proc? stx inferred-name-stx)
      (syntax-case stx ()
        [(_ cases ...)
         (let-values ([(arguments-check build-projs check-val wrapper)
                       (case->/h method-proc? stx (syntax->list (syntax (cases ...))))])
           (let ([outer-args (syntax (val pos-blame neg-blame src-info orig-str name-id))])
             (with-syntax ([(inner-check ...) (check-val outer-args)]
                           [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                           [(body ...) (wrapper outer-args)])
               (with-syntax ([inner-lambda 
                              (set-inferred-name-from
                               inferred-name-stx
                               (syntax/loc stx (case-lambda body ...)))])
                 (let ([inner-lambda-w/err-check
                        (syntax
                         (lambda (val)
                           inner-check ...
                           inner-lambda))])
                   (with-syntax ([proj-code (build-projs outer-args inner-lambda-w/err-check)])
                     (arguments-check
                      outer-args
                      (syntax/loc stx
                        (make-contract
                         (apply build-compound-type-name 'case-> name-id)
                         (lambda (pos-blame neg-blame src-info orig-str)
                           proj-code))))))))))]))
    
    (define (make-opt->/proc method-proc? stx)
      (syntax-case stx (any)
        [(_ (reqs ...) (opts ...) any)
         (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) any)) stx)]
        [(_ (reqs ...) (opts ...) res)
         (make-opt->*/proc method-proc? (syntax (opt->* (reqs ...) (opts ...) (res))) stx)]))
  
    (define (make-opt->*/proc method-proc? stx inferred-name-stx)
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
                             (syntax (case-> (-> case-doms ... any) ...))
                             inferred-name-stx)])
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
                               (syntax (case-> (-> case-doms ... single-case-result) ...))
                               inferred-name-stx)])
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
      (let ([ilist (syntax-object->datum stx)])
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
    ;;         -> (values (syntax -> syntax)
    ;;                    (syntax -> syntax)
    ;;                    (syntax syntax -> syntax) 
    ;;                    (syntax -> syntax))
    ;; like the other /h functions, but composes the wrapper functions
    ;; together and combines the cases of the case-lambda into a single list.
    (define (case->/h method-proc? orig-stx cases)
      (let loop ([cases cases]
                 [name-ids '()])
        (cond
          [(null? cases) (values (lambda (outer-args body)
                                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                                 [body body]
                                                 [(name-ids ...) (reverse name-ids)])
                                     (syntax
                                      (let ([name-id (list name-ids ...)])
                                        body))))
                                 (lambda (x y) y)
                                 (lambda (args) (syntax ()))
                                 (lambda (args) (syntax ())))]
          [else
           (let ([/h (select/h (car cases) 'case-> orig-stx)]
                 [new-id (car (generate-temporaries (syntax (case->name-id))))])
             (let-values ([(arguments-checks build-projs check-vals wrappers)
                           (loop (cdr cases) (cons new-id name-ids))]
                          [(arguments-check build-proj check-val wrapper)
                           (/h method-proc? (car cases))])
               (values
                (lambda (outer-args x) 
                  (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                [new-id new-id])
                    (arguments-check 
                     (syntax (val pos-blame neg-blame src-info orig-str new-id)) 
                     (arguments-checks 
                      outer-args
                      x))))
                (lambda (args inner)
                  (build-projs
                   args
                   (build-proj
                    args
                    inner)))
                (lambda (args)
                  (with-syntax ([checks (check-vals args)]
                                [check (check-val args)])
                    (syntax (check . checks))))
                (lambda (args)
                  (with-syntax ([case (wrapper args)]
                                [cases (wrappers args)])
                    (syntax (case . cases)))))))])))
    
    (define (object-contract/proc stx)
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      ;; mtd-arg-stx : syntax[list of arg-specs] (ie, for use in a case-lambda)
      (define-struct mtd (name ctc-stx mtd-arg-stx))
      
      ;; name : syntax
      ;; ctc-stx : syntax[evals to a contract]
      (define-struct fld (name ctc-stx))
      
      ;; expand-field/mtd-spec : stx -> (union mtd fld)
      (define (expand-field/mtd-spec f/m-stx)
        (syntax-case f/m-stx (field)
          [(field field-name ctc)
           (identifier? (syntax field-name))
           (make-fld (syntax field-name) (syntax ctc))]
          [(field field-name ctc)
           (raise-syntax-error 'object-contract "expected name of field" stx (syntax field-name))]
          [(mtd-name ctc)
           (identifier? (syntax mtd-name))
           (let-values ([(ctc-stx proc-stx) (expand-mtd-contract (syntax ctc))])
             (make-mtd (syntax mtd-name)
                       ctc-stx
                       proc-stx))]
          [(mtd-name ctc)
           (raise-syntax-error 'object-contract "expected name of method" stx (syntax mtd-name))]
          [_ (raise-syntax-error 'object-contract "expected field or method clause" stx f/m-stx)]))
      
      ;; expand-mtd-contract : syntax -> (values syntax[expanded ctc] syntax[mtd-arg])
      (define (expand-mtd-contract mtd-stx)
        (syntax-case mtd-stx (case-> opt-> opt->*)
          [(case-> cases ...)
           (let loop ([cases (syntax->list (syntax (cases ...)))]
                      [ctc-stxs null]
                      [args-stxs null])
             (cond
               [(null? cases) 
                (values
                 (with-syntax ([(x ...) (reverse ctc-stxs)])
                   (obj-case->/proc (syntax (case-> x ...))))
                 (with-syntax ([(x ...) (apply append (map syntax->list (reverse args-stxs)))])
                   (syntax (x ...))))]
               [else
                (let-values ([(trans ctc-stx mtd-args) (expand-mtd-arrow (car cases))])
                  (loop (cdr cases)
                        (cons ctc-stx ctc-stxs)
                        (cons mtd-args args-stxs)))]))]
          [(opt->* (req-contracts ...) (opt-contracts ...) (res-contracts ...))
           (values
            (obj-opt->*/proc (syntax (opt->* (any/c req-contracts ...) (opt-contracts ...) (res-contracts ...))))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [(opt->* (req-contracts ...) (opt-contracts ...) any)
           (values
            (obj-opt->*/proc (syntax (opt->* (any/c req-contracts ...) (opt-contracts ...) any)))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [(opt-> (req-contracts ...) (opt-contracts ...) res-contract) 
           (values
            (obj-opt->/proc (syntax (opt-> (any/c req-contracts ...) (opt-contracts ...) res-contract)))
            (generate-opt->vars (syntax (req-contracts ...))
                                (syntax (opt-contracts ...))))]
          [else 
           (let-values ([(x y z) (expand-mtd-arrow mtd-stx)])
             (values (x y) z))]))
      
      ;; generate-opt->vars : syntax[requried contracts] syntax[optional contracts] -> syntax[list of arg specs]
      (define (generate-opt->vars req-stx opt-stx)
        (with-syntax ([(req-vars ...) (generate-temporaries req-stx)]
                      [(ths) (generate-temporaries (syntax (ths)))])
          (let loop ([opt-vars (generate-temporaries opt-stx)])
            (cond
              [(null? opt-vars) (list (syntax (ths req-vars ...)))]
              [else (with-syntax ([(opt-vars ...) opt-vars]
                                  [(rests ...) (loop (cdr opt-vars))])
                      (syntax ((ths req-vars ... opt-vars ...)
                               rests ...)))]))))
      
      ;; expand-mtd-arrow : stx -> (values (syntax[ctc] -> syntax[expanded ctc]) syntax[ctc] syntax[mtd-arg])
      (define (expand-mtd-arrow mtd-stx)
        (syntax-case mtd-stx (-> ->* ->d ->d* ->r ->pp ->pp-rest)
          [(->) (raise-syntax-error 'object-contract "-> must have arguments" stx mtd-stx)]
          [(-> args ...)
           ;; this case cheats a little bit --
           ;; (args ...) contains the right number of arguments
           ;; to the method because it also contains one arg for the result! urgh.
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (args ...)))])
             (values obj->/proc
                     (syntax (-> any/c args ...))
                     (syntax ((arg-vars ...)))))]
          [(->* (doms ...) (rngs ...))
           (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))])
             (values obj->*/proc
                     (syntax (->* (any/c doms ...) (rngs ...)))
                     (syntax ((this-var args-vars ...)))))]
          [(->* (doms ...) rst (rngs ...))
           (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                         [(rst-var) (generate-temporaries (syntax (rst)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))])
             (values obj->*/proc
                     (syntax (->* (any/c doms ...) rst (rngs ...)))
                     (syntax ((this-var args-vars ... . rst-var)))))]
          [(->* x ...)
           (raise-syntax-error 'object-contract "malformed ->*" stx mtd-stx)]
          [(->d) (raise-syntax-error 'object-contract "->d must have arguments" stx mtd-stx)]
          [(->d doms ... rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (values
              obj->d/proc
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [arity-count (length doms-val)])
                (syntax 
                 (->d any/c doms ... 
                      (let ([f rng-proc])
			(check->* f arity-count)
                        (lambda (_this-var arg-vars ...)
                          (f arg-vars ...))))))
              (with-syntax ([(args-vars ...) (generate-temporaries doms-val)])
                (syntax ((this-var args-vars ...))))))]
          [(->d* (doms ...) rng-proc)
           (values
            obj->d*/proc
            (let ([doms-val (syntax->list (syntax (doms ...)))])
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [arity-count (length doms-val)])
                (syntax (->d* (any/c doms ...)
                              (let ([f rng-proc])
				(check->* f arity-count)
                                (lambda (_this-var arg-vars ...)
                                  (f arg-vars ...)))))))
            (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                          [(this-var) (generate-temporaries (syntax (this-var)))])
              (syntax ((this-var args-vars ...)))))]
          [(->d* (doms ...) rst-ctc rng-proc)
           (let ([doms-val (syntax->list (syntax (doms ...)))])
             (values
              obj->d*/proc
              (with-syntax ([(arg-vars ...) (generate-temporaries doms-val)]
                            [(rest-var) (generate-temporaries (syntax (rst-ctc)))]
                            [arity-count (length doms-val)])
                (syntax (->d* (any/c doms ...)
                              rst-ctc
                              (let ([f rng-proc])
				(check->*/more f arity-count)
                                (lambda (_this-var arg-vars ... . rest-var)
                                  (apply f arg-vars ... rest-var))))))
              (with-syntax ([(args-vars ...) (generate-temporaries (syntax (doms ...)))]
                            [(rst-var) (generate-temporaries (syntax (rst-ctc)))]
                            [(this-var) (generate-temporaries (syntax (this-var)))])
                (syntax ((this-var args-vars ... . rst-var))))))]
          [(->d* x ...)
           (raise-syntax-error 'object-contract "malformed ->d* method contract" stx mtd-stx)]
          
          [(->r ([x dom] ...) rng)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))]
                         [this (datum->syntax-object mtd-stx 'this)])
             (values
              obj->r/proc
              (syntax (->r ([this any/c] [x dom] ...) rng))
              (syntax ((this-var arg-vars ...)))))]
          
          [(->r ([x dom] ...) rest-x rest-dom rng)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))]
                         [this (datum->syntax-object mtd-stx 'this)])
             (values
              obj->r/proc
              (syntax (->r ([this any/c] [x dom] ...) rest-x rest-dom rng))
              (syntax ((this-var arg-vars ... . rest-var)))))]
          
          [(->r . x)
           (raise-syntax-error 'object-contract "malformed ->r declaration")]
          [(->pp ([x dom] ...) . other-stuff)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))]
                         [this (datum->syntax-object mtd-stx 'this)])
             (values
              obj->pp/proc
              (syntax (->pp ([this any/c] [x dom] ...) . other-stuff))
              (syntax ((this-var arg-vars ...)))))]
          [(->pp . x)
           (raise-syntax-error 'object-contract "malformed ->pp declaration")]
          [(->pp-rest ([x dom] ...) rest-id . other-stuff)
           (and (identifier? (syntax id))
                (andmap identifier? (syntax->list (syntax (x ...)))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))]
                         [this (datum->syntax-object mtd-stx 'this)])
             (values
              obj->pp-rest/proc
              (syntax (->pp ([this any/c] [x dom] ...) rest-id . other-stuff))
              (syntax ((this-var arg-vars ... . rest-id)))))]
          [(->pp-rest . x)
           (raise-syntax-error 'object-contract "malformed ->pp-rest declaration")]
          [else (raise-syntax-error 'object-contract "unknown method contract syntax" stx mtd-stx)]))
      
      ;; build-methods-stx : syntax[list of lambda arg specs] -> syntax[method realized as proc]
      (define (build-methods-stx mtds)
        (let loop ([arg-spec-stxss (map mtd-mtd-arg-stx mtds)]
                   [names (map mtd-name mtds)]
                   [i 0])
          (cond
            [(null? arg-spec-stxss) null]
            [else (let ([arg-spec-stxs (car arg-spec-stxss)])
                    (with-syntax ([(cases ...)
                                   (map (lambda (arg-spec-stx)
                                          (with-syntax ([i i])
                                            (syntax-case arg-spec-stx ()
                                              [(this rest-ids ...)
                                               (syntax
                                                ((this rest-ids ...)
                                                 ((field-ref this i) (wrapper-object-wrapped this) rest-ids ...)))]
                                              [else
                                               (let-values ([(this rest-ids last-var)
                                                             (let ([lst (syntax->improper-list arg-spec-stx)])
                                                               (values (car lst)
                                                                       (all-but-last (cdr lst))
                                                                       (cdr (last-pair lst))))])
                                                 (with-syntax ([this this]
                                                               [(rest-ids ...) rest-ids]
                                                               [last-var last-var])
                                                   (syntax
                                                    ((this rest-ids ... . last-var)
                                                     (apply (field-ref this i)
                                                            (wrapper-object-wrapped this)
                                                            rest-ids ...
                                                            last-var)))))])))
                                        (syntax->list arg-spec-stxs))]
                                  [name (string->symbol (format "~a method" (syntax-object->datum (car names))))])
                      (with-syntax ([proc (syntax-property (syntax (case-lambda cases ...)) 'method-arity-error #t)])
                        (cons (syntax (lambda (field-ref) (let ([name proc]) name)))
                              (loop (cdr arg-spec-stxss)
                                    (cdr names)
                                    (+ i 1))))))])))
      
      (define (syntax->improper-list stx)
        (define (se->il se)
          (cond
            [(pair? se) (sp->il se)]
            [else se]))
        (define (stx->il stx)
          (se->il (syntax-e stx)))
        (define (sp->il p)
          (cond
            [(null? (cdr p)) p]
            [(pair? (cdr p)) (cons (car p) (sp->il (cdr p)))]
            [(syntax? (cdr p)) 
             (let ([un (syntax-e (cdr p))])
               (if (pair? un)
                   (cons (car p) (sp->il un))
                   p))]))
        (stx->il stx))
      
      (syntax-case stx ()
        [(_ field/mtd-specs ...)
         (let* ([mtd/flds (map expand-field/mtd-spec (syntax->list (syntax (field/mtd-specs ...))))]
		[mtds (filter mtd? mtd/flds)]
		[flds (filter fld? mtd/flds)])
           (with-syntax ([(method-ctc-stx ...) (map mtd-ctc-stx mtds)]
                         [(method-name ...) (map mtd-name mtds)]
                         [(method-ctc-var ...) (generate-temporaries mtds)]
                         [(method-var ...) (generate-temporaries mtds)]
                         [(method/app-var ...) (generate-temporaries mtds)]
                         [(methods ...) (build-methods-stx mtds)]
                         
                         [(field-ctc-stx ...) (map fld-ctc-stx flds)]
                         [(field-name ...) (map fld-name flds)]
                         [(field-ctc-var ...) (generate-temporaries flds)]
                         [(field-var ...) (generate-temporaries flds)]
                         [(field/app-var ...) (generate-temporaries flds)])
             (syntax
              (let ([method-ctc-var method-ctc-stx] 
                    ...
                    [field-ctc-var (coerce-contract object-contract field-ctc-stx)]
                    ...)
                (let ([method-var (contract-proc method-ctc-var)] 
                      ...
                      [field-var (contract-proc field-ctc-var)] 
                      ...)
                  (make-contract
                   `(object-contract 
                     ,(build-compound-type-name 'method-name method-ctc-var) ...
                     ,(build-compound-type-name 'field 'field-name field-ctc-var) ...)
                   (lambda (pos-blame neg-blame src-info orig-str)
                     (let ([method/app-var (method-var pos-blame neg-blame src-info orig-str)] 
                           ...
                           [field/app-var (field-var pos-blame neg-blame src-info orig-str)]
                           ...)
                       (let ([cls (make-wrapper-class 'wrapper-class 
                                                      '(method-name ...)
                                                      (list methods ...)
                                                      '(field-name ...))]
                             [field-names-list '(field-name ...)])
                         (lambda (val)
			   (check-object val src-info pos-blame neg-blame orig-str)
                           (let ([val-mtd-names
                                  (interface->method-names
                                   (object-interface
                                    val))])
                             (void)
			     (check-method val 'method-name val-mtd-names src-info pos-blame neg-blame orig-str)
                             ...)
                           
                           (unless (field-bound? field-name val)
			     (field-error val 'field-name src-info pos-blame neg-blame orig-str)) ...
                           
                           (let ([vtable (extract-vtable val)]
                                 [method-ht (extract-method-ht val)])
                             (make-object cls
                               val
                               (method/app-var (vector-ref vtable (hash-table-get method-ht 'method-name))) ...
                               (field/app-var (get-field field-name val)) ...
                               ))))))))))))]))

    ;; ensure-no-duplicates : syntax (listof syntax[identifier]) -> void
    (define (ensure-no-duplicates stx form-name names)
      (let ([ht (make-hash-table)])
        (for-each (lambda (name)
                    (let ([key (syntax-e name)])
                      (when (hash-table-get ht key (lambda () #f))
                        (raise-syntax-error form-name
                                            "duplicate method name"
                                            stx
                                            name))
                      (hash-table-put! ht key #t)))
                  names)))
    
    ;; method-specifier? : syntax -> boolean
    ;; returns #t if x is the syntax for a valid method specifier
    (define (method-specifier? x)
      (or (eq? 'public (syntax-e x))
          (eq? 'override (syntax-e x))))
    
    ;; make-object-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the original object's method
    (define (make-object-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/public (method-name . args)
            (let ([other-method (lambda x (send/apply val method-name x))]
                  [method-specific-src-info 
                   (if (identifier? src-info)
                       (datum->syntax-object
                        src-info
                        (string->symbol
                         (string-append
                          (symbol->string (syntax-e src-info))
                          " method "
                          method-name-string)))
                       src-info)])
              (apply (contract-var
                      other-method
                      pos-blame
                      neg-blame
                      method-specific-src-info)
                     args))))))

    ;; make-class-wrapper-method : syntax syntax[identifier] syntax[identifier] syntax -> syntax
    ;; constructs a wrapper method that checks the pre and post-condition, and
    ;; calls the super method inbetween.
    (define (make-class-wrapper-method outer-args method-name contract-var contract-stx)
      (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                    [super-method-name (prefix-super method-name)]
                    [method-name method-name]
                    [method-name-string (symbol->string (syntax-e method-name))]
                    [contract-var contract-var])
        (syntax/loc contract-stx
          (define/override method-name
            (lambda args
              (let* ([super-method (lambda x (super-method-name . x))]
                     [method-specific-src-info 
                      (if (identifier? src-info)
                          (datum->syntax-object
                           src-info
                           (string->symbol
                            (string-append
                             (symbol->string (syntax-e src-info))
                             " method "
                             method-name-string)))
                          src-info)]
                     [super-contract (and super-contracts-ht
                                          (hash-table-get super-contracts-ht
                                                          'method-name
                                                          (lambda () #f)))]
                     [wrapped-method (contract-var
                                      super-method
                                      pos-blame
                                      neg-blame
                                      method-specific-src-info)])
                (apply wrapped-method args)))))))
    
    ;; prefix-super : syntax[identifier] -> syntax[identifier]
    ;; adds super- to the front of the identifier
    (define (prefix-super stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "super-~a"
         (syntax-object->datum
          stx)))))
    
    ;; method-name->contract-method-name : syntax[identifier] -> syntax[identifier]
    ;; given the syntax for a method name, constructs the name of a method
    ;; that returns the super's contract for the original method.
    (define (method-name->contract-method-name stx)
      (datum->syntax-object
       #'here
       (string->symbol
        (format 
         "ACK_DONT_GUESS_ME-super-contract-~a"
         (syntax-object->datum
          stx)))))
    
    ;; Each of the /h functions builds four pieces of syntax:
    ;;  - [arguments-check]
    ;;    code that binds the contract values to names and
    ;;    does error checking for the contract specs
    ;;    (were the arguments all contracts?)
    ;;  - [build-proj]
    ;;    code that partially applies the input contracts to build projections
    ;;  - [check-val]
    ;;    code that does error checking on the contract'd value itself
    ;;    (is it a function of the right arity?)
    ;;  - [wrapper]
    ;;    a piece of syntax that has the arguments to the wrapper
    ;;    and the body of the wrapper.
    ;; the first function accepts a body expression and wraps
    ;;    the body expression with checks. In addition, it
    ;;    adds a let that binds the contract exprssions to names
    ;;    the results of the other functions mention these names.
    ;; the second and third function's input syntax should be four
    ;;    names: val, pos-blame, neg-blame, src-info, orig-str, name-id
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
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
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
                                 [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      (let ([dom-contract-x (coerce-contract -> dom)] ...)
                        (let ([dom-x (contract-proc dom-contract-x)] ...)
                          (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... 'any)])
                            body))))))
                 
                 (lambda (outer-args inner-lambda)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                 [inner-lambda inner-lambda])
                     (syntax
                      (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                        inner-lambda))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
		      (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                 
                 (lambda (outer-args)
                   (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                     (syntax
                      ((arg-x ...)
                       (val (dom-projection-x arg-x) ...))))))]
               [(values rng ...)
                (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                              [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                              [(res-x ...) (generate-temporaries (syntax (rng ...)))])
                  (values
                   (lambda (outer-args body)
                     (with-syntax ([body body]
                                   [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        (let ([dom-contract-x (coerce-contract -> dom)] ...
                              [rng-contract-x (coerce-contract -> rng)] ...)
                          (let ([dom-x (contract-proc dom-contract-x)] ...
                                [rng-x (contract-proc rng-contract-x)] ...)
                            (let ([name-id 
                                   (build-compound-type-name 
                                    '-> 
                                    name-dom-contract-x ...
                                    (build-compound-type-name 'values rng-contract-x ...))])
                              body))))))
                   
                   (lambda (outer-args inner-lambda)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                   [inner-lambda inner-lambda])
                       (syntax
                        (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                              [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                          inner-lambda))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
			(check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
                                   [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        (let ([dom-contract-x (coerce-contract -> dom)] ...
                              [rng-contract-x (coerce-contract -> rng)])
                          (let ([dom-x (contract-proc dom-contract-x)] ...
                                [rng-x (contract-proc rng-contract-x)])
                            (let ([name-id (build-compound-type-name '-> name-dom-contract-x ... rng-contract-x)])
                              body))))))
                   
                   (lambda (outer-args inner-lambda)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                                   [inner-lambda inner-lambda])
                       (syntax
                        (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                              [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)])
                          inner-lambda))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
			(check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
                   
                   (lambda (outer-args)
                     (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                       (syntax
                        ((arg-x ...)
                         (let ([res-x (val (dom-projection-x arg-x) ...)])
                           (rng-projection-x res-x))))))))])))]))
    
    ;; ->*/h : boolean stx -> (values (syntax -> syntax) (syntax syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->*/h method-proc? stx)
      (syntax-case stx (any)
        [(_ (dom ...) (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id  (build-compound-type-name
                                      '->*
                                      (build-compound-type-name name-dom-contract-x ...)
                                      (build-compound-type-name rng-contract-x ...))])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (let-values ([(res-x ...) (val (dom-projection-x arg-x) ...)])
                    (values (rng-projection-x
                             res-x)
                            ...))))))))]
        [(_ (dom ...) any)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...)
                     (let ([name-id (build-compound-type-name
                                     '->* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     'any)])
                       body))))))
            
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (val (dom-projection-x arg-x) ...)))))))]
        [(_ (dom ...) rest (rng ...))
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-projection-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [(rng-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-contract-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-projection-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(rng-length rng-index ...) (generate-indicies (syntax (rng ...)))]
                       [(rng-ant-x ...) (generate-temporaries (syntax (rng ...)))]
                       [(res-x ...) (generate-temporaries (syntax (rng ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [body body]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)]
                       [rng-contract-x (coerce-contract ->* rng)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-x (contract-proc rng-contract-x)] ...)
                     (let ([name-id 
                            (build-compound-type-name 
                             '->*
                             (build-compound-type-name dom-contract-x ...)
                             dom-rest-contract-x
                             (build-compound-type-name rng-contract-x ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)]
                       [rng-projection-x (rng-x pos-blame neg-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure/more val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [dom-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-rest-contract-x (car (generate-temporaries (list (syntax rest))))]
                       [dom-projection-rest-x (car (generate-temporaries (list (syntax rest))))]
                       [arg-rest-x (car (generate-temporaries (list (syntax rest))))]
                       
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)])
                     (let ([name-id (build-compound-type-name
                                     '->* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     dom-rest-contract-x
                                     'any)])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-projection-rest-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 ;; CHECK: previously, this test didn't use `procedure-arity' and compare to `dom-length'
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))]
                       [arity (length (syntax->list (syntax (dom ...))))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-x rng])
		     (check-rng-procedure '->d rng-x arity)
                     (let ([name-id (build-compound-type-name '->d name-dom-contract-x ... '(... ...))])
                       
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val arity src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
                 ((arg-x ...)
                  (let ([arg-x (dom-projection-x arg-x)] ...) 
                    (let ([rng-contract (rng-x arg-x ...)])
                      (((coerce/select-contract ->d rng-contract)
                        pos-blame
                        neg-blame
                        src-info
                        orig-str)
                       (val arg-x ...))))))))))]))
    
    ;; ->d*/h : boolean stx -> (values (syntax -> syntax) (syntax -> syntax) (syntax -> syntax))
    (define (->d*/h method-proc? stx)
      (syntax-case stx ()
        [(_ (dom ...) rng-mk)
         (with-syntax ([(dom-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-contract-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-projection-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(dom-length dom-index ...) (generate-indicies (syntax (dom ...)))]
                       [(dom-ant-x ...) (generate-temporaries (syntax (dom ...)))]
                       [(arg-x ...) (generate-temporaries (syntax (dom ...)))])
           (values
            (lambda (outer-args body)
              (with-syntax ([body body]
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...)
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [rng-mk-x rng-mk])
		     (check-rng-procedure '->d* rng-mk-x dom-length)
                     (let ([name-id (build-compound-type-name
                                     '->d* 
                                     (build-compound-type-name name-dom-contract-x ...)
                                     '(... ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...)
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 (check-procedure val dom-length src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
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
                            [(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [(name-dom-contract-x ...)
                             (if method-proc?
                                 (cdr
                                  (syntax->list
                                   (syntax (dom-contract-x ...))))
                                 (syntax (dom-contract-x ...)))])
                (syntax
                 (let ([dom-contract-x (coerce-contract ->d* dom)] ...
                       [dom-rest-contract-x (coerce-contract ->d* rest)])
                   (let ([dom-x (contract-proc dom-contract-x)] ...
                         [dom-rest-x (contract-proc dom-rest-contract-x)]
                         [rng-mk-x rng-mk])
		     (check-rng-procedure/more rng-mk-x arity)
                     (let ([name-id (build-compound-type-name 
                                     '->d*
                                     (build-compound-type-name name-dom-contract-x ...)
                                     dom-rest-contract-x
                                     '(... ...))])
                       body))))))
            (lambda (outer-args inner-lambda)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                            [inner-lambda inner-lambda])
                (syntax
                 (let ([dom-projection-x (dom-x neg-blame pos-blame src-info orig-str)] ...
                       [dom-rest-projection-x (dom-rest-x neg-blame pos-blame src-info orig-str)])
                   inner-lambda))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                (syntax
		 ;; CHECK: old check use "and more", but error message didn't
		 (check-procedure/more val arity src-info pos-blame neg-blame orig-str))))
            (lambda (outer-args)
              (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
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
                                (((coerce/select-contract ->d* rng-contract)
                                  pos-blame
                                  neg-blame
                                  src-info
                                  orig-str)
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
                              [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (let ([name-id name-stx])
                     body))))
              (lambda (outer-args inner-lambda) inner-lambda)
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                              [kind-of-thing (if method-proc? 'method 'procedure)])
                  (syntax
                   (begin 
                     (check-procedure/kind val arity 'kind-of-thing src-info pos-blame neg-blame orig-str)))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax-case* (syntax result-stuff) (any values) module-or-top-identifier=?
                    [(any)
                     (syntax
                      ((x ...)
                       (begin
                         (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...)
                           (val (dom-id x) ...)))))]
                    [((values (rng-ids rng-ctc) ...) post-expr)
                     (and (andmap identifier? (syntax->list (syntax (rng-ids ...))))
                          (not (check-duplicate-identifier (syntax->list (syntax (rng-ids ...))))))
                     (with-syntax ([(rng-ids-x ...) (generate-temporaries (syntax (rng-ids ...)))])
                       (syntax
                        ((x ...)
                         (begin
                           (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                           (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                                 ...)
                             (let-values ([(rng-ids ...) (val (dom-id x) ...)])
                               (check-post-expr->pp/h val post-expr src-info pos-blame neg-blame orig-str)
                               (let ([rng-ids-x ((coerce/select-contract stx-name rng-ctc)
                                                 pos-blame neg-blame src-info orig-str)] ...)
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
                         (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rng-id ((coerce/select-contract stx-name rng) pos-blame neg-blame src-info orig-str)])
                           (let ([res-id (rng-id (val (dom-id x) ...))])
                             (check-post-expr->pp/h val post-expr src-info pos-blame neg-blame orig-str)
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
                              [(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax
                   (let ([name-id name-stx])
                     body))))
              (lambda (outer-args inner-lambda) inner-lambda)
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args]
                              [kind-of-thing (if method-proc? 'method 'procedure)])
                  (syntax
                   (begin 
                     (check-procedure/more/kind val arity 'kind-of-thing src-info pos-blame neg-blame orig-str)))))
              (lambda (outer-args)
                (with-syntax ([(val pos-blame neg-blame src-info orig-str name-id) outer-args])
                  (syntax-case* (syntax result-stuff) (values any) module-or-top-identifier=?
                    [(any)
                     (syntax
                      ((x ... . rest-x)
                       (begin
                         (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)])
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
                           (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                           (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                                 ...
                                 [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)])
                             (let-values ([(rng-ids ...) (apply val (dom-id x) ... (rest-id rest-x))])
                               (check-post-expr->pp/h val post-expr src-info pos-blame neg-blame orig-str)
                               (let ([rng-ids-x ((coerce/select-contract stx-name rng-ctc)
                                                 pos-blame neg-blame src-info orig-str)] ...)
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
                         (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
                         (let ([dom-id ((coerce/select-contract stx-name dom) neg-blame pos-blame src-info orig-str)]
                               ...
                               [rest-id ((coerce/select-contract stx-name rest-dom) neg-blame pos-blame src-info orig-str)]
                               [rng-id ((coerce/select-contract stx-name rng) pos-blame neg-blame src-info orig-str)])
                           (let ([res-id (rng-id (apply val (dom-id x) ... (rest-id rest-x)))])
                             (check-post-expr->pp/h val post-expr src-info pos-blame neg-blame orig-str)
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
    
    ;; select/h : syntax -> /h-function
    (define (select/h stx err-name ctxt-stx)
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
    
    ;; (cons X (listof X)) -> (listof X)
    ;; returns the elements of `l', minus the last element
    ;; special case: if l is an improper list, it leaves off
    ;; the contents of the last cdr (ie, making a proper list
    ;; out of the input), so (all-but-last '(1 2 . 3)) = '(1 2)
    (define (all-but-last l)
      (cond
        [(null? l) (error 'all-but-last "bad input")]
        [(not (pair? l)) '()]
        [(null? (cdr l)) null]
        [(pair? (cdr l)) (cons (car l) (all-but-last (cdr l)))]
        [else (list (car l))]))
    
    ;; generate-indicies : syntax[list] -> (cons number (listof number))
    ;; given a syntax list of length `n', returns a list containing
    ;; the number n followed by th numbers from 0 to n-1
    (define (generate-indicies stx)
      (let ([n (length (syntax->list stx))])
        (cons n
              (let loop ([i n])
                (cond
                  [(zero? i) null]
                  [else (cons (- n i)
                              (loop (- i 1)))]))))))

  ;; procedure-accepts-and-more? : procedure number -> boolean
  ;; returns #t if val accepts dom-length arguments and
  ;; any number of arguments more than dom-length. 
  ;; returns #f otherwise.
  (define (procedure-accepts-and-more? val dom-length)
    (let ([arity (procedure-arity val)])
      (cond
        [(number? arity) #f]
        [(arity-at-least? arity)
         (<= (arity-at-least-value arity) dom-length)]
        [else
         (let ([min-at-least (let loop ([ars arity]
                                        [acc #f])
                               (cond
                                 [(null? ars) acc]
                                 [else (let ([ar (car ars)])
                                         (cond
                                           [(arity-at-least? ar)
                                            (if (and acc
                                                     (< acc (arity-at-least-value ar)))
                                                (loop (cdr ars) acc)
                                                (loop (cdr ars) (arity-at-least-value ar)))]
                                           [(number? ar)
                                            (loop (cdr ars) acc)]))]))])
           (and min-at-least
                (begin
                  (let loop ([counts (quicksort (filter number? arity) >=)])
                    (unless (null? counts)
                      (let ([count (car counts)])
                        (cond
                          [(= (+ count 1) min-at-least)
                           (set! min-at-least count)
                           (loop (cdr counts))]
                          [(< count min-at-least)
                           (void)]
                          [else (loop (cdr counts))]))))
                  (<= min-at-least dom-length))))])))

  ;; ----------------------------------------
  ;; Checks and error functions used in macro expansions
  
  (define (check->* f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-arity-includes? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a arguments, got ~e"
	     arity-count
	     f)))

  (define (check->*/more f arity-count)
    (unless (procedure? f)
      (error 'object-contract "expected last argument of ->d* to be a procedure, got ~e" f))
    (unless (procedure-accepts-and-more? f arity-count)
      (error 'object-contract 
	     "expected last argument of ->d* to be a procedure that accepts ~a arguments and arbitrarily many more, got ~e"
	     arity-count
	     f)))


  (define (check-pre-expr->pp/h val pre-expr src-info pos-blame neg-blame orig-str)
    (unless pre-expr
      (raise-contract-error val
                            src-info
                            neg-blame
                            pos-blame
                            orig-str
                            "pre-condition expression failure")))
  
  (define (check-post-expr->pp/h val post-expr src-info pos-blame neg-blame orig-str)
    (unless post-expr
      (raise-contract-error val
                            src-info
                            pos-blame
                            neg-blame
                            orig-str
                            "post-condition expression failure")))
  
  (define (check-procedure val dom-length src-info pos-blame neg-blame orig-str)
    (unless (and (procedure? val)
		 (procedure-arity-includes? val dom-length))
      (raise-contract-error
       val
       src-info
       pos-blame
       neg-blame
       orig-str
       "expected a procedure that accepts ~a arguments, given: ~e"
       dom-length
       val)))

  (define (check-procedure/kind val arity kind-of-thing src-info pos-blame neg-blame orig-str)
    (unless (procedure? val)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-arity-includes? val arity)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a ~a of arity ~a (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more/kind val arity kind-of-thing src-info pos-blame neg-blame orig-str)
    (unless (procedure? val)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a procedure, got ~e"
			    val))
    (unless (procedure-accepts-and-more? val arity)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected a ~a that accepts ~a arguments and aribtrarily more (not arity ~a), got  ~e"
			    kind-of-thing
			    arity
			    (procedure-arity val)
			    val)))

  (define (check-procedure/more val dom-length src-info pos-blame neg-blame orig-str)
    (unless (and (procedure? val)
		 (procedure-accepts-and-more? val dom-length))
      (raise-contract-error
       val
       src-info
       pos-blame
       neg-blame
       orig-str
       "expected a procedure that accepts ~a arguments and any number of arguments larger than ~a, given: ~e"
       dom-length
       dom-length
       val)))


  (define (check-rng-procedure who rng-x arity)
    (unless (and (procedure? rng-x)
		 (procedure-arity-includes? rng-x arity))
      (error who "expected range position to be a procedure that accepts ~a arguments, given: ~e"
	     arity
	     rng-x)))

  (define (check-rng-procedure/more rng-mk-x arity)
    (unless (and (procedure? rng-mk-x)
		 (procedure-accepts-and-more? rng-mk-x arity))
      (error '->d* "expected range position to be a procedure that accepts ~a arguments and arbitrarily many more, given: ~e"
	     arity 
	     rng-mk-x)))

  (define (check-rng-lengths results rng-contracts)
    (unless (= (length results) (length rng-contracts))
      (error '->d* 
	     "expected range contract contructor and function to have the same number of values, given: ~a and ~a respectively" 
	     (length results) (length rng-contracts))))

  (define (check-object val src-info pos-blame neg-blame orig-str)
    (unless (object? val)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected an object, got ~e"
			    val)))

  (define (check-method val method-name val-mtd-names src-info pos-blame neg-blame orig-str)
    (unless (memq method-name val-mtd-names)
      (raise-contract-error val
                            src-info
			    pos-blame
			    neg-blame
			    orig-str
			    "expected an object with method ~s"
			    method-name)))

  (define (field-error val field-name src-info pos-blame neg-blame orig-str)
    (raise-contract-error val
                          src-info
			  pos-blame
			  neg-blame
			  orig-str
			  "expected an object with field ~s"
			  field-name))
                                                
  #|

  test cases for procedure-accepts-and-more?

  (and (procedure-accepts-and-more? (lambda (x . y) 1) 3)
       (procedure-accepts-and-more? (lambda (x . y) 1) 2)
       (procedure-accepts-and-more? (lambda (x . y) 1) 1)
       (not (procedure-accepts-and-more? (lambda (x . y) 1) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 3)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x . y) 1] [(y) 1]) 0))
       
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 2)
       (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 1)
       (not (procedure-accepts-and-more? (case-lambda [(x y . z) 1] [(x) 1]) 0)))
  
  |#

  (define (make-mixin-contract . %/<%>s)
    ((and/c (flat-contract class?)
            (apply and/c (map sub/impl?/c %/<%>s)))
     . ->d .
     subclass?/c))
 
  (define (subclass?/c %)
    (unless (class? %)
      (error 'subclass?/c "expected <class>, given: ~e" %))
    (let ([name (object-name %)])
      (flat-named-contract
       `(subclass?/c ,(or name 'unknown%))
       (lambda (x) (subclass? x %)))))
  
  (define (implementation?/c <%>)
    (unless (interface? <%>)
      (error 'implementation?/c "expected <interface>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       `(implementation?/c ,(or name 'unknown<%>))
       (lambda (x) (implementation? x <%>)))))
  
  (define (sub/impl?/c %/<%>)
    (cond
      [(interface? %/<%>) (implementation?/c %/<%>)]
      [(class? %/<%>) (subclass?/c %/<%>)]
      [else (error 'make-mixin-contract "unknown input ~e" %/<%>)]))

  (define (is-a?/c <%>)
    (unless (or (interface? <%>)
		(class? <%>))
      (error 'is-a?/c "expected <interface> or <class>, given: ~e" <%>))
    (let ([name (object-name <%>)])
      (flat-named-contract
       (cond
         [name
          `(is-a?/c ,name)]
         [(class? <%>)
          `(is-a?/c unknown%)]
         [else `(is-a?/c unknown<%>)])
       (lambda (x) (is-a? x <%>)))))
  
  (define mixin-contract (class? . ->d . subclass?/c)))