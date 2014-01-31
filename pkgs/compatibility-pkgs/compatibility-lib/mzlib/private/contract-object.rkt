#lang racket/base
(require "contract-arrow.rkt"
         racket/contract/private/guts
         racket/contract/private/misc
         racket/contract/private/prop
         racket/private/class-internal
         racket/private/class-c-old
         "contract-arr-checks.rkt")

(require (for-syntax racket/base
                     "contract-arr-obj-helpers.rkt"))

(provide object-contract)

(define-syntax object-contract
  (let ()
    (define (obj->/proc stx) (make-/proc #t ->/h stx))
    (define (obj->*/proc stx) (make-/proc #t ->*/h stx))
    (define (obj->d/proc stx) (make-/proc #t ->d/h stx))
    (define (obj->d*/proc stx) (make-/proc #t ->d*/h stx))
    (define (obj->r/proc stx) (make-/proc #t ->r/h stx))
    (define (obj->pp/proc stx) (make-/proc #t ->pp/h stx))
    (define (obj->pp-rest/proc stx) (make-/proc #t ->pp-rest/h stx))
    (define (obj-case->/proc stx) (make-case->/proc #t stx stx select/h))
    
    ;; WARNING: select/h is copied from contract-arrow.rkt. I'm not sure how
    ;; I can avoid this duplication -robby
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
    
    
    (define (obj-opt->/proc stx) (make-opt->/proc #t stx select/h #'case-> #'->))
    (define (obj-opt->*/proc stx) (make-opt->*/proc #t stx stx select/h #'case-> #'->))
    
    (λ (stx)
      
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
                         [this (datum->syntax mtd-stx 'this)])
             (values
              obj->r/proc
              (syntax (->r ([this any/c] [x dom] ...) rng))
              (syntax ((this-var arg-vars ...)))))]
          
          [(->r ([x dom] ...) rest-x rest-dom rng)
           (andmap identifier? (syntax->list (syntax (x ...))))
           (with-syntax ([(arg-vars ...) (generate-temporaries (syntax (x ...)))]
                         [(this-var) (generate-temporaries (syntax (this-var)))]
                         [this (datum->syntax mtd-stx 'this)])
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
                         [this (datum->syntax mtd-stx 'this)])
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
                         [this (datum->syntax mtd-stx 'this)])
             (values
              obj->pp-rest/proc
              (syntax (->pp ([this any/c] [x dom] ...) rest-id . other-stuff))
              (syntax ((this-var arg-vars ... . rest-id)))))]
          [(->pp-rest . x)
           (raise-syntax-error 'object-contract "malformed ->pp-rest declaration")]
          [else (raise-syntax-error 'object-contract "unknown method contract syntax" stx mtd-stx)]))
      
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
                         
                         [(field-ctc-stx ...) (map fld-ctc-stx flds)]
                         [(field-name ...) (map fld-name flds)]
                         [(field-ctc-var ...) (generate-temporaries flds)])
             (syntax
              (let ([method-ctc-var method-ctc-stx] 
                    ...
                    [field-ctc-var (coerce-contract 'object-contract field-ctc-stx)]
                    ...)
                (define ctc
                  (make-contract
                   #:name
                   `(object-contract 
                     ,(build-compound-type-name 'method-name method-ctc-var) ...
                     ,(build-compound-type-name 'field 'field-name field-ctc-var) ...)
                   #:projection
                   (lambda (blame)
                     (lambda (val)
                       (make-wrapper-object ctc val blame
                                            (list 'method-name ...) (list method-ctc-var ...)
                                            (list 'field-name ...) (list field-ctc-var ...))))
                   #:first-order
                   (lambda (val)
                     (let/ec ret
                       (check-object-contract val (list 'method-name ...) (list 'field-name ...)
                                              (λ args (ret #f)))))))
                  ctc))))]))))



