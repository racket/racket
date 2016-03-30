#lang racket/base

(require (for-syntax racket/base
                     syntax/name)
         (only-in racket/list last)
         racket/stxparam
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt"
         "arrow-common.rkt"
         "arrow-val-first.rkt")

(provide case->
         (for-syntax case->-internal) ; for case->m
         base-case->? case->-name) ; for object-contract


;                                               
;                                               
;                                               
;                                               
;                                        ;      
;    ;;;;; ;;;;;;;   ;;;;;   ;;;         ;;;    
;   ;;;;;; ;;;;;;;; ;;;;;;  ;;;;;         ;;;;  
;  ;;;;;;;     ;;;; ;;;;   ;;;; ;;          ;;; 
;  ;;;;     ;;;;;;;  ;;;;  ;;;;;;; ;;;;;    ;;; 
;  ;;;;;;; ;;  ;;;;   ;;;; ;;;;;   ;;;;;  ;;;;  
;   ;;;;;; ;;;;;;;; ;;;;;;  ;;;;;;       ;;;    
;    ;;;;;  ;; ;;;; ;;;;;    ;;;;        ;      
;                                               
;                                               
;                                               


(define-for-syntax (parse-rng stx rng)
  (syntax-case rng (any values)
    [any #f]
    [(values x ...) #'(x ...)]
    [x #'(x)]))

(define-for-syntax (separate-out-doms/rst/rng stx case)
  (syntax-case case (->)
    [(-> doms ... #:rest rst rng)
     (values #'(doms ...) #'rst (parse-rng stx #'rng))]
    [(-> doms ... rng)
     (values #'(doms ...) #f (parse-rng stx #'rng))]
    [(x y ...)
     (raise-syntax-error #f "expected ->" stx #'x)]
    [_
     (raise-syntax-error #f "expected ->" stx case)]))

(define-for-syntax (parse-out-case stx neg-party blame-party-info case n method?)
  (let-values ([(dom-ctc-exprs rst-ctc-expr rng-ctc-exprs) (separate-out-doms/rst/rng stx case)])
    (with-syntax ([(dom-proj-x  ...) (generate-temporaries dom-ctc-exprs)]
                  [(rst-proj-x) (generate-temporaries '(rest-proj-x))]
                  [(rng-proj-x ...) (generate-temporaries (if rng-ctc-exprs rng-ctc-exprs '()))]
                  [(rng-ctcs-x) (generate-temporaries '(rng-ctc-x))])
      (with-syntax ([(dom-formals ...) (generate-temporaries dom-ctc-exprs)]
                    [(rst-formal) (generate-temporaries '(rest-param))]
                    [(rng-id ...) (if rng-ctc-exprs
                                      (generate-temporaries rng-ctc-exprs)
                                      '())]
                    [(this-parameter ...)
                     (if method?
                         (generate-temporaries '(this))
                         null)])
        #`(#,dom-ctc-exprs
           #,rst-ctc-expr
           #,(if rng-ctc-exprs #`(list #,@rng-ctc-exprs) #f)
           #,(length (syntax->list dom-ctc-exprs)) ;; spec
           (dom-proj-x ... #,@(if rst-ctc-expr #'(rst-proj-x) #'()))
           (rng-proj-x ...)
           rng-ctcs-x
           (this-parameter ... dom-formals ... . #,(if rst-ctc-expr #'rst-formal '()))
           #,(cond
               [rng-ctc-exprs
                (let ([rng-checkers 
                       (list #`(case-lambda
                                 [(rng-id ...) (values/drop (rng-proj-x rng-id neg-party) ...)]
                                 [args 
                                  (bad-number-of-results blame #:missing-party neg-party f 
                                                         #,(length (syntax->list #'(rng-id ...)))
                                                         args
                                                         #,n)]))]
                      [rng-length (length (syntax->list rng-ctc-exprs))])
                  (if rst-ctc-expr
                      (check-tail-contract #'rng-ctcs-x
                                           blame-party-info neg-party
                                           rng-checkers
                                           (λ (rng-checks)
                                             #`(apply values #,@rng-checks this-parameter ...
                                                      (dom-proj-x dom-formals neg-party) ...
                                                      (rst-proj-x rst-formal neg-party)))
                                           #'(cons blame neg-party))
                      (check-tail-contract
                       #'rng-ctcs-x blame-party-info neg-party rng-checkers
                       (λ (rng-checks)
                         #`(values/drop #,@rng-checks this-parameter ...
                                        (dom-proj-x dom-formals neg-party) ...))
                       #'(cons blame neg-party))))]
               [rst-ctc-expr
                #`(apply values this-parameter ...
                         (dom-proj-x dom-formals neg-party) ...
                         (rst-proj-x rst-formal neg-party))]
               [else
                #`(values/drop this-parameter ...
                               (dom-proj-x dom-formals neg-party) ...)]))))))

(define-syntax (case-> stx)
  (syntax-case stx ()
    [(_ . args)
     (case->-internal (syntax/loc stx (case-> . args)) #|method?|# #f)]))

(define-for-syntax (case->-internal stx mctc?)
  (syntax-case stx ()
    [(_ cases ...)
     (let ()
       (define name (syntax-local-infer-name stx))
       (with-syntax ([(((dom-ctc-expr ...)
                        rst-ctc-expr
                        rng-ctc-exprs
                        spec
                        (dom-proj-x ...)
                        (rng-proj-x ...)
                        rng-ctcs-x
                        formals
                        body) ...)
                      (for/list ([x (in-list (syntax->list #'(cases ...)))]
                                 [n (in-naturals)])
                        (parse-out-case stx #'neg-party #'blame-party-info x n mctc?))])
         #`(build-case->
            (list (list dom-ctc-expr ...) ...)
            (list rst-ctc-expr ...)
            (list rng-ctc-exprs ...)
            '(spec ...)
            #,mctc?
            (λ (chk
                wrapper
                blame
                blame-party-info
                ctc
                rng-ctcs-x ...
                #,@(apply append (map syntax->list (syntax->list #'((dom-proj-x ...) ...))))
                #,@(apply append (map syntax->list (syntax->list #'((rng-proj-x ...) ...)))))
              (λ (f neg-party)
                (define blame+neg-party (cons blame neg-party))
                (put-it-together
                 #,(let ([case-lam (syntax/loc stx
                                     (case-lambda [formals body] ...))])
                     (if name
                         #`(let ([#,name #,case-lam]) #,name)
                         case-lam))
                 f blame neg-party blame+neg-party blame-party-info wrapper ctc
                 chk #,mctc?))))))]))

(define (put-it-together the-case-lam f blame neg-party blame+neg-party blame-party-info wrapper ctc chk mtd?)
  (chk f mtd?)
  (define rng-ctcs (base-case->-rng-ctcs ctc))
  (define checker
    (make-keyword-procedure
     (raise-no-keywords-error f blame neg-party)
     (λ args
       (with-contract-continuation-mark
        blame+neg-party
        (apply the-case-lam args)))))
  (define same-rngs (same-range-contracts rng-ctcs))
  (if same-rngs
      (wrapper
       f
       checker
       impersonator-prop:contracted ctc
       impersonator-prop:blame (blame-add-missing-party blame neg-party)
       impersonator-prop:application-mark
       (cons tail-contract-key (list* neg-party blame-party-info same-rngs)))
      (wrapper
       f
       checker
       impersonator-prop:contracted ctc
       impersonator-prop:blame (blame-add-missing-party blame neg-party))))

(define (raise-no-keywords-error f blame neg-party)
  (λ (kwds kwd-args . args)
    (raise-blame-error blame f #:missing-party neg-party
                       "expected no keywords, got keyword ~a" (car kwds))))

;; dom-ctcs : (listof (listof contract))
;; rst-ctcs : (listof contract)
;; rng-ctcs : (listof (listof contract))
;; specs : (listof (list boolean exact-positive-integer)) 
;;     indicates the required arities of the input functions
;; mctc? : was created with case->m or object-contract
;; wrapper : (->* () () (listof contract?) (-> procedure? procedure?)) 
;;     generates a wrapper from projections
(define-struct base-case-> (dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper))

(define (case->-proj wrapper)
  (λ (ctc)
    (define dom-ctcs+case-nums (get-case->-dom-ctcs+case-nums ctc))
    (define rng-ctcs (get-case->-rng-ctcs ctc))
    (define rng-lol-ctcs (base-case->-rng-ctcs ctc))
    (define rng-late-neg-ctcs (map get/build-late-neg-projection rng-ctcs))
    (define rst-ctcs (base-case->-rst-ctcs ctc))
    (define specs (base-case->-specs ctc))
    (λ (blame)
      (define dom-blame (blame-add-context blame "the domain of" #:swap? #t))
      (define rng-blame (blame-add-context blame "the range of"))
      (define blame-party-info (get-blame-party-info blame))
      (define projs (append rng-lol-ctcs
                            (map (λ (f) ((cdr f)
                                         (blame-add-context 
                                          (blame-add-context 
                                           blame 
                                           (format "the ~a case of" (n->th (+ (car f) 1)))) 
                                          "the domain of" 
                                          #:swap? #t)))
                                 dom-ctcs+case-nums)
                            (map (let ([memo '()])
                                   ;; to preserve procedure-closure-contents-eq?ness of the
                                   ;; wrapped procedures, memoize with f as the key.
                                   (λ (f)
                                     (define target
                                       (assoc f memo procedure-closure-contents-eq?))
                                     (if target
                                         (cdr target)
                                         (let* ([p   (f rng-blame)]
                                                [new (lambda args
                                                       (with-contract-continuation-mark
                                                        ;; last arg is missing party
                                                        (cons blame (last args))
                                                        (apply p args)))])
                                           (set! memo (cons (cons f new) memo))
                                           new))))
                                 rng-late-neg-ctcs)))
      (define (chk val mtd?) 
        (cond
          [(null? specs)
           (unless (procedure? val)
             (raise-blame-error blame val "expected a procedure"))]
          [else
           (for-each 
            (λ (dom-length has-rest?)
              (if has-rest?
                  (check-procedure/more val mtd? dom-length '() '() blame #f)
                  (check-procedure val mtd? dom-length 0 '() '() blame #f)))
            specs rst-ctcs)]))
      (apply (base-case->-wrapper ctc)
             chk
             wrapper
             blame
             blame-party-info
             ctc
             projs))))

;; Re `print-as-method-if-method?`: See comment before `base->-name` in arrow-val-first.rkt
(define ((case->-name print-as-method-if-method?) ctc)
  (apply
   build-compound-type-name
   (if (and (base-case->-mctc? ctc) print-as-method-if-method?) 'case->m 'case->)
   (map (λ (dom rst range)
          (apply 
           build-compound-type-name 
           '-> 
           (append dom
                   (if rst
                       (list '#:rest rst)
                       '())
                   (list
                    (cond
                      [(not range) 'any]
                      [(and (pair? range) (null? (cdr range)))
                       (car range)]
                      [else (apply build-compound-type-name 'values range)])))))
        (base-case->-dom-ctcs ctc)
        (base-case->-rst-ctcs ctc)
        (base-case->-rng-ctcs ctc))))

(define (case->-first-order ctc) (λ (val) (procedure? val)))

(define (case->-stronger? this that) #f)

(define-struct (chaperone-case-> base-case->) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:late-neg-projection (case->-proj chaperone-procedure)
   #:name (case->-name #|print-as-method-if-method?|# #t)
   #:first-order case->-first-order
   #:stronger case->-stronger?))

(define-struct (impersonator-case-> base-case->) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (case->-proj impersonate-procedure)
   #:name (case->-name #|print-as-method-if-method?|# #t)
   #:first-order case->-first-order
   #:stronger case->-stronger?))

(define (build-case-> dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper)
  (let ([dom-ctcs (map (λ (l) (map (λ (x) (coerce-contract 'case-> x)) l)) dom-ctcs)]
        [rst-ctcs (map (λ (x) (and x (coerce-contract 'case-> x))) rst-ctcs)]
        [rng-ctcs (map (λ (l) (and l (map (λ (x) (coerce-contract 'case-> x)) l))) rng-ctcs)])
    (if (and (andmap (λ (l) (andmap chaperone-contract? l)) dom-ctcs)
             (andmap (λ (c) (or (not c) (chaperone-contract? c))) rst-ctcs)
             (andmap (λ (l) (or (not l) (andmap chaperone-contract? l))) rng-ctcs))
        (make-chaperone-case-> dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper)
        (make-impersonator-case-> dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper))))

(define (get-case->-dom-ctcs+case-nums ctc)
  (for/fold ([acc '()])
      ([doms (in-list (base-case->-dom-ctcs ctc))]
       [rst  (in-list (base-case->-rst-ctcs ctc))]
       [i (in-naturals)])
    (define dom+case-nums 
      (map (λ (dom) (cons i (get/build-late-neg-projection dom))) doms))
    (append acc
            (if rst
                (append dom+case-nums
                        (list (cons i (get/build-late-neg-projection rst))))
                dom+case-nums))))

(define (get-case->-rng-ctcs ctc)
  (for/fold ([acc '()])
            ([x (in-list (base-case->-rng-ctcs ctc))]
             #:when x)
    (append acc x)))

;; Takes a list of (listof projection), and returns one of the
;; lists if all the lists contain the same projections. If the list is
;; null, it returns #f.
(define (same-range-contracts rng-ctcss)
  (cond
    [(null? rng-ctcss) #f]
    [else
     (define fst (car rng-ctcss))
     (and (for/and ([ps (in-list (cdr rng-ctcss))])
            (and ps
                 (= (length fst) (length ps))
                 (for/and ([c (in-list ps)]
                           [fst-c (in-list fst)])
                   (and (contract-struct-stronger? c fst-c)
                        (contract-struct-stronger? fst-c c)))))
          fst)]))
