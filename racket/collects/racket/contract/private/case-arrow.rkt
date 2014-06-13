#lang racket/base

(require (for-syntax racket/base
                     syntax/name)
         racket/stxparam
         "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt"
         "arrow.rkt"
         "arrow-val-first.rkt")

(provide case->)


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
  (syntax-case case (-> ->2)
    [(-> doms ... #:rest rst rng)
     (values #'(doms ...) #'rst (parse-rng stx #'rng))]
    [(->2 doms ... #:rest rst rng)
     (values #'(doms ...) #'rst (parse-rng stx #'rng))]
    [(-> doms ... rng)
     (values #'(doms ...) #f (parse-rng stx #'rng))]
    [(->2 doms ... rng)
     (values #'(doms ...) #f (parse-rng stx #'rng))]
    [(x y ...)
     (raise-syntax-error #f "expected ->" stx #'x)]
    [_
     (raise-syntax-error #f "expected ->" stx case)]))

(define-for-syntax (parse-out-case stx case n)
  (let-values ([(doms rst rng) (separate-out-doms/rst/rng stx case)])
    (with-syntax ([(dom-proj-x  ...) (generate-temporaries doms)]
                  [(rst-proj-x) (generate-temporaries '(rest-proj-x))]
                  [(rng-proj-x ...) (generate-temporaries (if rng rng '()))])
      (with-syntax ([(dom-formals ...) (generate-temporaries doms)]
                    [(rst-formal) (generate-temporaries '(rest-param))]
                    [(rng-id ...) (if rng
                                      (generate-temporaries rng)
                                      '())]
                    [(this-parameter ...)
                     (make-this-parameters (car (generate-temporaries '(this))))])
        #`(#,doms
           #,rst
           #,(if rng #`(list #,@rng) #f)
           #,(length (syntax->list doms)) ;; spec
           (dom-proj-x ... #,@(if rst #'(rst-proj-x) #'()))
           (rng-proj-x ...)
           (this-parameter ... dom-formals ... . #,(if rst #'rst-formal '()))
           #,(cond
               [rng
                (let ([rng-checkers 
                       (list #`(case-lambda
                                 [(rng-id ...) (values/drop (rng-proj-x rng-id) ...)]
                                 [args 
                                  (bad-number-of-results blame f 
                                                         #,(length (syntax->list #'(rng-id ...)))
                                                         args
                                                         #,n)]))]
                      [rng-length (length (syntax->list rng))])
                  (if rst
                      (check-tail-contract #'(rng-proj-x ...) rng-checkers
                                           (λ (rng-checks)
                                             #`(apply values #,@rng-checks this-parameter ...
                                                      (dom-proj-x dom-formals) ...
                                                      (rst-proj-x rst-formal))))
                      (check-tail-contract #'(rng-proj-x ...) rng-checkers
                                           (λ (rng-checks)
                                             #`(values/drop #,@rng-checks this-parameter ...
                                                            (dom-proj-x dom-formals) ...)))))]
               [rst
                #`(apply values this-parameter ...
                         (dom-proj-x dom-formals) ...
                         (rst-proj-x rst-formal))]
               [else
                #`(values/drop this-parameter ...
                               (dom-proj-x dom-formals) ...)]))))))

(define-syntax (case-> stx)
  (syntax-case stx ()
    [(_ cases ...)
     (let ()
       (define name (syntax-local-infer-name stx))
       (with-syntax ([(((dom-proj ...)
                        rst-proj
                        rng-proj
                        spec
                        (dom-proj-x ...)
                        (rng-proj-x ...)
                        formals
                        body) ...)
                      (for/list ([x (in-list (syntax->list #'(cases ...)))]
                                 [n (in-naturals)])
                        (parse-out-case stx x n))]
                     [mctc? (and (syntax-parameter-value #'method-contract?) #t)])
         #`(syntax-parameterize 
            ((making-a-method #f)) 
            (build-case-> 
             (list (list dom-proj ...) ...)
             (list rst-proj ...)
             (list rng-proj ...)
             '(spec ...)
             mctc?
             (λ (chk
                 wrapper
                 blame
                 ctc
                 #,@(apply append (map syntax->list (syntax->list #'((dom-proj-x ...) ...))))
                 #,@(apply append (map syntax->list (syntax->list #'((rng-proj-x ...) ...)))))
               (λ (f)
                 (put-it-together 
                  #,(let ([case-lam (syntax/loc stx 
                                      (case-lambda [formals body] ...))])
                      (if name
                          #`(let ([#,name #,case-lam]) #,name)
                          case-lam))
                  (list (list rng-proj-x ...) ...)
                  f blame wrapper ctc
                  chk #,(and (syntax-parameter-value #'making-a-method) #t))))))))]))

(define (put-it-together the-case-lam range-projections f blame wrapper ctc chk mtd?)
  (chk f mtd?)
  (define checker
    (make-keyword-procedure
     (raise-no-keywords-error f blame)
     (λ args
       (with-continuation-mark contract-continuation-mark-key blame
         (apply the-case-lam args)))))
  (define same-rngs (same-range-projections range-projections))
  (if same-rngs
      (wrapper
       f
       checker
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame
       impersonator-prop:application-mark (cons contract-key same-rngs))
      (wrapper
       f
       checker
       impersonator-prop:contracted ctc
       impersonator-prop:blame blame)))

(define (raise-no-keywords-error f blame)
  (λ (kwds kwd-args . args)
    (raise-blame-error blame f "expected no keywords, got keyword ~a" (car kwds))))

;; dom-ctcs : (listof (listof contract))
;; rst-ctcs : (listof contract)
;; rng-ctcs : (listof (listof contract))
;; specs : (listof (list boolean exact-positive-integer)) 
;;     indicates the required arities of the input functions
;; mctc? : was created with case->m
;; wrapper : (->* () () (listof contract?) (-> procedure? procedure?)) 
;;     generates a wrapper from projections
(define-struct base-case-> (dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper))

(define (case->-proj wrapper)
  (λ (ctc)
    (define dom-ctcs+case-nums (get-case->-dom-ctcs+case-nums ctc))
    (define rng-ctcs (map contract-projection
                          (get-case->-rng-ctcs ctc)))
    (define rst-ctcs (base-case->-rst-ctcs ctc))
    (define specs (base-case->-specs ctc))
    (λ (blame)
      (define dom-blame (blame-add-context blame "the domain of" #:swap? #t))
      (define rng-blame (blame-add-context blame "the range of"))
      (define projs (append (map (λ (f) ((cdr f) 
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
                                                       (with-continuation-mark
                                                        contract-continuation-mark-key blame
                                                        (apply p args)))])
                                           (set! memo (cons (cons f new) memo))
                                           new))))
                                 rng-ctcs)))
      (define (chk val mtd?) 
        (cond
          [(null? specs)
           (unless (procedure? val)
             (raise-blame-error blame val "expected a procedure"))]
          [else
           (for-each 
            (λ (dom-length has-rest?)
              (if has-rest?
                  (check-procedure/more val mtd? dom-length '() '() blame)
                  (check-procedure val mtd? dom-length 0 '() '() blame)))
            specs rst-ctcs)]))
      (apply (base-case->-wrapper ctc)
             chk
             wrapper
             blame
             ctc
             projs))))

(define (case->-name ctc)
  (apply
   build-compound-type-name
   (if (base-case->-mctc? ctc) 'case->m 'case->)
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
   #:projection (case->-proj chaperone-procedure)
   #:name case->-name
   #:first-order case->-first-order
   #:stronger case->-stronger?))

(define-struct (impersonator-case-> base-case->) ()
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:projection (case->-proj impersonate-procedure)
   #:name case->-name
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
      (map (λ (dom) (cons i (contract-projection dom))) doms))
    (append acc
            (if rst
                (append dom+case-nums
                        (list (cons i (contract-projection rst))))
                dom+case-nums))))

(define (get-case->-rng-ctcs ctc)
  (for/fold ([acc '()])
      ([x (in-list (base-case->-rng-ctcs ctc))]
       #:when x)
    (append acc x)))

;; Takes a list of (listof projection), and returns one of the
;; lists if all the lists contain the same projections. If the list is
;; null, it returns #f.
(define (same-range-projections rng-ctcss)
  (if (null? rng-ctcss)
      #f
      (let* ([fst (car rng-ctcss)]
             [all-same? (for/and ([ps (in-list (cdr rng-ctcss))])
                          (and (= (length fst) (length ps))
                               (andmap procedure-closure-contents-eq? fst ps)))])
        (and all-same? fst))))
