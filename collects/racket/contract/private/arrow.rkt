#lang racket/base

#|

v4 todo:

- add case-> to object-contract

- test object-contract with keywords (both optional and mandatory)

- change mzlib/contract to rewrite into scheme/contract (maybe?)

- raise-syntax-errors
  . multiple identical keywords syntax error, sort-keywords
  . split-doms

- note timing/size tests at the end of the file.

|#


(require "guts.rkt"
         "blame.rkt"
         "prop.rkt"
         "misc.rkt"
         "generate.rkt"
         racket/stxparam
         racket/private/performance-hint)
(require (for-syntax racket/base)
         (for-syntax "helpers.rkt")
         (for-syntax syntax/stx)
         (for-syntax syntax/name)
         (for-syntax "arr-util.rkt"))

(provide ->
         base->?
         ->*
         ->d
         case->
         base->-rngs/c
         base->-doms/c
         unconstrained-domain->
         the-unsupplied-arg
         (rename-out [-predicate/c predicate/c])
         unsupplied-arg?
         making-a-method
         method-contract?
         procedure-accepts-and-more?
         check-procedure
         check-procedure/more
         contracted-function?
         contracted-function-proc
         contracted-function-ctc
         make-contracted-function
         matches-arity-exactly?
         bad-number-of-results)

(define-syntax-parameter making-a-method #f)
(define-syntax-parameter method-contract? #f)
(define-for-syntax (make-this-parameters id)
  (if (syntax-parameter-value #'making-a-method)
      (list id)
      null))

(define-struct contracted-function (proc ctc)
  #:property prop:procedure 0
  #:property prop:contracted 1)

(define contract-key (gensym 'contract-key))

(define-for-syntax (check-tail-contract rng-ctcs rng-checkers call-gen)
  #`(call-with-immediate-continuation-mark
     contract-key
     (λ (m)
       (cond
         [(tail-marks-match? m . #,rng-ctcs)
          #,(call-gen #'())]
         [else #,(call-gen rng-checkers)]))))

(begin-encourage-inline
 (define tail-marks-match?
    (case-lambda
      [(m) (and m (null? m))]
      [(m rng-ctc) (and m
                        (not (null? m))
                        (null? (cdr m))
                        (procedure-closure-contents-eq? (car m) rng-ctc))]
      [(m rng-ctc1 rng-ctc2)
       (and m
            (= (length m) 2)
            (procedure-closure-contents-eq? (car m) rng-ctc1)
            (procedure-closure-contents-eq? (cadr m) rng-ctc1))]
      [(m . rng-ctcs)
       (and m
            (= (length m) (length rng-ctcs))
            (andmap procedure-closure-contents-eq? m rng-ctcs))])))

(define-syntax (unconstrained-domain-> stx)
  (syntax-case stx ()
    [(_ rngs ...)
     (with-syntax ([(rngs-x ...) (generate-temporaries #'(rngs ...))]
                   [(proj-x ...) (generate-temporaries #'(rngs ...))]
                   [(p-app-x ...) (generate-temporaries #'(rngs ...))]
                   [(res-x ...) (generate-temporaries #'(rngs ...))])
       #`(let ([rngs-x (coerce-contract 'unconstrained-domain-> rngs)] ...)
           (let ([proj-x (contract-projection rngs-x)] ...)
             (define name
               (build-compound-type-name 'unconstrained-domain-> (contract-name rngs-x) ...))
             (define (projection wrapper)
               (λ (orig-blame)
                 (let ([rng-blame (blame-add-context orig-blame "the range of")])
                   (let* ([p-app-x (proj-x rng-blame)] ...
                          [res-checker (λ (res-x ...) (values (p-app-x res-x) ...))])
                     (λ (val)
                       (unless (procedure? val)
                         (raise-blame-error orig-blame val '(expected: "a procedure" given: "~v") val))
                       (wrapper
                        val
                        (make-keyword-procedure
                         (λ (kwds kwd-vals . args)
                           #,(check-tail-contract
                              #'(p-app-x ...)
                              (list #'res-checker)
                              (λ (s) #`(apply values #,@s kwd-vals args))))
                         (λ args
                           #,(check-tail-contract
                              #'(p-app-x ...)
                              (list #'res-checker)
                              (λ (s) #`(apply values #,@s args)))))
                        impersonator-prop:contracted ctc
                        impersonator-prop:application-mark (cons contract-key (list p-app-x ...))))))))
             (define ctc
               (if (and (chaperone-contract? rngs-x) ...)
                   (make-chaperone-contract
                    #:name name
                    #:projection (projection chaperone-procedure)
                    #:first-order procedure?)
                   (make-contract
                    #:name name
                    #:projection (projection impersonate-procedure)
                    #:first-order procedure?)))
             ctc)))]))


;              
;              
;              
;         ;    
;          ;   
;           ;  
;   ;;;;;;;;;; 
;           ;  
;          ;   
;         ;    
;              
;              
;              
;              

;; matches-arity-exactly? : procedure number (or/c number #f) (listof keyword?) (listof keyword?) -> boolean
(define (matches-arity-exactly? val min-arity max-arity contract-req-kwds contract-opt-kwds)
  (define proc-arity (procedure-arity val)) 
  (and (let-values ([(vr va) (procedure-keywords val)])
         (and va (equal? vr contract-req-kwds) 
	         (keywords-match? va contract-req-kwds contract-opt-kwds)))
       (cond
         [(number? proc-arity) (and (number? max-arity)
                                    (= min-arity max-arity)
                                    (= proc-arity min-arity))]
         [(arity-at-least? proc-arity) (and (not max-arity)
                                            (= (arity-at-least-value proc-arity)
                                               min-arity))]
         [else
          (let loop ([arity proc-arity]
                     [i min-arity])
            (cond
              [(null? arity) 
               (= i (+ max-arity 1))]
              [else
               (let ([fst (car arity)])
                 (if (arity-at-least? fst)
                     (and (number? max-arity)
                          (= (arity-at-least-value fst)
                             max-arity))
                     (and (= i fst)
                          (loop (cdr arity)
                                (+ i 1)))))]))])))

(define (keywords-match? accepted-keywords contract-req-kwds contract-opt-kwds)
  (let loop ([accepted accepted-keywords]
             [req-kwds contract-req-kwds]
             [opt-kwds contract-opt-kwds])
    (cond
      [(null? accepted) (and (null? opt-kwds) (null? req-kwds))]
      [else
       (let ([kwd (car accepted)])
         (cond
          [(and (pair? req-kwds) (eq? (car req-kwds) kwd))
           (loop (cdr accepted) (cdr req-kwds) opt-kwds)]
          [(and (pair? opt-kwds) (eq? (car opt-kwds) kwd))
           (loop (cdr accepted) req-kwds (cdr opt-kwds))]
          [else #f]))])))

(define-for-syntax (create-chaperone blame val pre post this-args doms opt-doms dom-rest req-kwds opt-kwds rngs)
  (with-syntax ([blame blame]
                [val val])
    (with-syntax ([(pre ...) 
                   (if pre
                       (list #`(unless #,pre
                                 (raise-blame-error (blame-swap blame) val "#:pre condition")))
                       null)]
                  [(post ...)
                   (if post
                       (list #`(unless #,post
                                 (raise-blame-error blame val "#:post condition")))
                       null)])
      (with-syntax ([(this-param ...) this-args]
                    [(dom-ctc ...) doms]
                    [(dom-x ...) (generate-temporaries doms)]
                    [(opt-dom-ctc ...) opt-doms]
                    [(opt-dom-x ...) (generate-temporaries opt-doms)]
                    [(rest-ctc (rest-x)) (list dom-rest (generate-temporaries '(rest)))]
                    [(req-kwd ...) (map car req-kwds)]
                    [(req-kwd-ctc ...) (map cadr req-kwds)]
                    [(req-kwd-x ...) (generate-temporaries (map car req-kwds))]
                    [(opt-kwd ...) (map car opt-kwds)]
                    [(opt-kwd-ctc ...) (map cadr opt-kwds)]
                    [(opt-kwd-x ...) (generate-temporaries (map car opt-kwds))]
                    [(rng-ctc ...) (if rngs rngs '())]
                    [(rng-x ...) (if rngs (generate-temporaries rngs) '())])
        (with-syntax ([(rng-checker-name ...)
                       (if rngs
                           (list (gensym 'rng-checker))
                           null)]
                      [(rng-checker ...)
                       (if rngs
                           (list
                            (with-syntax ([rng-len (length rngs)])
                              (with-syntax ([rng-results
                                             (if (and (pair? rngs) (null? (cdr rngs)))
                                                 (with-syntax ([proj (car (syntax->list #'(rng-ctc ...)))]
                                                               [name (car (syntax->list #'(rng-x ...)))])
                                                   #'(proj name))
                                                 #'(values (rng-ctc rng-x) ...))])
                                #'(case-lambda
                                    [(rng-x ...)
                                     post ...
                                     rng-results]
                                    [args
                                     (bad-number-of-results blame val rng-len args)]))))
                           null)])
          (let* ([min-method-arity (length doms)]
                 [max-method-arity (+ min-method-arity (length opt-doms))]
                 [min-arity (+ (length this-args) min-method-arity)]
                 [max-arity (+ min-arity (length opt-doms))]
                 [req-keywords (map (λ (p) (syntax-e (car p))) req-kwds)]
                 [opt-keywords (map (λ (p) (syntax-e (car p))) opt-kwds)]
                 [need-apply-values? (or dom-rest (not (null? opt-doms)))]
                 [no-rng-checking? (not rngs)])
            (with-syntax ([basic-params
                           (cond
                             [dom-rest
                              #'(this-param ... dom-x ... [opt-dom-x unspecified-dom] ... . rest-x)]
                             [else
                              #'(this-param ... dom-x ... [opt-dom-x unspecified-dom] ...)])]
                          [opt+rest-uses
                           (for/fold ([i (if dom-rest #'(rest-ctc rest-x) #'null)])
                             ([o (in-list (reverse (syntax->list #'([opt-dom-ctc opt-dom-x] ...))))])
                             (let* ([l (syntax->list o)]
                                    [c (car l)]
                                    [x (cadr l)])
                               #`(let ([r #,i])
                                   (if (eq? unspecified-dom #,x) r (cons (#,c #,x) r)))))]
                          [(kwd-param ...)
                           (apply append
                                  (map list
                                       (syntax->list #'(req-kwd ... opt-kwd ...))
                                       (syntax->list #'(req-kwd-x ... [opt-kwd-x unspecified-dom] ...))))]
                          [kwd-stx
                           (let* ([req-stxs
                                   (map (λ (s) (λ (r) #`(cons #,s #,r)))
                                        (syntax->list #'((req-kwd-ctc req-kwd-x) ...)))]
                                  [opt-stxs 
                                   (map (λ (x c) (λ (r) #`(let ([r #,r]) (if (eq? unspecified-dom #,x) r (cons (#,c #,x) r)))))
                                        (syntax->list #'(opt-kwd-x ...))
                                        (syntax->list #'(opt-kwd-ctc ...)))]
                                  [reqs (map cons req-keywords req-stxs)]
                                  [opts (map cons opt-keywords opt-stxs)]
                                  [all-together-now (append reqs opts)]
                                  [put-in-reverse (sort all-together-now (λ (k1 k2) (keyword<? k2 k1)) #:key car)])
                             (for/fold ([s #'null])
                               ([tx (in-list (map cdr put-in-reverse))])
                               (tx s)))])
              (with-syntax ([kwd-lam-params
                             (if dom-rest
                                 #'(dom-x ... [opt-dom-x unspecified-dom] ... kwd-param ... . rest-x)
                                 #'(dom-x ... [opt-dom-x unspecified-dom] ... kwd-param ...))]
                            [basic-return
                             (let ([inner-stx-gen
                                    (if need-apply-values?
                                        (λ (s) #`(apply values #,@s this-param ... (dom-ctc dom-x) ... opt+rest-uses))
                                        (λ (s) #`(values #,@s this-param ... (dom-ctc dom-x) ...)))])
                               (if no-rng-checking?
                                   (inner-stx-gen #'())
                                   (check-tail-contract #'(rng-ctc ...) #'(rng-checker-name ...) inner-stx-gen)))]
                            [kwd-return
                             (let* ([inner-stx-gen
                                     (if need-apply-values?
                                         (λ (s k) #`(apply values #,@s #,@k this-param ... (dom-ctc dom-x) ... opt+rest-uses))
                                         (λ (s k) #`(values #,@s #,@k this-param ... (dom-ctc dom-x) ...)))]
                                    [outer-stx-gen
                                     (if (null? req-keywords)
                                         (λ (s)
                                           #`(if (null? kwd-results)
                                                 #,(inner-stx-gen s #'())
                                                 #,(inner-stx-gen s #'(kwd-results))))
                                         (λ (s)
                                           (inner-stx-gen s #'(kwd-results))))])
                               #`(let ([kwd-results kwd-stx])
                                   #,(if no-rng-checking?
                                         (outer-stx-gen #'())
                                         (check-tail-contract #'(rng-ctc ...) #'(rng-checker-name ...) outer-stx-gen))))])
                (with-syntax ([basic-lambda-name (gensym 'basic-lambda)]
                              [basic-lambda #'(λ basic-params pre ... basic-return)]
                              [kwd-lambda-name (gensym 'kwd-lambda)]
                              [kwd-lambda #`(λ kwd-lam-params pre ... kwd-return)])
                  (with-syntax ([(basic-checker-name) (generate-temporaries '(basic-checker))])
                    (cond
                      [(and (null? req-keywords) (null? opt-keywords))
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([basic-lambda-name basic-lambda])
                             (arity-checking-wrapper val blame
                                                     basic-lambda-name
                                                     void
                                                     #,min-method-arity
                                                     #,max-method-arity
                                                     #,min-arity
                                                     #,(if dom-rest #f max-arity)
                                                     '(req-kwd ...)
                                                     '(opt-kwd ...))))]
                      [(pair? req-keywords)
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([kwd-lambda-name kwd-lambda])
                             (arity-checking-wrapper val blame
                                                     void
                                                     kwd-lambda-name
                                                     #,min-method-arity
                                                     #,max-method-arity
                                                     #,min-arity
                                                     #,(if dom-rest #f max-arity)
                                                     '(req-kwd ...)
                                                     '(opt-kwd ...))))]
                      [else
                       #`(let-values ([(rng-checker-name ...) (values rng-checker ...)])
                           (let ([basic-lambda-name basic-lambda]
                                 [kwd-lambda-name kwd-lambda])
                             (arity-checking-wrapper val blame
                                                     basic-lambda-name
                                                     kwd-lambda-name
                                                     #,min-method-arity
                                                     #,max-method-arity
                                                     #,min-arity
                                                     #,(if dom-rest #f max-arity)
                                                     '(req-kwd ...)
                                                     '(opt-kwd ...))))])))))))))))

;; should we pass both the basic-lambda and the kwd-lambda?
(define (arity-checking-wrapper val blame basic-lambda kwd-lambda min-method-arity max-method-arity min-arity max-arity req-kwd opt-kwd)
  ;; should not build this unless we are in the 'else' case (and maybe not at all)
  (cond
    [(matches-arity-exactly? val min-arity max-arity req-kwd opt-kwd)
     (if (and (null? req-kwd) (null? opt-kwd)) 
         basic-lambda
         kwd-lambda)]
    [else
     (define arity-string 
       (if max-arity
           (if (= min-method-arity max-method-arity)
               (format "~a non-keyword argument~a" min-method-arity (if (= min-method-arity 1) "" "s"))
               (format "~a to ~a non-keyword arguments" min-method-arity max-method-arity))
           (format "at least ~a non-keyword argument~a" min-method-arity (if (= min-method-arity 1) "" "s"))))
     (define-values (vr va) (procedure-keywords val))
     (define all-kwds (append req-kwd opt-kwd))
     (define (valid-number-of-args? args)
       (if max-arity
           (<= min-arity (length args) max-arity)
           (<= min-arity (length args))))
     (define kwd-checker
       (if (and (null? req-kwd) (null? opt-kwd))
           (λ (kwds kwd-args . args)
             (raise-blame-error (blame-swap blame) val
                                '(expected: "no keywords")))
           (λ (kwds kwd-args . args)
             (define args-len (length args))
             (unless (valid-number-of-args? args)
               (raise-blame-error (blame-swap blame) val
                                  '("received ~a argument~a" expected: "~a")
                                  args-len (if (= args-len 1) "" "s") arity-string))
             
             ;; these two for loops are doing O(n^2) work that could be linear 
             ;; (since the keyword lists are sorted)
             (for ([req-kwd (in-list req-kwd)])
               (unless (memq req-kwd kwds)
                 (raise-blame-error (blame-swap blame) val
                                    '(expected "keyword argument ~a")
                                    req-kwd)))
             (for ([k (in-list kwds)])
               (unless (memq k all-kwds)
                 (raise-blame-error (blame-swap blame) val
                                    "received unexpected keyword argument ~a"
                                    k)))
             (keyword-apply kwd-lambda kwds kwd-args args))))
     (define basic-checker-name 
       (if (null? req-kwd)
           (λ args
             (unless (valid-number-of-args? args)
               (define args-len (length args))
               (raise-blame-error (blame-swap blame) val
                                  '("received ~a argument~a" expected: "~a")
                                  args-len (if (= args-len 1) "" "s") arity-string))
             (apply basic-lambda args))
           (λ args
             (raise-blame-error (blame-swap blame) val
                                "expected required keyword ~a"
                                (car req-kwd)))))
     (if (or (not va) (pair? vr) (pair? va))
         (make-keyword-procedure kwd-checker basic-checker-name)
         basic-checker-name)]))

;; pre : (or/c #f (-> any)) -- checks the pre-condition, if there is one.
;; post : (or/c #f (-> any)) -- checks the post-condition, if there is one.
;; doms : (listof contract)
;; optional-doms/c : (listof contract)
;; dom-rest : (or/c false/c contract)
;; mandatory-kwds/c : (listof contract)
;; mandatory-kwds : (listof keyword) -- must be sorted by keyword<
;; optional-kwds/c : (listof contract)
;; optional-kwds : (listof keyword) -- must be sorted by keyword<
;; rngs : (listof contract) -- may be ignored by the wrapper function in the case of any
;; rng-any? : boolean
;; mtd? : contract is for a method (implicit this in first position)
;; mctc? : contract was formed with ->m or ->*m (so print out same way)
;; func : the wrapper function maker. It accepts a procedure for
;;        checking the first-order properties and the contracts
;;        and it produces a wrapper-making function.
(define-struct base-> (pre post
                       doms/c optional-doms/c dom-rest/c
                       mandatory-kwds/c mandatory-kwds
                       optional-kwds/c optional-kwds
                       rngs/c rng-any?
                       mtd? mctc?
                       func))

(define ((->-proj wrapper) ctc) 
  (let* ([doms-proj (map contract-projection
                         (if (base->-dom-rest/c ctc)
                             (append (base->-doms/c ctc) (list (base->-dom-rest/c ctc)))
                             (base->-doms/c ctc)))]
         [doms-optional-proj (map contract-projection (base->-optional-doms/c ctc))]
         [rngs-proj (map contract-projection (base->-rngs/c ctc))]
         [mandatory-kwds-proj (map contract-projection (base->-mandatory-kwds/c ctc))]
         [optional-kwds-proj (map contract-projection (base->-optional-kwds/c ctc))]
         [mandatory-keywords (base->-mandatory-kwds ctc)]
         [optional-keywords (base->-optional-kwds ctc)]
         [func (base->-func ctc)]
         [dom-length (length (base->-doms/c ctc))]
         [optionals-length (length (base->-optional-doms/c ctc))]
         [has-rest? (and (base->-dom-rest/c ctc) #t)]
         [pre (base->-pre ctc)]
         [post (base->-post ctc)]
         [mtd? (base->-mtd? ctc)])
    (λ (orig-blame)
      (define rng-blame (blame-add-context orig-blame "the range of"))
      (define swapped-domain (blame-add-context orig-blame "the domain of" #:swap? #t))
      (define partial-doms 
        (for/list ([dom (in-list doms-proj)]
                   [n (in-naturals 1)])
          (dom (blame-add-context orig-blame 
                                  (if (and has-rest?
                                           (n . > . dom-length))
                                      "the rest argument of"
                                      (format "the ~a argument of"
                                              (n->th n)))
                                  #:swap? #t))))
      (define partial-optional-doms 
        (for/list ([dom (in-list doms-optional-proj)]
                   [n (in-naturals (+ 1 (length doms-proj)))])
          (dom (blame-add-context orig-blame
                                  (format "the ~a argument of"
                                          (n->th n))
                                  #:swap? #t))))
      (define partial-ranges (map (λ (rng) (rng rng-blame)) rngs-proj))
      (define partial-mandatory-kwds 
        (for/list ([kwd-proj (in-list mandatory-kwds-proj)]
                   [kwd (in-list mandatory-keywords)])
          (kwd-proj (blame-add-context orig-blame 
                                       (format "the ~a argument of" kwd)
                                       #:swap? #t))))
      (define partial-optional-kwds 
        (for/list ([kwd-proj (in-list optional-kwds-proj)]
                   [kwd (in-list optional-keywords)])
          (kwd-proj (blame-add-context orig-blame
                                       (format "the ~a argument of" kwd)
                                       #:swap? #t))))
      (define the-args (append partial-doms partial-optional-doms 
                               partial-mandatory-kwds partial-optional-kwds
                               partial-ranges))
      (λ (val)
        (if has-rest?
            (check-procedure/more val mtd? dom-length mandatory-keywords optional-keywords orig-blame)
            (check-procedure val mtd? dom-length optionals-length mandatory-keywords optional-keywords orig-blame))
        (define chap/imp-func (apply func orig-blame val the-args))
        (if post
            (wrapper
             val
             chap/imp-func
             impersonator-prop:contracted ctc)
            (wrapper
             val
             chap/imp-func
             impersonator-prop:contracted ctc
             impersonator-prop:application-mark (cons contract-key 
                                                      ;; is this right?
                                                      partial-ranges)))))))

(define (->-name ctc)
  (single-arrow-name-maker 
   (base->-doms/c ctc)
   (base->-optional-doms/c ctc)
   (base->-dom-rest/c ctc)
   (base->-mandatory-kwds/c ctc)
   (base->-mandatory-kwds ctc)
   (base->-optional-kwds/c ctc)
   (base->-optional-kwds ctc)
   (base->-rng-any? ctc)
   (base->-rngs/c ctc)
   (base->-pre ctc)
   (base->-post ctc)
   (base->-mctc? ctc)))

(define (->-first-order ctc)
  (λ (x)
    (let ([l (length (base->-doms/c ctc))])
      (and (procedure? x) 
           (if (base->-dom-rest/c ctc)
               (procedure-accepts-and-more? x l)
               (procedure-arity-includes? x l #t))
           (keywords-match (base->-mandatory-kwds ctc) (base->-optional-kwds ctc) x)
           #t))))

(define (->-stronger? this that)
  (and (base->? that)
       (= (length (base->-doms/c that)) (length (base->-doms/c this)))
       (andmap contract-stronger? (base->-doms/c that) (base->-doms/c this))
       
       (equal? (base->-mandatory-kwds this) (base->-mandatory-kwds that))
       (andmap contract-stronger? (base->-mandatory-kwds/c that) (base->-mandatory-kwds/c this))
       
       (equal? (base->-optional-kwds this) (base->-optional-kwds that))
       (andmap contract-stronger? (base->-optional-kwds/c that) (base->-optional-kwds/c this))
       
       (= (length (base->-rngs/c that)) (length (base->-rngs/c this)))
       (andmap contract-stronger? (base->-rngs/c this) (base->-rngs/c that))))

(define (->-generate ctc)
  (let ([doms-l (length (base->-doms/c ctc))])
    (λ (fuel)
       (let ([rngs-gens (map (λ (c) (generate/choose c (/ fuel 2)))
                             (base->-rngs/c ctc))])
         (if (member #t (map generate-ctc-fail? rngs-gens))
           (make-generate-ctc-fail)
           (procedure-reduce-arity
             (λ args
                ; Make sure that the args match the contract
                (begin (unless ((contract-struct-exercise ctc) args (/ fuel 2))
                           (error "Arg(s) ~a do(es) not match contract ~a\n" ctc))
                       ; Stash the valid value
                       ;(env-stash (generate-env) ctc args)
                       (apply values rngs-gens)))
             doms-l))))))

(define (->-exercise ctc) 
  (λ (args fuel)
     (let* ([new-fuel (/ fuel 2)]
            [gen-if-fun (λ (c v)
                           ; If v is a function we need to gen the domain and call
                           (if (procedure? v)
                             (let ([newargs (map (λ (c) (contract-random-generate c new-fuel))
                                                 (base->-doms/c c))])
                               (let* ([result (call-with-values 
                                                (λ () (apply v newargs))
                                                list)]
                                      [rngs (base->-rngs/c c)])
                                 (andmap (λ (c v) 
                                            ((contract-struct-exercise c) v new-fuel))
                                         rngs 
                                         result)))
                             ; Delegate to check-ctc-val
                             ((contract-struct-exercise c) v new-fuel)))])
       (andmap gen-if-fun (base->-doms/c ctc) args))))

(define-struct (chaperone-> base->) ()
  #:property prop:chaperone-contract
  (parameterize ([skip-projection-wrapper? #t])
    (build-chaperone-contract-property
     #:projection (->-proj chaperone-procedure)
     #:name ->-name
     #:first-order ->-first-order
     #:stronger ->-stronger?
     #:generate ->-generate
     #:exercise ->-exercise)))

(define-struct (impersonator-> base->) ()
  #:property prop:contract
  (build-contract-property
   #:projection (->-proj impersonate-procedure)
   #:name ->-name
   #:first-order ->-first-order
   #:stronger ->-stronger?
   #:generate ->-generate
   #:exercise ->-exercise))

(define (build--> name
                  pre post
                  doms/c-or-p optional-doms/c-or-p doms-rest/c-or-p-or-f 
                  mandatory-kwds/c-or-p mandatory-kwds optional-kwds/c-or-p optional-kwds
                  rngs/c-or-p
                  rng-any? mtd? ->m-ctc? func)
  (let ([cc (λ (c-or-p) (coerce-contract name c-or-p))])
    (let ([doms/c (map cc doms/c-or-p)]
          [opt-doms/c (map cc optional-doms/c-or-p)]
          [rest/c (and doms-rest/c-or-p-or-f (cc doms-rest/c-or-p-or-f))]
          [kwds/c (map cc mandatory-kwds/c-or-p)]
          [opt-kwds/c (map cc optional-kwds/c-or-p)]
          [rngs/c (map cc rngs/c-or-p)])
      (if (and (andmap chaperone-contract? doms/c)
               (andmap chaperone-contract? opt-doms/c)
               (or (not rest/c) (chaperone-contract? rest/c))
               (andmap chaperone-contract? kwds/c)
               (andmap chaperone-contract? opt-kwds/c)
               (or rng-any? (andmap chaperone-contract? rngs/c)))
          (make-chaperone-> pre post doms/c opt-doms/c rest/c
                            kwds/c mandatory-kwds opt-kwds/c optional-kwds
                            rngs/c rng-any? mtd? ->m-ctc? func)
          (make-impersonator-> pre post doms/c opt-doms/c rest/c
                               kwds/c mandatory-kwds opt-kwds/c optional-kwds
                               rngs/c rng-any? mtd? ->m-ctc? func)))))

(define (single-arrow-name-maker doms/c optional-doms/c doms-rest kwds/c kwds optional-kwds/c optional-kwds rng-any? rngs pre post ->m-ctc?)
  (cond
    [(or doms-rest 
         (not (null? optional-kwds))
         (not (null? optional-doms/c))
         pre post)
     (let ([name (if ->m-ctc? '->*m '->*)]
           [range
            (cond
              [rng-any? 'any]
              [(and (pair? rngs)
                    (null? (cdr rngs)))
               (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply
        build-compound-type-name 
        name
        (apply build-compound-type-name (append doms/c (apply append (map list kwds kwds/c))))
        (append (let ([opts
                       (apply build-compound-type-name (append optional-doms/c (apply append (map list optional-kwds optional-kwds/c))))])
                  (if (null? opts)
                      '()
                      (list opts)))
                (if doms-rest
                    (list '#:rest doms-rest)
                    (list))
                (if pre 
                    (list '#:pre '...)
                    (list))
                (list range)
                (if post
                    (list '#:post '...)
                    (list)))))]
    [else
     (let ([name (if ->m-ctc? '->m '->)]
           [rng-name
            (cond
              [rng-any? 'any]
              [(null? rngs) '(values)]
              [(null? (cdr rngs)) (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply build-compound-type-name 
              name
              (append doms/c
                      (apply append (map list kwds kwds/c))
                      (list rng-name))))]))

(define-for-syntax (->-helper stx)
  (syntax-case stx ()
    [(-> raw-doms ... last-one)
     (with-syntax ([((doms ...) ((dom-kwd dom-kwd-ctc) ...)) (split-doms stx '-> #'(raw-doms ...))])
       (with-syntax ([(dom-kwd-arg ...) (generate-temporaries (syntax (dom-kwd ...)))]
                     [(dom-kwd-ctc-id ...) (generate-temporaries (syntax (dom-kwd ...)))])
         (with-syntax ([(keyword-call/ctc ...) (apply append (map syntax->list (syntax->list #'((dom-kwd (dom-kwd-ctc-id dom-kwd-arg)) ...))))]
                       [(keyword-formal-parameters ...) (apply append (map syntax->list (syntax->list #'((dom-kwd dom-kwd-arg) ...))))]
                       [(args ...) (generate-temporaries (syntax (doms ...)))]
                       [(dom-ctc ...) (generate-temporaries (syntax (doms ...)))]
                       [(this-parameter ...)
                        (make-this-parameters (car (generate-temporaries '(this))))])
           (syntax-case* #'last-one (-> any values) module-or-top-identifier=?
             [any
              (with-syntax ([(ignored) (generate-temporaries (syntax (rng)))])
                (values (syntax (this-parameter ...))
                        (syntax (dom-ctc ...))
                        (syntax (ignored))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (any/c))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax (this-parameter ... args ... keyword-formal-parameters ...))
                        #t))]
             [(values rngs ...)
              (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                            [(rng-ctc ...) (generate-temporaries (syntax (rngs ...)))])
                (values (syntax (this-parameter ...))
                        (syntax (dom-ctc ...))
                        (syntax (rng-ctc ...))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rngs ...))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax (this-parameter ... args ... keyword-formal-parameters ...))
                        #f))]
             [rng
              (with-syntax ([(rng-ctc) (generate-temporaries (syntax (rng)))])
                (values (syntax (this-parameter ...))
                        (syntax (dom-ctc ...))
                        (syntax (rng-ctc))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rng))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax (this-parameter ... args ... keyword-formal-parameters ...))
                        #f))]))))]))

(define-for-syntax (maybe-a-method/name stx)
  (if (syntax-parameter-value #'making-a-method)
      (syntax-property stx 'method-arity-error #t)
      stx))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->/proc/main stx)
  (let-values ([(this-params dom-names rng-names kwd-names dom-ctcs rng-ctcs kwd-ctcs kwds args use-any?) (->-helper stx)]
               [(this->) (gensym 'this->)])
    (with-syntax ([(this-params ...) this-params]
                  [(dom-names ...) dom-names]
                  [(rng-names ...) rng-names]
                  [(kwd-names ...) kwd-names]
                  [(dom-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:negative-position this->)) 
                                       (syntax->list dom-ctcs))]
                  [(rng-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:positive-position this->))
                                       (syntax->list rng-ctcs))]
                  [(kwd-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:negative-position this->))
                                       (syntax->list kwd-ctcs))]
                  [(kwds ...) kwds]
                  [use-any? use-any?])
      (with-syntax ([mtd? (and (syntax-parameter-value #'making-a-method) #t)]
                    [->m-ctc? (and (syntax-parameter-value #'method-contract?) #t)]
                    [outer-lambda
                     #`(lambda (blame val dom-names ... kwd-names ... rng-names ...)
                         #,(create-chaperone 
                            #'blame #'val #f #f
                            (syntax->list #'(this-params ...))
                            (syntax->list #'(dom-names ...)) null #f
                            (map list (syntax->list #'(kwds ...))
                                 (syntax->list #'(kwd-names ...)))
                            null
                            (if (syntax->datum #'use-any?) #f (syntax->list #'(rng-names ...)))))])
        (syntax-property
         (syntax/loc stx
          (build--> '->
                    #f #f
                    (list dom-ctcs ...) '() #f
                    (list kwd-ctcs ...) '(kwds ...) '() '()
                    (list rng-ctcs ...) use-any?
                    mtd? ->m-ctc?
                    outer-lambda))
         'racket/contract:contract
         (vector this->
                 ;; the -> in the original input to this guy
                 (list (car (syntax-e stx)))
                 '()))))))


;                     
;                     
;                     
;         ;       ;   
;          ;      ;   
;           ;   ;;;;; 
;   ;;;;;;;;;;   ;;   
;           ;    ; ;  
;          ;          
;         ;           
;                     
;                     
;                     
;                     


(define unspecified-dom (gensym 'unspecified-keyword))

;; check-duplicate-kwds : syntax (listof syntax[keyword]) -> void
(define-for-syntax (check-duplicate-kwds stx kwds)
  (let loop ([kwds kwds])
    (unless (null? kwds)
      (when (member (syntax-e (car kwds)) (map syntax-e (cdr kwds)))
        (raise-syntax-error #f "duplicate keyword" stx (car kwds))))))

(define-for-syntax (parse-leftover->* stx leftover)
  (let*-values ([(raw-optional-doms leftover)
                 (syntax-case leftover ()
                   [(kwd . rst)
                    (keyword? (syntax-e #'kwd))
                    (values #'() leftover)]
                   [(rng #:post . rst)
                    (values #'() leftover)]
                   [(rng)
                    (values #'() leftover)]
                   [((raw-optional-dom ...) . leftover)
                    (values #'(raw-optional-dom ...) #'leftover)]
                   [_ 
                    (values #'() leftover)])]
                [(rst leftover)
                 (syntax-case leftover ()
                   [(#:rest rest-expr . leftover)
                    (values #'rest-expr #'leftover)]
                   [_ (values #f leftover)])]
                [(pre leftover)
                 (syntax-case leftover ()
                   [(#:pre pre-expr . leftover)
                    (values #'pre-expr #'leftover)]
                   [_ (values #f leftover)])]
                [(rng leftover)
                 (syntax-case leftover (any values)
                   [(any) (values #f #'())]
                   [(any . more) (raise-syntax-error #f "expected nothing to follow any" stx #'any)]
                   [((values ctc ...) . leftover)
                    (values #'(ctc ...) #'leftover)]
                   [(rng . leftover)
                    (begin
                      (when (keyword? (syntax-e #'rng))
                        (raise-syntax-error #f "expected a range contract" stx #'rng))
                      (values #'(rng) #'leftover))]
                   [_
                    (raise-syntax-error #f "expected a range contract" stx leftover)])]
                [(post leftover)
                 (syntax-case leftover ()
                   [(#:post post-expr . leftover)
                    (values #'post-expr #'leftover)]
                   [else
                    (values #f leftover)])])
    (syntax-case leftover ()
      [() (values raw-optional-doms rst pre rng post)]
      [x (raise-syntax-error #f "expected the end of the contract" stx #'x)])))

;; ->*/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->*/proc/main stx)
  (syntax-case* stx (->* any) module-or-top-identifier=?
    [(->* (raw-mandatory-dom ...) . rst)
     (let-values ([(raw-optional-doms rest-ctc pre rng-ctc post)
                   ;; rest-ctc (or/c #f syntax) -- #f means no rest contract, syntax is the contract
                   ;; rng-ctc (or/c #f syntax) -- #f means `any', syntax is a sequence of result values
                   (parse-leftover->* stx #'rst)])
       (with-syntax ([((mandatory-dom ...) ((mandatory-dom-kwd mandatory-dom-kwd-ctc) ...))
                      (split-doms stx '->* #'(raw-mandatory-dom ...))]
                     [((optional-dom ...) ((optional-dom-kwd optional-dom-kwd-ctc) ...))
                      (split-doms stx '->* raw-optional-doms)])
         ;(check-duplicate-kwds stx (syntax->list #'(mandatory-dom-kwd ... optional-dom-kwd ...)))
         (with-syntax ([(mandatory-dom-proj ...) (generate-temporaries #'(mandatory-dom ...))]
                       [(mandatory-dom-arg ...) (generate-temporaries #'(mandatory-dom ...))]
                       [(mandatory-dom-kwd-proj ...) (generate-temporaries #'(mandatory-dom-kwd ...))]
                       [(mandatory-dom-kwd-arg ...) (generate-temporaries #'(mandatory-dom-kwd ...))]
                       
                       [(optional-dom-proj ...) (generate-temporaries #'(optional-dom ...))]
                       [(optional-dom-arg ...) (generate-temporaries #'(optional-dom ...))]
                       [(optional-dom-kwd-proj ...) (generate-temporaries #'(optional-dom-kwd ...))]
                       [(optional-dom-kwd-arg ...) (generate-temporaries #'(optional-dom-kwd ...))])
           (with-syntax ([(mandatory-dom-kwd/var-seq ...) (apply append 
                                                                 (map list
                                                                      (syntax->list #'(mandatory-dom-kwd ...)) 
                                                                      (syntax->list #'(mandatory-dom-kwd-arg ...))))]
                         [(optional-dom-kwd/var-seq ...) (apply append 
                                                                (map list 
                                                                     (syntax->list #'(optional-dom-kwd ...)) 
                                                                     (syntax->list #'([optional-dom-kwd-arg unspecified-dom] ...))))]
                         [(mandatory-dom-kwd-proj-apps ...) (apply append 
                                                                   (map list
                                                                        (syntax->list #'(mandatory-dom-kwd ...)) 
                                                                        (syntax->list #'((mandatory-dom-kwd-proj mandatory-dom-kwd-arg) ...))))]
                         [((sorted-dom-kwd sorted-dom-kwd-arg sorted-dom-kwd-proj) ...) 
                          (sort-keywords stx (syntax->list
                                              #'((mandatory-dom-kwd mandatory-dom-kwd-arg mandatory-dom-kwd-proj) ...
                                                 (optional-dom-kwd optional-dom-kwd-arg optional-dom-kwd-proj) ...)))])
             (with-syntax ([((rev-sorted-dom-kwd rev-sorted-dom-kwd-arg rev-sorted-dom-kwd-proj) ...)
                            (reverse (syntax->list #'((sorted-dom-kwd sorted-dom-kwd-arg sorted-dom-kwd-proj) ...)))]
                           [(rev-optional-dom-arg ...) (reverse (syntax->list #'(optional-dom-arg ...)))]
                           [(rev-optional-dom-proj ...) (reverse (syntax->list #'(optional-dom-proj ...)))])
               
               
               (with-syntax ([mtd? (and (syntax-parameter-value #'making-a-method) #t)]
                             [->m-ctc? (and (syntax-parameter-value #'method-contract?) #t)]
                             [(rng-proj ...) (generate-temporaries (or rng-ctc '()))]
                             [(rng ...) (generate-temporaries (or rng-ctc '()))]
                             [(this-parameter ...)
                              (make-this-parameters (car (generate-temporaries '(this))))])
                 (quasisyntax/loc stx
                   (build-->
                    '->*
                    #,(if pre #`(λ () #,pre) #'#f)
                    #,(if post #`(λ () #,post) #'#f)
                    (list mandatory-dom ...)
                    (list optional-dom ...)
                    #,rest-ctc
                    (list mandatory-dom-kwd-ctc ...)
                    '(mandatory-dom-kwd ...)
                    (list optional-dom-kwd-ctc ...)
                    '(optional-dom-kwd ...)
                    #,(if rng-ctc
                          (with-syntax ([(rng-ctc ...) rng-ctc])
                            #'(list rng-ctc ...))
                          #''())
                    #,(if rng-ctc #f #t)
                    mtd? ->m-ctc?
                    (λ (blame f
                        mandatory-dom-proj ...  
                        #,@(if rest-ctc
                               #'(rest-proj)
                               #'())
                        optional-dom-proj ... 
                        mandatory-dom-kwd-proj ... 
                        optional-dom-kwd-proj ... 
                        rng-proj ...)
                      #,(create-chaperone 
                         #'blame #'f pre post
                         (syntax->list #'(this-parameter ...))
                         (syntax->list #'(mandatory-dom-proj ...))
                         (syntax->list #'(optional-dom-proj ...))
                         (if rest-ctc #'rest-proj #f)
                         (map list (syntax->list #'(mandatory-dom-kwd ...))
                              (syntax->list #'(mandatory-dom-kwd-proj ...)))
                         (map list (syntax->list #'(optional-dom-kwd ...))
                              (syntax->list #'(optional-dom-kwd-proj ...)))
                         (if rng-ctc (syntax->list #'(rng-proj ...)) #f)))))))))))]))

(define-syntax (->* stx) #`(syntax-parameterize ((making-a-method #f)) #,(->*/proc/main stx)))



;                     
;                     
;                     
;         ;       ;;; 
;          ;       ;; 
;           ;      ;; 
;   ;;;;;;;;;;  ;;;;; 
;           ;  ;;  ;; 
;          ;   ;;  ;; 
;         ;    ;;  ;; 
;               ;;;;;;
;                     
;                     
;                     

;; parses everything after the mandatory and optional doms in a ->d contract
(define-for-syntax (parse-leftover->d stx leftover)
  (let*-values ([(raw-optional-doms leftover)
                 (syntax-case leftover ()
                   [(kwd . leftover2)
                    (keyword? (syntax-e #'kwd))
                    (values '() leftover)]
                   [(dep-range)
                    (values '() leftover)]
                   [(dep-range #:post-cond expr)
                    (values '() leftover)]
                   [(dep-range #:post expr)
                    (values '() leftover)]
                   [((opts ...) . rest)
                    (values #'(opts ...) #'rest)]
                   [_ (values '() leftover)])]
                [(id/rest-id leftover) 
                 (syntax-case leftover ()
                   [(#:rest id rest-expr . leftover)
                    (and (identifier? #'id)
                         (not (keyword? (syntax-e #'rest-expr))))
                    (values #'(id rest-expr) #'leftover)]
                   [(#:rest id rest-expr . leftover)
                    (begin
                      (unless (identifier? #'id)
                        (raise-syntax-error #f "expected an identifier" stx #'id))
                      (when (keyword? (syntax-e #'rest-expr))
                        (raise-syntax-error #f "expected an expression, not a keyword" stx #'rest-expr)))]
                   [_ (values #f leftover)])]
                [(pre-cond leftover)
                 (syntax-case leftover ()
                   [(#:pre pre-cond . leftover)
                    (values #'pre-cond #'leftover)]
                   [(#:pre-cond pre-cond . leftover)
                    (values #'pre-cond #'leftover)]
                   [_ (values #f leftover)])]
                [(range leftover) 
                 (syntax-case leftover ()
                   [(range . leftover) (values #'range #'leftover)]
                   [_
                    (raise-syntax-error #f "expected a range expression, but found nothing" stx)])]
                [(post-cond leftover) 
                 (syntax-case leftover ()
                   [(#:post post-cond . leftover)
                    (begin
                      (syntax-case range (any)
                        [any (raise-syntax-error #f "cannot have a #:post with any as the range" stx #'post-cond)]
                        [_ (void)])
                      (values #'post-cond #'leftover))]
                   [(#:post-cond post-cond . leftover)
                    (begin
                      (syntax-case range (any)
                        [any (raise-syntax-error #f "cannot have a #:post-cond with any as the range" stx #'post-cond)]
                        [_ (void)])
                      (values #'post-cond #'leftover))]
                   [_ (values #f leftover)])])
    (syntax-case leftover ()
      [() 
       (values raw-optional-doms id/rest-id pre-cond range post-cond)]
      [_ 
       (raise-syntax-error #f "bad syntax" stx)])))

;; verify-->d-structure : syntax syntax -> syntax
;; returns the second argument when it has the proper shape for the first two arguments to ->d*
;; otherwise, raises a syntax error.
(define-for-syntax (verify-->d-structure stx doms)
  (syntax-case doms ()
    [((regular ...) (kwd ...))
     (let ([check-pair-shape
            (λ (reg)
              (syntax-case reg ()
                [(id dom)
                 (identifier? #'id)
                 (void)]
                [(a b)
                 (raise-syntax-error #f "expected an identifier" stx #'a)]
                [_
                 (raise-syntax-error #f "expected an identifier and a contract-expr" stx reg)]))])
       (for-each check-pair-shape (syntax->list #'(regular ...)))
       (for-each
        (λ (kwd)
          (syntax-case kwd ()
            [(kwd ps)
             (check-pair-shape #'ps)]))
        (syntax->list #'(kwd ...))))])
  doms)

(define-for-syntax (make-this-transformer this-arg)
  (with-syntax ([this-arg this-arg])
    (make-set!-transformer
     (λ (sstx)
       (syntax-case sstx (set!)
         [(set! id arg)
          (raise-syntax-error #f 
                              "can't mutate this"
                              sstx)]
         [id
          (identifier? #'id)
          (syntax/loc sstx this-arg)]
         [(id . args)
          (datum->syntax sstx (cons #'this-arg #'args) sstx)])))))

(define-syntax (->d stx)
  (syntax-case stx ()
    [(_ (raw-mandatory-doms ...)
        .
        leftover)
     (let-values ([(raw-optional-doms id/rest pre-cond range post-cond) (parse-leftover->d stx #'leftover)])
       (with-syntax ([(([mandatory-regular-id mandatory-doms] ... ) ([mandatory-kwd (mandatory-kwd-id mandatory-kwd-dom)] ...)) 
                      (verify-->d-structure stx (split-doms stx '->d #'(raw-mandatory-doms ...)))]
                     [(([optional-regular-id optional-doms] ... ) ([optional-kwd (optional-kwd-id optional-kwd-dom)] ...))
                      (verify-->d-structure stx (split-doms stx '->d raw-optional-doms))])
         (with-syntax ([((kwd kwd-id) ...)
                        (sort-keywords 
                         stx
                         (syntax->list
                          #'((optional-kwd optional-kwd-id) ...
                             (mandatory-kwd mandatory-kwd-id) ...)))]
                       [(this-parameter ...)
                        (make-this-parameters (if (syntax? (syntax-parameter-value #'making-a-method))
                                                  (car (generate-temporaries '(this)))
                                                  (datum->syntax stx 'this #f)))])
           (with-syntax ([(dom-params ...)
                          #`(this-parameter ...
                             mandatory-regular-id ... 
                             optional-regular-id ... 
                             #,@(if id/rest 
                                    (with-syntax ([(id rst-ctc) id/rest])
                                      #'(id))
                                    #'())
                             kwd-id ...)])
             (with-syntax ([((rng-params ...) rng-ctcs)
                            (syntax-case range (any values)
                              [(values [id ctc] ...) #'((id ...) (ctc ...))]
                              [(values [id ctc] ... x . y) (raise-syntax-error #f "expected binding pair" stx #'x)]
                              [any #'(() #f)]
                              [[id ctc] #'((id) (ctc))]
                              [x (raise-syntax-error #f "expected binding pair or any" stx #'x)])]
                           [mtd? (and (syntax-parameter-value #'making-a-method) #t)]
                           [->m-ctc? (and (syntax-parameter-value #'method-contract?) #t)])
               (let ([rng-underscores? 
                      (let ([is-underscore?
                             (λ (x) 
                               (syntax-case x (_)
                                 [_ #t]
                                 [else #f]))])
                        (cond
                          [(andmap is-underscore? (syntax->list #'(rng-params ...)))
                           #t]
                          [(ormap (λ (x) (and (is-underscore? x) x))
                                  (syntax->list #'(rng-params ...)))
                           =>
                           (λ (id)
                             (raise-syntax-error '->d 
                                                 "expected all of the identifiers to be underscores, or none of them to be"
                                                 stx
                                                 id))]
                          [else #f]))])
                 (let ([dup (check-duplicate-identifier 
                             (append (if rng-underscores? 
                                         '()
                                         (syntax->list #'(rng-params ...)))
                                     (syntax->list #'(dom-params ...))))])
                   (when dup
                     (raise-syntax-error #f "duplicate identifier" stx dup)))
                 #`(let-syntax ([parameterize-this
                                 (let ([old-param (syntax-parameter-value #'making-a-method)])
                                   (λ (stx)
                                     (syntax-case stx ()
                                       [(_ body) #'body]
                                       [(_ id body)
                                        (if (syntax? old-param)
                                            (with-syntax ([param old-param])
                                              (syntax/loc stx
                                                (syntax-parameterize
                                                 ([param (make-this-transformer #'id)])
                                                 body)))
                                            #'body)])))])
                     (syntax-parameterize 
                      ((making-a-method #f)) 
                      (build-->d mtd? ->m-ctc?
                                 (list (λ (dom-params ...)
                                         (parameterize-this this-parameter ... mandatory-doms)) ...)
                                 (list (λ (dom-params ...) 
                                         (parameterize-this this-parameter ... optional-doms)) ...)
                                 (list (λ (dom-params ...) 
                                         (parameterize-this this-parameter ... mandatory-kwd-dom)) ...)
                                 (list (λ (dom-params ...) 
                                         (parameterize-this this-parameter ... optional-kwd-dom)) ...)
                                 #,(if id/rest 
                                       (with-syntax ([(id rst-ctc) id/rest])
                                         #`(λ (dom-params ...)
                                             (parameterize-this this-parameter ... rst-ctc)))
                                       #f)
                                 #,(if pre-cond
                                       #`(λ (dom-params ...)
                                           (parameterize-this this-parameter ... #,pre-cond))
                                       #f)
                                 #,(syntax-case #'rng-ctcs ()
                                     [#f #f]
                                     [(ctc ...) 
                                      (if rng-underscores?
                                          #'(box (list (λ (dom-params ...) 
                                                         (parameterize-this this-parameter ... ctc)) ...))
                                          #'(list (λ (rng-params ... dom-params ...)
                                                    (parameterize-this this-parameter ... ctc)) ...))])
                                 #,(if post-cond
                                       #`(λ (rng-params ... dom-params ...)
                                           (parameterize-this this-parameter ... #,post-cond))
                                       #f)
                                 '(mandatory-kwd ...)
                                 '(optional-kwd ...)
                                 (λ (f) 
                                   #,(add-name-prop
                                      (syntax-local-infer-name stx)
                                      #`(λ args (apply f args)))))))))))))]))

(define ((->d-proj wrap-procedure) ->d-stct)
  (let* ([opt-count (length (base-->d-optional-dom-ctcs ->d-stct))]
         [mandatory-count (+ (length (base-->d-mandatory-dom-ctcs ->d-stct)) 
                             (if (base-->d-mtd? ->d-stct) 1 0))]
         [non-kwd-ctc-count (+ mandatory-count opt-count)]
         [arity 
          (cond
            [(base-->d-rest-ctc ->d-stct)
             (make-arity-at-least mandatory-count)]
            [else
             (let loop ([i 0])
               (cond
                 [(= i opt-count)
                  (list (+ mandatory-count i))]
                 [else
                  (cons (+ mandatory-count i) (loop (+ i 1)))]))])])
    (λ (blame)
        (λ (val)
          (if (base-->d-rest-ctc ->d-stct)
              (check-procedure/more val
                                    (base-->d-mtd? ->d-stct)
                                    (length (base-->d-mandatory-dom-ctcs ->d-stct)) ;dom-length
                                    (base-->d-mandatory-keywords ->d-stct)
                                    (base-->d-optional-keywords ->d-stct)
                                    blame)
              (check-procedure val
                               (base-->d-mtd? ->d-stct)
                               (length (base-->d-mandatory-dom-ctcs ->d-stct)) ;dom-length
                               (length (base-->d-optional-dom-ctcs ->d-stct)) ; optionals-length
                               (base-->d-mandatory-keywords ->d-stct)
                               (base-->d-optional-keywords ->d-stct)
                               blame))
          (wrap-procedure
           val
           (make-keyword-procedure
            (λ (kwd-args kwd-arg-vals . raw-orig-args)
              (let* ([orig-args (if (base-->d-mtd? ->d-stct)
                                    (cdr raw-orig-args)
                                    raw-orig-args)]
                     [this (and (base-->d-mtd? ->d-stct) (car raw-orig-args))]
                     [dep-pre-args
                      (build-dep-ctc-args non-kwd-ctc-count raw-orig-args (base-->d-rest-ctc ->d-stct)
                                          (base-->d-keywords ->d-stct) kwd-args kwd-arg-vals)])
                (when (base-->d-pre-cond ->d-stct)
                  (unless (apply (base-->d-pre-cond ->d-stct) dep-pre-args)
                    (raise-blame-error (blame-swap blame)
                                       val
                                       "#:pre violation~a"
                                       (build-values-string ", argument" dep-pre-args))))
                (apply 
                 values
                 
                 (append
                  
                  (let ([rng (let ([rng (base-->d-range ->d-stct)])
                               (cond
                                 [(not rng) #f]
                                 [(box? rng) 
                                  (map (λ (val) (apply val dep-pre-args))
                                       (unbox rng))]
                                 [else rng]))]
                        [rng-underscore? (box? (base-->d-range ->d-stct))])
                    (if rng
                        (list (λ orig-results
                                (let* ([range-count (length rng)]
                                       [post-args (append orig-results raw-orig-args)]
                                       [post-non-kwd-arg-count (+ non-kwd-ctc-count range-count)]
                                       [dep-post-args (build-dep-ctc-args post-non-kwd-arg-count
                                                                          post-args (base-->d-rest-ctc ->d-stct)
                                                                          (base-->d-keywords ->d-stct) kwd-args kwd-arg-vals)])
                                  (when (base-->d-post-cond ->d-stct)
                                    (unless (apply (base-->d-post-cond ->d-stct) dep-post-args)
                                      (raise-blame-error blame
                                                         val
                                                         "#:post violation~a~a"
                                                         (build-values-string ", argument" dep-pre-args)
                                                         (build-values-string (if (null? dep-pre-args)
                                                                                  ", result"
                                                                                  "\n result")
                                                                              orig-results))))
                                  
                                  (unless (= range-count (length orig-results))
                                    (raise-blame-error blame
                                                       val
                                                       "expected ~a results, got ~a"
                                                       range-count
                                                       (length orig-results)))
                                  (apply
                                   values
                                   (let loop ([results orig-results]
                                              [result-contracts rng])
                                     (cond
                                       [(null? result-contracts) '()]
                                       [else
                                        (cons
                                         (invoke-dep-ctc (car result-contracts)
                                                         (if rng-underscore? #f dep-post-args)
                                                         (car results)
                                                         blame
                                                         #f)
                                         (loop (cdr results) (cdr result-contracts)))]))))))
                        null))
                  
                  ;; contracted keyword arguments
                  (let ([kwd-res (let loop ([all-kwds (base-->d-keywords ->d-stct)]
                                            [kwd-ctcs (base-->d-keyword-ctcs ->d-stct)] 
                                            [building-kwd-args kwd-args]
                                            [building-kwd-arg-vals kwd-arg-vals])
                                   (cond
                                     [(or (null? building-kwd-args) (null? all-kwds)) '()]
                                     [else (if (eq? (car all-kwds)
                                                    (car building-kwd-args))
                                               (cons (invoke-dep-ctc (car kwd-ctcs) dep-pre-args (car building-kwd-arg-vals) blame #t)
                                                     (loop (cdr all-kwds) (cdr kwd-ctcs) (cdr building-kwd-args) (cdr building-kwd-arg-vals)))
                                               (loop (cdr all-kwds) (cdr kwd-ctcs) building-kwd-args building-kwd-arg-vals))]))])
                    (if (null? kwd-res) null (list kwd-res)))
                  
                  
                  ;; this parameter (if necc.)
                  (if (base-->d-mtd? ->d-stct)
                      (list (car raw-orig-args))
                      '())
                  
                  ;; contracted ordinary arguments
                  (let loop ([args orig-args]
                             [non-kwd-ctcs (append (base-->d-mandatory-dom-ctcs ->d-stct)
                                                   (base-->d-optional-dom-ctcs ->d-stct))])
                    (cond
                      [(null? args) 
                       (if (base-->d-rest-ctc ->d-stct)
                           (invoke-dep-ctc (base-->d-rest-ctc ->d-stct) dep-pre-args '() blame #t)
                           '())]
                      [(null? non-kwd-ctcs) 
                       (if (base-->d-rest-ctc ->d-stct)
                           (invoke-dep-ctc (base-->d-rest-ctc ->d-stct) dep-pre-args args blame #t)
                           
                           ;; ran out of arguments, but don't have a rest parameter.
                           ;; procedure-reduce-arity (or whatever the new thing is
                           ;; going to be called) should ensure this doesn't happen.
                           (error 'shouldnt\ happen))]
                      [else (cons (invoke-dep-ctc (car non-kwd-ctcs) dep-pre-args (car args) blame #t)
                                  (loop (cdr args)
                                        (cdr non-kwd-ctcs)))])))))))
           impersonator-prop:contracted ->d-stct)))))

(define (build-values-string desc dep-pre-args)
  (cond
    [(null? dep-pre-args) ""]
    [(null? (cdr dep-pre-args)) (format "~a was: ~e" desc (car dep-pre-args))]
    [else
     (apply
      string-append
      (format "~as were:" desc)
      (let loop ([lst dep-pre-args])
        (cond
          [(null? lst) '()]
          [else (cons (format "\n  ~e" (car lst))
                      (loop (cdr lst)))])))]))

;; invoke-dep-ctc : (...? -> ctc) (or/c #f (listof tst)) val pos-blame neg-blame src-info orig-src -> tst
(define (invoke-dep-ctc dep-ctc dep-args val blame dom?)
  (let ([ctc (coerce-contract '->d (if dep-args
                                       (apply dep-ctc dep-args)
                                       dep-ctc))])
    (((contract-projection ctc) 
      (blame-add-context
       blame
       (if dom? "the domain of" "the range of")
       #:swap? dom?))
      val)))

;; build-dep-ctc-args : number (listof any) boolean (listof keyword) (listof keyword) (listof any)
(define (build-dep-ctc-args non-kwd-ctc-count args rest-arg? all-kwds supplied-kwds supplied-args)
  (append 
   
   ;; ordinary args (possibly including `this' as the first element)
   (let loop ([count non-kwd-ctc-count]
              [args args])
     (cond
       [(zero? count) 
        (if rest-arg?
            (list args)
            '())]
       [(null? args) (cons the-unsupplied-arg (loop (- count 1) null))]
       [else (cons (car args) (loop (- count 1) (cdr args)))]))
   
   ;; kwd args
   (let loop ([all-kwds all-kwds]
              [kwds supplied-kwds]
              [args supplied-args])
     (cond
       [(null? all-kwds) null]
       [else (let* ([kwd (car all-kwds)]
                    [kwd-matches? (and (not (null? kwds)) (eq? (car kwds) kwd))])
               (if kwd-matches?
                   (cons (car args) (loop (cdr all-kwds) (cdr kwds) (cdr args)))
                   (cons the-unsupplied-arg (loop (cdr all-kwds) kwds args))))]))))

(define-struct unsupplied-arg ())
(define the-unsupplied-arg (make-unsupplied-arg))

(define (build-->d mtd? mctc?
                   mandatory-dom-ctcs optional-dom-ctcs
                   mandatory-kwd-dom-ctcs optional-kwd-dom-ctcs
                   rest-ctc pre-cond range post-cond
                   mandatory-kwds optional-kwds
                   name-wrapper)
  (let ([kwd/ctc-pairs (sort
                        (map cons 
                             (append mandatory-kwds optional-kwds)
                             (append mandatory-kwd-dom-ctcs optional-kwd-dom-ctcs))
                        (λ (x y) (keyword<? (car x) (car y))))])
    (make-impersonator-->d mtd? mctc?
                           mandatory-dom-ctcs optional-dom-ctcs
                           (map cdr kwd/ctc-pairs)
                           rest-ctc pre-cond range post-cond
                           (map car kwd/ctc-pairs)
                           mandatory-kwds
                           optional-kwds
                           name-wrapper)))

(define (->d-name ctc) 
  (let* ([name (if (base-->d-mctc? ctc) '->dm '->d)]
         [counting-id 'x]
         [ids '(x y z w)]
         [next-id
          (λ ()
            (cond
              [(pair? ids)
               (begin0 (car ids)
                       (set! ids (cdr ids)))]
              [(null? ids)
               (begin0 
                 (string->symbol (format "~a0" counting-id))
                 (set! ids 1))]
              [else
               (begin0 
                 (string->symbol (format "~a~a" counting-id ids))
                 (set! ids (+ ids 1)))]))])
    `(,name (,@(map (λ (x) `(,(next-id) ...)) (base-->d-mandatory-dom-ctcs ctc))
           ,@(apply append (map (λ (kwd) (list kwd `(,(next-id) ...))) (base-->d-mandatory-keywords ctc))))
          (,@(map (λ (x) `(,(next-id) ...)) (base-->d-optional-dom-ctcs ctc))
           ,@(apply append (map (λ (kwd) (list kwd `(,(next-id) ...))) (base-->d-optional-keywords ctc))))
          ,@(if (base-->d-rest-ctc ctc)
                (list '#:rest (next-id) '...)
                '())
          ,@(if (base-->d-pre-cond ctc)
                (list '#:pre '...)
                (list))
          ,(let ([range (base-->d-range ctc)])
             (cond
               [(not range) 'any]
               [(box? range)
                (let ([range (unbox range)])
                  (cond
                    [(and (not (null? range))
                          (null? (cdr range)))
                     `[_ ...]]
                    [else
                     `(values ,@(map (λ (x) `(_ ...)) range))]))]
               [(and (not (null? range))
                     (null? (cdr range)))
                `[,(next-id) ...]]
               [else
                `(values ,@(map (λ (x) `(,(next-id) ...)) range))]))
          ,@(if (base-->d-post-cond ctc)
                (list '#:post '...)
                (list)))))

(define (->d-first-order ctc)
  (let* ([mtd? (base-->d-mtd? ctc)]
         [dom-length (length (base-->d-mandatory-dom-ctcs ctc))]
         [optionals (length (base-->d-optional-dom-ctcs ctc))]
         [mandatory-kwds (base-->d-mandatory-keywords ctc)]
         [optional-kwds (base-->d-optional-keywords ctc)])
    (λ (val)
      (if (base-->d-rest-ctc ctc)
          (check-procedure/more val mtd? dom-length mandatory-kwds optional-kwds #f)
          (check-procedure val mtd? dom-length optionals mandatory-kwds optional-kwds #f)))))
(define (->d-stronger? this that) (eq? this that))

;; in the struct type descriptions "d???" refers to the arguments (domain) of the function that
;; is under the contract, and "dr???" refers to the arguments & the results of the function that 
;; is under the contract.
;; the `box' in the range only serves to differentiate between range contracts that depend on
;; both the domain and the range from those that depend only on the domain (and thus, those
;; that can be applied early)
(define-struct base-->d (mtd?                ;; boolean; indicates if this is a contract on a method, for error reporing purposes.
                         mctc?               ;; boolean; indicates if this contract was constructed with ->dm (from racket/class)
                         mandatory-dom-ctcs  ;; (listof (-> d??? ctc))
                         optional-dom-ctcs   ;; (listof (-> d??? ctc))
                         keyword-ctcs        ;; (listof (-> d??? ctc))
                         rest-ctc            ;; (or/c false/c (-> d??? ctc))
                         pre-cond            ;; (-> d??? boolean)
                         range               ;; (or/c false/c (listof (-> dr??? ctc)) (box (listof (-> r??? ctc))))
                         post-cond           ;; (-> dr??? boolean)
                         keywords            ;; (listof keywords) -- sorted by keyword<
                         mandatory-keywords  ;; (listof keywords) -- sorted by keyword<
                         optional-keywords   ;; (listof keywords) -- sorted by keyword<
                         name-wrapper))      ;; (-> proc proc) 

;; Factored out in case there are ever chaperone ->d contracts.
;; However, to do that, you'd either a) have to somehow check
;; that the subcontracts are chaperones or b) allow contract
;; application-time failures if the subpieces did not convert
;; appropriately.  b) might be okay, but we should think about
;; it first.  At the very least, the projection function would
;; need to add checks in the appropriate places.
(define-struct (impersonator-->d base-->d) ()
  #:property prop:contract
  (build-contract-property
   #:projection (->d-proj impersonate-procedure)
   #:name ->d-name
   #:first-order ->d-first-order
   #:stronger ->d-stronger?))

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

(define-for-syntax (parse-out-case stx case)
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
                (let ([rng-checkers (list #'(λ (rng-id ...) (values (rng-proj-x rng-id) ...)))]
                      [rng-length (length (syntax->list rng))])
                  (if rst
                      (check-tail-contract #'(rng-proj-x ...) rng-checkers
                                           (λ (rng-checks)
                                             #`(apply values #,@rng-checks this-parameter ...
                                                      (dom-proj-x dom-formals) ...
                                                      (rst-proj-x rst-formal))))
                      (check-tail-contract #'(rng-proj-x ...) rng-checkers
                                           (λ (rng-checks)
                                             #`(values #,@rng-checks this-parameter ...
                                                       (dom-proj-x dom-formals) ...)))))]
               [rst
                #`(apply values this-parameter ... (dom-proj-x dom-formals) ... (rst-proj-x rst-formal))]
               [else
                #`(values this-parameter ... (dom-proj-x dom-formals) ...)]))))))

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

(define-syntax (case-> stx)
  (syntax-case stx ()
    [(_ cases ...)
     (begin 
       (with-syntax ([(((dom-proj ...)
                        rst-proj
                        rng-proj
                        spec
                        (dom-proj-x ...)
                        (rng-proj-x ...)
                        formals
                        body) ...)
                      (map (λ (x) (parse-out-case stx x)) (syntax->list #'(cases ...)))]
                     [mctc? (and (syntax-parameter-value #'method-contract?) #t)])
         #`(syntax-parameterize 
            ((making-a-method #f)) 
            (build-case-> (list (list dom-proj ...) ...)
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
                              (chk f #,(and (syntax-parameter-value #'making-a-method) #t))
                              (let ([checker
                                     (make-keyword-procedure
                                      (λ (kwds kwd-args . args)
                                        (raise-blame-error blame f "expected no keywords, got keyword ~a" (car kwds)))
                                      (λ args
                                        (apply (case-lambda [formals body] ...) args)))]
                                    [same-rngs (same-range-projections (list (list rng-proj-x ...) ...))])
                                (if same-rngs
                                    (wrapper
                                     f
                                     checker
                                     impersonator-prop:contracted ctc
                                     impersonator-prop:application-mark (cons contract-key same-rngs))
                                    (wrapper
                                     f
                                     checker
                                     impersonator-prop:contracted ctc)))))))))]))

;; dom-ctcs : (listof (listof contract))
;; rst-ctcs : (listof contract)
;; rng-ctcs : (listof (listof contract))
;; specs : (listof (list boolean exact-positive-integer)) ;; indicates the required arities of the input functions
;; mctc? : was created with case->m
;; wrapper : (->* () () (listof contract?) (-> procedure? procedure?)) -- generates a wrapper from projections
(define-struct base-case-> (dom-ctcs rst-ctcs rng-ctcs specs mctc? wrapper))

(define (case->-proj wrapper)
  (λ (ctc)
    (define dom-ctcs (map contract-projection (get-case->-dom-ctcs ctc)))
    (define rng-ctcs (let ([rngs (get-case->-rng-ctcs ctc)])
                       (and rngs (map contract-projection (get-case->-rng-ctcs ctc)))))
    (define rst-ctcs (base-case->-rst-ctcs ctc))
    (define specs (base-case->-specs ctc))
    (λ (blame)
      (define dom-blame (blame-add-context blame "the domain of" #:swap? #t))
      (define rng-blame (blame-add-context blame "the range of"))
      (define projs (append (map (λ (f) (f dom-blame)) dom-ctcs)
                            (map (λ (f) (f rng-blame)) rng-ctcs)))
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
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (case->-proj chaperone-procedure)
   #:name case->-name
   #:first-order case->-first-order
   #:stronger case->-stronger?))

(define-struct (impersonator-case-> base-case->) ()
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

(define (get-case->-dom-ctcs ctc)
  (apply append
         (map (λ (doms rst) (if rst
                                (append doms (list rst))
                                doms))
              (base-case->-dom-ctcs ctc)
              (base-case->-rst-ctcs ctc))))

(define (get-case->-rng-ctcs ctc)
  (apply append (map (λ (x) (or x '())) (base-case->-rng-ctcs ctc))))



;                       
;                       
;                       
;                       
;    ;          ;;; ;;; 
;  ;;;              ;;; 
;  ;;;;  ;;;;;  ;;; ;;; 
;  ;;;; ;;;;;;; ;;; ;;; 
;  ;;;  ;;  ;;; ;;; ;;; 
;  ;;;    ;;;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; 
;  ;;;; ;;; ;;; ;;; ;;; 
;   ;;;  ;;;;;; ;;; ;;; 
;                       
;                       
;                       
;                       

(define-syntax (apply-projections stx)
  (syntax-case stx ()
    [(_ ((x f) ...) e) 
     (with-syntax ([count (length (syntax->list #'(x ...)))])
       #'(let ([fs (list f ...)]
               [thunk (λ () e)])
           (call-with-immediate-continuation-mark
            multiple-contract-key
            (λ (first-mark)
              (if (and first-mark
                       (= (length first-mark) count)
                       (andmap procedure-closure-contents-eq? fs first-mark))
                  (thunk)
                  (let-values ([(x ...) (with-continuation-mark multiple-contract-key fs
                                          (thunk))])
                    (values (f x) ...)))))))]))


(define multiple-contract-key (gensym 'multiple-contract-key))

(define-syntax (apply-projection stx)
  (syntax-case stx ()
    [(_ ctc arg)
     #'(apply-projection/proc ctc (λ () arg))]))

(define single-contract-key (gensym 'single-contract-key))

(define (apply-projection/proc ctc thnk)
  (call-with-immediate-continuation-mark
   single-contract-key
   (λ (first-mark)  ;; note this is #f if there is no mark (so if #f can be a contract, something must change)
     (if (and first-mark (procedure-closure-contents-eq? first-mark ctc))
         (thnk)
         (ctc
          (with-continuation-mark single-contract-key ctc
            (thnk)))))))



;                                                                                 
;                                                                                 
;                                                                                 
;                                       ;;;;                      ;;;;            
;                                       ;;;;                      ;;;;            
;  ;;;;;;;  ;;; ;;; ;;; ;;;       ;;;;; ;;;; ;;;    ;;;     ;;;;; ;;;; ;;;  ;;;;; 
;  ;;;;;;;; ;;;;;;; ;;;;;;;      ;;;;;; ;;;;;;;;;  ;;;;;   ;;;;;; ;;;; ;;; ;;;;;; 
;      ;;;; ;;;; ;; ;;;; ;;     ;;;;;;; ;;;; ;;;; ;;;; ;; ;;;;;;; ;;;;;;;  ;;;;   
;   ;;;;;;; ;;;;    ;;;;        ;;;;    ;;;; ;;;; ;;;;;;; ;;;;    ;;;;;;;   ;;;;  
;  ;;  ;;;; ;;;;    ;;;;        ;;;;;;; ;;;; ;;;; ;;;;;   ;;;;;;; ;;;; ;;;   ;;;; 
;  ;;;;;;;; ;;;;    ;;;;         ;;;;;; ;;;; ;;;;  ;;;;;;  ;;;;;; ;;;; ;;; ;;;;;; 
;   ;; ;;;; ;;;;    ;;;;          ;;;;; ;;;; ;;;;   ;;;;    ;;;;; ;;;; ;;; ;;;;;  
;                                                                                 
;                                                                                 
;                                                                                 

;; ----------------------------------------
;; Checks and error functions used in macro expansions

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
                (let loop ([counts (sort (filter number? arity) >=)])
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

(define (get-mandatory-keywords f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    mandatory))

(define (no-mandatory-keywords? f)
  (let-values ([(mandatory optional) (procedure-keywords f)])
    (null? mandatory)))

;; check-procedure : ... (or/c #f blame) -> (or/c boolean? void?)
;; if blame is #f, then just return a boolean indicating that this matched 
;;   (for use in arity checking)
(define (check-procedure val mtd? dom-length optionals mandatory-kwds optional-keywords blame)
  (define passes?
    (and (procedure? val)
         (procedure-arity-includes?/optionals val (if mtd? (+ dom-length 1) dom-length) optionals)
         (keywords-match mandatory-kwds optional-keywords val)))
  (cond
    [blame
     (unless passes?
       (raise-blame-error
        blame
        val
        '(expected " a ~a that accepts ~a~a~a argument~a~a~a" given: "~e")
        (if mtd? "method" "procedure")
        (if (zero? dom-length) "no" dom-length)
        (if (null? optionals) "" " mandatory")
        (if (null? mandatory-kwds) "" " ordinary")
        (if (= 1 dom-length) "" "s")
        (if (zero? optionals) ""
            (format " and up to ~a optional argument~a" optionals (if (= 1 optionals) "" "s")))
        (keyword-error-text mandatory-kwds optional-keywords)
        val))]
    [else
     passes?]))

(define (procedure-arity-includes?/optionals f base optionals)
  (cond
    [(zero? optionals) (procedure-arity-includes? f base #t)]
    [else (and (procedure-arity-includes? f (+ base optionals) #t)
               (procedure-arity-includes?/optionals f base (- optionals 1)))]))

(define (keywords-match mandatory-kwds optional-kwds val)
  (let-values ([(proc-mandatory proc-all) (procedure-keywords val)])
    (and ;; proc accepts all ctc's mandatory keywords
         (or (not proc-all)
             (andmap (λ (kwd) (member kwd proc-all))
                     mandatory-kwds))
         ;; proc's mandatory keywords are still mandatory in ctc
         (andmap (λ (kwd) (member kwd mandatory-kwds))
                 proc-mandatory)
         ;; proc accepts (but does not require) ctc's optional keywords
         ;;
         ;; if proc-all is #f, then proc accepts all keywords and thus
         ;; this is triviably true (e.g. result of make-keyword-procedure)
         (or (not proc-all)
             (andmap (λ (kwd) (and (member kwd proc-all)
                                   (not (member kwd proc-mandatory))))
                     optional-kwds)))))

(define (keyword-error-text mandatory-keywords optional-keywords)
  (define (format-keywords-error type kwds)
    (cond
      [(null? kwds) ""]
      [(null? (cdr kwds))
       (format "the ~a keyword ~a" type (car kwds))]
      [else
       (format
        "the ~a keywords ~a~a"
        type
        (car kwds)
        (apply string-append (map (λ (x) (format " ~a" x)) (cdr kwds))))]))
  (cond
    [(and (null? optional-keywords) (null? mandatory-keywords)) " without any keywords"]
    [(null? optional-keywords)
     (string-append " and " (format-keywords-error 'mandatory mandatory-keywords))]
    [(null? mandatory-keywords)
     (string-append " and " (format-keywords-error 'optional optional-keywords))]
    [else
     (string-append ", "
                    (format-keywords-error 'mandatory mandatory-keywords)
                    ", and "
                    (format-keywords-error 'optional optional-keywords))]))

;; check-procedure/more : ... (or/c #f blame) -> (or/c boolean? void?)
;; if blame is #f, then just return a boolean indicating that this matched 
;;   (for use in arity checking)
(define (check-procedure/more val mtd? dom-length mandatory-kwds optional-kwds blame)
  (define passes?
    (and (procedure? val)
         (procedure-accepts-and-more? val (if mtd? (+ dom-length 1) dom-length))
         (keywords-match mandatory-kwds optional-kwds val)))
  (cond
    [blame
     (unless passes?
       (raise-blame-error
        blame
        val
        '(expected " a ~a that accepts ~a argument~a and arbitrarily more~a" given: "~e")
        (if mtd? "method" "procedure")
        (cond
          [(zero? dom-length) "no"]
          [else dom-length])
        (if (= 1 dom-length) "" "s")
        (keyword-error-text mandatory-kwds optional-kwds)
        val))]
    [else
     passes?]))

(define (bad-number-of-results blame val rng-len args)
  (define num-values (length args))
  (raise-blame-error (blame-add-context blame "the range of")
                     val 
                     "expected ~a value~a, returned ~a value~a"
                     rng-len (if (= rng-len 1) "" "s")
                     num-values (if (= num-values 1) "" "s")))

;; timing & size tests

#;
(begin
  (require (prefix-in mz: mzlib/contract))
  (define (time-test f)
    (time
     (let loop ([n 2000])
       (unless (zero? n)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1) (f 1)
         (loop (- n 1))))))
  
  (define (size stx)
    (let ([sp (open-output-string)])
      (write (compile stx) sp)
      (close-output-port sp)
      (string-length (get-output-string sp))))
  
  'raw
  (size #'(λ (x) x))
  (time-test (λ (x) x))
  
  '->
  (size #'(-> number? number?))
  (time-test (contract (-> number? number?) (λ (x) x) 'pos 'neg))
  
  'mz:->
  (size #'(mz:-> number? number?))
  (time-test (contract (mz:-> number? number?) (λ (x) x) 'pos 'neg))
  
  '->*
  (size #'(->* (number?) () number?))
  (time-test (contract (->* (number?) () number?) (λ (x) x) 'pos 'neg))
  
  'mz:->*
  (size #'(mz:->* (number?) any/c (number?)))
  (time-test (contract (mz:->* (number?) any/c (number?)) (λ (x . y) x) 'pos 'neg))
  
  'case->
  (size #'(case-> (-> number? number?)))
  (time-test (contract (case-> (-> number? number?)) (λ (x) x) 'pos 'neg))
  
  'mz:case->
  (size #'(mz:case-> (-> number? number?)))
  (time-test (contract (mz:case-> (-> number? number?)) (λ (x) x) 'pos 'neg))
  
  '->d
  (size #'(->d ([x number?]) () [r number?]))
  (time-test (contract (->d ([x number?]) () [r number?]) (λ (x) x) 'pos 'neg))
  
  'mz:->r
  (size #'(mz:->r ([x number?]) number?))
  (time-test (contract (mz:->r ([x number?]) number?) (λ (x) x) 'pos 'neg))
  
  'object-contract
  (size #'(object-contract [m (-> number? number?)]))
  (time-test 
   (let ([o (contract (object-contract [m (-> number? number?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)])
     (λ (x) (send o m x))))
  
  
  'mz:object-contract
  (size #'(mz:object-contract [m (mz:-> number? number?)]))
  (time-test 
   (let ([o (contract (mz:object-contract [m (mz:-> number? number?)])
                      (new (class object% (define/public (m x) x) (super-new)))
                      'pos
                      'neg)])
     (λ (x) (send o m x)))))


(define predicate/c-private->ctc
  (let-syntax ([m (λ (stx)
                    ;; we don't use -> directly here to avoid a circularity, since
                    ;; (-> any/c boolean?) expands into the identifier -predicate/c
                    (syntax-case stx ()
                      [(_ arg)
                       #`(syntax-parameterize ((making-a-method #f)) #,(->/proc/main #'arg))]))])
    (let ([predicate/c (m (-> any/c boolean?))])
      predicate/c)))

(struct predicate/c ()
  #:property prop:chaperone-contract
  (build-chaperone-contract-property
   #:projection (let ([pc (contract-struct-projection predicate/c-private->ctc)])
                  (λ (ctc)
                    (λ (blame)
                      (let ([proj (pc blame)])
                        (λ (val)
                          (if (struct-predicate-procedure? val)
                              val
                              (proj val)))))))
   #:name (lambda (ctc) 'predicate/c)
   #:first-order (let ([f (contract-struct-first-order predicate/c-private->ctc)]) (λ (ctc) f))
   #:stronger (λ (this that) (contract-struct-stronger? predicate/c-private->ctc that))))

(define -predicate/c (predicate/c))

(define-syntax (-> stx) 
  (syntax-case stx (any any/c boolean?)
    [(_ any/c ... any)
     (not (syntax-parameter-value #'making-a-method))
     ;; special case the (-> any/c ... any) contracts to be first-order checks only
     (with-syntax ([dom-len (- (length (syntax->list stx)) 2)]
                   [name (syntax->datum stx)])
       #'(flat-named-contract 'name (λ (x) (and (procedure? x) (procedure-arity-includes? x dom-len #t)))))]
    [(_ any/c boolean?)
     ;; special case (-> any/c boolean?) to use predicate/c
     (not (syntax-parameter-value #'making-a-method))
     #'-predicate/c]
    [_
     #`(syntax-parameterize ((making-a-method #f)) #,(->/proc/main stx))]))
