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
         "opt.rkt"
         racket/stxparam)
(require (for-syntax racket/base)
         (for-syntax "opt-guts.rkt")
         (for-syntax "helpers.rkt")
         (for-syntax syntax/stx)
         (for-syntax syntax/name)
         (for-syntax "arr-util.rkt"))

(provide ->
         ->*
         ->d
         case->
         unconstrained-domain->
         the-unsupplied-arg
         unsupplied-arg?
         making-a-method
         procedure-accepts-and-more?
         check-procedure
         check-procedure/more
         (struct-out contracted-function))

(define-syntax-parameter making-a-method #f)
(define-for-syntax (make-this-parameters id)
  (if (syntax-parameter-value #'making-a-method)
      (list id)
      null))

(define-struct contracted-function (proc ctc)
  #:property prop:procedure 0
  #:property prop:contracted 1)

(define-syntax (unconstrained-domain-> stx)
  (syntax-case stx ()
    [(_ rngs ...)
     (with-syntax ([(rngs-x ...) (generate-temporaries #'(rngs ...))]
                   [(proj-x ...) (generate-temporaries #'(rngs ...))]
                   [(p-app-x ...) (generate-temporaries #'(rngs ...))]
                   [(res-x ...) (generate-temporaries #'(rngs ...))])
       #'(let ([rngs-x (coerce-contract 'unconstrained-domain-> rngs)] ...)
           (let ([proj-x (contract-projection rngs-x)] ...)
             (define ctc
               (make-contract
                #:name
                (build-compound-type-name 'unconstrained-domain-> (contract-name rngs-x) ...)
                #:projection
                (λ (blame)
                  (let ([p-app-x (proj-x blame)] ...)
                    (λ (val)
                      (if (procedure? val)
                          (make-contracted-function
                           (make-keyword-procedure
                            (λ (kwds kwd-vals . args)
                              (let-values ([(res-x ...) (keyword-apply val kwds kwd-vals args)])
                                (values (p-app-x res-x) ...)))
                            (λ args
                              (let-values ([(res-x ...) (apply val args)])
                                (values (p-app-x res-x) ...))))
                           ctc)
                          (raise-blame-error blame
                                             val
                                             "expected a procedure")))))
                #:first-order
                procedure?))
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
;; func : the wrapper function maker. It accepts a procedure for
;;        checking the first-order properties and the contracts
;;        and it produces a wrapper-making function.
(define-struct -> (pre post doms/c optional-doms/c dom-rest/c mandatory-kwds/c mandatory-kwds optional-kwds/c optional-kwds rngs/c rng-any? func)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc) 
     (let* ([doms-proj (map contract-projection
                            (if (->-dom-rest/c ctc)
                                (append (->-doms/c ctc) (list (->-dom-rest/c ctc)))
                                (->-doms/c ctc)))]
            [doms-optional-proj (map contract-projection (->-optional-doms/c ctc))]
            [rngs-proj (map contract-projection (->-rngs/c ctc))]
            [mandatory-kwds-proj (map contract-projection (->-mandatory-kwds/c ctc))]
            [optional-kwds-proj (map contract-projection (->-optional-kwds/c ctc))]
            [mandatory-keywords (->-mandatory-kwds ctc)]
            [optional-keywords (->-optional-kwds ctc)]
            [func (->-func ctc)]
            [dom-length (length (->-doms/c ctc))]
            [optionals-length (length (->-optional-doms/c ctc))]
            [has-rest? (and (->-dom-rest/c ctc) #t)]
            [pre (->-pre ctc)]
            [post (->-post ctc)])
       (λ (blame)
         (let ([swapped (blame-swap blame)])
           (let ([partial-doms (map (λ (dom) (dom swapped)) doms-proj)]
                 [partial-optional-doms (map (λ (dom) (dom swapped)) doms-optional-proj)]
                 [partial-ranges (map (λ (rng) (rng blame)) rngs-proj)]
                 [partial-mandatory-kwds (map (λ (kwd) (kwd swapped)) mandatory-kwds-proj)]
                 [partial-optional-kwds (map (λ (kwd) (kwd swapped)) optional-kwds-proj)])
             (apply func
                    (λ (val mtd?)
                      (if has-rest?
                          (check-procedure/more val mtd? dom-length mandatory-keywords optional-keywords blame)
                          (check-procedure val mtd? dom-length optionals-length mandatory-keywords optional-keywords blame)))
                    ctc
                    (if pre 
                        (λ (val)
                          (unless (pre)
                            (raise-blame-error swapped
                                               val
                                               "#:pre violation")))
                        void)
                    (if post
                        (λ (val)
                          (unless (post)
                            (raise-blame-error blame
                                               val
                                               "#:post violation")))
                        void)
                    (append partial-doms partial-optional-doms 
                            partial-mandatory-kwds partial-optional-kwds
                            partial-ranges)))))))

   #:name
   (λ (ctc) (single-arrow-name-maker 
             (->-doms/c ctc)
             (->-optional-doms/c ctc)
             (->-dom-rest/c ctc)
             (->-mandatory-kwds/c ctc)
             (->-mandatory-kwds ctc)
             (->-optional-kwds/c ctc)
             (->-optional-kwds ctc)
             (->-rng-any? ctc)
             (->-rngs/c ctc)
             (->-pre ctc)
             (->-post ctc)))

   #:first-order
   (λ (ctc)
      (λ (x)
         (let ([l (length (->-doms/c ctc))])
           (and (procedure? x) 
                (if (->-dom-rest/c ctc)
                  (procedure-accepts-and-more? x l)
                  (procedure-arity-includes? x l))
                (keywords-match (->-mandatory-kwds ctc) (->-optional-kwds ctc) x)
                #t))))
   #:stronger
   (λ (this that)
      (and (->? that)
           (= (length (->-doms/c that)) (length (->-doms/c this)))
           (andmap contract-stronger? (->-doms/c that) (->-doms/c this))
           
           (equal? (->-mandatory-kwds this) (->-mandatory-kwds that))
           (andmap contract-stronger? (->-mandatory-kwds/c that) (->-mandatory-kwds/c this))
           
           (equal? (->-optional-kwds this) (->-optional-kwds that))
           (andmap contract-stronger? (->-optional-kwds/c that) (->-optional-kwds/c this))
           
           (= (length (->-rngs/c that)) (length (->-rngs/c this)))
           (andmap contract-stronger? (->-rngs/c this) (->-rngs/c that))))))

(define (build--> name
                  pre post
                  doms/c-or-p optional-doms/c-or-p doms-rest/c-or-p-or-f 
                  mandatory-kwds/c-or-p mandatory-kwds optional-kwds/c-or-p optional-kwds
                  rngs/c-or-p
                  rng-any? func)
  (let ([cc (λ (c-or-p) (coerce-contract name c-or-p))])
    (make--> 
     pre post
     (map cc doms/c-or-p) (map cc optional-doms/c-or-p) (and doms-rest/c-or-p-or-f (cc doms-rest/c-or-p-or-f))
     (map cc mandatory-kwds/c-or-p) mandatory-kwds (map cc optional-kwds/c-or-p) optional-kwds
     (map cc rngs/c-or-p) rng-any?
     func)))

(define (single-arrow-name-maker doms/c optional-doms/c doms-rest kwds/c kwds optional-kwds/c optional-kwds rng-any? rngs pre post)
  (cond
    [(or doms-rest 
         (not (null? optional-kwds))
         (not (null? optional-doms/c))
         pre post)
     (let ([range
            (cond
              [rng-any? 'any]
              [(and (pair? rngs)
                    (null? (cdr rngs)))
               (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply
        build-compound-type-name 
        '->*
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
     (let ([rng-name
            (cond
              [rng-any? 'any]
              [(null? rngs) '(values)]
              [(null? (cdr rngs)) (car rngs)]
              [else (apply build-compound-type-name 'values rngs)])])
       (apply build-compound-type-name 
              '->
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
                (values (syntax (dom-ctc ...))
                        (syntax (ignored))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (any/c))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((this-parameter ... args ... keyword-formal-parameters ...)
                                 (val this-parameter ... (dom-ctc args) ... keyword-call/ctc ...)))
                        #t))]
             [(values rngs ...)
              (with-syntax ([(rng-x ...) (generate-temporaries (syntax (rngs ...)))]
                            [(rng-ctc ...) (generate-temporaries (syntax (rngs ...)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc ...))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rngs ...))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((this-parameter ... args ... keyword-formal-parameters ...)
                                 (apply-projections ((rng-x rng-ctc) ...)
                                                    (val this-parameter ... (dom-ctc args) ... keyword-call/ctc ...))))
                        #f))]
             [rng
              (with-syntax ([(rng-ctc) (generate-temporaries (syntax (rng)))])
                (values (syntax (dom-ctc ...))
                        (syntax (rng-ctc))
                        (syntax (dom-kwd-ctc-id ...))
                        (syntax (doms ...))
                        (syntax (rng))
                        (syntax (dom-kwd-ctc ...))
                        (syntax (dom-kwd ...))
                        (syntax ((this-parameter ... args ... keyword-formal-parameters ...)
                                 (apply-projection rng-ctc (val  this-parameter ... (dom-ctc args) ... keyword-call/ctc ...))))
                        #f))]))))]))

(define-for-syntax (maybe-a-method/name stx)
   (if (syntax-parameter-value #'making-a-method)
       (syntax-property stx 'method-arity-error #t)
       stx))

;; ->/proc/main : syntax -> (values syntax[contract-record] syntax[args/lambda-body] syntax[names])
(define-for-syntax (->/proc/main stx)
  (let-values ([(dom-names rng-names kwd-names dom-ctcs rng-ctcs kwd-ctcs kwds inner-args/body use-any?) (->-helper stx)]
               [(this->) (gensym 'this->)])
    (with-syntax ([(args body) inner-args/body])
      (with-syntax ([(dom-names ...) dom-names]
                    [(rng-names ...) rng-names]
                    [(kwd-names ...) kwd-names]
                    [(dom-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:negative-position this->)) 
                                         (syntax->list dom-ctcs))]
                    [(rng-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:positive-position this->))
                                         (syntax->list rng-ctcs))]
                    [(kwd-ctcs ...) (map (λ (x) (syntax-property x 'racket/contract:negative-position this->))
                                         (syntax->list kwd-ctcs))]
                    [(kwds ...) kwds]
                    [inner-lambda 
                     (maybe-a-method/name
                      (add-name-prop
                       (syntax-local-infer-name stx)
                       (syntax (lambda args body))))]
                    [use-any? use-any?])
        (with-syntax ([outer-lambda
                       #`(lambda (chk ctc pre post dom-names ... kwd-names ... rng-names ...)
                           ;; ignore the pre and post arguments here because -> never fills them in with something useful
                           (lambda (val)
                             (chk val #,(and (syntax-parameter-value #'making-a-method) #t))
                             (make-contracted-function inner-lambda ctc)))])
          (values
           (syntax-property 
            (syntax 
             (build--> '->
                       #f #f
                       (list dom-ctcs ...) '() #f
                       (list kwd-ctcs ...) '(kwds ...) '() '()
                       (list rng-ctcs ...) use-any?
                       outer-lambda))
            'racket/contract:contract 
            (vector this-> 
                    ;; the -> in the original input to this guy
                    (list (car (syntax-e stx)))
                    '()))
           inner-args/body
           (syntax (dom-names ... rng-names ...))))))))
  
(define-syntax (-> stx) 
  (let-values ([(stx _1 _2) (->/proc/main stx)])
    #`(syntax-parameterize ((making-a-method #f)) #,stx)))

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
       (with-syntax ((blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((rng-arg ...) rng-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars))
                     ((next-rng ...) next-rngs))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #| keywords |# blame)
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
       (with-syntax ((blame (opt/info-blame opt/info))
                     ((dom-arg ...) dom-vars)
                     ((next-dom ...) next-doms)
                     (dom-len (length dom-vars)))
         (syntax (begin
                   (check-procedure val #f dom-len 0 '() '() #|keywords|# blame)
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
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (syntax->list (syntax (rng ...)))))]
    [(-> dom ... any)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-any-ctc (syntax->list (syntax (dom ...)))))]
    [(-> dom ... rng)
     (if (ormap (λ (x) (keyword? (syntax-e x))) (syntax->list #'(dom ...)))
         (opt/unknown opt/i opt/info stx) ;; give up if there is a mandatory keyword 
         (opt/arrow-ctc (syntax->list (syntax (dom ...)))
                        (list #'rng)))]))



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
               
               
               (with-syntax ([(rng-proj ...) (generate-temporaries (or rng-ctc '()))]
                             [(rng ...) (generate-temporaries (or rng-ctc '()))]
                             [(this-parameter ...)
                              (make-this-parameters (car (generate-temporaries '(this))))])
                 #`(build-->
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
                    (λ (chk 
                        ctc
                        pre post
                        mandatory-dom-proj ...  
                        #,@(if rest-ctc
                               #'(rest-proj)
                               #'())
                        optional-dom-proj ... 
                        mandatory-dom-kwd-proj ... 
                        optional-dom-kwd-proj ... 
                        rng-proj ...)
                      (λ (f)
                        (chk f #,(and (syntax-parameter-value #'making-a-method) #t))
                        (make-contracted-function
                         #,(maybe-a-method/name
                            (add-name-prop
                             (syntax-local-infer-name stx)
                             #`(λ (this-parameter ...
                                   mandatory-dom-arg ... 
                                   [optional-dom-arg unspecified-dom] ... 
                                   mandatory-dom-kwd/var-seq ... 
                                   optional-dom-kwd/var-seq ...
                                   #,@(if rest-ctc #'rest #'()))
                                 (let*-values ([(kwds kwd-args) (values '() '())]
                                               [(kwds kwd-args) (if (eq? unspecified-dom rev-sorted-dom-kwd-arg)
                                                                    (values kwds kwd-args)
                                                                    (values (cons 'rev-sorted-dom-kwd kwds)
                                                                            (cons (rev-sorted-dom-kwd-proj rev-sorted-dom-kwd-arg)
                                                                                  kwd-args)))] 
                                               ...
                                               [(opt-args) #,(if rest-ctc 
                                                                 #'(rest-proj rest)
                                                                 #''())]
                                               [(opt-args) (if (eq? unspecified-dom rev-optional-dom-arg)
                                                               opt-args
                                                               (cons (rev-optional-dom-proj rev-optional-dom-arg) opt-args))]
                                               ...)
                                   (pre f)
                                   #,(let ([call 
                                            (if (null? (syntax->list #'(rev-sorted-dom-kwd ...)))
                                                #'(apply f this-parameter ... (mandatory-dom-proj mandatory-dom-arg) ... opt-args)
                                                #'(keyword-apply f this-parameter ... kwds kwd-args (mandatory-dom-proj mandatory-dom-arg) ... opt-args))])
                                       (cond
                                         [(and rng-ctc post)
                                          #`(let-values ([(rng ...) #,call])
                                              (begin0 (values (rng-proj rng) ...)
                                                      (post f)))]
                                         [rng-ctc
                                          #`(apply-projections ((rng rng-proj) ...)
                                                               #,call)]
                                         [else
                                          call]))))))
                         ctc))))))))))]))
           
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
                           [mtd? (and (syntax-parameter-value #'making-a-method) #t)])
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
                      (build-->d mtd? 
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

(define ->d-tail-key (gensym '->d-tail-key))

(define (->d-proj ->d-stct)
  (let* ([opt-count (length (->d-optional-dom-ctcs ->d-stct))]
         [mandatory-count (+ (length (->d-mandatory-dom-ctcs ->d-stct)) 
                             (if (->d-mtd? ->d-stct) 1 0))]
         [non-kwd-ctc-count (+ mandatory-count opt-count)]
         [arity 
          (cond
            [(->d-rest-ctc ->d-stct)
             (make-arity-at-least mandatory-count)]
            [else
             (let loop ([i 0])
               (cond
                 [(= i opt-count)
                  (list (+ mandatory-count i))]
                 [else
                  (cons (+ mandatory-count i) (loop (+ i 1)))]))])])
    (λ (blame)
      (let ([this->d-id (gensym '->d-tail-key)])
        (λ (val)
          (check-procedure val
                           (->d-mtd? ->d-stct)
                           (length (->d-mandatory-dom-ctcs ->d-stct)) ;dom-length
                           (length (->d-optional-dom-ctcs ->d-stct)) ; optionals-length
                           (->d-mandatory-keywords ->d-stct)
                           (->d-optional-keywords ->d-stct)
                           blame)
          (let ([kwd-proc
                 (λ (kwd-args kwd-arg-vals . raw-orig-args)
                   (let* ([orig-args (if (->d-mtd? ->d-stct)
                                         (cdr raw-orig-args)
                                         raw-orig-args)]
                          [this (and (->d-mtd? ->d-stct) (car raw-orig-args))]
                          [dep-pre-args
                           (build-dep-ctc-args non-kwd-ctc-count raw-orig-args (->d-rest-ctc ->d-stct)
                                               (->d-keywords ->d-stct) kwd-args kwd-arg-vals)]
                          [thunk
                           (λ ()
                             (keyword-apply
                              val
                              kwd-args
                              
                              ;; contracted keyword arguments
                              (let loop ([all-kwds (->d-keywords ->d-stct)]
                                         [kwd-ctcs (->d-keyword-ctcs ->d-stct)] 
                                         [building-kwd-args kwd-args]
                                         [building-kwd-arg-vals kwd-arg-vals])
                                (cond
                                  [(or (null? building-kwd-args) (null? all-kwds)) '()]
                                  [else (if (eq? (car all-kwds)
                                                 (car building-kwd-args))
                                            (cons (invoke-dep-ctc (car kwd-ctcs) dep-pre-args (car building-kwd-arg-vals) (blame-swap blame))
                                                  (loop (cdr all-kwds) (cdr kwd-ctcs) (cdr building-kwd-args) (cdr building-kwd-arg-vals)))
                                            (loop (cdr all-kwds) (cdr kwd-ctcs) building-kwd-args building-kwd-arg-vals))]))
                              
                              (append
                               ;; this parameter (if necc.)
                               (if (->d-mtd? ->d-stct)
                                   (list (car raw-orig-args))
                                   '())
                               
                               ;; contracted ordinary arguments
                               (let loop ([args orig-args]
                                          [non-kwd-ctcs (append (->d-mandatory-dom-ctcs ->d-stct)
                                                                (->d-optional-dom-ctcs ->d-stct))])
                                 (cond
                                   [(null? args) 
                                    (if (->d-rest-ctc ->d-stct)
                                        (invoke-dep-ctc (->d-rest-ctc ->d-stct) dep-pre-args '() (blame-swap blame))
                                        '())]
                                   [(null? non-kwd-ctcs) 
                                    (if (->d-rest-ctc ->d-stct)
                                        (invoke-dep-ctc (->d-rest-ctc ->d-stct) dep-pre-args args (blame-swap blame))
                                        
                                        ;; ran out of arguments, but don't have a rest parameter.
                                        ;; procedure-reduce-arity (or whatever the new thing is
                                        ;; going to be called) should ensure this doesn't happen.
                                        (error 'shouldnt\ happen))]
                                   [else (cons (invoke-dep-ctc (car non-kwd-ctcs) dep-pre-args (car args) (blame-swap blame))
                                               (loop (cdr args)
                                                     (cdr non-kwd-ctcs)))])))))]
                          [rng (let ([rng (->d-range ->d-stct)])
                                 (cond
                                   [(not rng) #f]
                                   [(box? rng) 
                                    (map (λ (val) (apply val dep-pre-args))
                                         (unbox rng))]
                                   [else rng]))]
                          [rng-underscore? (box? (->d-range ->d-stct))])
                     (when (->d-pre-cond ->d-stct)
                       (unless (apply (->d-pre-cond ->d-stct) dep-pre-args)
                         (raise-blame-error (blame-swap blame)
                                            val
                                            "#:pre violation~a"
                                            (build-values-string ", argument" dep-pre-args))))
                     (call-with-immediate-continuation-mark
                      ->d-tail-key
                      (λ (first-mark)
                        (cond
                          [(and rng
                                (not (and first-mark
                                          (eq? this->d-id (car first-mark))
                                          (andmap eq? raw-orig-args (cdr first-mark)))))
                           (call-with-values
                            (λ ()
                              (with-continuation-mark ->d-tail-key (cons this->d-id raw-orig-args)
                                (thunk)))
                            (λ orig-results
                              (let* ([range-count (length rng)]
                                     [post-args (append orig-results raw-orig-args)]
                                     [post-non-kwd-arg-count (+ non-kwd-ctc-count range-count)]
                                     [dep-post-args (build-dep-ctc-args post-non-kwd-arg-count
                                                                        post-args (->d-rest-ctc ->d-stct)
                                                                        (->d-keywords ->d-stct) kwd-args kwd-arg-vals)])
                                (when (->d-post-cond ->d-stct)
                                  (unless (apply (->d-post-cond ->d-stct) dep-post-args)
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
                                                       blame)
                                       (loop (cdr results) (cdr result-contracts)))]))))))]
                          [else
                           (thunk)])))))])
            (make-contracted-function
             (procedure-reduce-keyword-arity
              (make-keyword-procedure kwd-proc
                                      ((->d-name-wrapper ->d-stct)
                                       (λ args
                                         (apply kwd-proc '() '() args))))
              
              arity 
              (->d-mandatory-keywords ->d-stct)
              (->d-keywords ->d-stct))
             ->d-stct)))))))

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
(define (invoke-dep-ctc dep-ctc dep-args val blame)
  (let ([ctc (coerce-contract '->d (if dep-args
                                       (apply dep-ctc dep-args)
                                       dep-ctc))])
    (((contract-projection ctc) blame) val)))

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

(define (build-->d mtd?
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
    (make-->d mtd?
              mandatory-dom-ctcs optional-dom-ctcs
              (map cdr kwd/ctc-pairs)
              rest-ctc pre-cond range post-cond
              (map car kwd/ctc-pairs)
              mandatory-kwds
              optional-kwds
              name-wrapper)))

;; in the struct type descriptions "d???" refers to the arguments (domain) of the function that
;; is under the contract, and "dr???" refers to the arguments & the results of the function that 
;; is under the contract.
;; the `box' in the range only serves to differentiate between range contracts that depend on
;; both the domain and the range from those that depend only on the domain (and thus, those
;; that can be applied early)
(define-struct ->d (mtd?                ;; boolean; indicates if this is a contract on a method, for error reporing purposes.
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
                    name-wrapper)       ;; (-> proc proc) 
  
  #:omit-define-syntaxes

  #:property prop:contract
  (build-contract-property
   #:projection ->d-proj
   #:name
   (λ (ctc) 
      (let* ([counting-id 'x]
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
        `(->d (,@(map (λ (x) `(,(next-id) ...)) (->d-mandatory-dom-ctcs ctc))
               ,@(apply append (map (λ (kwd) (list kwd `(,(next-id) ...))) (->d-mandatory-keywords ctc))))
              (,@(map (λ (x) `(,(next-id) ...)) (->d-optional-dom-ctcs ctc))
               ,@(apply append (map (λ (kwd) (list kwd `(,(next-id) ...))) (->d-optional-keywords ctc))))
              ,@(if (->d-rest-ctc ctc)
                  (list '#:rest (next-id) '...)
                  '())
              ,@(if (->d-pre-cond ctc)
                  (list '#:pre '...)
                  (list))
              ,(let ([range (->d-range ctc)])
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
              ,@(if (->d-post-cond ctc)
                  (list '#:post '...)
                  (list)))))

   #:first-order (λ (ctc) (λ (x) #f))
   #:stronger (λ (this that) (eq? this that))))


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
                (if rst
                    #`(apply-projections ((rng-id rng-proj-x) ...)
                                         (apply f
                                                this-parameter ... 
                                                (dom-proj-x dom-formals) ... 
                                                (rst-proj-x rst-formal)))
                    
                    #`(apply-projections ((rng-id rng-proj-x) ...)
                                         (f this-parameter ... (dom-proj-x dom-formals) ...)))]
               [rst
                #`(apply f this-parameter ... (dom-proj-x dom-formals) ... (rst-proj-x rst-formal))]
               [else
                #`(f this-parameter ... (dom-proj-x dom-formals) ...)]))))))

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
                      (map (λ (x) (parse-out-case stx x)) (syntax->list #'(cases ...)))])
         #`(syntax-parameterize 
            ((making-a-method #f)) 
            (build-case-> (list (list dom-proj ...) ...)
                          (list rst-proj ...)
                          (list rng-proj ...)
                          '(spec ...)
                          (λ (chk
                              ctc
                              #,@(apply append (map syntax->list (syntax->list #'((dom-proj-x ...) ...))))
                              #,@(apply append (map syntax->list (syntax->list #'((rng-proj-x ...) ...)))))
                            (λ (f)
                              (chk f #,(and (syntax-parameter-value #'making-a-method) #t))
                              (make-contracted-function
                               (case-lambda
                                [formals body] ...)
                               ctc)))))))]))

;; dom-ctcs : (listof (listof contract))
;; rst-ctcs : (listof contract)
;; rng-ctcs : (listof (listof contract))
;; specs : (listof (list boolean exact-positive-integer)) ;; indicates the required arities of the input functions
;; wrapper : (->* () () (listof contract?) (-> procedure? procedure?)) -- generates a wrapper from projections
(define-struct case-> (dom-ctcs rst-ctcs rng-ctcs specs wrapper)
  #:omit-define-syntaxes
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc)
      (let* ([dom-ctcs (map contract-projection (get-case->-dom-ctcs ctc))]
             [rng-ctcs (let ([rngs (get-case->-rng-ctcs ctc)])
                         (and rngs (map contract-projection (get-case->-rng-ctcs ctc))))]
             [rst-ctcs (case->-rst-ctcs ctc)]
             [specs (case->-specs ctc)])
        (λ (blame)
           (let ([projs (append (map (λ (f) (f (blame-swap blame))) dom-ctcs)
                                (map (λ (f) (f blame)) rng-ctcs))]
                 [chk
                  (λ (val mtd?) 
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
                        specs rst-ctcs)]))])
             (apply (case->-wrapper ctc)
                    chk
                    ctc
                    projs)))))
   #:name
   (λ (ctc)
      (apply
       build-compound-type-name
       'case->
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
            (case->-dom-ctcs ctc)
            (case->-rst-ctcs ctc)
            (case->-rng-ctcs ctc))))

   #:first-order (λ (ctc) (λ (val) #f))
   #:stronger (λ (this that) #f)))

(define (build-case-> dom-ctcs rst-ctcs rng-ctcs specs wrapper)
  (make-case-> (map (λ (l) (map (λ (x) (coerce-contract 'case-> x)) l)) dom-ctcs)
               (map (λ (x) (and x (coerce-contract 'case-> x))) rst-ctcs)
               (and rng-ctcs (map (λ (l) (and l (map (λ (x) (coerce-contract 'case-> x)) l))) rng-ctcs))
               specs
               wrapper))
  

(define (get-case->-dom-ctcs ctc)
  (apply append
         (map (λ (doms rst) (if rst
                                (append doms (list rst))
                                doms))
              (case->-dom-ctcs ctc)
              (case->-rst-ctcs ctc))))

(define (get-case->-rng-ctcs ctc)
  (apply append (map (λ (x) (or x '())) (case->-rng-ctcs ctc))))



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

(define (check-procedure val mtd? dom-length optionals mandatory-kwds optional-keywords blame)
  (unless (and (procedure? val)
               (procedure-arity-includes?/optionals val (if mtd? (+ dom-length 1) dom-length) optionals)
               (keywords-match mandatory-kwds optional-keywords val))
    (raise-blame-error
     blame
     val
     "expected a ~a that accepts ~a~a~a argument~a~a~a, given: ~e"
     (if mtd? "method" "procedure")
     (if (zero? dom-length) "no" dom-length)
     (if (null? optionals) "" " mandatory")
     (if (null? mandatory-kwds) "" " ordinary")
     (if (= 1 dom-length) "" "s")
     (if (zero? optionals) ""
         (format " and up to ~a optional argument~a" optionals (if (= 1 optionals) "" "s")))
     (keyword-error-text mandatory-kwds optional-keywords)
     val)))

(define (procedure-arity-includes?/optionals f base optionals)
  (cond
    [(zero? optionals) (procedure-arity-includes? f base)]
    [else (and (procedure-arity-includes? f (+ base optionals))
               (procedure-arity-includes?/optionals f base (- optionals 1)))]))

(define (keywords-match mandatory-kwds optional-kwds val)
  (let-values ([(proc-mandatory proc-all) (procedure-keywords val)])
    (and ;; proc accepts all ctc's mandatory keywords
         (andmap (λ (kwd) (member kwd proc-all))
                 mandatory-kwds)
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
     
(define (check-procedure/more val mtd? dom-length mandatory-kwds optional-kwds blame)
  (unless (and (procedure? val)
               (procedure-accepts-and-more? val (if mtd? (+ dom-length 1) dom-length))
               (keywords-match mandatory-kwds optional-kwds val))
    (raise-blame-error
     blame
     val
     "expected a ~a that accepts ~a argument~a and arbitrarily more~a, given: ~e"
     (if mtd? "method" "procedure")
     (cond
       [(zero? dom-length) "no"]
       [else dom-length])
     (if (= 1 dom-length) "" "s")
     (keyword-error-text mandatory-kwds optional-kwds)
     val)))

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
