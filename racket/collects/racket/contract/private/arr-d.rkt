#lang racket/base

(require "blame.rkt"
         "guts.rkt"
         "misc.rkt"
         "prop.rkt"
         racket/stxparam)
(require (for-syntax racket/base)
         (for-syntax "helpers.rkt")
         (for-syntax syntax/name)
         (for-syntax "arr-util.rkt")
         "arrow-common.rkt")

(provide ->d
         (for-syntax ->d-internal) ; for ->dm
         base-->d? ->d-name) ; for object-contract

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
    [(_ . args)
     (->d-internal (syntax/loc stx (->d . args)) #|method?|# #f)]))

(define-for-syntax (->d-internal stx maybe-this-param) ; non-#f is creating an ->dm
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
                        (if maybe-this-param
                            (generate-temporaries '(this))
                            null)])
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
                           [mtd? (and maybe-this-param #t)])
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
                                 (let ([old-param #,maybe-this-param])
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
                                     #`(λ args (apply f args))))))))))))]))

(define ((late-neg-->d-proj wrap-procedure) ->d-stct)
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
      (define dom-blame (blame-add-context blame "the domain of" #:swap? #t))
      (define rng-blame (blame-add-range-context blame))
      (λ (val neg-party)
        (define blame+neg-party (cons blame neg-party))
        (if (base-->d-rest-ctc ->d-stct)
            (check-procedure/more val
                                  (base-->d-mtd? ->d-stct)
                                  (length (base-->d-mandatory-dom-ctcs ->d-stct)) ;dom-length
                                  (base-->d-mandatory-keywords ->d-stct)
                                  (base-->d-optional-keywords ->d-stct)
                                  blame
                                  neg-party)
            (check-procedure val
                             (base-->d-mtd? ->d-stct)
                             (length (base-->d-mandatory-dom-ctcs ->d-stct)) ;dom-length
                             (length (base-->d-optional-dom-ctcs ->d-stct)) ; optionals-length
                             (base-->d-mandatory-keywords ->d-stct)
                             (base-->d-optional-keywords ->d-stct)
                             blame
                             neg-party))
        (wrap-procedure
         val
         (make-keyword-procedure
          (λ (kwd-args kwd-arg-vals . raw-orig-args)
            (with-contract-continuation-mark
              blame+neg-party
              (let* ([orig-args (if (base-->d-mtd? ->d-stct)
                                    (cdr raw-orig-args)
                                    raw-orig-args)]
                     [this (and (base-->d-mtd? ->d-stct) (car raw-orig-args))]
                     [dep-pre-args
                      (build-dep-ctc-args non-kwd-ctc-count raw-orig-args (base-->d-rest-ctc ->d-stct)
                                          (base-->d-keywords ->d-stct) kwd-args kwd-arg-vals)])
                (when (base-->d-pre-cond ->d-stct)
                  (unless (apply (base-->d-pre-cond ->d-stct) dep-pre-args)
                    (raise-blame-error (blame-swap blame) #:missing-party neg-party
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
                                (with-contract-continuation-mark
                                  blame+neg-party
                                  (let* ([range-count (length rng)]
                                         [post-args (append orig-results raw-orig-args)]
                                         [post-non-kwd-arg-count (+ non-kwd-ctc-count range-count)]
                                         [dep-post-args (build-dep-ctc-args post-non-kwd-arg-count
                                                                            post-args (base-->d-rest-ctc ->d-stct)
                                                                            (base-->d-keywords ->d-stct) kwd-args kwd-arg-vals)])
                                    (when (base-->d-post-cond ->d-stct)
                                      (unless (apply (base-->d-post-cond ->d-stct) dep-post-args)
                                        (raise-blame-error blame #:missing-party neg-party
                                                           val
                                                           "#:post violation~a~a"
                                                           (build-values-string ", argument" dep-pre-args)
                                                           (build-values-string (if (null? dep-pre-args)
                                                                                    ", result"
                                                                                    "\n result")
                                                                                orig-results))))
                                    
                                    (unless (= range-count (length orig-results))
                                      (raise-blame-error blame #:missing-party neg-party
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
                                                           rng-blame
                                                           neg-party)
                                           (loop (cdr results) (cdr result-contracts)))])))))))
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
                                               (cons (invoke-dep-ctc (car kwd-ctcs)
                                                                     dep-pre-args
                                                                     (car building-kwd-arg-vals)
                                                                     dom-blame
                                                                     neg-party)
                                                     (loop (cdr all-kwds) (cdr kwd-ctcs)
                                                           (cdr building-kwd-args)
                                                           (cdr building-kwd-arg-vals)))
                                               (loop (cdr all-kwds) (cdr kwd-ctcs)
                                                     building-kwd-args building-kwd-arg-vals))]))])
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
                           (invoke-dep-ctc (base-->d-rest-ctc ->d-stct) dep-pre-args '()
                                           dom-blame neg-party)
                           '())]
                      [(null? non-kwd-ctcs) 
                       (if (base-->d-rest-ctc ->d-stct)
                           (invoke-dep-ctc (base-->d-rest-ctc ->d-stct)
                                           dep-pre-args args dom-blame neg-party)
                           
                           ;; ran out of arguments, but don't have a rest parameter.
                           ;; procedure-reduce-arity (or whatever the new thing is
                           ;; going to be called) should ensure this doesn't happen.
                           (error 'shouldnt\ happen))]
                      [else (cons (invoke-dep-ctc (car non-kwd-ctcs)
                                                  dep-pre-args (car args)
                                                  dom-blame neg-party)
                                  (loop (cdr args)
                                        (cdr non-kwd-ctcs)))]))))))))
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
(define (invoke-dep-ctc dep-ctc dep-args val blame neg-party)
  (let ([ctc (coerce-contract '->d (if dep-args
                                       (apply dep-ctc dep-args)
                                       dep-ctc))])
    (((get/build-late-neg-projection ctc)
      blame)
     val
     neg-party)))

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
    (make-impersonator-->d mtd?
                           mandatory-dom-ctcs optional-dom-ctcs
                           (map cdr kwd/ctc-pairs)
                           rest-ctc pre-cond range post-cond
                           (map car kwd/ctc-pairs)
                           mandatory-kwds
                           optional-kwds
                           name-wrapper)))

;; Re `print-as-method-if-method?`: See comment before `base->-name` in arrow-val-first.rkt
(define ((->d-name print-as-method-if-method?) ctc)
  (let* ([name (if (and (base-->d-mtd? ctc) print-as-method-if-method?) '->dm '->d)]
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
          (check-procedure/more val mtd? dom-length mandatory-kwds optional-kwds #f #f)
          (check-procedure val mtd? dom-length optionals mandatory-kwds optional-kwds #f #f)))))
(define (->d-stronger? this that) (eq? this that))

;; in the struct type descriptions "d???" refers to the arguments (domain) of the function that
;; is under the contract, and "dr???" refers to the arguments & the results of the function that 
;; is under the contract.
;; the `box' in the range only serves to differentiate between range contracts that depend on
;; both the domain and the range from those that depend only on the domain (and thus, those
;; that can be applied early)
(define-struct base-->d (mtd?                ;; boolean; indicates if this is a contract on a method, for error reporing purposes.
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
  #:property prop:custom-write custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:late-neg-projection (late-neg-->d-proj impersonate-procedure)
   #:name (->d-name #|print-as-method-if-method?|# #t)
   #:first-order ->d-first-order
   #:stronger ->d-stronger?))
