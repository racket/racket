#lang racket/base

(require "arrow.rkt"
         "prop.rkt"
         "guts.rkt"
         "opt.rkt"
         "misc.rkt"
         "blame.rkt"
         syntax/location
         (for-syntax racket/base
                     racket/stxparam-exptime
                     "arr-i-parse.rkt"
                     
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library 
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]
                                [module-identifier-mapping-for-each free-identifier-mapping-for-each])))

(provide (rename-out [->i/m ->i]))

;; arg-ctcs      : (listof contract)
;; arg-dep-ctcs  : (-> ??? (listof contract))
;; indy-arg-ctcs : (listof contract)
;; rng-ctcs      : (listof contract)
;; rng-dep-ctcs  : (-> ??? (listof contract))
;; indy-rng-ctcs : (listof contract)
;; mandatory-args, opt-args : number
;; mandatory-kwds, opt-kwds : (listof keyword?) sorted by keyword<?
;; rest? : boolean
;; here : quoted-spec for use in assigning indy blame
;; mk-wrapper : creates the a wrapper function that implements the contract checking
(struct ->i (arg-ctcs arg-dep-ctcs indy-arg-ctcs
                      rng-ctcs rng-dep-ctcs indy-rng-ctcs
                      pre/post-procs
                      mandatory-args opt-args mandatory-kwds opt-kwds rest? mtd?
                      here
                      mk-wrapper
                      name-info)
        #:property prop:contract
        (build-contract-property
         #:projection
         (λ (ctc) 
           (let ([arg-ctc-projs (map contract-projection (->i-arg-ctcs ctc))]
                 [indy-arg-ctc-projs (map contract-projection (->i-indy-arg-ctcs ctc))]
                 [rng-ctc-projs (map contract-projection (->i-rng-ctcs ctc))]
                 [indy-rng-ctc-projs (map contract-projection (->i-indy-rng-ctcs ctc))]
                 [func (->i-mk-wrapper ctc)]
                 [has-rest? (->i-rest? ctc)]
                 [here (->i-here ctc)])
             (λ (blame)
               (let* ([swapped-blame (blame-swap blame)]
                      [indy-dom-blame (blame-replace-negative swapped-blame here)]
                      [indy-rng-blame (blame-replace-negative blame here)]
                      
                      [partial-doms (map (λ (dom) (dom swapped-blame)) arg-ctc-projs)]
                      [partial-indy-doms (map (λ (dom) (dom indy-dom-blame)) indy-arg-ctc-projs)]
                      
                      [partial-rngs (map (λ (rng) (rng blame)) rng-ctc-projs)]
                      [partial-indy-rngs (map (λ (rng) (rng indy-rng-blame)) indy-rng-ctc-projs)])
                 (apply func
                        blame
                        swapped-blame
                        indy-dom-blame
                        indy-rng-blame
                        (λ (val mtd?)
                          (if has-rest?
                              (check-procedure/more val mtd? (->i-mandatory-args ctc) (->i-mandatory-kwds ctc) (->i-opt-kwds ctc) blame)
                              (check-procedure val mtd? (->i-mandatory-args ctc) (->i-opt-args ctc) (->i-mandatory-kwds ctc) (->i-opt-kwds ctc) blame)))
                        ctc
                        (append (->i-pre/post-procs ctc)
                                partial-doms
                                (->i-arg-dep-ctcs ctc)
                                partial-indy-doms
                                partial-rngs
                                (->i-rng-dep-ctcs ctc)
                                partial-indy-rngs))))))
         #:name (λ (ctc) 
                  (define (arg/ress->spec infos ctcs dep-ctcs skip?)
                    (let loop ([infos infos]
                               [ctcs ctcs]
                               [dep-ctcs dep-ctcs])
                      (cond
                        [(null? infos) '()]
                        [else 
                         (let* ([info (car infos)]
                                [dep/nodep (list-ref info 0)]
                                [var (list-ref info 1)]
                                [vars (list-ref info 2)]
                                [kwd (list-ref info 3)])
                           (case dep/nodep
                             [(nodep)
                              (if (skip? info)
                                  (loop (cdr infos) (cdr ctcs) dep-ctcs)
                                  `(,@(if kwd
                                          (list kwd)
                                          (list))
                                    [,var ,(contract-name (car ctcs))]
                                    .
                                    ,(loop (cdr infos) (cdr ctcs) dep-ctcs)))]
                             [(dep)
                              (if (skip? info)
                                  (loop (cdr infos) ctcs (cdr dep-ctcs))
                                  `(,@(if kwd
                                          (list kwd)
                                          (list))
                                    [,var ,vars ...]
                                    .
                                    ,(loop (cdr infos) ctcs (cdr dep-ctcs))))]))])))
                  (let* ([name-info (->i-name-info ctc)]
                         [args-info (vector-ref name-info 0)]
                         [rest-info (vector-ref name-info 1)]
                         [pre-infos  (vector-ref name-info 2)]
                         [rng-info  (vector-ref name-info 3)]
                         [post-infos (vector-ref name-info 4)])
                    `(->i ,(arg/ress->spec args-info
                                           (->i-arg-ctcs ctc)
                                           (->i-arg-dep-ctcs ctc)
                                           (λ (x) (list-ref x 4)))
                          ,@(let ([rests (arg/ress->spec args-info
                                                         (->i-arg-ctcs ctc)
                                                         (->i-arg-dep-ctcs ctc)
                                                         (λ (x) (not (list-ref x 4))))])
                              (if (null? rests)
                                  '()
                                  (list rests)))
                          ,@(if rest-info
                                (case (car rest-info)
                                  [(nodep) `(#:rest [,(list-ref rest-info 1) ,(contract-name (car (reverse (->i-arg-ctcs ctc))))])]
                                  [(dep) `(#:rest [,(list-ref rest-info 1) ,(list-ref rest-info 2) ...])])
                                '())
                          ,@(apply
                             append
                             (for/list ([pre-info pre-infos])
                               (if (cadr pre-info)
                                 `(#:pre/name ,@pre-info ...)
                                 `(#:pre ,(car pre-info) ...))))
                          ,(cond
                             [(not rng-info)
                              'any]
                             [else
                              (let ([infos (arg/ress->spec rng-info
                                                           (->i-rng-ctcs ctc)
                                                           (->i-rng-dep-ctcs ctc)
                                                           (λ (x) #f))])
                                (cond
                                  [(or (null? infos) (not (null? (cdr infos))))
                                   `(values ,@infos)]
                                  [else
                                   (car infos)]))])
                          ,@(apply
                             append
                             (for/list ([post-info post-infos])
                               (if (cadr post-info)
                                 `(#:post/name ,@post-info ...)
                                 `(#:post ,(car post-info) ...)))))))
         #:first-order
         (λ (ctc)
             (let ([has-rest? (->i-rest? ctc)]
                   [mtd? (->i-mtd? ctc)]
                   [mand-args (->i-mandatory-args ctc)]
                   [opt-args (->i-opt-args ctc)]
                   [mand-kwds (->i-mandatory-kwds ctc)]
                   [opt-kwds (->i-opt-kwds ctc)])
               (λ (val)
                 (if has-rest?
                     (check-procedure/more val mtd? mand-args mand-kwds opt-kwds #f)
                     (check-procedure val mtd? mand-args opt-args mand-kwds opt-kwds #f)))))
         #:stronger (λ (this that) (eq? this that)))) ;; WRONG

;; find-ordering : (listof arg) -> (values (listof arg) (listof number)) 
;; sorts the arguments according to the dependency order.
;; returns them in the reverse of that order, ie expressions that need
;; to be evaluted first come later in the list.
(define-for-syntax (find-ordering args)
  
  (define (comes-before? x y)
    (cond
      [(depends-on? (car x) (car y)) #t]
      [(depends-on? (car y) (car x)) #f]
      [else (< (cdr x) (cdr y))]))
  
  (define (depends-on? arg1 arg2)
    (and (arg/res-vars arg2)
         (ormap (λ (x) (free-identifier=? x (arg/res-var arg1)))
                (arg/res-vars arg2))))
  
  (let* ([numbered (for/list ([arg (in-list args)]
                              [i (in-naturals)])
                     (cons arg i))]
         [sorted
          (sort 
           numbered
           (λ (x y) (not (comes-before? x y))))])
    (values (map car sorted)
            (map cdr sorted))))

;; args/vars->arglist : (listof arg?) (vectorof identifier?) -> syntax
;; (vector-length vars) = (length args)
;; builds the parameter list for the wrapper λ
(define-for-syntax (args/vars->arglist an-istx vars this-param)
  (let ([args (istx-args an-istx)])
    #`(#,@(if this-param
              (list this-param)
              '())
       .
       #, 
       (let loop ([args args]
                  [i 0])
         (cond
           [(null? args) (if (istx-rst an-istx)
                             #'rest-args
                             #'())]
           [else
            (let* ([arg (car args)]
                   [kwd (arg-kwd arg)]
                   [opt? (arg-optional? arg)]
                   [arg-exp
                    (cond
                      [(and kwd opt?)
                       #`(#,kwd [#,(vector-ref vars i) the-unsupplied-arg])]
                      [kwd
                       #`(#,kwd #,(vector-ref vars i))]
                      [opt?
                       #`([#,(vector-ref vars i) the-unsupplied-arg])]
                      [else
                       #`(#,(vector-ref vars i))])])
              
              #`(#,@arg-exp
                 .
                 #,(loop (cdr args) (+ i 1))))])))))

(define-for-syntax (all-but-last lst)
  (reverse (cdr (reverse lst))))

;; vars : (listof identifier)
;;    vars will contain one identifier for each arg, plus one more for rst,
;;    unless rst is #f, in which case it just contains one identifier for each arg.
;;
;; FIXME: Currently, none of the resulting argument checkers attempt to preserve tail
;; recursion.  If all of the result contracts (which would need to be passed to
;; this function as well as results-checkers) can be evaluated early, then we can
;; preserve tail recursion in the fashion of -> etc.
(define-for-syntax (args/vars->arg-checker result-checkers args rst vars this-param)
  (let ([opts? (ormap arg-optional? args)]
        [this-params (if this-param (list this-param) '())])
    (cond
      [(and opts? (ormap arg-kwd args))
       (let* ([arg->var (make-hash)]
              [kwd-args (filter arg-kwd args)]
              [non-kwd-args (filter (λ (x) (not (arg-kwd x))) args)])
         
         (for ([arg (in-list args)]
               [var (in-vector vars)])
           (hash-set! arg->var arg var))
         
         (let ([sorted-kwd/arg-pairs 
                (sort
                 (map (λ (arg) (cons (arg-kwd arg) (hash-ref arg->var arg))) kwd-args)
                 (λ (x y) (keyword<? (syntax-e (car x)) (syntax-e (car y)))))])
           
           ;; has both optional and keyword args
           #`(keyword-return/no-unsupplied 
              #,(if (null? result-checkers) #f (car result-checkers)) 
              '#,(map car sorted-kwd/arg-pairs)
              (list #,@(map cdr sorted-kwd/arg-pairs))
              #,(if rst
                    #'rest-args
                    #''())
              #,@this-params
              #,@(map (λ (arg) (hash-ref arg->var arg)) non-kwd-args))))]
      [opts?
       ;; has optional args, but no keyword args
       #`(return/no-unsupplied #,(if (null? result-checkers) #f (car result-checkers))
                               #,(if rst
                                     #'rest-args
                                     #''())
                               #,@this-params
                               #,@(if rst
                                      (all-but-last (vector->list vars))
                                      (vector->list vars)))] 
      [else
       (let*-values ([(rev-regs rev-kwds)
                      (for/fold ([regs null]
                                 [kwds null])
                        ([arg (in-list args)]
                         [i (in-naturals)])
                        (if (arg-kwd arg)
                            (values regs (cons (vector-ref vars i) kwds))
                            (values (cons (vector-ref vars i) regs) kwds)))]
                     [(regular-arguments keyword-arguments)
                      (values (reverse rev-regs) (reverse rev-kwds))])
         (cond
           [(and (null? keyword-arguments) rst)
            #`(apply values #,@result-checkers #,@this-params #,@regular-arguments rest-args)]
           [(null? keyword-arguments)
            #`(values #,@result-checkers #,@this-params #,@regular-arguments)]
           [rst
            #`(apply values #,@result-checkers (list #,@keyword-arguments) #,@this-params #,@regular-arguments rest-args)]
           [else
            #`(values #,@result-checkers (list #,@keyword-arguments) #,@this-params #,@regular-arguments)]))])))

(define (return/no-unsupplied res-checker rest-args . args)
  (if res-checker
      (apply values res-checker 
             (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))
      (apply values (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))))

(define (keyword-return/no-unsupplied res-checker kwds kwd-args rest-args . args)
  (let-values ([(supplied-kwds supplied-kwd-args)
                (let loop ([kwds kwds]
                           [kwd-args kwd-args])
                  (cond
                    [(null? kwds) (values '() '())]
                    [else 
                     (let-values ([(kwds-rec args-rec) (loop (cdr kwds) (cdr kwd-args))])
                       (cond
                         [(eq? (car kwd-args) the-unsupplied-arg)
                          (values kwds-rec args-rec)]
                         [else
                          (values (cons (car kwds) kwds-rec)
                                  (cons (car kwd-args) args-rec))]))]))])
    (cond
      [(and res-checker (null? supplied-kwd-args))
       (apply values res-checker
              (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))]
      [(null? supplied-kwd-args)
       (apply values (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))]
      [res-checker
       (apply values res-checker supplied-kwd-args 
              (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))]
      [else
       (apply values supplied-kwd-args 
              (append (filter (λ (x) (not (eq? x the-unsupplied-arg))) args) rest-args))])))

(define-for-syntax (maybe-generate-temporary x)
  (and x (car (generate-temporaries (list x)))))

(define (signal-pre/post pre? val str blame . var-infos)
  (define pre-str (or str 
                      (string-append 
                       (if pre? "#:pre" "#:post")
                       " condition violation"
                       (if (null? var-infos)
                           ""
                           "; variables are:"))))
  (raise-blame-error blame val
                     (apply
                      string-append
                      pre-str
                      (for/list ([var-info (in-list var-infos)])
                        (format "\n      ~s: ~e" 
                                (list-ref var-info 0)
                                (list-ref var-info 1))))))

(define-for-syntax (add-pre-cond an-istx arg/res-to-indy-var call-stx)
  #`(begin #,@(for/list ([pre (in-list (istx-pre an-istx))]
                         [i (in-naturals)])
                (define id (string->symbol (format "pre-proc~a" i)))
                #`(unless (#,id #,@(map arg/res-to-indy-var (pre/post-vars pre)))
                    (signal-pre/post #t
                                     val
                                     #,(pre/post-str pre)
                                     swapped-blame
                                     #,@(map (λ (x) #`(list '#,x #,(arg/res-to-indy-var x)))
                                             (pre/post-vars pre)))))
           #,call-stx))

(define-for-syntax (add-post-cond an-istx arg/res-to-indy-var call-stx)
  #`(begin #,@(for/list ([post (in-list (istx-post an-istx))]
                         [i (in-naturals)])
                (define id (string->symbol (format "post-proc~a" i)))
                #`(unless (#,id #,@(map arg/res-to-indy-var (pre/post-vars post)))
                    (signal-pre/post #f
                                     val
                                     #,(pre/post-str post)
                                     blame
                                     #,@(map (λ (x) #`(list '#,x #,(arg/res-to-indy-var x)))
                                             (pre/post-vars post)))))
           #,call-stx))

;; add-wrapper-let : syntax
;;                   (listof arg/res) -- sorted version of the arg/res structs, ordered by evaluation order
;;                   (listof int) -- indicies that give the mapping from the ordered-args to the original order
;;                   (listof identifier) -- arg-proj-vars, bound to projections with ordinary blame
;;                   (listof identifier) -- indy-arg-proj-args, bound to projections with indy blame
;;                   (listof identifier) -- wrapper-args, bound to the original, unwrapped values, sorted like original arg/ress
;;                                       -- the generated lets rebind these variables to their projected counterparts, with normal blame
;;                   (listof identifier) -- indy-arg-vars, bound to wrapped values with indy blame, sorted like the second input
;;                   (identifier arg -> identifier) -- maps the original var in the arg to the corresponding indy-var
;; adds nested lets that bind the wrapper-args and the indy-arg-vars to projected values, with 'body' in the body of the let
;; also handles adding code to checki to see if usupplied args are present (skipping the contract check, if so)
;; WRONG: need to rename the variables in this function from "arg" to "arg/res"
(define-for-syntax (add-wrapper-let body swapped-blame?
                                    ordered-args arg-indicies
                                    arg-proj-vars indy-arg-proj-vars 
                                    wrapper-args indy-arg-vars
                                    arg/res-to-indy-var)
  
  (define (add-unsupplied-check arg wrapper-arg stx)
    (if (and (arg? arg)
             (arg-optional? arg))
        #`(if (eq? #,wrapper-arg the-unsupplied-arg)
              #,wrapper-arg
              #,stx)
        stx))
  
  (for/fold ([body body])
    ([indy-arg-var (in-list indy-arg-vars)]
     [arg (in-list ordered-args)]
     [arg-index arg-indicies]
     [i (in-naturals)])
    (let ([wrapper-arg (vector-ref wrapper-args arg-index)]
          [arg-proj-var (vector-ref arg-proj-vars arg-index)]
          [indy-arg-proj-var (vector-ref indy-arg-proj-vars arg-index)])
      
      
      (let ([indy-binding
             ;; if indy-arg-proj-var is #f, that means that we don't need that binding here, so skip it
             (if indy-arg-proj-var
                 (list 
                  #`[#,indy-arg-var 
                     #,(add-unsupplied-check
                        arg
                        wrapper-arg
                        (if (arg/res-vars arg)
                            #`(#,arg-proj-var #,@(map arg/res-to-indy-var (arg/res-vars arg))
                                              #,wrapper-arg 
                                              #,(if swapped-blame?
                                                    #'indy-dom-blame
                                                    #'indy-rng-blame))
                            #`(#,indy-arg-proj-var #,wrapper-arg)))])
                 (list))])
        #`(let (#,@indy-binding
                [#,wrapper-arg 
                 #,(add-unsupplied-check
                    arg
                    wrapper-arg
                    (cond
                      [(and (eres? arg) (arg/res-vars arg))
                       #`(un-dep #,(eres-eid arg) 
                                 #,wrapper-arg
                                 #,(if swapped-blame?
                                       #'swapped-blame
                                       #'blame))]
                      [(arg/res-vars arg)
                       #`(#,arg-proj-var #,@(map arg/res-to-indy-var (arg/res-vars arg))
                                         #,wrapper-arg 
                                         #,(if swapped-blame?
                                               #'swapped-blame
                                               #'blame))]
                      [else
                       #`(#,arg-proj-var #,wrapper-arg)]))])
            #,body)))))

;; Returns an empty list if no result contracts and a list of a single syntax value
;; which should be a function from results to projection-applied versions of the same
;; if there are result contracts.
(define-for-syntax (result-checkers an-istx
                                    ordered-ress res-indicies
                                    res-proj-vars indy-res-proj-vars
                                    wrapper-ress indy-res-vars
                                    arg/res-to-indy-var)
  (cond
    [(istx-ress an-istx)
     (list
      #`(λ #,(vector->list wrapper-ress)
          #,(add-wrapper-let 
             (add-post-cond an-istx arg/res-to-indy-var #`(values #,@(vector->list wrapper-ress)))
             #f
             ordered-ress res-indicies
             res-proj-vars indy-res-proj-vars 
             wrapper-ress indy-res-vars
             arg/res-to-indy-var)))]
    [else
     null]))

(define-for-syntax (add-eres-lets an-istx res-proj-vars arg/res-to-indy-var stx)
  (cond
    [(and (istx-ress an-istx)
          (andmap eres? (istx-ress an-istx)))
     (for/fold ([body stx])
       ([an-arg/res (in-list (reverse (istx-ress an-istx)))]
        [res-proj-var (in-vector res-proj-vars (- (vector-length res-proj-vars) 1) -1 -1)])
       (if (arg/res-vars an-arg/res)
           #`(let ([#,(eres-eid an-arg/res) (#,res-proj-var #,@(map arg/res-to-indy-var (arg/res-vars an-arg/res)))])
               #,body)
           body))]
    [else stx]))
  
(define-for-syntax (mk-wrapper-func an-istx used-indy-vars)
  (let ([args+rst (append (istx-args an-istx)
                          (if (istx-rst an-istx)
                              (list (istx-rst an-istx))
                              '()))])
    (let-values ([(ordered-args arg-indicies) (find-ordering args+rst)]
                 [(ordered-ress res-indicies) (if (istx-ress an-istx)
                                                  (find-ordering (istx-ress an-istx))
                                                  (values '() '()))])
      
      (let ([wrapper-args (list->vector 
                           (append (generate-temporaries (map arg/res-var (istx-args an-istx)))
                                   (if (istx-rst an-istx)
                                       (list #'rest-args)
                                       '())))]
            [indy-arg-vars (generate-temporaries (map arg/res-var ordered-args))]
            [arg-proj-vars (list->vector (generate-temporaries (map arg/res-var args+rst)))]
            
            ;; this list is parallel to arg-proj-vars (so use arg-indicies to find the right ones)
            ;; but it contains #fs in places where we don't need the indy projections (because the corresponding
            ;; argument is not dependened on by anything)
            [indy-arg-proj-vars (list->vector (map (λ (x) (maybe-generate-temporary
                                                           (and (free-identifier-mapping-get used-indy-vars 
                                                                                             (arg/res-var x)
                                                                                             (λ () #f))
                                                                (arg/res-var x)))) 
                                                   args+rst))]
            
            
            [wrapper-ress (list->vector (generate-temporaries (map arg/res-var (or (istx-ress an-istx) '()))))]
            [indy-res-vars (generate-temporaries (map arg/res-var ordered-ress))]
            [res-proj-vars (list->vector (generate-temporaries (map arg/res-var (or (istx-ress an-istx) '()))))]
            
            ;; this list is parallel to res-proj-vars (so use res-indicies to find the right ones)
            ;; but it contains #fs in places where we don't need the indy projections (because the corresponding
            ;; result is not dependened on by anything)
            [indy-res-proj-vars (list->vector (map (λ (x) (maybe-generate-temporary
                                                           (and (free-identifier-mapping-get used-indy-vars 
                                                                                             (arg/res-var x)
                                                                                             (λ () #f))
                                                                (arg/res-var x))))
                                                   (or (istx-ress an-istx) '())))])
        
        (define (arg/res-to-indy-var var)
          (let loop ([iargs (append indy-arg-vars indy-res-vars)]
                     [args (append (map arg/res-var ordered-args) (map arg/res-var ordered-ress))])
            (cond
              [(null? args)
               (error '->i "internal error; did not find a matching var for ~s" var)]
              [else
               (let ([arg (car args)]
                     [iarg (car iargs)])
                 (cond
                   [(free-identifier=? var arg) iarg]
                   [else (loop (cdr iargs) (cdr args))]))])))
        
        (define this-param (and (syntax-parameter-value #'making-a-method)
                                (car (generate-temporaries '(this)))))
        
        #`(λ (blame swapped-blame indy-dom-blame indy-rng-blame chk ctc
                    
                    ;; the pre- and post-condition procs
                    #,@(for/list ([pres (istx-pre an-istx)]
                                  [i (in-naturals)])
                         (string->symbol (format "pre-proc~a" i)))
                    #,@(for/list ([pres (istx-post an-istx)]
                                  [i (in-naturals)])
                         (string->symbol (format "post-proc~a" i)))
                    
                    ;; first the non-dependent arg projections
                    #,@(filter values (map (λ (arg/res arg-proj-var) (and (not (arg/res-vars arg/res)) arg-proj-var))
                                           args+rst
                                           (vector->list arg-proj-vars)))
                    ;; then the dependent arg projections
                    #,@(filter values (map (λ (arg/res arg-proj-var) (and (arg/res-vars arg/res) arg-proj-var))
                                           args+rst
                                           (vector->list arg-proj-vars)))
                    ;; then the non-dependent indy arg projections
                    #,@(filter values (map (λ (arg/res arg-proj-var) (and (not (arg/res-vars arg/res)) arg-proj-var))
                                           args+rst
                                           (vector->list indy-arg-proj-vars)))
                    
                    
                    ;; then the non-dependent res projections
                    #,@(filter values (map (λ (arg/res res-proj-var) (and (not (arg/res-vars arg/res)) res-proj-var))
                                           (or (istx-ress an-istx) '())
                                           (vector->list res-proj-vars)))
                    ;; then the dependent res projections
                    #,@(filter values (map (λ (arg/res res-proj-var) (and (arg/res-vars arg/res) res-proj-var))
                                           (or (istx-ress an-istx) '())
                                           (vector->list res-proj-vars)))
                    ;; then the non-dependent indy res projections
                    #,@(filter values (map (λ (arg/res res-proj-var) (and (not (arg/res-vars arg/res)) res-proj-var))
                                           (or (istx-ress an-istx) '())
                                           (vector->list indy-res-proj-vars))))

            (λ (val)
              (chk val #,(and (syntax-parameter-value #'making-a-method) #t))
              (let ([arg-checker
                     (λ #,(args/vars->arglist an-istx wrapper-args this-param)
                       #,(add-wrapper-let 
                          (add-pre-cond 
                           an-istx 
                           arg/res-to-indy-var 
                           (add-eres-lets
                            an-istx
                            res-proj-vars
                            arg/res-to-indy-var
                            (args/vars->arg-checker
                             (result-checkers
                              an-istx
                              ordered-ress res-indicies
                              res-proj-vars indy-res-proj-vars
                              wrapper-ress indy-res-vars
                              arg/res-to-indy-var)
                             (istx-args an-istx)
                             (istx-rst an-istx)
                             wrapper-args
                             this-param)))
                          #t
                          ordered-args arg-indicies
                          arg-proj-vars indy-arg-proj-vars 
                          wrapper-args indy-arg-vars
                          arg/res-to-indy-var))])
                (impersonate-procedure
                 val
                 (make-keyword-procedure
                  (λ (kwds kwd-args . args)
                    (keyword-apply arg-checker kwds kwd-args args))
                  (λ args (apply arg-checker args)))
                 impersonator-prop:contracted ctc))))))))

(define (un-dep ctc obj blame)
  (let ([ctc (coerce-contract '->i ctc)])
    (((contract-projection ctc) blame) obj)))

(define-for-syntax (mk-used-indy-vars an-istx)
  (let ([vars (make-free-identifier-mapping)])
    
    ;; add in regular arguments' uses
    (for ([an-arg (in-list (istx-args an-istx))])
      (when (arg/res-vars an-arg)
        (for ([var (in-list (arg/res-vars an-arg))])
          (free-identifier-mapping-put! vars var #t))))
    
    ;; add in rest argument uses
    (when (istx-rst an-istx)
      (let ([an-arg/rst (istx-rst an-istx)])
        (when (arg/res-vars an-arg/rst)
          (for ([var (in-list (arg/res-vars an-arg/rst))])
            (free-identifier-mapping-put! vars var #t)))))
    
    ;; pre-condition
    (for ([pre (in-list (istx-pre an-istx))])
      (for ([var (in-list (pre/post-vars pre))])
        (free-identifier-mapping-put! vars var #t)))
    
    ;; results
    (when (istx-ress an-istx)
      (for ([a-res (in-list (istx-ress an-istx))])
        (when (arg/res-vars a-res)
          (for ([var (in-list (arg/res-vars a-res))])
            (free-identifier-mapping-put! vars var #t)))))
    
    ;; post-condition
    (for ([post (in-list (istx-post an-istx))])
      (for ([var (in-list (pre/post-vars post))])
        (free-identifier-mapping-put! vars var #t)))
    
    vars))

(define-syntax (->i/m stx)
  (let* ([an-istx (parse-->i stx)]
         [used-indy-vars (mk-used-indy-vars an-istx)]
         [wrapper-func (mk-wrapper-func an-istx used-indy-vars)]
         [args+rst (append (istx-args an-istx)
                           (if (istx-rst an-istx)
                               (list (istx-rst an-istx))
                               '()))]
         [this->i (gensym 'this->i)])
    (with-syntax ([(arg-exp-xs ...) 
                   (generate-temporaries (filter values (map (λ (arg) (and (not (arg/res-vars arg)) (arg/res-var arg)))
                                                             args+rst)))]
                  [(arg-exps ...)
                   (filter values (map (λ (arg) (and (not (arg/res-vars arg)) 
                                                     (syntax-property
                                                      (syntax-property
                                                       (arg/res-ctc arg)
                                                       'racket/contract:negative-position
                                                       this->i)
                                                      'racket/contract:contract-on-boundary 
                                                      (gensym '->i-indy-boundary))))
                                       args+rst))]
                  
                  [(res-exp-xs ...) 
                   (if (istx-ress an-istx)
                       (generate-temporaries (filter values (map (λ (res) (and (not (arg/res-vars res)) (arg/res-var res)))
                                                                 (istx-ress an-istx))))
                       '())]
                  [(res-exps ...)
                   (if (istx-ress an-istx)
                       (filter values (map (λ (res) (and (not (arg/res-vars res)) 
                                                         (syntax-property
                                                          (syntax-property 
                                                           (arg/res-ctc res)
                                                           'racket/contract:positive-position
                                                           this->i)
                                                          'racket/contract:contract-on-boundary 
                                                          (gensym '->i-indy-boundary))))
                                           (istx-ress an-istx)))
                       '())])
      
      #`(let ([arg-exp-xs arg-exps] ...
              [res-exp-xs res-exps] ...)
          #,(syntax-property
             #`(->i 
                ;; all of the non-dependent argument contracts
                (list arg-exp-xs ...)
                ;; all of the dependent argument contracts
                (list #,@(filter values (map (λ (arg) 
                                               (and (arg/res-vars arg)
                                                    #`(λ (#,@(arg/res-vars arg) val blame)
                                                        ;; this used to use opt/direct, but opt/direct duplicates code (bad!)
                                                        (un-dep #,(syntax-property
                                                                   (syntax-property 
                                                                    (arg/res-ctc arg)
                                                                    'racket/contract:negative-position 
                                                                    this->i)
                                                                   'racket/contract:contract-on-boundary 
                                                                   (gensym '->i-indy-boundary)) 
                                                                val blame))))
                                             args+rst)))
                ;; then the non-dependent argument contracts that are themselves dependend on
                (list #,@(filter values
                                 (map (λ (arg/res indy-id) 
                                        (and (free-identifier-mapping-get used-indy-vars (arg/res-var arg/res) (λ () #f))
                                             indy-id))
                                      (filter (λ (arg/res) (not (arg/res-vars arg/res))) args+rst)
                                      (syntax->list #'(arg-exp-xs ...)))))
                
                
                #,(if (istx-ress an-istx)
                      #`(list res-exp-xs ...)
                      #''())
                #,(if (istx-ress an-istx) 
                      #`(list #,@(filter values (map (λ (arg) 
                                                       (and (arg/res-vars arg)
                                                            (if (eres? arg)
                                                                #`(λ #,(arg/res-vars arg)
                                                                    (opt/c #,(syntax-property
                                                                              (syntax-property 
                                                                               (arg/res-ctc arg)
                                                                               'racket/contract:positive-position
                                                                               this->i)
                                                                              'racket/contract:contract-on-boundary 
                                                                              (gensym '->i-indy-boundary))))
                                                                #`(λ (#,@(arg/res-vars arg) val blame)
                                                                    ;; this used to use opt/direct, but opt/direct duplicates code (bad!)
                                                                    (un-dep #,(syntax-property
                                                                               (syntax-property 
                                                                                (arg/res-ctc arg)
                                                                                'racket/contract:positive-position
                                                                                this->i)
                                                                               'racket/contract:contract-on-boundary 
                                                                               (gensym '->i-indy-boundary))
                                                                            val blame)))))
                                                     (istx-ress an-istx))))
                      #''())
                #,(if (istx-ress an-istx)
                      #`(list #,@(filter values
                                         (map (λ (arg/res indy-id) 
                                                (and (free-identifier-mapping-get used-indy-vars (arg/res-var arg/res) (λ () #f))
                                                     indy-id))
                                              (filter (λ (arg/res) (not (arg/res-vars arg/res))) (istx-ress an-istx))
                                              (syntax->list #'(res-exp-xs ...)))))
                      #''())
                      
                #,(let ([func (λ (pre/post) #`(λ #,(pre/post-vars pre/post) #,(pre/post-exp pre/post)))])
                    #`(list #,@(for/list ([pre (in-list (istx-pre an-istx))])
                                 (func pre))
                            #,@(for/list ([post (in-list (istx-post an-istx))])
                                 (func post))))
                
                #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg)) (not (arg-optional? arg))))
                                              (istx-args an-istx))))
                #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg)) (arg-optional? arg)))
                                              (istx-args an-istx))))
                '#,(sort (filter values (map (λ (arg) (and (not (arg-optional? arg)) (arg-kwd arg) (syntax-e (arg-kwd arg))))
                                             (istx-args an-istx))) 
                         keyword<?)
                '#,(sort (filter values (map (λ (arg) (and (arg-optional? arg) (arg-kwd arg) (syntax-e (arg-kwd arg))))
                                             (istx-args an-istx))) 
                         keyword<?)
                #,(and (istx-rst an-istx) #t)
                #,(and (syntax-parameter-value #'making-a-method) #t)
                (quote-module-name)
                #,wrapper-func
                '#(#,(for/list ([an-arg (in-list (istx-args an-istx))])
                       `(,(if (arg/res-vars an-arg) 'dep 'nodep)
                         ,(syntax-e (arg/res-var an-arg)) 
                         ,(if (arg/res-vars an-arg)
                              (map syntax-e (arg/res-vars an-arg))
                              '())
                         ,(and (arg-kwd an-arg)
                               (syntax-e (arg-kwd an-arg)))
                         ,(arg-optional? an-arg)))
                   #,(if (istx-rst an-istx)
                         (if (arg/res-vars (istx-rst an-istx))
                             `(dep ,(syntax-e (arg/res-var (istx-rst an-istx)))
                                   ,(map syntax-e (arg/res-vars (istx-rst an-istx))))
                             `(nodep ,(syntax-e (arg/res-var (istx-rst an-istx)))))
                         #f)
                   #,(for/list ([pre (in-list (istx-pre an-istx))])
                       (list (map syntax-e (pre/post-vars pre))
                             (pre/post-str pre)))
                   #,(and (istx-ress an-istx)
                          (for/list ([a-res (in-list (istx-ress an-istx))])
                            `(,(if (arg/res-vars a-res) 'dep 'nodep)
                              ,(if (eres? a-res)
                                   '_
                                   (syntax-e (arg/res-var a-res)))
                              ,(if (arg/res-vars a-res)
                                   (map syntax-e (arg/res-vars a-res))
                                   '())
                              #f
                              #f)))
                   #,(for/list ([post (in-list (istx-post an-istx))])
                       (list (map syntax-e (pre/post-vars post))
                             (pre/post-str post)))))
             'racket/contract:contract 
             (let ()
               (define (find-kwd kwd)
                 (for/or ([x (in-list (syntax->list stx))])
                   (and (eq? (syntax-e x) kwd)
                        x)))
               (define pre (find-kwd '#:pre))
               (define post (find-kwd '#:post))
               (define orig (list (car (syntax-e stx))))
               (vector this->i 
                       ;; the ->i in the original input to this guy
                       (if post (cons post orig) orig)
                       (if pre (list pre) '()))))))))
