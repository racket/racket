#lang racket/base

(require "arrow.rkt"
         "prop.rkt"
         "guts.rkt"
         unstable/location
         (for-syntax racket/base
                     racket/stxparam-exptime
                     "arr-i-parse.rkt"))

(provide (rename-out [->i/m ->i]))

;; arg-ctcs     : (listof contract)
;; arg-dep-ctcs : (-> ??? (listof contract))
;; rng-ctcs     : (listof contract)
;; rng-dep-ctcs : (-> ??? (listof contract))
;; mandatory-args, opt-args : number
;; mandatory-kwds, opt-kwds : (listof keyword?) sorted by keyword<?
;; rest? : boolean
;; mk-wrapper : creates the a wrapper function that implements the contract checking
(struct ->i (arg-ctcs indy-arg-ctcs arg-dep-ctcs rng-ctcs indy-rng-ctcs rng-dep-ctcs mandatory-args opt-args mandatory-kwds opt-kwds rest? mk-wrapper)
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc) 
     (let ([arg-ctc-projs (map contract-projection (->i-arg-ctcs ctc))]
           [indy-arg-ctc-projs (map contract-projection (->i-indy-arg-ctcs ctc))]
           [rng-ctc-projs (map contract-projection (->i-rng-ctcs ctc))]
           [indy-rng-ctc-projs (map contract-projection (->i-indy-rng-ctcs ctc))]
           [func (->i-mk-wrapper ctc)]
           [has-rest? (->i-rest? ctc)])
       (λ (blame)
         (let* ([swapped-blame (blame-swap blame)]
                [here (quote-module-path)]
                [indy-dom-blame (blame-replace-negative swapped-blame here)]
                [indy-rng-blame (blame-replace-negative blame here)])
           (let ([partial-doms (map (λ (dom) (dom swapped-blame)) arg-ctc-projs)]
                 [partial-indy-doms (map (λ (dom) (dom indy-dom-blame)) indy-arg-ctc-projs)]
                 [partial-rngs (map (λ (rng) (rng blame)) rng-ctc-projs)]
                 [partial-indy-rngs (map (λ (rng) (rng indy-rng-blame)) rng-ctc-projs)])
             (printf "partial-doms ~s partial-indy-doms ~s\n" partial-doms partial-indy-doms)
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
                    (append partial-doms
                            partial-indy-doms
                            (->i-arg-dep-ctcs ctc)
                            partial-rngs
                            partial-indy-rngs
                            (->i-rng-dep-ctcs ctc))))))))
   #:name (λ (ctc) '->i)
   #:first-order (λ (ctc) (λ (x) #f))
   #:stronger (λ (this that) #f)))

;; find-ordering : (listof arg) -> (listof (cons number arg))
(define-for-syntax (find-ordering args)
  (values (reverse args)
          (reverse
           (for/list ([arg (in-list args)]
                      [i (in-naturals)])
             i))))

;; args/vars->arglist : (listof arg?) (vectorof identifier?) -> syntax
;; (vector-length vars) = (length args)
;; builds the parameter list for the wrapper λ
(define-for-syntax (args/vars->arglist args vars)
  (let loop ([args args]
             [i 0])
    (cond
      [(null? args) #'()]
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
            #,(loop (cdr args) (+ i 1))) )])))

(define-for-syntax (args/vars->callsite fn args vars)
  (let ([opts? (ormap arg-optional? args)]) 
    (cond
      [(and opts? (ormap arg-kwd args))
       (let ([arg->var (make-hash)]
             [kwd-args (filter arg-kwd args)]
             [non-kwd-args (filter (λ (x) (not (arg-kwd x))) args)])
         (for ([arg (in-list args)]
               [var (in-vector vars)])
           (hash-set! arg->var arg var))
         ;; has both optional and keyword args
         #`(keyword-apply/no-unsupplied 
            #,fn 
            '#,(map arg-kwd kwd-args)
            (list #,@(map (λ (arg) (hash-ref arg->var arg)) kwd-args))
            #,@(map (λ (arg) (hash-ref arg->var arg)) non-kwd-args)))]
      [opts?
       ;; has optional args, but no keyword args
       #`(apply/no-unsupplied #,fn #,@(vector->list vars))] 
      [else
       ;; no optional args
       #`(#,fn
          #,@(let loop ([args args]
                        [i 0])
               (cond
                 [(null? args) #'()]
                 [else
                  (let ([arg (car args)])
                    #`(#,@(if (arg-kwd arg)
                              #`(#,(arg-kwd arg) #,(vector-ref vars i))
                              #`(#,(vector-ref vars i)))
                       .
                       #,(loop (cdr args) (+ i 1))))])))])))

(define (apply/no-unsupplied fn . args)
  (apply fn (filter (λ (x) (not (eq? x the-unsupplied-arg))) args)))

(define (keyword-apply/no-unsupplied fn kwds kwd-args . args)
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
    (keyword-apply fn kwds kwd-args (filter (λ (x) (not (eq? x the-unsupplied-arg))) args))))

(define-for-syntax (mk-wrapper-func an-istx)
  (let-values ([(ordered-args arg-indicies) (find-ordering (istx-args an-istx))])
    
    (let ([wrapper-args (list->vector (generate-temporaries (map arg-var (istx-args an-istx))))]
          [indy-args (generate-temporaries (map arg-var ordered-args))]
          [arg-proj-vars (list->vector (generate-temporaries (map arg-var (istx-args an-istx))))]
          [indy-arg-proj-vars (list->vector (generate-temporaries (map arg-var (istx-args an-istx))))])
      
      (printf "arg-proj-vars ~s indy-arg-proj-vars ~s\n" arg-proj-vars indy-arg-proj-vars)
      
      (define (arg-to-indy-var var)
        (let loop ([iargs indy-args]
                   [args (map arg-var ordered-args)])
          (cond
            [(null? args)
             (error '->i "internal error; did not find a matching var for ~s" var)]
            [else
             (let ([arg (car args)]
                   [iarg (car iargs)])
               (cond
                 [(free-identifier=? var arg) iarg]
                 [else (loop (cdr iargs) (cdr args))]))])))
      #`(λ (blame swapped-blame indy-dom-blame indy-rng-blame chk ctc #,@(vector->list arg-proj-vars) #,@(vector->list indy-arg-proj-vars))
          (λ (val)
            (chk val #,(and (syntax-parameter-value #'making-a-method) #t))
            (make-contracted-function
             (λ #,(args/vars->arglist (istx-args an-istx) wrapper-args)
               #,(for/fold ([body (args/vars->callsite #'val (istx-args an-istx) wrapper-args)])
                   ([indy-arg (in-list indy-args)]
                    [arg (in-list ordered-args)]
                    [arg-index arg-indicies])
                   (let ([wrapper-arg (vector-ref wrapper-args arg-index)]
                         [arg-proj-var (vector-ref arg-proj-vars arg-index)]
                         [indy-arg-proj-var (vector-ref indy-arg-proj-vars arg-index)])
                     (define (add-unsupplied-check stx)
                       (if (arg-optional? arg)
                           #`(if (eq? #,wrapper-arg the-unsupplied-arg)
                                 #,wrapper-arg
                                 #,stx)
                           stx))
                     #`(let (
                             ;; WRONG! can avoid creating this thing if it isn't used elsewhere.
                             [#,indy-arg 
                              #,(add-unsupplied-check
                                 (if (arg-vars arg)
                                     #`(un-dep (#,arg-proj-var #,@(map arg-to-indy-var (arg-vars arg))) #,wrapper-arg indy-dom-blame)
                                     ;; WRONG! (need to pass in the indy'ized projections somewhere)
                                     #`(#,indy-arg-proj-var #,wrapper-arg)))]
                             [#,wrapper-arg 
                              #,(add-unsupplied-check
                                 (if (arg-vars arg)
                                     #`(un-dep (#,arg-proj-var #,@(map arg-to-indy-var (arg-vars arg))) #,wrapper-arg swapped-blame)
                                     #`(#,arg-proj-var #,wrapper-arg)))])
                         #,body))))
             ctc))))))

(define (un-dep ctc obj blame)
  (printf "un-dep blame ~s\n" blame)
  ;; WRONG (well, just need to avoid calling coerce-contract if 'ctc' is something simple)
  (let ([ctc (coerce-contract '->i ctc)])
    (((contract-projection ctc) blame) obj)))

(define-syntax (->i/m stx)
  (let* ([an-istx (parse-->i stx)]
         [wrapper-func (mk-wrapper-func an-istx)])
    #`(->i (list #,@(filter values (map (λ (arg) (and (not (arg-vars arg)) (arg-ctc arg)))
                                        (istx-args an-istx))))
           ;; WRONG! this needs to be a subset of the previous list (and to generate a let to share appropriately)
           (list #,@(filter values (map (λ (arg) (and (not (arg-vars arg)) (arg-ctc arg)))
                                        (istx-args an-istx))))
           (list #,@(filter values (map (λ (arg) (and (arg-vars arg) #`(λ #,(arg-vars arg) #,(arg-ctc arg))))
                                        (istx-args an-istx))))
           
           #,(if (istx-ress an-istx)
                 #`(list #,@(filter values (map (λ (arg) (and (not (res-vars arg)) (res-ctc arg)))
                                                (istx-ress an-istx))))
                 #''())
           ;; WRONG! this needs to be a subset of the previuos (and to generate a let to share appropriately)
           #,(if (istx-ress an-istx)
                 #`(list #,@(filter values (map (λ (arg) (and (not (res-vars arg)) (res-ctc arg)))
                                                (istx-ress an-istx))))
                 #''())
           #,(if (istx-ress an-istx) 
                 #`(list #,@(filter values (map (λ (arg) (and (res-vars arg) #`(λ #,(res-vars arg) #,(res-ctc arg))))
                                                (istx-ress an-istx))))
                 #''())
           
           #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg)) (not (arg-optional? arg))))
                                         (istx-args an-istx))))
           #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg)) (arg-optional? arg)))
                                         (istx-args an-istx))))
           '#,(sort (filter values (map (λ (arg) (and (not (arg-optional? arg)) (arg-kwd arg)))
                                        (istx-args an-istx))) 
                    keyword<?)
           '#,(sort (filter values (map (λ (arg) (and (arg-optional? arg) (arg-kwd arg)))
                                        (istx-args an-istx))) 
                    keyword<?)
           #,(and (istx-rst an-istx) #t)
           #,wrapper-func)))
