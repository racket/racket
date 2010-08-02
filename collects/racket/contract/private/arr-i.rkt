#lang racket/base

(require "arrow.rkt"
         "prop.rkt"
         "guts.rkt"
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
(struct ->i (arg-ctcs arg-dep-ctcs rng-ctcs rng-dep-ctcs mandatory-args opt-args mandatory-kwds opt-kwds rest? mk-wrapper)
  #:property prop:contract
  (build-contract-property
   #:projection
   (λ (ctc) 
     (let ([arg-ctc-projs (map contract-projection (->i-arg-ctcs ctc))]
           [rng-ctc-projs (map contract-projection (->i-rng-ctcs ctc))]
           [func (->i-mk-wrapper ctc)]
           [has-rest? (->i-rest? ctc)])
       (λ (blame)
         (let ([swapped-blame (blame-swap blame)]
               [indy-blame blame]) ;; WRONG!
           (let ([partial-doms (map (λ (dom) (dom swapped-blame)) arg-ctc-projs)]
                 [partial-rngs (map (λ (rng) (rng blame)) rng-ctc-projs)])
             (apply func
                    blame
                    swapped-blame
                    indy-blame
                    (λ (val mtd?)
                      ' ;; WRONG!
                      (if has-rest?
                          (check-procedure/more val mtd? dom-length mandatory-keywords optional-keywords blame)
                          (check-procedure val mtd? dom-length optionals-length mandatory-keywords optional-keywords blame)))
                    ctc
                    (append partial-doms
                            (->i-arg-dep-ctcs ctc)
                            partial-rngs
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

(define-for-syntax (mk-wrapper-func an-istx)
  (let-values ([(ordered-args arg-indicies) (find-ordering (istx-args an-istx))])
    
    (let ([wrapper-args (list->vector (generate-temporaries (map arg-var (istx-args an-istx))))]
          [indy-args (generate-temporaries (map arg-var ordered-args))]
          [arg-proj-vars (list->vector (generate-temporaries (map arg-var (istx-args an-istx))))])
      
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
      
      #`(λ (blame swapped-blame indy-blame chk ctc #,@(vector->list arg-proj-vars))
          (λ (val)
            (chk val #,(and (syntax-parameter-value #'making-a-method) #t))
            (make-contracted-function
             (λ #,(vector->list wrapper-args)
               #,(for/fold ([body #`(val #,@(vector->list wrapper-args))])
                   ([indy-arg (in-list indy-args)]
                    [arg (in-list ordered-args)]
                    [arg-index arg-indicies])
                   (let ([wrapper-arg (vector-ref wrapper-args arg-index)]
                         [arg-proj-var (vector-ref arg-proj-vars arg-index)])
                     #`(let ([#,indy-arg #,(if (arg-vars arg)
                                               #`(un-dep (#,arg-proj-var #,@(map arg-to-indy-var (arg-vars arg))) #,wrapper-arg indy-blame)
                                               ;; WRONG! (need to pass in the indy'ized projections somewhere)
                                               #`(#,arg-proj-var #,wrapper-arg))]
                             [#,wrapper-arg 
                              #,(if (arg-vars arg)
                                    #`(un-dep (#,arg-proj-var #,@(map arg-to-indy-var (arg-vars arg))) #,wrapper-arg swapped-blame)
                                    #`(#,arg-proj-var #,wrapper-arg))])
                         #,body))))
             ctc))))))

(define (un-dep ctc obj blame)
  ;; WRONG (well, just need to avoid calling coerce-contract if 'ctc' is something simple)
  (let ([ctc (coerce-contract '->i ctc)])
    (((contract-projection ctc) blame) obj)))

(define-syntax (->i/m stx)
  (let* ([an-istx (parse-->i stx)]
         [wrapper-func (mk-wrapper-func an-istx)])
    #`(->i (list #,@(filter values (map (λ (arg) (and (not (arg-vars arg)) (arg-ctc arg)))
                                        (istx-args an-istx))))
           (list #,@(filter values (map (λ (arg) (and (arg-vars arg) #`(λ #,(arg-vars arg) #,(arg-ctc arg))))
                                        (istx-args an-istx))))
           
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
