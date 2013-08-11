#lang racket/base

(require "arrow.rkt"
         "prop.rkt"
         "guts.rkt"
         "opt.rkt"
         "misc.rkt"
         "blame.rkt"
         syntax/location
         racket/private/performance-hint
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

;; blame-info    : (listof (vector symbol boolean?[indy?] boolean?[swap?]))
;; arg-ctcs      : (listof (cons symbol? contract))
;; arg-dep-ctcs  : (-> ??? (listof contract))
;; indy-arg-ctcs : (listof (cons symbol? contract))
;; rng-ctcs      : (listof (cons symbol? contract))
;; rng-dep-ctcs  : (-> ??? (listof contract))
;; indy-rng-ctcs : (listof (cons symbol? contract))
;; mandatory-args, opt-args : number
;; mandatory-kwds, opt-kwds : (listof keyword?) sorted by keyword<?
;; rest : (or/c symbol? #f)
;; here : quoted-spec for use in assigning indy blame
;; mk-wrapper : creates the a wrapper function that implements the contract checking
(struct ->i (blame-info 
             arg-ctcs arg-dep-ctcs indy-arg-ctcs
             rng-ctcs rng-dep-ctcs indy-rng-ctcs
             pre/post-procs
             mandatory-args opt-args mandatory-kwds opt-kwds rest
             mtd? here mk-wrapper name-info)
        #:property prop:contract
        (build-contract-property
         #:projection
         (λ (ctc) 
           (define arg-ctc-projs (map (λ (x) (contract-projection (cdr x))) (->i-arg-ctcs ctc)))
           (define indy-arg-ctc-projs (map (λ (x) (contract-projection (cdr x))) (->i-indy-arg-ctcs ctc)))
           (define rng-ctc-projs (map (λ (x) (contract-projection (cdr x))) (->i-rng-ctcs ctc)))
           (define indy-rng-ctc-projs (map (λ (x) (contract-projection (cdr x))) (->i-indy-rng-ctcs ctc)))
           (define func (->i-mk-wrapper ctc))
           (define has-rest (->i-rest ctc))
           (define here (->i-here ctc))
           (λ (blame)
             (define blames (for/list ([blame-info (->i-blame-info ctc)])
                              (define name (vector-ref blame-info 0))
                              (define indy? (vector-ref blame-info 1))
                              (define dom? (vector-ref blame-info 2))
                              (define non-indy-blame
                                (blame-add-context blame 
                                                   (format (if dom? "the ~a argument of" "the ~a result of")
                                                           name)
                                                   #:swap? dom?))
                              (if indy?
                                  (blame-replace-negative non-indy-blame here)
                                  non-indy-blame)))
             (define swapped-blame (blame-swap blame))
             (define indy-dom-blame (blame-replace-negative swapped-blame here))
             (define indy-rng-blame (blame-replace-negative blame here))
             
             (define partial-doms 
               (for/list ([dom-proj (in-list arg-ctc-projs)]
                          [pr (in-list (->i-arg-ctcs ctc))])
                 (dom-proj (blame-add-context swapped-blame 
                                              (format "the ~a argument of" (car pr))))))
             (define partial-indy-doms
               (for/list ([dom-proj (in-list indy-arg-ctc-projs)]
                          [dom-pr (in-list (->i-indy-arg-ctcs ctc))])
                 (dom-proj (blame-add-context indy-dom-blame (format "the ~a argument of" (car dom-pr))))))
             
             (define partial-rngs 
               (for/list ([rng-proj (in-list rng-ctc-projs)]
                          [pr (in-list (->i-rng-ctcs ctc))]
                          [n (in-naturals 1)])
                 (define name (car pr))
                 (rng-proj (blame-add-context blame 
                                              (if (eq? '_ name)
                                                  (if (null? (cdr rng-ctc-projs))
                                                      "the result of"
                                                      (format "the ~a result of" (n->th n)))
                                                  (format "the ~a result of" name))))))
             (define partial-indy-rngs 
               (for/list ([rng-proj (in-list indy-rng-ctc-projs)]
                          [rng-pr (in-list (->i-indy-rng-ctcs ctc))])
                 (rng-proj (blame-add-context indy-rng-blame (format "the ~a result of" (car rng-pr))))))
             (apply func
                    (λ (val mtd?)
                      (if has-rest
                          (check-procedure/more val mtd? (->i-mandatory-args ctc) (->i-mandatory-kwds ctc) (->i-opt-kwds ctc) blame)
                          (check-procedure val mtd?
                                           (->i-mandatory-args ctc) (->i-opt-args ctc)
                                           (->i-mandatory-kwds ctc) (->i-opt-kwds ctc)
                                           blame)))
                    ctc
                    blame swapped-blame ;; used by the #:pre and #:post checking
                    (append blames
                            (->i-pre/post-procs ctc)
                            partial-doms
                            (->i-arg-dep-ctcs ctc)
                            partial-indy-doms
                            partial-rngs
                            (->i-rng-dep-ctcs ctc)
                            partial-indy-rngs))))
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
                                           (map cdr (->i-arg-ctcs ctc))
                                           (->i-arg-dep-ctcs ctc)
                                           (λ (x) (list-ref x 4)))
                          ,@(let ([rests (arg/ress->spec args-info
                                                         (map cdr (->i-arg-ctcs ctc))
                                                         (->i-arg-dep-ctcs ctc)
                                                         (λ (x) (not (list-ref x 4))))])
                              (if (null? rests)
                                  '()
                                  (list rests)))
                          ,@(if rest-info
                                (case (car rest-info)
                                  [(nodep) `(#:rest [,(list-ref rest-info 1) ,(contract-name (car (reverse (map cdr (->i-arg-ctcs ctc)))))])]
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
                                                           (map cdr (->i-rng-ctcs ctc))
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
             (let ([has-rest (->i-rest ctc)]
                   [mtd? (->i-mtd? ctc)]
                   [mand-args (->i-mandatory-args ctc)]
                   [opt-args (->i-opt-args ctc)]
                   [mand-kwds (->i-mandatory-kwds ctc)]
                   [opt-kwds (->i-opt-kwds ctc)])
               (λ (val)
                 (if has-rest
                     (check-procedure/more val mtd? mand-args mand-kwds opt-kwds #f)
                     (check-procedure val mtd? mand-args opt-args mand-kwds opt-kwds #f)))))
         #:stronger (λ (this that) (eq? this that)))) ;; WRONG

;; find-ordering : (listof arg) -> (values (listof arg) (listof number)) 
;; sorts the arguments according to the dependency order.
;; returns them in the reverse of that order, ie expressions that need
;; to be evaluted first come later in the list.
;; BAD: this seem wrong, as it doesn't consider transitive dependencies
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
;;                   (listof int) -- indices that give the mapping from the ordered-args to the original order
;;                   (listof identifier) -- arg-proj-vars, bound to projections with ordinary blame
;;                   (listof identifier) -- indy-arg-proj-args, bound to projections with indy blame
;;                   (listof identifier) -- wrapper-args, bound to the original, unwrapped values, sorted like original arg/ress
;;                                       -- the generated lets rebind these variables to their projected counterparts, with normal blame
;;                   (listof identifier) -- indy-arg-vars, bound to wrapped values with indy blame, sorted like the second input
;;                   (identifier arg -> identifier) -- maps the original var in the arg to the corresponding indy-var
;;                   free-identifier-mapping[id -o> (listof (list/c boolean?[indy?] boolean?[dom?]))]
;; adds nested lets that bind the wrapper-args and the indy-arg-vars to projected values, with 'body' in the body of the let
;; also handles adding code to check to see if usupplied args are present (skipping the contract check, if so)
;; mutates blame-var-table to record which blame records needs to be computed (and passed in)
;; WRONG: need to rename the variables in this function from "arg" to "arg/res"
(define-for-syntax (add-wrapper-let body swapped-blame?
                                    ordered-args arg-indices
                                    arg-proj-vars indy-arg-proj-vars 
                                    wrapper-args indy-arg-vars
                                    arg/res-to-indy-var
                                    blame-var-table)
  
  (define (add-unsupplied-check arg wrapper-arg stx)
    (if (and (arg? arg)
             (arg-optional? arg))
        #`(if (eq? #,wrapper-arg the-unsupplied-arg)
              #,wrapper-arg
              #,stx)
        stx))
  
  (define needed-blame-vars (make-hash))
  (define (add-blame-var indy? dom? id)
    (define olds (free-identifier-mapping-get blame-var-table id (λ () '())))
    (define new (list indy? dom?))
    (unless (member new olds)
      (free-identifier-mapping-put! blame-var-table id (cons new olds)))
    (build-blame-identifier indy? dom? id))
  
  (for/fold ([body body])
    ([indy-arg-var (in-list indy-arg-vars)]
     [arg (in-list ordered-args)]
     [arg-index arg-indices]
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
                                              #,(add-blame-var #t swapped-blame? (arg/res-var arg)))
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
                                 #,(add-blame-var #f swapped-blame? (arg/res-var arg)))]
                      [(arg/res-vars arg)
                       #`(#,arg-proj-var #,@(map arg/res-to-indy-var (arg/res-vars arg))
                                         #,wrapper-arg 
                                         #,(add-blame-var #f swapped-blame? (arg/res-var arg)))]
                      [else
                       #`(#,arg-proj-var #,wrapper-arg)]))])
            #,body)))))

(define-for-syntax (build-blame-identifier indy? dom? id)
  (datum->syntax id
                 (string->symbol
                  (string-append (symbol->string (syntax-e id))
                                 (if indy? "-indy" "")
                                 (if dom? "-dom" "-rng")
                                 "-blame"))))

;; Returns an empty list if no result contracts and a list of a single syntax value
;; which should be a function from results to projection-applied versions of the same
;; if there are result contracts.
(define-for-syntax (build-result-checkers an-istx
                                          ordered-ress res-indices
                                          res-proj-vars indy-res-proj-vars
                                          wrapper-ress indy-res-vars
                                          arg/res-to-indy-var
                                          blame-var-table)
  (cond
    [(istx-ress an-istx)
     (list
      #`(case-lambda
          [#,(vector->list wrapper-ress)
           (with-continuation-mark
            contract-continuation-mark-key blame
            #,(add-wrapper-let 
               (add-post-cond an-istx arg/res-to-indy-var #`(values #,@(vector->list wrapper-ress)))
               #f
               ordered-ress res-indices
               res-proj-vars indy-res-proj-vars 
               wrapper-ress indy-res-vars
               arg/res-to-indy-var
               blame-var-table))]
          [args
           (bad-number-of-results blame val
                                  #,(vector-length wrapper-ress)
                                  args)]))]
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

(define-for-syntax (mk-wrapper-func/blame-id-info an-istx used-indy-vars)
  (define args+rst (append (istx-args an-istx)
                           (if (istx-rst an-istx)
                               (list (istx-rst an-istx))
                               '())))
  (define-values (ordered-args arg-indices) (find-ordering args+rst))
  (define-values (ordered-ress res-indices) (if (istx-ress an-istx)
                                                (find-ordering (istx-ress an-istx))
                                                (values '() '())))
  
  (define wrapper-args (list->vector 
                        (append (generate-temporaries (map arg/res-var (istx-args an-istx)))
                                (if (istx-rst an-istx)
                                    (list #'rest-args)
                                    '()))))
  (define indy-arg-vars (generate-temporaries (map arg/res-var ordered-args)))
  (define arg-proj-vars (list->vector (generate-temporaries (map arg/res-var args+rst))))
  
  ;; this list is parallel to arg-proj-vars (so use arg-indices to find the right ones)
  ;; but it contains #fs in places where we don't need the indy projections (because the corresponding
  ;; argument is not dependened on by anything)
  (define indy-arg-proj-vars (list->vector (map (λ (x) (maybe-generate-temporary
                                                        (and (free-identifier-mapping-get used-indy-vars 
                                                                                          (arg/res-var x)
                                                                                          (λ () #f))
                                                             (arg/res-var x)))) 
                                                args+rst)))
  
  
  (define wrapper-ress (list->vector (generate-temporaries (map arg/res-var (or (istx-ress an-istx) '())))))
  (define indy-res-vars (generate-temporaries (map arg/res-var ordered-ress)))
  (define res-proj-vars (list->vector (generate-temporaries (map arg/res-var (or (istx-ress an-istx) '())))))
  
  ;; this list is parallel to res-proj-vars (so use res-indices to find the right ones)
  ;; but it contains #fs in places where we don't need the indy projections (because the corresponding
  ;; result is not dependened on by anything)
  (define indy-res-proj-vars (list->vector (map (λ (x) (maybe-generate-temporary
                                                        (and (free-identifier-mapping-get used-indy-vars 
                                                                                          (arg/res-var x)
                                                                                          (λ () #f))
                                                             (arg/res-var x))))
                                                (or (istx-ress an-istx) '()))))
  
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
  
  (define blame-var-table (make-free-identifier-mapping))
  
  (define wrapper-body
    (add-wrapper-let 
     (add-pre-cond 
      an-istx 
      arg/res-to-indy-var 
      (add-eres-lets
       an-istx
       res-proj-vars
       arg/res-to-indy-var
       (args/vars->arg-checker
        (build-result-checkers
         an-istx
         ordered-ress res-indices
         res-proj-vars indy-res-proj-vars
         wrapper-ress indy-res-vars
         arg/res-to-indy-var
         blame-var-table)
        (istx-args an-istx)
        (istx-rst an-istx)
        wrapper-args
        this-param)))
     #t
     ordered-args arg-indices
     arg-proj-vars indy-arg-proj-vars 
     wrapper-args indy-arg-vars
     arg/res-to-indy-var
     blame-var-table))
  
  (define blame-ids '())
  (free-identifier-mapping-for-each
   blame-var-table
   (λ (id prs)
     (for ([pr (in-list prs)])
       (define indy? (list-ref pr 0))
       (define dom? (list-ref pr 1))
       (set! blame-ids (cons (cons (build-blame-identifier indy? dom? id)
                                   (vector (syntax-e id) indy? dom?))
                             blame-ids)))))
  (set! blame-ids (sort blame-ids symbol<? #:key (λ (x) (syntax-e (car x)))))
  
  (values
   (map cdr blame-ids)
  #`(λ (chk ctc blame swapped-blame #,@(map car blame-ids)
        
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
                 #,wrapper-body)])
          (impersonate-procedure
           val
           (make-keyword-procedure
            (λ (kwds kwd-args . args)
              (with-continuation-mark
               contract-continuation-mark-key blame
               (keyword-apply arg-checker kwds kwd-args args)))
            (λ args
              (with-continuation-mark
               contract-continuation-mark-key blame
               (apply arg-checker args))))
           impersonator-prop:contracted ctc))))))

(begin-encourage-inline
  (define (un-dep ctc obj blame)
    (let ([ctc (coerce-contract '->i ctc)])
      (((contract-projection ctc) blame) obj))))

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
  (define an-istx (parse-->i stx))
  (define used-indy-vars (mk-used-indy-vars an-istx))
  (define-values (blame-ids-info wrapper-func) (mk-wrapper-func/blame-id-info an-istx used-indy-vars))
  (define args+rst (append (istx-args an-istx)
                           (if (istx-rst an-istx)
                               (list (istx-rst an-istx))
                               '())))
  (define this->i (gensym 'this->i))
  (with-syntax ([(arg-exp-xs ...) 
                 (generate-temporaries (filter values (map (λ (arg) (and (not (arg/res-vars arg)) (arg/res-var arg)))
                                                           args+rst)))]
                [((arg-names arg-exps) ...)
                 (filter values (map (λ (arg) (and (not (arg/res-vars arg)) 
                                                   (list
                                                    (arg/res-var arg)
                                                    (syntax-property
                                                     (syntax-property
                                                      (arg/res-ctc arg)
                                                      'racket/contract:negative-position
                                                      this->i)
                                                     'racket/contract:contract-on-boundary 
                                                     (gensym '->i-indy-boundary)))))
                                     args+rst))]
                
                [(res-exp-xs ...) 
                 (if (istx-ress an-istx)
                     (generate-temporaries (filter values (map (λ (res) (and (not (arg/res-vars res)) (arg/res-var res)))
                                                               (istx-ress an-istx))))
                     '())]
                [((res-names res-exps) ...)
                 (if (istx-ress an-istx)
                     (filter values (map (λ (res) (and (not (arg/res-vars res)) 
                                                       (list
                                                        (arg/res-var res)
                                                        (syntax-property
                                                         (syntax-property 
                                                          (arg/res-ctc res)
                                                          'racket/contract:positive-position
                                                          this->i)
                                                         'racket/contract:contract-on-boundary 
                                                         (gensym '->i-indy-boundary)))))
                                         (istx-ress an-istx)))
                     '())])
    
    (define (find-orig-vars arg arg/ress-to-look-in)
      (for/list ([an-id (in-list (arg/res-vars arg))])
        (define ans
          (for/or ([o-arg (in-list arg/ress-to-look-in)])
            (and (free-identifier=? an-id (arg/res-var o-arg))
                 (arg/res-var o-arg))))
        (unless ans
          (error 'contract/arr-i.rkt:find-orig-vars "could not find ~s in ~s\n" an-id arg/ress-to-look-in))
        ans))
    
    #`(let ([arg-exp-xs (coerce-contract '->i arg-exps)] ...
            [res-exp-xs (coerce-contract '->i res-exps)] ...)
        #,(syntax-property
           #`(->i 
              ;; the information needed to make the blame records and their new contexts
              '#,blame-ids-info
              ;; all of the non-dependent argument contracts
              (list (cons 'arg-names arg-exp-xs) ...)
              ;; all of the dependent argument contracts
              (list #,@(for/list ([arg (in-list args+rst)]
                                  #:when (arg/res-vars arg))
                         (define orig-vars (find-orig-vars arg args+rst))
                         (define ctc-stx
                           (syntax-property
                            (syntax-property 
                             (arg/res-ctc arg)
                             'racket/contract:negative-position 
                             this->i)
                            'racket/contract:contract-on-boundary
                            (gensym '->i-indy-boundary)))
                         #`(λ (#,@orig-vars val blame)
                             #,@(arg/res-vars arg)
                             ;; this used to use opt/direct, but 
                             ;; opt/direct duplicates code (bad!)
                             (un-dep #,ctc-stx val blame))))
              ;; then the non-dependent argument contracts that are themselves dependend on
              (list #,@(filter values
                               (map (λ (arg/res indy-id) 
                                      (and (free-identifier-mapping-get used-indy-vars (arg/res-var arg/res) (λ () #f))
                                           #`(cons '#,(arg/res-var arg/res) #,indy-id)))
                                    (filter (λ (arg/res) (not (arg/res-vars arg/res))) args+rst)
                                    (syntax->list #'(arg-exp-xs ...)))))
              
              
              #,(if (istx-ress an-istx)
                    #`(list (cons 'res-names res-exp-xs) ...)
                    #''())
              #,(if (istx-ress an-istx) 
                    #`(list #,@(for/list ([arg (in-list 
                                                (istx-ress an-istx))]
                                          #:when (arg/res-vars arg))
                                 (define orig-vars 
                                   (find-orig-vars 
                                    arg 
                                    (append (istx-ress an-istx)
                                            args+rst)))
                                 (define arg-stx
                                   (syntax-property
                                    (syntax-property 
                                     (arg/res-ctc arg)
                                     'racket/contract:positive-position
                                     this->i)
                                    'racket/contract:contract-on-boundary 
                                    (gensym '->i-indy-boundary)))
                                 (if (eres? arg)
                                     #`(λ #,orig-vars
                                         #,@(arg/res-vars arg)
                                         (opt/c #,arg-stx))
                                     #`(λ (#,@orig-vars val blame)
                                         ;; this used to use opt/direct, but 
                                         ;; opt/direct duplicates code (bad!)
                                         #,@(arg/res-vars arg)
                                         (un-dep #,arg-stx val blame)))))
                    #''())
              #,(if (istx-ress an-istx)
                    #`(list #,@(filter values
                                       (map (λ (arg/res indy-id) 
                                              (and (free-identifier-mapping-get used-indy-vars (arg/res-var arg/res) (λ () #f))
                                                   #`(cons '#,(arg/res-var arg/res) #,indy-id)))
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
              '#,(and (istx-rst an-istx) (arg/res-var (istx-rst an-istx)))
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
                     (if pre (list pre) '())))))))
