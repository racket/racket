#lang racket/base

(require "arrow-common.rkt"
         "prop.rkt"
         "guts.rkt"
         "opt.rkt"
         "misc.rkt"
         "blame.rkt"
         "generate.rkt"
         syntax/location
         racket/private/performance-hint
         (for-syntax racket/base
                     racket/stxparam-exptime
                     syntax/name
                     "arr-i-parse.rkt"

                     (rename-in
                      syntax/private/boundmap
                      ;; the private version of the library
                      ;; (the one without contracts)
                      ;; has these old, wrong names in it.
                      [make-module-identifier-mapping make-free-identifier-mapping]
                      [module-identifier-mapping-get free-identifier-mapping-get]
                      [module-identifier-mapping-put! free-identifier-mapping-put!]
                      [module-identifier-mapping-for-each free-identifier-mapping-for-each])))

(provide (rename-out [->i/m ->i])
         (for-syntax ->i-internal)) ; for method version of ->i

(define (build-??-args c-or-i-procedure ctc blame)
  (define arg-ctc-projs (map (λ (x) (get/build-late-neg-projection (->i-arg-contract x)))
                             (->i-arg-ctcs ctc)))
  (define indy-arg-ctc-projs (map (λ (x) (get/build-late-neg-projection (cdr x)))
                                  (->i-indy-arg-ctcs ctc)))
  (define rng-ctc-projs (map (λ (x) (get/build-late-neg-projection (cdr x))) (->i-rng-ctcs ctc)))
  (define indy-rng-ctc-projs (map (λ (x) (get/build-late-neg-projection (cdr x)))
                                  (->i-indy-rng-ctcs ctc)))
  (define has-rest (->i-rest ctc))
  (define here (->i-here ctc))

  (define blames (for/list ([blame-info (->i-blame-info ctc)])
                   (define name (vector-ref blame-info 0))
                   (define indy? (vector-ref blame-info 1))
                   (define dom? (vector-ref blame-info 2))
                   (define non-indy-blame
                     (blame-add-context
                      blame
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
                                   (format "the ~a argument of" (->i-arg-name pr))))))
  (define partial-indy-doms
    (for/list ([dom-proj (in-list indy-arg-ctc-projs)]
               [dom-pr (in-list (->i-indy-arg-ctcs ctc))])
      (dom-proj (blame-add-context indy-dom-blame
                                   (format "the ~a argument of" (car dom-pr))))))

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
      (rng-proj (blame-add-context indy-rng-blame (format "the ~a result of"
                                                          (car rng-pr))))))
  (list* c-or-i-procedure
         (λ (val mtd?)
           (if has-rest
               (check-procedure/more val mtd?
                                     (->i-mandatory-args ctc)
                                     (->i-mandatory-kwds ctc)
                                     (->i-opt-kwds ctc)
                                     blame #f)
               (check-procedure val mtd?
                                (->i-mandatory-args ctc) (->i-opt-args ctc)
                                (->i-mandatory-kwds ctc) (->i-opt-kwds ctc)
                                blame #f)))
         ctc
         blame swapped-blame ;; used by the #:pre and #:post checking
         (append blames
                 (->i-pre/post-procs ctc)
                 partial-doms
                 (->i-arg-dep-ctcs ctc)
                 partial-indy-doms
                 partial-rngs
                 (->i-rng-dep-ctcs ctc)
                 partial-indy-rngs)))

(define arr->i-late-neg-proj
  (λ (ctc c-or-i-procedure)
    (define func (->i-mk-wrapper ctc))
    (λ (blame)
      (define ???-args (build-??-args c-or-i-procedure ctc blame))
      (apply func ???-args))))


(define (exercise->i ctc)
  (define arg-deps (->i-arg-dep-ctcs ctc))
  (cond
    [(and (null? arg-deps) (not (->i-rest ctc)))
     (λ (fuel)
       (define gens (for/list ([arg-ctc (in-list (->i-arg-ctcs ctc))]
                               #:when (and (not (->i-arg-optional? arg-ctc))
                                           (not (->i-arg-kwd arg-ctc))))
                      (contract-random-generate/choose (->i-arg-contract arg-ctc) fuel)))
       (define kwd-gens (for/list ([arg-ctc (in-list (->i-arg-ctcs ctc))]
                                   #:when (and (not (->i-arg-optional? arg-ctc))
                                               (->i-arg-kwd arg-ctc)))
                          (contract-random-generate/choose (->i-arg-contract arg-ctc) fuel)))
       (define dom-kwds (for/list ([arg-ctc (in-list (->i-arg-ctcs ctc))]
                                   #:when (and (not (->i-arg-optional? arg-ctc))
                                               (->i-arg-kwd arg-ctc)))
                          (->i-arg-kwd arg-ctc)))
       (define rng-ctcs (map cdr (->i-rng-ctcs ctc)))
       (define rng-exers
         (and rng-ctcs
              (for/list ([rng-ctc (in-list rng-ctcs)])
                (define-values (exer ctcs)
                  ((contract-struct-exercise rng-ctc) fuel))
                exer)))
       (cond
         [(andmap values gens)
          (define env (contract-random-generate-get-current-environment))
          (values (λ (f)
                    (call-with-values
                     (λ ()
                       (define kwd-args
                         (for/list ([kwd-gen (in-list kwd-gens)])
                           (kwd-gen)))
                       (define regular-args
                         (for/list ([gen (in-list gens)])
                           (gen)))
                       (keyword-apply
                        f
                        dom-kwds
                        kwd-args
                        regular-args))
                     (λ results
                       (void)
                       (when rng-ctcs
                         (for ([res-ctc (in-list rng-ctcs)]
                               [result (in-list results)])
                           (contract-random-generate-stash env res-ctc result))
                         (for ([exer (in-list rng-exers)]
                               [result (in-list results)])
                           (exer result))))))
                  (or rng-ctcs '()))]
         [else
          (values void '())]))]
    [else
     (λ (fuel) (values void '()))]))

;; name : symbol?
;; kwd : (or/c #f keyword?)
;; optional? : boolean?
;; contract : contract?
(struct ->i-arg (name kwd optional? contract) #:transparent)

;; blame-info    : (listof (vector symbol boolean?[indy?] boolean?[swap?]))
;; arg-ctcs      : (listof ->i-arg?)
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
        #:property prop:custom-write custom-write-property-proc)

(define (mk-prop chaperone?)
  (define c-or-i-procedure (if chaperone? chaperone-procedure impersonate-procedure))
  ((if chaperone? build-chaperone-contract-property build-contract-property)
   #:trusted trust-me
   #:late-neg-projection
   (λ (ctc) (arr->i-late-neg-proj ctc c-or-i-procedure))
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
                        (define body-src (list-ref info 5))
                        (if (skip? info)
                            (loop (cdr infos) ctcs (cdr dep-ctcs))
                            `(,@(if kwd
                                    (list kwd)
                                    (list))
                              [,var ,vars ,body-src]
                              .
                              ,(loop (cdr infos) ctcs (cdr dep-ctcs))))]))])))
            (let* ([name-info (->i-name-info ctc)]
                   [args-info (vector-ref name-info 0)]
                   [rest-info (vector-ref name-info 1)]
                   [pre-infos  (vector-ref name-info 2)]
                   [rng-info  (vector-ref name-info 3)]
                   [post-infos (vector-ref name-info 4)])
              `(->i ,(arg/ress->spec args-info
                                     (map ->i-arg-contract (->i-arg-ctcs ctc))
                                     (->i-arg-dep-ctcs ctc)
                                     (λ (x) (list-ref x 4)))
                    ,@(let ([rests (arg/ress->spec args-info
                                                   (map ->i-arg-contract (->i-arg-ctcs ctc))
                                                   (->i-arg-dep-ctcs ctc)
                                                   (λ (x) (not (list-ref x 4))))])
                        (if (null? rests)
                            '()
                            (list rests)))
                    ,@(if rest-info
                          (case (car rest-info)
                            [(nodep) `(#:rest
                                       [,(list-ref rest-info 1)
                                        ,(contract-name
                                          (car
                                           (reverse
                                            (map ->i-arg-contract (->i-arg-ctcs ctc)))))])]
                            [(dep) `(#:rest [,(list-ref rest-info 1)
                                             ,(list-ref rest-info 2)
                                             ,(list-ref rest-info 3)])])
                          '())
                    ,@(apply
                       append
                       (for/list ([pre-info pre-infos])
                         (define ids (list-ref pre-info 0))
                         (define name (list-ref pre-info 1))
                         (define code (list-ref pre-info 2))
                         (cond
                           [(string? name)
                            `(#:pre/name ,ids ,name ,code)]
                           [(equal? name 'bool)
                            `(#:pre ,ids ,code)]
                           [(equal? name 'desc)
                            `(#:pre/desc ,ids ,code)])))
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
                         (define ids (list-ref post-info 0))
                         (define name (list-ref post-info 1))
                         (define code (list-ref post-info 2))
                         (cond
                           [(string? name)
                            `(#:post/name ,ids ,name ,code)]
                           [(equal? name 'bool)
                            `(#:post ,ids ,code)]
                           [(equal? name 'desc)
                            `(#:post/desc ,ids ,code)]))))))
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
             (check-procedure/more val mtd? mand-args mand-kwds opt-kwds #f #f)
             (check-procedure val mtd? mand-args opt-args mand-kwds opt-kwds #f #f)))))
   #:exercise exercise->i
   #:equivalent (λ (this that) (eq? this that))
   #:stronger (λ (this that) (eq? this that)))) ;; WRONG

(struct chaperone->i ->i () #:property prop:chaperone-contract (mk-prop #t))
(struct impersonator->i ->i () #:property prop:contract (mk-prop #f))
(define (make-->i is-chaperone-contract? blame-info
                  arg-ctcs arg-dep-ctcs indy-arg-ctcs
                  rng-ctcs rng-dep-ctcs indy-rng-ctcs
                  pre/post-procs
                  mandatory-args opt-args mandatory-kwds opt-kwds rest
                  mtd? here mk-wrapper name-info)
  (define maker (if is-chaperone-contract? chaperone->i impersonator->i))
  (maker blame-info
         arg-ctcs arg-dep-ctcs indy-arg-ctcs
         rng-ctcs rng-dep-ctcs indy-rng-ctcs
         pre/post-procs
         mandatory-args opt-args mandatory-kwds opt-kwds rest
         mtd? here mk-wrapper name-info))



;; find-ordering : (listof (or/c pre/post arg))
;;              -> (values (listof (or/c pre/pos arg) (listof (or/c #f nat)))
;; sorts the arguments according to the dependency order.
;; returns them in the reverse of that order, ie expressions that need
;; to be evaluted first come later in the list.
;; the second result maps back from the sorted order
;;   (in the first result) to the original order (in `args`)
(define-for-syntax (find-ordering args)

  #|

This uses a variation of the topological sorting algorithm
from Wikipedia attributed to Kahn (1962). It doesn't run in
linear time since it uses a linear scan at each step to find
the 'least' argument contract to pick. (Picking the least arg
ensures that args that are independent of each other are still
evaluted left-to-right.)

  |#


  ;; set up some unreferred to variables for
  ;; the pre/post conditions to base the graph on
  ;; get-var : (or/c pre/post arg) -> identifier
  ;; (unfortuntately we rely on `eq?` here)
  (define pre/post-fake-vars (make-hasheq))
  (for ([arg (in-list args)]
        #:when (pre/post? arg))
    (hash-set! pre/post-fake-vars arg
               (car (generate-temporaries (list arg)))))
  (define (get-var arg)
    (if (arg/res? arg)
        (arg/res-var arg)
        (hash-ref pre/post-fake-vars arg)))

  ;; track the indicies into `args` for the nodes in the graph
  ;; and do the same thing but only for the subset that are actually args
  ;; (unfortuntately we rely on `eq?` here)
  (define numbers (make-hasheq))
  (define id->arg/res (make-free-identifier-mapping))
  (for ([arg (in-list args)]
        [i (in-naturals)])
    (hash-set! numbers arg i)
    (free-identifier-mapping-put! id->arg/res (get-var arg) arg))

  ;; track the original order of the pre/post conditions
  (define pre/post-numbers (make-hasheq))
  (let ([i 0])
    (for ([arg (in-list args)])
      (when (pre/post? arg)
        (hash-set! pre/post-numbers arg i)
        (set! i (+ i 1)))))
  ;; build the graph, where `comes-before` are the backwards
  ;; edges and `comes-after` are the forwards edges
  ;; we use new temporary variables for the pre/posts
  ;; as they are not referred to (but only refer to other things)
  (define comes-before (make-free-identifier-mapping))
  (define comes-after (make-free-identifier-mapping))
  (for ([arg (in-list args)])
    (define the-var (get-var arg))
    (free-identifier-mapping-put! comes-before the-var '())
    (free-identifier-mapping-put! comes-after the-var '()))
  (for ([arg (in-list args)])
    (define the-vars (if (arg/res? arg)
                         (or (arg/res-vars arg) '())
                         (pre/post-vars arg)))
    (define arg-id (get-var arg))
    (for ([dep-id (in-list the-vars)])
      (define dep (free-identifier-mapping-get id->arg/res dep-id (λ () #f)))
      (when dep
        ;; dep = #f should happen only when we're handling the result
        ;; contracts and dep-id is one of the argument contracts.
        ;; in that case, we can just ignore the edge since we know
        ;; it will be bound already
        (free-identifier-mapping-put!
         comes-before
         arg-id
         (cons dep (free-identifier-mapping-get comes-before arg-id)))
        (free-identifier-mapping-put!
         comes-after
         dep-id
         (cons arg (free-identifier-mapping-get comes-after dep-id))))))

  (define sorted '())
  (define no-incoming-edges
    (for/list ([arg (in-list args)]
               #:when (null? (free-identifier-mapping-get comes-before (get-var arg))))
      arg))

  (define (pick-next-node)
    (define least-node
      (let loop ([nodes (cdr no-incoming-edges)]
                 [least-node (car no-incoming-edges)])
        (cond
          [(null? nodes) least-node]
          [else
           (define node (car nodes))
           (cond
             [(< (hash-ref numbers node) (hash-ref numbers least-node))
              (loop (cdr nodes) node)]
             [else
              (loop (cdr nodes) least-node)])])))
    (set! no-incoming-edges (remove least-node no-incoming-edges))
    least-node)

  (define (remove-edge from to)
    (define from-id (get-var from))
    (define to-id (get-var to))
    (free-identifier-mapping-put!
     comes-before
     to-id
     (remove from (free-identifier-mapping-get comes-before to-id)))
    (free-identifier-mapping-put!
     comes-after
     from-id
     (remove to (free-identifier-mapping-get comes-after from-id))))

  (let loop ()
    (unless (null? no-incoming-edges)
      (define n (pick-next-node))
      (set! sorted (cons n sorted))
      (for ([m (in-list (free-identifier-mapping-get comes-after (get-var n)))])
        (remove-edge n m)
        (when (null? (free-identifier-mapping-get comes-before (get-var m)))
          (set! no-incoming-edges (cons m no-incoming-edges))))
      (loop)))

  (values sorted
          (for/list ([arg (in-list sorted)])
            (if (arg/res? arg)
                (hash-ref numbers arg)
                "pre/post, which has an index we don't want to use"))
          pre/post-numbers))

;; args/vars->arglist : (listof arg?) (vectorof identifier?) -> syntax
;; (vector-length vars) = (length args)
;; builds the parameter list for the wrapper λ
(define-for-syntax (args/vars->arglist an-istx wrapper-args this-param)
  #`(#,@(if this-param
            (list this-param)
            '())
     .
     #,
     (let loop ([args (istx-args an-istx)])
       (cond
         [(null? args) (if (istx-rst an-istx)
                           (hash-ref wrapper-args (istx-rst an-istx))
                           #'())]
         [else
          (define arg (car args))
          (define kwd (arg-kwd arg))
          (define opt? (arg-optional? arg))
          (define wrapper-arg (hash-ref wrapper-args arg))
          (define arg-exp
            (cond
              [(and kwd opt?)
               #`(#,kwd [#,wrapper-arg the-unsupplied-arg])]
              [kwd
               #`(#,kwd #,wrapper-arg)]
              [opt?
               #`([#,wrapper-arg the-unsupplied-arg])]
              [else
               #`(#,wrapper-arg)]))
          #`(#,@arg-exp
             .
             #,(loop (cdr args)))]))))

(define-for-syntax (all-but-last lst)
  (reverse (cdr (reverse lst))))

;; wrapper-args : (listof identifier)
;;    wrapper-args will contain one identifier for each arg, plus one more for rst,
;;    unless rst is #f, in which case it just contains one identifier for each arg.
;;
;; FIXME: Currently, none of the resulting argument checkers attempt to preserve tail
;; recursion.  If all of the result contracts (which would need to be passed to
;; this function as well as results-checkers) can be evaluated early, then we can
;; preserve tail recursion in the fashion of -> etc.
(define-for-syntax (args/vars->arg-checker result-checkers args rst wrapper-args this-param)
  (let ([opts? (ormap arg-optional? args)]
        [this-params (if this-param (list this-param) '())])

    (define kwd-args (filter arg-kwd args))
    (define non-kwd-args (filter (λ (x) (not (arg-kwd x))) args))

    (define sorted-kwd/arg-pairs
      (sort
       (map (λ (arg) (cons (arg-kwd arg) (hash-ref wrapper-args arg))) kwd-args)
       (λ (x y) (keyword<? (syntax-e (car x)) (syntax-e (car y))))))
    (define keyword-arguments (map cdr sorted-kwd/arg-pairs))
    (define regular-arguments (map (λ (arg) (hash-ref wrapper-args arg)) non-kwd-args))
    (cond
      [(and opts? (ormap arg-kwd args))
       ;; has both optional and keyword args
       #`(keyword-return/no-unsupplied
          #,(if (null? result-checkers) #f (car result-checkers))
          '#,(map car sorted-kwd/arg-pairs)
          (list #,@keyword-arguments)
          #,(if rst
                #'rest-args
                #''())
          #,@this-params
          #,@regular-arguments)]
      [opts?
       ;; has optional args, but no keyword args
       (define wrapper-args-as-list
         (for/list ([arg (in-list args)])
           (hash-ref wrapper-args arg)))
       #`(return/no-unsupplied #,(if (null? result-checkers) #f (car result-checkers))
                               #,(if rst
                                     #'rest-args
                                     #''())
                               #,@this-params
                               #,@wrapper-args-as-list)]
      [else
       (cond
         [(and (null? keyword-arguments) rst)
          #`(apply values #,@result-checkers #,@this-params #,@regular-arguments rest-args)]
         [(null? keyword-arguments)
          #`(values #,@result-checkers #,@this-params #,@regular-arguments)]
         [rst
          #`(apply values #,@result-checkers (list #,@keyword-arguments)
                   #,@this-params #,@regular-arguments rest-args)]
         [else
          #`(values #,@result-checkers (list #,@keyword-arguments)
                    #,@this-params #,@regular-arguments)])])))

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

(define (signal-pre/post pre? val kind blame neg-party condition-result . var-infos)
  (define vars-str
    (apply
     string-append
     (for/list ([var-info (in-list var-infos)])
       (format "\n      ~s: ~e"
               (list-ref var-info 0)
               (list-ref var-info 1)))))
  (define msg
    (cond
      [(string? kind) (string-append kind vars-str)]
      [(or (equal? kind 'bool)
           (and (equal? kind 'desc)
                (equal? condition-result #f)))
       (string-append
        (if pre? "#:pre" "#:post")
        " condition violation"
        (if (null? var-infos)
            ""
            "; variables are:")
        vars-str)]
      [else
       (pre-post/desc-result->string condition-result pre? '->i)]))
  (raise-blame-error blame #:missing-party neg-party val "~a" msg))

(define-for-syntax (add-pre-conds an-istx pre-indicies
                                  indy-arg-vars ordered-args indy-res-vars ordered-ress
                                  call-stx)
  call-stx #;
  #`(begin #,@(for/list ([pre (in-list (istx-pre an-istx))])
                (build-pre/post-code pre pre-indicies
                                     indy-arg-vars ordered-args indy-res-vars ordered-ress))
           #,call-stx))

(define-for-syntax (add-post-conds an-istx post-indices
                                   indy-arg-vars ordered-args indy-res-vars ordered-ress
                                   call-stx)
  call-stx
  #;
  #`(begin #,@(for/list ([post (in-list (istx-post an-istx))])
                (build-pre/post-code post post-indices
                                     indy-arg-vars ordered-args indy-res-vars ordered-ress))
           #,call-stx))

(define-for-syntax (build-pre/post-code a-pre/post pre-indicies/post-indicies
                                        indy-arg-vars ordered-args indy-res-vars ordered-ress)
  (define pre? (pre/post-pre? a-pre/post))
  (define id (string->symbol (format (if pre? "pre-proc~a" "post-proc~a")
                                     (hash-ref pre-indicies/post-indicies a-pre/post))))
  #`(let ([condition-result
           (#,id #,@(map (λ (var) (arg/res-to-indy-var indy-arg-vars
                                                       ordered-args
                                                       indy-res-vars
                                                       ordered-ress
                                                       var))
                         (pre/post-vars a-pre/post)))])
      (unless #,(if (equal? (pre/post-kind a-pre/post) 'desc)
                    #'(equal? condition-result #t)
                    #'condition-result)
        (signal-pre/post
         #,pre?
         val
         '#,(pre/post-kind a-pre/post)
         #,(if pre? #'swapped-blame #'blame)
         neg-party
         condition-result
         #,@(map (λ (x) #`(list '#,x #,(arg/res-to-indy-var indy-arg-vars
                                                            ordered-args
                                                            indy-res-vars
                                                            ordered-ress
                                                            x)))
                 (pre/post-vars a-pre/post))))))

;; add-wrapper-let :
;;   syntax? -- placed into the body position of the generated let expression
;;   boolean? -- indicates if this is a chaperone contract
;;   boolean? -- indicates if this is an arg or a res; affects only how blame-var-table is filled in
;;   (listof (or/c arg/res pre/post)) -- ordered-arg/reses,
;;          sorted version of the arg/res and pre/post cond structs,
;;          ordered by evaluation order
;;   (listof (or/c int #f) -- indices that give the mapping from the ordered-args
;;          to the original order, #f if this position is a pre/post-condition
;;   (vectorof identifier) -- arg/res-proj-vars, bound to projections with ordinary blame
;;                            not in evaluation order, but in the order from istx
;;   (vectorof identifier) -- indy-arg/res-proj-args, bound to projections with indy blame
;;                            not in evaluation order, but in the order from istx
;;   (vectorof identifier) -- wrapper-arg/ress, bound to the original, unwrapped values, sorted like
;;        original arg/ress (not evaluation order). the generated lets rebind these variables to
;;        their projected counterparts, with normal blame
;;   (listof identifier) -- indy-arg/res-vars, bound to wrapped values with indy blame,
;;        sorted like `ordered-arg/reses`
;;   (hash [pre/post -o> nat]) pre-indicies/post-indicies, indicates the original
;;        ordering of the pre/post conditions (mapping from the order in indy-arg/res-vars
;;        to the ordering in the original istx object, aka program order)
;;   (listof identifier) (listof arg/var) (listof identifier) (listof arg/var)
;;        the last four inputs are used only to call arg/res-to-indy-var.
;; adds nested lets that bind the wrapper-args and the indy-arg/res-vars to projected values,
;; with 'body' in the body of the let also handles adding code to check to see if unsupplied
;; args are present (skipping the contract check, if so)
(define-for-syntax (add-wrapper-let body is-chaperone-contract? swapped-blame?
                                    ordered-arg/reses indicies
                                    arg/res-proj-vars indy-arg/res-proj-vars
                                    wrapper-arg/ress indy-arg/res-vars
                                    pre-indicies/post-indicies
                                    indy-arg-vars ordered-args indy-res-vars ordered-ress)

  (define (add-unsupplied-check an-arg/res wrapper-arg stx)
    (if (and (arg? an-arg/res)
             (arg-optional? an-arg/res))
        #`(if (eq? #,wrapper-arg the-unsupplied-arg)
              #,wrapper-arg
              #,stx)
        stx))

  (for/fold ([body body])
            ([indy-arg/res-var (in-list indy-arg/res-vars)]
             [an-arg/res (in-list ordered-arg/reses)]
             [index (in-list indicies)]
             [i (in-naturals)])
    (cond
      [(arg/res? an-arg/res)
       (define wrapper-arg (hash-ref wrapper-arg/ress an-arg/res))
       (define arg/res-proj-var (vector-ref arg/res-proj-vars index))
       (define indy-arg/res-proj-var (vector-ref indy-arg/res-proj-vars index))

       ;; bound to the result of calling the dependent function
       ;; (which isn't a contract directly, but is a function that returns
       ;; the projection for a contract)
       ;; the result computes what the contract will be for the given argument/res value.
       (define contract-identifier (car (generate-temporaries (list indy-arg/res-var))))

       (define indy-binding
         ;; if indy-arg/res-proj-var is #f, that means that we don't need that binding, so skip it
         (if indy-arg/res-proj-var
             (list
              #`[#,indy-arg/res-var
                 #,(add-unsupplied-check
                    an-arg/res
                    wrapper-arg
                    (if (arg/res-vars an-arg/res)
                        #`(#,(if is-chaperone-contract? #'un-dep/chaperone #'un-dep)
                           #,contract-identifier
                           #,wrapper-arg
                           #,(build-blame-identifier #t swapped-blame? (arg/res-var an-arg/res))
                           neg-party
                           #t)
                        #`(#,indy-arg/res-proj-var #,wrapper-arg neg-party)))])
             (list)))

       #`(let (#,@(if (and (arg/res-vars an-arg/res) (not (eres? an-arg/res)))
                      (list #`[#,contract-identifier
                               #,(add-unsupplied-check
                                  an-arg/res
                                  wrapper-arg
                                  #`(#,arg/res-proj-var
                                     #,@(map (λ (var)
                                               (arg/res-to-indy-var indy-arg-vars
                                                                    ordered-args
                                                                    indy-res-vars
                                                                    ordered-ress
                                                                    var))
                                             (arg/res-vars an-arg/res))))])
                      (list)))
           (let ([#,wrapper-arg
                  #,(add-unsupplied-check
                     an-arg/res
                     wrapper-arg
                     (cond
                       [(and (eres? an-arg/res) (arg/res-vars an-arg/res))
                        #`(#,(if is-chaperone-contract? #'un-dep/chaperone #'un-dep)
                           #,(eres-eid an-arg/res)
                           #,wrapper-arg
                           #,(build-blame-identifier #f
                                                     swapped-blame?
                                                     (arg/res-var an-arg/res))
                           neg-party
                           #f)]
                       [(arg/res-vars an-arg/res)
                        #`(#,(if is-chaperone-contract? #'un-dep/chaperone #'un-dep)
                           #,contract-identifier
                           #,wrapper-arg
                           #,(build-blame-identifier #f swapped-blame? (arg/res-var an-arg/res))
                           neg-party
                           #f)]
                       [else
                        #`(#,arg/res-proj-var #,wrapper-arg neg-party)]))]
                 #,@indy-binding)
             #,body))]
      [else
       #`(begin #,(build-pre/post-code an-arg/res pre-indicies/post-indicies
                                       indy-arg-vars ordered-args indy-res-vars ordered-ress)
                #,body)])))


;;   (identifier arg -o> identifier) -- maps the original var in the arg to the corresponding indy-var
;;      free-identifier-mapping[id -o> (listof (list/c boolean?[indy?] boolean?[dom?]))]
;; mutates blame-var-table to record which
;; blame records needs to be computed (and passed in)
(define-for-syntax (build-blame-ids ordered-args ordered-reses)
  (define blame-var-table (make-free-identifier-mapping))
  (define needed-blame-vars (make-hash))

  (define (add-blame-var indy? dom? id)
    (define olds (free-identifier-mapping-get blame-var-table id (λ () '())))
    (define new (list indy? dom?))
    (unless (member new olds)
      (free-identifier-mapping-put! blame-var-table id (cons new olds))))

  (define (build-some ordered-arg/reses swapped-blame?)
    (for ([an-arg/res (in-list ordered-arg/reses)])
      (when (and (arg/res? an-arg/res) (arg/res-vars an-arg/res))
        (add-blame-var #t swapped-blame? (arg/res-var an-arg/res))
        (if (eres? an-arg/res)
            (add-blame-var #f swapped-blame? (arg/res-var an-arg/res))
            (add-blame-var #f swapped-blame? (arg/res-var an-arg/res))))))

  (build-some ordered-args #t)
  (build-some ordered-reses #f)

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
  (sort blame-ids symbol<? #:key (λ (x) (syntax-e (car x)))))

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
(define-for-syntax (build-result-checkers an-istx post-indicies
                                          ordered-ress res-indices
                                          res-proj-vars indy-res-proj-vars
                                          wrapper-ress indy-res-vars
                                          ordered-args indy-arg-vars)
  (cond
    [(istx-ress an-istx)
     (define wrapper-ress-as-list
       (for/list ([a-res (in-list (istx-ress an-istx))])
         (hash-ref wrapper-ress a-res)))
     (list
      #`(case-lambda
          [#,wrapper-ress-as-list
           (with-contract-continuation-mark
            blame+neg-party
            #,(add-wrapper-let
               (add-post-conds an-istx post-indicies
                               indy-arg-vars ordered-args indy-res-vars ordered-ress
                               #`(values #,@wrapper-ress-as-list))
               (istx-is-chaperone-contract? an-istx)
               #f
               ordered-ress res-indices
               res-proj-vars indy-res-proj-vars
               wrapper-ress indy-res-vars
               post-indicies
               indy-arg-vars ordered-args indy-res-vars ordered-ress))]
          [args
           (bad-number-of-results blame val
                                  #,(length wrapper-ress-as-list)
                                  args)]))]
    [else
     null]))

(define-for-syntax (add-eres-lets an-istx res-proj-vars
                                  indy-arg-vars ordered-args indy-res-vars ordered-ress
                                  stx)
  (cond
    [(and (positive? (vector-length res-proj-vars))
          (istx-ress an-istx)
          (andmap eres? (istx-ress an-istx)))
     (for/fold ([body stx])
       ([an-arg/res (in-list (reverse (istx-ress an-istx)))]
        [res-proj-var (in-vector res-proj-vars (- (vector-length res-proj-vars) 1) -1 -1)])
       (if (arg/res-vars an-arg/res)
           #`(let ([#,(eres-eid an-arg/res)
                    (#,res-proj-var #,@(map (λ (var) (arg/res-to-indy-var indy-arg-vars
                                                                          ordered-args
                                                                          indy-res-vars
                                                                          ordered-ress
                                                                          var))
                                            (arg/res-vars an-arg/res)))])
               #,body)
           body))]
    [else stx]))

(define-for-syntax (mk-wrapper-func/blame-id-info stx an-istx used-indy-vars method?)

  (define-values (wrapper-proc-arglist
                  blame-ids args+rst
                  ordered-args arg-indices
                  ordered-ress res-indices
                  arg-proj-vars indy-arg-proj-vars
                  res-proj-vars indy-res-proj-vars
                  pre-indicies post-indicies)
    (build-wrapper-proc-arglist an-istx used-indy-vars))

  ;; hash[arg/res -o> identifier]
  (define wrapper-args (make-hasheq))
  (for ([an-arg/res (in-list (istx-args an-istx))])
    (hash-set! wrapper-args an-arg/res
               (car (generate-temporaries (list (arg/res-var an-arg/res))))))
  (when (istx-rst an-istx)
    (hash-set! wrapper-args (istx-rst an-istx) #'rest-args))

  ;; hash[arg/res -o> identifier]
  (define wrapper-ress (make-hasheq))
  (when (istx-ress an-istx)
    (for ([an-arg/res (in-list (istx-ress an-istx))])
      (hash-set! wrapper-ress an-arg/res
                 (car (generate-temporaries (list (arg/res-var an-arg/res)))))))

  ;; indy-arg-vars & indy-res-vars
  ;; contains `#f`s at the places where pre/post conditions go
  (define indy-arg-vars
    (for/list ([ordered-arg (in-list ordered-args)])
      (and (arg/res? ordered-arg)
           (car (generate-temporaries (list (arg/res-var ordered-arg)))))))
  (define indy-res-vars
    (for/list ([ordered-arg (in-list ordered-ress)])
      (and (arg/res? ordered-arg)
           (car (generate-temporaries (list (arg/res-var ordered-arg)))))))


  (define this-param (and method? (car (generate-temporaries '(this)))))

  (define wrapper-body
    (add-wrapper-let
     (add-pre-conds
      an-istx pre-indicies
      indy-arg-vars ordered-args indy-res-vars ordered-ress
      (add-eres-lets
       an-istx
       res-proj-vars
       indy-arg-vars ordered-args indy-res-vars ordered-ress
       (args/vars->arg-checker
        (build-result-checkers
         an-istx post-indicies
         ordered-ress res-indices
         res-proj-vars indy-res-proj-vars
         wrapper-ress indy-res-vars
         ordered-args indy-arg-vars)
        (istx-args an-istx)
        (istx-rst an-istx)
        wrapper-args
        this-param)))
     (istx-is-chaperone-contract? an-istx)
     #t
     ordered-args arg-indices
     arg-proj-vars indy-arg-proj-vars
     wrapper-args indy-arg-vars
     pre-indicies
     indy-arg-vars ordered-args indy-res-vars ordered-ress))
  (values
   (map cdr blame-ids)
   (with-syntax ([arg-checker (or (syntax-local-infer-name stx) 'arg-checker)])
     #`(λ #,wrapper-proc-arglist
         (λ (val neg-party)
           (define blame+neg-party (cons blame neg-party))
           (chk val #,method?)
           (c-or-i-procedure
            val
            (let ([arg-checker
                   (λ #,(args/vars->arglist an-istx wrapper-args this-param)
                     #,wrapper-body)])
              (make-keyword-procedure
               (λ (kwds kwd-args . args)
                 (with-contract-continuation-mark
                  blame+neg-party
                  (keyword-apply arg-checker kwds kwd-args args)))
               (λ args
                 (with-contract-continuation-mark
                  blame+neg-party
                  (apply arg-checker args)))))
            impersonator-prop:contracted ctc
            impersonator-prop:blame (blame-add-missing-party blame neg-party)))))))

(define-for-syntax (arg/res-to-indy-var indy-arg-vars ordered-args indy-res-vars ordered-ress var)
  (define (try vars ordered)
    (let loop ([iargs vars]
               [args ordered])
      (cond
        [(null? args) #f]
        [else
         (define arg (car args))
         (cond
           [(arg/res? arg)
            (define arg-var (arg/res-var (car args)))
            (define iarg (car iargs))
            (cond
              [(free-identifier=? var arg-var) iarg]
              [else (loop (cdr iargs) (cdr args))])]
           [else (loop (cdr iargs) (cdr args))])])))
  (or (try indy-arg-vars ordered-args)
      (try indy-res-vars ordered-ress)
      (error '->i "internal error; did not find a matching var for ~s" var)))

(define-for-syntax (build-wrapper-proc-arglist an-istx used-indy-vars)

  (define pre+args+rst (append (istx-pre an-istx)
                               (istx-args an-istx)
                               (if (istx-rst an-istx)
                                   (list (istx-rst an-istx))
                                   '())))
  (define res+post (append (istx-post an-istx)
                           (or (istx-ress an-istx) '())))
  (define-values (ordered-args arg-indices pre-indicies) (find-ordering pre+args+rst))
  (define-values (ordered-ress res-indices post-indicies) (find-ordering res+post))

  (define arg-proj-vars
    (for/vector ([pre+arg+rst (in-list pre+args+rst)])
      (and (arg/res? pre+arg+rst)
           (car (generate-temporaries (list (arg/res-var pre+arg+rst)))))))

  (define blame-ids (build-blame-ids ordered-args ordered-ress))

  ;; this vector is parallel to arg-proj-vars (so use arg-indices to find the right ones)
  ;; but it contains #fs in places where we don't need the indy projections (because the corresponding
  ;; argument is not dependened on by anything or this one is a pre/post condition)
  (define indy-arg-proj-vars
    (for/vector ([an-arg/res (in-list pre+args+rst)])
      (and (arg/res? an-arg/res)
           (maybe-generate-temporary
            (and (free-identifier-mapping-get used-indy-vars
                                              (arg/res-var an-arg/res)
                                              (λ () #f))
                 (arg/res-var an-arg/res))))))

  (define res-proj-vars
    (for/vector ([an-arg/res (in-list res+post)])
      (and (arg/res? an-arg/res)
           (car (generate-temporaries (list (arg/res-var an-arg/res)))))))

  ;; this list is parallel to res-proj-vars (so use res-indices to find the right ones)
  ;; but it contains #fs in places where we don't need the indy projections (because the
  ;; corresponding result is not dependened on by anything)
  (define indy-res-proj-vars
    (for/vector ([an-arg/res (in-list res+post)])
      (and (arg/res? an-arg/res)
           (maybe-generate-temporary
            (and (free-identifier-mapping-get used-indy-vars
                                              (arg/res-var an-arg/res)
                                              (λ () #f))
                 (arg/res-var an-arg/res))))))

  (define wrapper-proc-arglist
    #`(c-or-i-procedure chk ctc blame swapped-blame #,@(map car blame-ids)

           ;; the pre- and post-condition procs
           #,@(for/list ([pres (istx-pre an-istx)]
                         [i (in-naturals)])
                (string->symbol (format "pre-proc~a" i)))
           #,@(for/list ([pres (istx-post an-istx)]
                         [i (in-naturals)])
                (string->symbol (format "post-proc~a" i)))

           ;; first the non-dependent arg projections
           #,@(for/list ([arg/res (in-list pre+args+rst)]
                         [arg-proj-var (in-vector arg-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (not (arg/res-vars arg/res))))
                arg-proj-var)

           ;; then the dependent arg projections
           #,@(for/list ([arg/res (in-list pre+args+rst)]
                         [arg-proj-var (in-vector arg-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (arg/res-vars arg/res)))
                arg-proj-var)

           ;; then the non-dependent indy arg projections
           #,@(for/list ([arg/res (in-list pre+args+rst)]
                         [arg-proj-var (in-vector indy-arg-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (not (arg/res-vars arg/res))
                                     arg-proj-var))
                arg-proj-var)

           ;; then the non-dependent res projections
           #,@(for/list ([arg/res (in-list res+post)]
                         [res-proj-var (in-vector res-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (not (arg/res-vars arg/res))))
                res-proj-var)

           ;; then the dependent res projections
           #,@(for/list ([arg/res (in-list res+post)]
                         [res-proj-var (in-vector res-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (arg/res-vars arg/res)))
                res-proj-var)

           ;; then the non-dependent indy res projections
           #,@(for/list ([arg/res (in-list res+post)]
                         [indy-res-proj-var (in-vector indy-res-proj-vars)]
                         #:when (and (arg/res? arg/res)
                                     (not (arg/res-vars arg/res))
                                     indy-res-proj-var))
                indy-res-proj-var)))

  (values wrapper-proc-arglist
          blame-ids pre+args+rst
          ordered-args arg-indices
          ordered-ress res-indices
          arg-proj-vars indy-arg-proj-vars
          res-proj-vars indy-res-proj-vars
          pre-indicies post-indicies))

(define-for-syntax (build-call-to-original-function args rst vars this-param)
  (define argument-list
    (apply
     append
     (for/list ([arg (in-list args)]
                [var (in-vector vars)])
       (cond
         [(arg-kwd arg)
          (list (arg-kwd arg) var)]
         [else
          (list var)]))))
  (if rst
      #`(apply f #,@argument-list rest-args)
      #`(f #,@argument-list)))

(begin-encourage-inline
  (define (un-dep/maybe-chaperone orig-ctc obj blame neg-party chaperone? indy-blame?)
    (cond
      [(and (procedure? orig-ctc)
            (procedure-arity-includes? orig-ctc 1))
       (if (or indy-blame? (orig-ctc obj))
           obj
           (raise-predicate-blame-error-failure blame obj neg-party
                                                (contract-name orig-ctc)))]
      [(and indy-blame? (flat-contract? orig-ctc))
       obj]
      [else
       (define ctc (if chaperone?
                       (coerce-chaperone-contract '->i orig-ctc)
                       (coerce-contract '->i orig-ctc)))
       (((get/build-late-neg-projection ctc) blame) obj neg-party)]))

  (define (un-dep/chaperone orig-ctc obj blame neg-party indy-blame?)
    (un-dep/maybe-chaperone orig-ctc obj blame neg-party #t indy-blame?))

  (define (un-dep orig-ctc obj blame neg-party indy-blame?)
    (un-dep/maybe-chaperone orig-ctc obj blame neg-party #f indy-blame?)))

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
  (syntax-case stx ()
    [(_ . args)
     (->i-internal (syntax/loc stx (->i . args)) #|method?|# #f)]))

(define-for-syntax (->i-internal stx method?)
  (define an-istx (parse-->i stx))
  (define used-indy-vars (mk-used-indy-vars an-istx))
  (define-values (blame-ids wrapper-func)
    (mk-wrapper-func/blame-id-info stx an-istx used-indy-vars method?))
  (define args+rst (append (istx-args an-istx)
                           (if (istx-rst an-istx)
                               (list (istx-rst an-istx))
                               '())))
  (define args+rst+results
    (append (or (istx-ress an-istx) '())
            args+rst))
  (define this->i (gensym 'this->i))
  (with-syntax ([(arg-exp-xs ...)
                 (generate-temporaries
                  (filter values (map (λ (arg)
                                        (and (not (arg/res-vars arg)) (arg/res-var arg)))
                                      args+rst)))]
                [((arg-names arg-kwds arg-is-optional?s arg-exps) ...)
                 (filter values (map (λ (arg) (and (not (arg/res-vars arg))
                                                   (list
                                                    (arg/res-var arg)
                                                    (and (arg? arg) (arg-kwd arg))
                                                    (and (arg? arg) (arg-optional? arg))
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
                     (generate-temporaries (filter values (map (λ (res) (and (not (arg/res-vars res))
                                                                             (arg/res-var res)))
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

    (define (find-orig-vars ids arg/ress-to-look-in)
      (for/list ([an-id (in-list ids)])
        (define ans
          (for/or ([o-arg (in-list arg/ress-to-look-in)])
            (and (free-identifier=? an-id (arg/res-var o-arg))
                 (arg/res-var o-arg))))
        (unless ans
          (error 'contract/arr-i.rkt:find-orig-vars
                 "could not find ~s in ~s\n"
                 an-id arg/ress-to-look-in))
        ans))

    (define is-chaperone-contract? (istx-is-chaperone-contract? an-istx))

    #`(let ([arg-exp-xs (coerce-contract '->i arg-exps)] ...
            [res-exp-xs (coerce-contract '->i res-exps)] ...)
        #,(syntax-property
           #`(make-->i
              #,is-chaperone-contract?
              ;; the information needed to make the blame records and their new contexts
              '#,blame-ids
              ;; all of the non-dependent argument contracts
              (list (->i-arg 'arg-names 'arg-kwds arg-is-optional?s arg-exp-xs) ...)
              ;; all of the dependent argument contracts
              (list #,@(for/list ([arg (in-list args+rst)]
                                  #:when (arg/res-vars arg))
                         (define orig-vars (find-orig-vars (arg/res-vars arg) args+rst))
                         (define ctc-stx
                           (syntax-property
                            (syntax-property
                             (arg/res-ctc arg)
                             'racket/contract:negative-position
                             this->i)
                            'racket/contract:contract-on-boundary
                            (gensym '->i-indy-boundary)))
                         #`(λ (#,@orig-vars)
                             #,@(arg/res-vars arg) ;; needed for check syntax arrows
                             ;; this used to use opt/direct, but
                             ;; opt/direct duplicates code (bad!)
                             #,ctc-stx)))
              ;; then the non-dependent argument contracts that are themselves depended on
              (list #,@(filter values
                               (map (λ (arg/res indy-id)
                                      (and (free-identifier-mapping-get used-indy-vars
                                                                        (arg/res-var arg/res)
                                                                        (λ () #f))
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
                                   (find-orig-vars (arg/res-vars arg) args+rst+results))
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
                                         #,@(arg/res-vars arg) ;; needed for check syntax arrows
                                         (opt/c #,arg-stx))
                                     #`(λ (#,@orig-vars)
                                         #,@(arg/res-vars arg) ;; needed for check syntax arrows
                                         ;; this used to use opt/direct, but
                                         ;; opt/direct duplicates code (bad!)
                                         #,arg-stx))))
                    #''())
              #,(if (istx-ress an-istx)
                    #`(list #,@(filter values
                                       (map (λ (arg/res indy-id)
                                              (and (free-identifier-mapping-get used-indy-vars
                                                                                (arg/res-var arg/res)
                                                                                (λ () #f))
                                                   #`(cons '#,(arg/res-var arg/res) #,indy-id)))
                                            (filter (λ (arg/res)
                                                      (not (arg/res-vars arg/res)))
                                                    (istx-ress an-istx))
                                            (syntax->list #'(res-exp-xs ...)))))
                    #''())

              #,(let ([func (λ (pre/post vars-to-look-in)
                              (define orig-vars (find-orig-vars (pre/post-vars pre/post)
                                                                vars-to-look-in))
                              #`(λ #,orig-vars
                                  (void #,@(pre/post-vars pre/post))
                                  #,(pre/post-exp pre/post)))])
                  #`(list #,@(for/list ([pre (in-list (istx-pre an-istx))])
                               (func pre args+rst))
                          #,@(for/list ([post (in-list (istx-post an-istx))])
                               (func post args+rst+results))))

              #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg))
                                                          (not (arg-optional? arg))))
                                            (istx-args an-istx))))
              #,(length (filter values (map (λ (arg) (and (not (arg-kwd arg)) (arg-optional? arg)))
                                            (istx-args an-istx))))
              '#,(sort (filter values (map (λ (arg) (and (not (arg-optional? arg))
                                                         (arg-kwd arg)
                                                         (syntax-e (arg-kwd arg))))
                                           (istx-args an-istx)))
                       keyword<?)
              '#,(sort (filter values (map (λ (arg) (and (arg-optional? arg)
                                                         (arg-kwd arg)
                                                         (syntax-e (arg-kwd arg))))
                                           (istx-args an-istx)))
                       keyword<?)
              '#,(and (istx-rst an-istx) (arg/res-var (istx-rst an-istx)))
              #,method?
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
                       ,(arg-optional? an-arg)
                       ,(arg/res-quoted-dep-src-code an-arg)))
                 #,(if (istx-rst an-istx)
                       (if (arg/res-vars (istx-rst an-istx))
                           `(dep ,(syntax-e (arg/res-var (istx-rst an-istx)))
                                 ,(map syntax-e (arg/res-vars (istx-rst an-istx)))
                                 ,(arg/res-quoted-dep-src-code (istx-rst an-istx)))
                           `(nodep ,(syntax-e (arg/res-var (istx-rst an-istx)))))
                       #f)
                 #,(for/list ([pre (in-list (istx-pre an-istx))])
                     (list (map syntax-e (pre/post-vars pre))
                           (pre/post-kind pre)
                           (pre/post-quoted-dep-src-code pre)))
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
                            #f
                            ,(arg/res-quoted-dep-src-code a-res))))
                 #,(for/list ([post (in-list (istx-post an-istx))])
                     (list (map syntax-e (pre/post-vars post))
                           (pre/post-kind post)
                           (pre/post-quoted-dep-src-code post)))))
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
