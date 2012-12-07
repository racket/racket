#lang racket/base

(require "pat-unify.rkt" 
         "error.rkt"
         racket/match
         racket/list
         racket/contract
         racket/set
         (for-syntax racket/base))

(provide (struct-out clause)
         (struct-out prem)
         (struct-out eqn)
         (struct-out dqn)
         search/next
         (struct-out gen-trace))

;; search tracing facility
(provide enable-gen-trace!
         disable-gen-trace!
         last-gen-trace
         get-most-recent-trace
         update-gen-trace!
         generation-logger)

;; clause : head-pat eq/dqs (listof prem)
(define-struct clause (head-pat eq/dqs prems lang name) #:transparent)
;; prem : (-> (listof clauses)) pat
(define-struct prem (mk-clauses pat) #:transparent)
;; eq/dqs : (listof (or/c eq? dq?))
(define-struct eqn (lhs rhs) #:transparent)
(define-struct dqn (lhs rhs) #:transparent)
(define (prem-clauses prem) ((prem-mk-clauses prem)))

(define-struct partial-rule (pat clauses tr-loc bound) 
  #:transparent)
;; fringe = (listof partial-rule)
;; partial-rule = (partial-rule pat (listof clause) tr-frame)
;; the partial rule is the order we're going to process a given rule in. 
;; If above the bound, we randomize them before putting them into the fringe.

(define-struct fail-cont (env fringe bound)
  #:transparent)

(define-struct gen-trace (tr-loc clause input state bound env) #:prefab)

(define (search/next clauses input bound lang)
  (define name-nums 0)
  (define fresh-pat (parameterize ([unique-name-nums 0])
                      (begin0
                       (fresh-pat-vars input (make-hash))
                       (set! name-nums (unique-name-nums)))))
  (define fs (list (fail-cont empty-env
                             (list (make-partial-rule fresh-pat (shuffle clauses) '() bound))
                             bound)))
  (define v-locs (make-hash))
  (λ ()
    (parameterize ([unique-name-nums name-nums]
                   [visited-locs v-locs])
      (define-values (ans fails)
        (with-handlers ([exn:fail:redex:search-failure? (λ (e) 
                                                          (define f-conts (exn:fail:redex:search-failure-fails e))
                                                          (values #f (trim-fails f-conts)))])
          (define-values (env/f fails)
            (fail-back fs))
          (values (and env/f (unify fresh-pat 'any env/f lang))
                  fails)))
      (set-last-gen-trace! (generation-trace))
      (set! fs (trim-fails fails))
      (set! name-nums (unique-name-nums))
      (set! v-locs (visited-locs))
      ans)))
  
(define (trim-fails fs)
  (define rev-fs (reverse fs))
  (reverse
   (let loop ([rfs rev-fs])
     (match rfs
       [(cons (fail-cont _1 _2 (? (λ (b) (< b 0)) bound)) rest)
        (loop rest)]
       [else
        rfs]))))

(define (shuffle-fails fs)
  (cond 
    [((length fs) . > . 0)
     (define-values (ls rs) (split-at fs (random (length fs))))
     (append rs ls)]
    [else fs]))

(define (fail-back fs)
  (match fs
    [(list (fail-cont e f b) rest ...)
     (choose-rule e f rest)]
    [else (values #f fs)]))

(define (choose-rule env fringe fail)
  ;(-> env? (listof partial-rule?) number? (-> (or/c env? #f)) (or/c env? #f))
  (cond
    [(empty? fringe)
     (values env fail)]
    [else
     (define new-f fringe #;(prune-fringe fringe env))
     (if new-f
         (push-down (car new-f) env (cdr new-f) fail)
         (fail-back fail))]))

(define (push-down a-partial-rule env fringe fail)
  (match a-partial-rule
    [(partial-rule pat clauses tr-loc bound)
     (check-depth-limits bound tr-loc fail)
     (cond
       [(null? clauses)
        (fail-back fail)]
       [else
        (define the-clause (fresh-clause-vars (car clauses)))
        (define res-pe (do-unification the-clause pat env))
        (when (log-receiver? gen-log-recv)
          (log-message (current-logger) 'info (symbol->string (clause-name the-clause))
                       (gen-trace tr-loc the-clause pat (and res-pe #t) bound env)))
        (define failure-fringe
          (cons (struct-copy partial-rule
                             a-partial-rule
                             [clauses (cdr clauses)])
                fringe))
        (cond
          [(not res-pe)
           (choose-rule env failure-fringe fail)]
          [else
           (define new-fringe-elements
             (for/list ([prem (in-list (clause-prems the-clause))]
                        [n (in-naturals)])
               (define prem-cls (prem-clauses prem))
               (make-partial-rule (prem-pat prem) 
                                  (if (positive? bound)
                                      #;(shuffle-clauses-stlc prem-cls (sub1 bound))
                                      (shuffle prem-cls)
                                      prem-cls)
                                  (cons n tr-loc)
                                  (- bound 1))))
           (define new-fringe (append new-fringe-elements
                                      fringe))
           (choose-rule (p*e-e res-pe)
                        new-fringe
                        (cons (fail-cont env failure-fringe bound) fail))])])]))


(define (do-unification clse input env)
  (match clse
    [(clause head-pat eq/dqs prems lang name)
     (define env1
       (let loop ([e env]
                  [eqdqs eq/dqs])
          (match eqdqs
            ['() e]
            [(cons eqdq rest)
             (match eqdq
               [(eqn lhs rhs)
                (loop e rest)]
               [(dqn lhs rhs)
                (define u-res (disunify lhs rhs e lang))
                (and u-res
                     (loop (trim-dqs u-res rhs) rest))])])))
     (define head-p*e (and env1 (unify input head-pat env1 lang)))
     (cond
       [head-p*e
        (define res-p (p*e-p head-p*e))
        (let loop ([e (p*e-e head-p*e)]
                   [eqdqs eq/dqs])
          (match eqdqs
            ['() 
             (p*e (p*e-p head-p*e) e)]
            [(cons eqdq rest)
             (match eqdq
               [(eqn lhs rhs)
                (define u-res (unify lhs rhs e lang))
                (and u-res
                     (loop (p*e-e u-res) rest))]
               [(dqn lhs rhs)
                (loop e rest)])]))]
       [else #f])]))

(define (trim-dqs e pat)
  (define p-vars
    (let loop ([p pat])
    (match p
      [`(name ,id ,pat)
       (set-union (set id)
                  (loop pat))]
      [`(list ,pats ...)
       (apply set-union (for/list ([p pats])
                          (loop p)))]
      [_ (set)])))
  (struct-copy env e
               [dqs (for/list ([dq (env-dqs e)])
                      (trim-dq-vars dq p-vars))]))

;; remove variables from lhs patterns from dqs
;; since we only care about constraints on the input pattern
;; if it's on one side of one equation, drop it
;; if it's on one side of two equations, eliminate it
(define (trim-dq-vars dq vs)
  (define dqps0 (dq->dq-pairs dq))
  (define var-vals (make-hash))
  (define (update-vvs id val)
    (hash-set! var-vals id 
                    (set-union (set val)
                               (hash-ref var-vals id (set)))))
  (define (eliminable? id) (set-member? vs id))
  (define dqps
    (for/fold ([new-dps '()])
      ([d dqps0])
      (match d
        [(dqp `(name ,(? eliminable? id) ,_) r)
         (update-vvs id r)
         new-dps]
        [(dqp l `(name ,(? eliminable? id) ,_))
         (update-vvs id l)
         new-dps]
        [else
         (cons d new-dps)])))
  (define-values (ls0 rs0)
    (for/fold ([l '()] [r '()])
      ([(id vals) (in-hash var-vals)])
      (cond
        [(< (set-count vals) 2)
         (values l r)]
        [else
         (for/fold ([l1 l] [r1 r])
           ([a-pair (in-list (for*/list ([a vals] [b (set-remove vals a)])
                             (list a b)))])
           (values (cons (first a-pair) l1)
                   (cons (second a-pair) r1)))])))
  (let loop ([ps dqps]
             [new-ls ls0]
             [new-rs rs0])
    (match ps
      ['()
       (list (cons 'list (reverse new-ls)) 
             (cons 'list (reverse new-rs)))]
      [(cons (dqp (list 'name id bnd) r) ps+)
       (if (set-member? vs id)
           (loop ps+ new-ls new-rs)
           (loop ps+ 
                 (cons (list 'name id bnd) new-ls)
                 (cons r new-rs)))]
      [else
       (error 'disunify "bad-dq: ~s" dq)])))

(define-struct dqp (l r))

(define (dq->dq-pairs dq)
  (map (λ (a b) (dqp a b)) 
       (cdr (first dq)) 
       (cdr (second dq))))
           
         

;; TODO: compare with free-identifier=? so renaming is safe
;; w/r/t macro expansion
;; (use free-id-table)
(define (fresh-pat-vars pre-pat instantiations)
  (match pre-pat
    [`(name ,id ,pat)
     (define new-id (hash-ref instantiations id
                              (λ ()
                                (define unique-id (make-uid id))
                                (hash-set! instantiations id unique-id)
                                unique-id)))
     `(name ,new-id ,(fresh-pat-vars pat instantiations))]
    [`(list ,pats ...)
     `(list ,@(for/list ([p pats]) (fresh-pat-vars p instantiations)))]
    [_ pre-pat]))

(define (make-uid id)
  (let ([uid-num (unique-name-nums)])
    (unique-name-nums (add1 uid-num))
    (string->symbol (string-append (symbol->string id) "_" (number->string uid-num)))))

(define (fresh-clause-vars clause-raw)
  (define instantiations (make-hash))
  (struct-copy clause clause-raw
               [head-pat (fresh-pat-vars (clause-head-pat clause-raw) instantiations)]
               [eq/dqs (for/list ([eq/dq (in-list (clause-eq/dqs clause-raw))])
                         (match eq/dq
                           [(eqn lhs rhs)
                            (eqn (fresh-pat-vars lhs instantiations)
                                 (fresh-pat-vars rhs instantiations))]
                           [(dqn lhs rhs)
                            (dqn (fresh-pat-vars lhs instantiations)
                                 (fresh-pat-vars rhs (make-hash)))]))]
               [prems (for/list ([p (clause-prems clause-raw)])
                        (match p
                          [(prem mk-clauses pat)
                           (prem mk-clauses (fresh-pat-vars pat instantiations))]))]))

(define visited-locs (make-parameter (make-hash)))

(define-struct (exn:fail:redex:search-failure exn:fail:redex) (fails))

(define (check-depth-limits bound tr-loc fails)
  (when (> (length tr-loc) (* 3 (+ (length tr-loc) bound)))
    (define str (format "depth bound exceeded at depth: ~s" (length tr-loc)))
    (raise (make-exn:fail:redex:search-failure str (current-continuation-marks) fails))))

(define (check-backtrack-limits tr-loc)
  (define v-locs (visited-locs))
  (when (< 5 (hash-ref v-locs tr-loc 0))
    (define str (format "backtracking limit exceeded at location: ~s" tr-loc))
    (raise (make-exn:fail:redex:search-failure str (current-continuation-marks))))
  (define loc-count (hash-ref v-locs tr-loc 0))
  (for ([(k v) (in-hash v-locs)])
    (let loop ([l1 tr-loc]
               [l2 k])
      (cond
        [(null? l1)
         (void)]
        [(null? l2)
         (hash-set! v-locs k 0)]
        [(= (car l1) (car l2))
         (loop (cdr l1) (cdr l2))]
        [else
         (void)])))
  (hash-set! v-locs tr-loc (add1 (hash-ref v-locs tr-loc 0))))

(define unique-name-nums (make-parameter 0))


(define generation-logger (make-logger 'generation-log (current-logger)))

(define gen-log-recv #f)

(define (enable-gen-trace!) 
  (set! gen-log-recv (make-log-receiver generation-logger 'info)))

(define (disable-gen-trace!)
  (set! gen-log-recv #f))

(define (get-most-recent-trace)
  (and (log-receiver? gen-log-recv)
       most-recent-trace))

(define (generation-trace)
  (if (log-receiver? gen-log-recv)
      (let loop ()
        (define next (sync/timeout 0 gen-log-recv))
        (if next
            (cons next (loop))
            '()))
      #f))

(define (update-gen-trace!)
  (set-last-gen-trace! (generation-trace)))

(define most-recent-trace #f)

(define (last-gen-trace)
  most-recent-trace)

(define (set-last-gen-trace! trace)
  (set! most-recent-trace trace))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the following is experimental and is not currently used....


(define (prune-fringe fringe env)
  (let/ec fail
    (define new-f
      (for/list ([a-p-rule (in-list fringe)])
      (define new-cs (for/list ([c (in-list (partial-rule-clauses a-p-rule))] 
                                #:when (do-unification (fresh-clause-vars c) (partial-rule-pat a-p-rule) env))
                       c))
      (when (empty? new-cs)
        (fail #f))
      (struct-copy partial-rule 
                   a-p-rule
                   [clauses new-cs])))
    (define candidate-length (length (partial-rule-clauses (car new-f))))
    (if (< candidate-length 2)
        new-f
        (let loop ([unchecked new-f]
                   [checked '()])
          (cond
            [(empty? unchecked)
             (reverse checked)]
            [(< (length (partial-rule-clauses (car unchecked))) candidate-length)
             (cons (car unchecked) (append (reverse checked) (cdr unchecked)))]
            [else
             (loop (cdr unchecked) (cons (car unchecked) checked))])))))
  

(define bound-fac
  (let ([memo (hash)])
    (λ (b)
      (hash-ref memo b (λ () (* 1000 (- 1 (exp (- (* 1.75 b))))))))))
  
;; specific to examples/chalmers/stlc
;; waht about preferring bound variables to constants
(define (shuffle-clauses-stlc clauses bound)
  (cond
    [(= (length clauses) 5) ;; typeof - last 3 recur
     (define-values (base v-rec)
       (split-at clauses 2))
     (define v-c (car v-rec))
     (define rec (cdr v-rec))
     (if (< (bound-fac bound) (random 1000))
         (append (shuffle base) (shuffle v-rec))
         (append (shuffle v-rec) (shuffle base)))]
    [(= (length clauses) 4) ;; const - all base
     (shuffle clauses)]
    [(= (length clauses) 3) ;; lookup - #2 recurs
     (define-values (base rec)
       (values (list (list-ref clauses 0) (list-ref clauses 2))
               (list (list-ref clauses 1))))
     (if (< (bound-fac bound) (random 1000))
         (append (shuffle base) (shuffle rec))
         (append (shuffle rec) (shuffle base)))]))

(define (better-shuffle-clauses clauses bound)
  (cond
    [(zero? (random 2))
     (shuffle clauses)]
    [(= (length clauses) 5) ;; typeof - last 3 recur
     (define app-weight (* (terms/depth bound) (terms/depth bound)))
     (define lam-weight (terms/depth bound))
     (define var-weight (vars/depth bound))
     (define cst-weight 3)
     (define num-weight 1)
     (define total (+ app-weight lam-weight var-weight cst-weight num-weight))
     (define rnd (big-random total))
     (define idx (cond [(rnd . < . app-weight) 4]
                       [(rnd . < . (+ lam-weight app-weight)) 3]
                       [(rnd . < . (+ lam-weight app-weight var-weight)) 2]
                       [(rnd . < . (+ lam-weight app-weight var-weight cst-weight)) 1]
                       [else 0]))
     (define-values (h t) (split-at clauses idx))
     (cons (list-ref clauses idx)
           (shuffle (append h (cdr t))))]
    [(= (length clauses) 4) ;; const - all base
     (shuffle clauses)]
    [(= (length clauses) 3) ;; lookup - #2 recurs
     (define rec-weight (vars/depth bound))
     (if ((big-random (+ rec-weight 2)) . < . 2)
         (append (shuffle (list (list-ref clauses 0) (list-ref clauses 2)))
                 (list (list-ref clauses 2)))
         (append (list (list-ref clauses 2))
                 (shuffle (list (list-ref clauses 0) (list-ref clauses 2)))))]
    #;[else
     (shuffle clauses)]))

(define (big-random n)
  (if (n . < . 4294967087)
      (random n)
      (+ (random 4294967087) (big-random (- n 4294967087)))))


(define (shuffle-clauses-poly-stlc clauses bound)
  (cond
    [(equal? (clause-name (car clauses)) 'typeof) ;; typeof - last 4 recur
     (define-values (base rec)
       (split-at clauses 2))
     (if (< (bound-fac bound) (random 1000))
         (append (shuffle base) (shuffle rec))
         (append (shuffle rec) (shuffle base)))]
    [(equal? (clause-name (car clauses)) 'const-type) ;; const - all base
     (shuffle clauses)]
    [(equal? (clause-name (car clauses)) 'lookup) ;; lookup - #2 recurs
     (define-values (base rec)
       (values (list (list-ref clauses 0) (list-ref clauses 2))
               (list (list-ref clauses 1))))
     (if (< (bound-fac bound) (random 1000))
         (append (shuffle base) (shuffle rec))
         (append (shuffle rec) (shuffle base)))]
    [else
     (shuffle clauses)]))

(define-syntax (define-memo stx)
  (syntax-case stx ()
    [(_ (f-id args ...) body)
     #'(define f-id
         (let ([memo (make-hash)])
           (λ (args ...)
             (hash-ref memo (list args ...)
                       (λ ()
                         (define res (begin body))
                         (hash-set! memo (list args ...) res)
                         res)))))]))

(define-memo (terms/depth d)
  (cond
    [(= d 0) 1]
    [else
     (+ 1 
        (vars/depth (sub1 d)) ;; vars 
        (terms/depth (sub1 d)) ;; lambda
        (* (terms/depth (sub1 d)) ;; app
           (terms/depth (sub1 d))))]))

(define-memo (vars/depth d)
  (cond
    [(= d 0) 1]
    [else
     (+ 1
        (vars/depth (sub1 d)))]))


(define (score-rule rule env)
  (match rule
    [(partial-rule pat clauses tr-loc bound)
     (length
      (filter (λ (c)
                (do-unification (fresh-clause-vars c) pat env))
              clauses))]))

(define (pick-rule rules env)
  (let loop ([rs-seen '()]
             [rs rules])
    (match rs
      ['() rs-seen]
      [(cons r rs*)
       (if ((score-rule r env) . < . 2)
           (cons r (append rs* rs-seen))
           (loop (cons r rs-seen) rs*))])))

(define (sort-rules rules env)
  (define weights (sort (for/list ([i (length rules)])
                          (list (score-rule (list-ref rules i) env) i))
                        <
                        #:key first))
  (for/list ([w (in-list weights)])
    (list-ref rules (second w))))

