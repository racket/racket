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
         generation-logger
         gen-state)

;; clause : head-pat eq/dqs (listof prem)
(define-struct clause (head-pat eq/dqs prems lang name) #:transparent)
;; prem : (-> (listof clauses)) pat
(define-struct prem (mk-clauses pat) #:transparent)
;; eq/dqs : (listof (or/c eq? dq?))
(define-struct eqn (lhs rhs) #:transparent)
;; ps : (listof symbol?) - the universally quantified variables ("parameters")
(define-struct dqn (ps lhs rhs) #:transparent)
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

(define gen-state (make-parameter (set 'shuffle-clauses 'success-jump)))

(define (shuffle-clauses?)
  (set-member? (gen-state) 'shuffle-clauses))

(define (success-jump?)
  (set-member? (gen-state) 'success-jump))


(define bt-count (make-parameter 0))
(define bt-limit (make-parameter 0))
(define (inc-bt-count) 
  (bt-count (add1 (bt-count))))

(define pushdown-count (make-parameter 0))
(define pushdown-limit (make-parameter 0))
(define (inc-pushdown-count)
  (pushdown-count (add1 (pushdown-count))))

(define (search/next clauses input bound lang)
  (define name-nums 0)
  (define fresh-pat (parameterize ([unique-name-nums 0])
                      (begin0
                       (fresh-pat-vars input (make-hash))
                       (set! name-nums (unique-name-nums)))))
  (define fs (list (fail-cont empty-env
                             (list (make-partial-rule fresh-pat (if (shuffle-clauses?)
                                                                    (shuffle clauses)
                                                                    (order-clauses clauses))
                                                      '() bound))
                             bound)))
  (define v-locs (make-hash))
  (λ ()
    (parameterize ([unique-name-nums name-nums]
                   [bt-count 0]
                   [bt-limit (* bound 10)]
                   [pushdown-count 0]
                   [pushdown-limit (* bound 200)])
      (define-values (ans fails)
        (with-handlers ([exn:fail:redex:search-failure? (λ (e) 
                                                          (define f-conts (exn:fail:redex:search-failure-fails e))
                                                          (values (unif-fail) (trim-fails f-conts)))])
          (define-values (env/f fails)
            (fail-back fs))
          (values (and/fail env/f (unify fresh-pat 'any env/f lang))
                  fails)))
      (set-last-gen-trace! (generation-trace))
      (when (success-jump?)
        (set! fs (shuffle-fails fails)))  ;; how to test if we're randomizing here?
      (set! name-nums (unique-name-nums))
      ans)))
  
(define (trim-fails fs)
  (define rev-fs (reverse fs))
  (reverse
   (let loop ([rfs rev-fs])
     (match rfs
       [(cons (fail-cont _1 _2 (? (λ (b) (< b 0)) bound)) rest)
        (loop rest)]
       [_
        rfs]))))

(define (shuffle-fails fs)
  (cond 
    [((length fs) . > . 0)
     (define-values (ls rs) (split-at fs (random (length fs))))
     (append rs ls)]
    [else fs]))

(define (fail-back fs)
  (inc-bt-count)
  (match fs
    [(list (fail-cont e f b) rest ...)
     (choose-rule e f rest)]
    [_ (values #f fs)]))

(define (choose-rule env fringe fail)
  (cond
    [(empty? fringe)
     (values env fail)]
    [else
     (push-down (car fringe) env (cdr fringe) fail)]))

(define (push-down a-partial-rule env fringe fail)
  (inc-pushdown-count)
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
                                      (if (shuffle-clauses?)
                                          (shuffle prem-cls)
                                          (order-clauses prem-cls))
                                      (order-clauses prem-cls))
                                  (cons n tr-loc)
                                  (- bound 1))))
           (define new-fringe (append new-fringe-elements
                                      fringe))
           (choose-rule (p*e-e res-pe)
                        new-fringe
                        (cons (fail-cont env failure-fringe bound) fail))])])]))

(define (order-clauses cs)
  (define num-prems->cs (hash))
  (for ([c cs])
    (set! num-prems->cs
          (hash-set num-prems->cs
                    (length (clause-prems c))
                    (set-add
                     (hash-ref num-prems->cs
                               (length (clause-prems c))
                               (λ () (set)))
                     c))))
  (apply append
         (for/list ([k (sort (hash-keys num-prems->cs) <)])
           (shuffle (set->list (hash-ref num-prems->cs k))))))
  


(define (do-unification clse input env)
  (match clse
    [(clause head-pat eq/dqs prems lang name)
     (define-values (eqs dqs) (partition eqn? eq/dqs))
     (define env1
       (let loop ([e env]
                  [dqs dqs])
         (match dqs
           ['() e]
           [(cons (dqn ps lhs rhs) rest)
            (dqn ps lhs rhs)
            (define u-res (disunify ps lhs rhs e lang))
            (and u-res
                 (loop u-res rest))])))
     (define head-p*e (and/fail env1 (unify input head-pat env1 lang)))
     (cond
       [(not-failed? head-p*e)
        (define res-p (p*e-p head-p*e))
        (let loop ([e (p*e-e head-p*e)]
                   [eqs eqs])
          (match eqs
            ['() 
             (p*e (p*e-p head-p*e) e)]
            [(cons (eqn lhs rhs) rest)
             (define u-res (unify lhs rhs e lang))
             (and (not-failed? u-res)
                  (loop (p*e-e u-res) rest))]))]
       [else #f])]))

(define (fresh-clause-vars clause-raw)
  (define instantiations (make-hash))
  (struct-copy clause clause-raw
               [head-pat (fresh-pat-vars (clause-head-pat clause-raw) instantiations)]
               [eq/dqs (for/list ([eq/dq (in-list (clause-eq/dqs clause-raw))])
                         (match eq/dq
                           [(eqn lhs rhs)
                            (eqn (fresh-pat-vars lhs instantiations)
                                 (fresh-pat-vars rhs instantiations))]
                           [(dqn ps lhs rhs)
                            (dqn (map (λ (id) (hash-ref instantiations id
                                                       (λ () 
                                                         (define unique-id (make-uid id))
                                                         (hash-set! instantiations id unique-id)
                                                         unique-id)))
                                      ps)
                                 (fresh-pat-vars lhs instantiations)
                                 (fresh-pat-vars rhs instantiations))]))]
               [prems (for/list ([p (clause-prems clause-raw)])
                        (match p
                          [(prem mk-clauses pat)
                           (prem mk-clauses (fresh-pat-vars pat instantiations))]))]))

(define-struct (exn:fail:redex:search-failure exn:fail:redex) (fails))

(define (check-depth-limits bound tr-loc fails)
  (when ((pushdown-count) . > . (pushdown-limit))
    (define str (format "pushdown count of ~s exceeded at ~s" (pushdown-count) tr-loc))
    ;(printf "!*\n\t~s\t*!\n" str)
    (raise (make-exn:fail:redex:search-failure str (current-continuation-marks) fails)))
  (when (> (bt-count) (bt-limit))
    (define str (format "backtrack count of ~s exceeded at ~s" (bt-limit) tr-loc))
    (raise (make-exn:fail:redex:search-failure str (current-continuation-marks) fails)))
  (when (> (length tr-loc) (* 3 (+ (length tr-loc) bound)))
    (define str (format "depth bound exceeded at depth: ~s" (length tr-loc)))
    (raise (make-exn:fail:redex:search-failure str (current-continuation-marks) fails))))


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