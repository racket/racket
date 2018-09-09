#lang racket/base
(require "place-local.rkt"
         "check.rkt"
         "internal-error.rkt"
         "atomic.rkt")

(provide (struct-out node)
         
         thread-group?
         make-thread-group
         current-thread-group
         make-another-initial-thread-group

         ;; Used by scheduler and place creation
         root-thread-group
         thread-group-next!
         
         ;; Called by thread creation and termination:
         thread-group-add!
         thread-group-remove!
         
         thread-group-all-threads
         num-threads-in-groups)

;; Threads and thread groups subtype `node`:
(struct node ([prev #:mutable]
              [next #:mutable]))
(define (child-node child) child) ; a child instantiates a `node` subtype
(define (node-child n) n)

(struct thread-group node (parent
                           [chain-start #:mutable] ; all children
                           [chain #:mutable] ; children remaining to be scheduled round-robin
                           [chain-end #:mutable]))

(define (make-root-thread-group)
  (thread-group 'none 'none #f #f #f #f))

(define-place-local root-thread-group (make-root-thread-group))

(define-place-local num-threads-in-groups 0)

(define/who current-thread-group
  (make-parameter root-thread-group
                  (lambda (v)
                    (check who thread-group? v)
                    v)))

(define (make-another-initial-thread-group)
  (set! root-thread-group (make-root-thread-group)))

(define/who (make-thread-group [parent (current-thread-group)])
  (check who thread-group? parent)
  (define tg (thread-group 'none 'none parent #f #f #f))
  tg)

;; Called atomically in scheduler:
(define (thread-group-next! tg)
  (define n (thread-group-chain tg))
  (cond
   [(not n)
    (define n (thread-group-chain-start tg))
    (cond
     [(not n)
      ;; No children
      #f]
     [else
      (set-thread-group-chain! tg (node-next n))
      n])]
   [else
    (set-thread-group-chain! tg (node-next n))
    (node-child n)]))

(define (thread-group-add! parent child)
  (atomically
   (let loop ([parent parent] [child child])
     ;; Adding to the start of the group tends to reverse the schedule
     ;; order, but it also avoids a problem where two threads that
     ;; both loop and `sleep` (which deschedules and reschedules) take
     ;; turns and starve everything else.
     (define t (thread-group-chain-start parent))
     (define was-empty? (not t))
     (define n (child-node child))
     (unless (and (eq? (node-prev n) 'none)
                  (eq? (node-next n) 'none))
       (internal-error "thread-group-add!: thread or group is added already"))
     (set-node-next! n t)
     (set-node-prev! n #f)
     (if t
         (set-node-prev! t n)
         (set-thread-group-chain-end! parent n))
     (set-thread-group-chain-start! parent n)
     (unless (thread-group? child)
       (set! num-threads-in-groups (add1 num-threads-in-groups)))
     (when was-empty?
       ;; added child to formerly empty parent => add the parent
       (define parent-parent (thread-group-parent parent))
       (when parent-parent
         (loop parent-parent parent))))))

(define (thread-group-remove! parent child)
  (atomically
   (let loop ([parent parent] [child child])
     (define n (child-node child))
     (when (or (eq? (node-prev n) 'none)
               (eq? (node-next n) 'none))
       (internal-error "thread-group-remove!: thread or group is removed already"))
     (if (node-next n)
         (set-node-prev! (node-next n) (node-prev n))
         (set-thread-group-chain-end! parent (node-prev n)))
     (if (node-prev n)
         (set-node-next! (node-prev n) (node-next n))
         (set-thread-group-chain-start! parent (node-next n)))
     (when (eq? n (thread-group-chain parent))
       (set-thread-group-chain! parent (node-next n)))
     (set-node-next! n 'none)
     (set-node-prev! n 'none)
     (unless (thread-group? child)
       (set! num-threads-in-groups (sub1 num-threads-in-groups)))
     (when (not (thread-group-chain-end parent))
       ;; parent thread group is now empty, so remove it, too
       (define parent-parent (thread-group-parent parent))
       (when parent-parent
         (loop parent-parent parent))))))

(define (thread-group-all-threads parent accum)
  (cond
   [(not (thread-group? parent)) (cons parent accum)]
   [else
    (let loop ([n (thread-group-chain-start parent)] [accum accum])
      (cond
       [(not n) accum]
       [else (loop (node-next n)
                   (thread-group-all-threads (node-child n) accum))]))]))

