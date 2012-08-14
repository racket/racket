#lang racket/base

;; Analyzer for the sampler results

(require "structs.rkt" "utils.rkt" racket/list)

(provide analyze-samples (all-from-out "structs.rkt"))

(define-syntax-rule (with-hash <hash> <key> <expr> ...)
  (hash-ref! <hash> <key> (λ () <expr> ...)))

;; This function analyzes the output of the sampler.  Returns a `profile'
;; struct holding a list of `node' values, each one representing a node in the
;; call graph, with the relevant information filled in.  The results are sorted
;; using a topological sort from the top, and by the total time for nodes at
;; the same level.
(define (analyze-samples cpu-time+samples)
  (define cpu-time (car cpu-time+samples))
  (define samples  (cdr cpu-time+samples))
  (define samples-by-thread
    (let ([by-thread (split-by-thread samples)])
      (for ([samples (in-vector by-thread)] [i (in-naturals 0)])
        (vector-set! by-thread i (get-times samples)))
      by-thread))
  (define id+src->node-hash (make-hasheq))
  (define (id+src->node id+src)
    (with-hash id+src->node-hash id+src
      (node (car id+src) (cdr id+src) '() 0 0 '() '())))
  ;; special node that is the caller of toplevels and callee of leaves
  (define *-node (id+src->node '(#f . #f)))
  (define call->edge
    (let ([t (make-hasheq)])
      (λ (ler lee)
        (with-hash (with-hash t ler (make-hasheq)) lee
          (define e (edge 0 ler 0 lee 0))
          (set-node-callers! lee (cons e (node-callers lee)))
          (set-node-callees! ler (cons e (node-callees ler)))
          e))))
  (define total-time 0)
  (define thread-times (make-vector (vector-length samples-by-thread) 0))
  (for ([thread-samples (in-vector samples-by-thread)]
        [thread-id (in-naturals 0)]
        #:when #t
        [sample (in-list thread-samples)])
    (define msecs (car sample))
    (define (connect ler lee ler# lee#)
      (define edge (call->edge ler lee))
      (set-edge-caller-time! edge (+ (edge-caller-time edge) (/ msecs lee#)))
      (set-edge-callee-time! edge (+ (edge-callee-time edge) (/ msecs ler#)))
      edge)
    (define stack ; the stack snapshot, translated to `node' values
      (for/list ([id+src (in-list (cdr sample))])
        (define node (id+src->node id+src))
        (define tids (node-thread-ids node))
        (unless (memq thread-id tids)
          (set-node-thread-ids! node (cons thread-id tids)))
        node))
    (define counts (get-counts stack))
    (define stack+counts (map (λ (x) (assq x counts)) stack))
    (define edges
      (if (null? stack)
        '()
        (append (let ([first (car stack+counts)] [last (last stack+counts)])
                  (list (connect *-node (car last) 1 (cdr last))
                        (connect (car first) *-node (cdr first) 1)))
                (for/list ([callee (in-list stack+counts)]
                           [caller (in-list (cdr stack+counts))])
                  (connect (car caller) (car callee)
                           (cdr caller) (cdr callee))))))
    (set! total-time (+ msecs total-time))
    (for ([p (in-list counts)])
      (set-node-total! (car p) (+ msecs (node-total (car p)))))
    (for ([e (remove-duplicates edges eq?)])
      (set-edge-total! e (+ msecs (edge-total e))))
    (vector-set! thread-times thread-id
                 (+ msecs (vector-ref thread-times thread-id)))
    (when (pair? stack)
      (set-node-self! (car stack) (+ (node-self (car stack)) msecs))))
  (set-node-total! *-node total-time)
  ;; convert the nodes from the hash to a list, do a topological sort, and then
  ;; sort by total time (combining both guarantees(?) sensible order)
  (define nodes (append-map (λ (nodes) (sort nodes > #:key node-total))
                            (topological-sort *-node)))
  ;; sort all the edges in the nodes according to total time
  (for ([n (in-list nodes)])
    (set-node-callees! n (sort (node-callees n) > #:key edge-callee-time))
    (set-node-callers! n (sort (node-callers n) > #:key edge-caller-time)))
  (profile total-time
           cpu-time
           (length samples)
           (for/list ([time (in-vector thread-times)] [n (in-naturals 0)])
             (cons n time))
           nodes
           *-node))

;; Groups raw samples by their thread-id, returns a vector with a field for
;; each thread id holding the sample data for that thread.  The samples in
;; these are reversed (so they'll be sorted going forward in time).
(define (split-by-thread samples)
  (define threads
    (make-vector (add1 (for/fold ([n -1]) ([sample (in-list samples)])
                         (max (car sample) n)))
                 '()))
  (for ([sample (in-list samples)])
    (define id (car sample))
    (define data (cdr sample))
    (vector-set! threads id (cons data (vector-ref threads id))))
  threads)

(module+ test
  (require rackunit)
  (check-equal? (split-by-thread '())
                '#())
  (check-equal? (split-by-thread '([0 x]))
                '#([(x)]))
  (check-equal? (split-by-thread '([0 x] [0 y] [0 z]))
                '#([(z) (y) (x)]))
  (check-equal? (split-by-thread '([0 x] [1 y] [2 z]))
                '#([(x)] [(y)] [(z)]))
  (check-equal? (split-by-thread '([0 x1] [1 y1] [0 x2] [2 z1] [0 x3] [2 z2]))
                '#([(x3) (x2) (x1)] [(y1)] [(z2) (z1)])))

;; gets a list of thread-id and data for that thread beginning with the
;; millisecond count, and returns a similar list where the samples begin with
;; the time spent for that sample.  The time spent is taken as half of the two
;; touching ranges; for example, if there are three samples showing snapshot
;; times of 10, 20, 60, then the middle one is assumed to have a time of 25.
;; For the first and last samples, the time is twice the half of the single
;; touching range -- with this example, this would be 10 for the first and 40
;; for the last.  If there is a thread with just one sample, it is dropped.
(define (get-times samples)
  (cond
    ;; nothing to do
    [(null? samples) '()]
    ;; throw out a single sample
    [(null? (cdr samples)) '()]
    [else (let loop ([samples samples]
                     [prevs   (cons #f (map car samples))]
                     [r       '()])
            (if (null? samples)
              (reverse r)
              (let* ([prev (car prevs)]
                     [cur  (caar samples)]
                     [data (cdar samples)]
                     [prevs (cdr prevs)]
                     [samples (cdr samples)]
                     [next (and (pair? samples) (caar samples))])
                (loop samples prevs
                      (cons (cons (if next
                                    ;; not the last: there must be a next
                                    (if prev (/ (- next prev) 2) (- next cur))
                                    ;; last one: there must be a prev
                                    (- cur prev))
                                  data)
                            r)))))]))

(module+ test
  (check-equal? (get-times '())
                '())
  (check-equal? (get-times '([10 a]))
                '())
  (check-equal? (get-times '([10 a] [20 b]))
                '([10 a] [10 b]))
  (check-equal? (get-times '([10 a] [20 b] [60 c]))
                '([10 a] [25 b] [40 c]))
  (check-equal? (get-times '([10 a] [20 b] [30 c] [40 d]))
                '([10 a] [10 b] [10 c] [10 d]))
  (check-equal? (get-times '([10 a] [20 b] [60 c] [80 d]))
                '([10 a] [25 b] [30 c] [20 d])))

;; returns a list of (cons item occurrences) for the items in l
(define (get-counts l)
  (let loop ([l l] [r '()])
    (if (null? l)
      r
      (let ([1st (car l)])
        (let loop* ([l1 '()] [c 1] [l (cdr l)])
          (cond [(null? l) (loop l1 (cons (cons 1st c) r))]
                [(eq? 1st (car l)) (loop* l1 (add1 c) (cdr l))]
                [else (loop* (cons (car l) l1) c (cdr l))]))))))

(module+ test
  (check-equal? (get-counts '()) '())
  (check-equal? (get-counts '(1)) '([1 . 1]))
  (check-equal? (get-counts '(1 1 1)) '([1 . 3]))
  (define (set=? xs ys) (null? (append (remove* xs ys) (remove* ys xs))))
  (check set=? (get-counts '(1 2 3)) '([1 . 1] [2 . 1] [3 . 1]))
  (check set=? (get-counts '(1 2 2 3 3 3)) '([1 . 1] [2 . 2] [3 . 3]))
  (check set=? (get-counts '(3 1 2 3 2 3)) '([1 . 1] [2 . 2] [3 . 3])))
