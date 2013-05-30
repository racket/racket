#lang racket/base

(require "structs.rkt" racket/list)

;; Format a percent number, possibly doing the division too.  If we do the
;; division, then be careful: if we're dividing by zero, then make the result
;; zero.  This is useful if the total time is zero because we didn't see any
;; activity (for example, the profiled code is just doing a `sleep'), in which
;; case all times will be 0.
(provide format-percent)
(define format-percent
  (case-lambda
    [(percent) (define p (inexact->exact (round (* percent 1000))))
               (format "~a.~a%" (quotient p 10) (modulo p 10))]
    [(x y) (format-percent (if (zero? y) 0 (/ x y)))]))

(provide format-source)
(define (format-source src)
  (if src
    (format "~a:~a"
            (srcloc-source src)
            (if (srcloc-line src)
              (format "~a:~a" (srcloc-line src) (srcloc-column src))
              (format "#~a" (srcloc-position src))))
    "(unknown source)"))

;; Hide a node if its self time is smaller than the self threshold *and* all of
;; its edges are below the sub-node threshold too -- this avoids confusing
;; output where a node does not have an entry but appears as a caller/callee.
(provide get-hidden)
(define (get-hidden profile hide-self% hide-subs%)
  (define self% (or hide-self% 0))
  (define subs% (or hide-subs% 0))
  (define total-time (profile-total-time profile))
  (define (hide? node)
    (define (hide-sub? get-subs edge-sub edge-sub-time)
      (define %s
        (map (λ (edge)
               (define total (node-total (edge-sub edge)))
               (if (zero? total) 0 (/ (edge-sub-time edge) total)))
             (get-subs node)))
      (subs% . >= . (apply max %s)))
    (and (self% . >= . (/ (node-self node) total-time))
         (hide-sub? node-callees edge-callee edge-caller-time)
         (hide-sub? node-callers edge-caller edge-callee-time)))
  (cond [(and (<= self% 0) (<= subs% 0)) '()]
        [(zero? total-time) (profile-nodes profile)]
        [else (filter hide? (profile-nodes profile))]))

;; A topological sort of nodes, starting from node `root' (which will be given
;; as the special *-node).  The result is a list of node lists, each one
;; corresponds to one level.  Conceptually, the root node is always the only
;; item in the first level, so it is not included in the result.  This is done
;; by assigning layers to nodes in a similar way to section 9.1 of "Graph
;; Drawing: Algorithms for the Visualization of Graphs" by Tollis, Di Battista,
;; Eades, and Tamassia.  It uses a technique similar to the one described in
;; section 9.4 for removing cycles in the input graph, but improved by the fact
;; that we have weights on input/output edges (this is the only point that is
;; specific to the fact that it's a profiler graph).  Note that this is useful
;; for a graphical rendering of the results, but it's also useful to sort the
;; results in a way that makes more sense.
(provide topological-sort)
(define (topological-sort root)

  ;; Make `nodes+io-times' map a node to an mcons of total input and total
  ;; output times ignoring edges to/from the *-node and self edges, the order
  ;; is the reverse of how we scan the graph
  (define (get-node+io node)
    (define (sum node-callers/lees edge-caller/lee edge-callee/ler-time)
      (for/fold ([sum 0]) ([e (in-list (node-callers/lees node))])
        (define n (edge-caller/lee e))
        (if (or (eq? n node) (eq? n root)) sum
            (+ sum (edge-callee/ler-time e)))))
    (cons node (mcons (sum node-callers edge-caller edge-callee-time)
                      (sum node-callees edge-callee edge-caller-time))))
  (define nodes+io-times
    (let loop ([todo (list root)] [r '()])
      (if (pair? todo)
        (let* ([cur (car todo)] [todo (cdr todo)]
               [r (if (eq? cur root) r (cons (get-node+io cur) r))])
          (loop (append todo ; append new things in the end, so it's a BFS
                        (filter-map (λ (e)
                                      (define lee (edge-callee e))
                                      (and (not (memq lee todo))
                                           (not (assq lee r))
                                           lee))
                                    (node-callees cur)))
                r))
        ;; note: the result does not include the root node
        r)))

  ;; Now create a linear order similar to the way section 9.4 describes, except
  ;; that this uses the total caller/callee times to get an even better
  ;; ordering (also, look for sources and sinks in every step).  Note that the
  ;; list we scan is in reverse order.
  (define acyclic-order
    (let loop ([todo nodes+io-times] [rev-left '()] [right '()])
      ;; heuristic for best sources: the ones with the lowest intime/outtime
      (define (best-sources)
        (let loop ([todo todo] [r '()] [best #f])
          (if (null? todo)
            r
            (let* ([1st (car todo)]
                   [rest (cdr todo)]
                   [ratio (/ (mcar (cdr 1st)) (mcdr (cdr 1st)))])
              (if (or (not best) (ratio . < . best))
                (loop rest (list 1st) ratio)
                (loop rest (if (ratio . > . best) r (cons 1st r)) best))))))
      (if (pair? todo)
        (let* ([sinks   (filter (λ (x) (zero? (mcdr (cdr x)))) todo)]
               [todo    (remq* sinks todo)]
               [sources (filter (λ (x) (zero? (mcar (cdr x)))) todo)]
               ;; if we have no sources and sinks, use the heuristic
               [sources (if (and (null? sinks) (null? sources))
                          (best-sources) sources)]
               [todo    (remq* sources todo)]
               [sinks   (map car sinks)]
               [sources (map car sources)])
          ;; remove the source and sink times from the rest
          (for* ([nodes (in-list (list sources sinks))]
                 [n (in-list nodes)])
            (for ([e (in-list (node-callees n))])
              (define x (assq (edge-callee e) todo))
              (when x
                (set-mcar! (cdr x) (- (mcar (cdr x)) (edge-callee-time e)))))
            (for ([e (in-list (node-callers n))])
              (define x (assq (edge-caller e) todo))
              (when x
                (set-mcdr! (cdr x) (- (mcdr (cdr x)) (edge-caller-time e))))))
          (loop todo (append (reverse sources) rev-left) (append sinks right)))
        ;; all done, get the order
        (append (reverse rev-left) right))))

  ;; We're done, so make `t' map nodes to their callers with only edges that
  ;; are consistent with this ordering
  (define t
    (let ([t (make-hasheq)])
      (let loop ([nodes acyclic-order])
        (when (pair? nodes)
          (define ler (car nodes))
          (define rest (cdr nodes))
          (unless (hash-ref t ler #f) (hash-set! t ler '()))
          (for ([e (in-list (node-callees ler))])
            (define lee (edge-callee e))
            (when (memq lee rest) ; only consistent edges
              ;; note that we connect each pair of nodes at most once, and
              ;; never a node with itself
              (hash-set! t lee (cons ler (hash-ref t lee '())))))
          (loop rest)))
      t))

  ;; finally, assign layers using the simple method from section 9.1: sources
  ;; are at 0, and other nodes are placed at one layer after their parents
  (define height 0)
  (for ([node (in-list acyclic-order)])
    (let loop ([node node])
      (define x (hash-ref t node))
      (if (number? x)
        x
        (let ([max (add1 (for/fold ([m -1]) ([ler (in-list x)])
                           (max m (loop ler))))])
          (when (max . > . height) (set! height max))
          (hash-set! t node max)
          max))))
  (define layers (make-vector (add1 height) '()))
  (for ([node (in-list acyclic-order)])
    (unless (eq? node root) ; filter out the root
      (define l (hash-ref t node))
      (vector-set! layers l (cons node (vector-ref layers l)))))
  ;; in almost all cases, the root is the full first layer (in a few cases it
  ;; can be there with another node, eg (* -> A 2-> B 3-> A)), but be safe and
  ;; look for any empty layer
  (filter pair? (vector->list layers)))

;; gets a list of thread-id and data for that thread beginning with the
;; millisecond count, and returns a similar list where the samples begin with
;; the time spent for that sample.  The time spent is taken as half of the two
;; touching ranges; for example, if there are three samples showing snapshot
;; times of 10, 20, 60, then the middle one is assumed to have a time of 25.
;; For the first and last samples, the time is twice the half of the single
;; touching range -- with this example, this would be 10 for the first and 40
;; for the last.  If there is a thread with just one sample, it is dropped.
(provide get-times)
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
  (require rackunit)
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
