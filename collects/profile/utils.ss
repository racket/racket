#lang scheme/base

(require "structs.ss" scheme/list)

;; Format a percent number, possibly doing the division too.  If we do the
;; division, then be careful: if we're dividing by zero, then make the result
;; zero.  This is useful if the total time is zero because we didn't see any
;; activity (for example, the profiled code is just doing a `sleep'), in which
;; case all times will be 0.
(provide format-percent)
(define format-percent
  (case-lambda
    [(percent)
     (let ([percent (inexact->exact (round (* percent 1000)))])
       (format "~a.~a%" (quotient percent 10) (modulo percent 10)))]
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
        (map (lambda (edge)
               (let ([total (node-total (edge-sub edge))])
                 (if (zero? total) 0 (/ (edge-sub-time edge) total))))
             (get-subs node)))
      (subs% . >= . (apply max %s)))
    (and (self% . >= . (/ (node-self node) total-time))
         (hide-sub? node-callees edge-callee edge-caller-time)
         (hide-sub? node-callers edge-caller edge-callee-time)))
  (cond [(and (<= self% 0) (<= subs% 0)) '()]
        [(zero? total-time) (profile-nodes profile)]
        [else (filter hide? (profile-nodes profile))]))

;; A simple topological sort of nodes using the Khan method, starting from node
;; `x' (which will be given as the special *-node).  The result is a list of
;; node lists, each one corresponds to one level.  Conceptually, the input node
;; is always only item in the first level, so it is not included in the result.
(provide topological-sort)
(define (topological-sort x)
  (let loop ([todo (list x)] [sorted '()] [seen (list x)])
    (let* (;; take the next level of nodes
           [next (append-map (lambda (x) (map edge-callee (node-callees x)))
                             todo)]
           ;; remove visited and duplicates
           [next (remove-duplicates (remq* seen next))]
           ;; leave only nodes with no other incoming edges
           [seen* (append next seen)] ; important for cycles
           [next* (filter (lambda (node)
                            (andmap (lambda (e) (memq (edge-caller e) seen*))
                                    (node-callers node)))
                          next)]
           ;; but if all nodes have other incoming edges, then there must be a
           ;; cycle, so just do them now (instead of dropping them)
           [next (if (null? next*) next next*)])
      (if (null? next)
        (reverse sorted)
        (loop next (cons next sorted) (append next seen))))))
