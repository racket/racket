#lang racket/base

(provide render)

(require "analyzer.rkt" "utils.rkt")

(define (render profile
                #:hide-self [hide-self% 1/100]
                #:hide-subs [hide-subs% 2/100])
  (define *-node (profile-*-node profile))
  (define hidden (get-hidden profile hide-self% hide-subs%))
  (define nodes  (remq* hidden (profile-nodes profile)))
  (define total-time (max 1e-20 (profile-total-time profile)))
  (define node->
    (let ([t (make-hasheq)])
      (for ([node (in-list nodes)] [idx (in-naturals 1)])
        (define id (node-id node))
        (define src (node-src node))
        (hash-set! t node
                   (list (format "node~a" idx)
                         (if id
                           (format "~a" id)
                           (regexp-replace #rx"^.*/"
                                           (format-source (node-src node))
                                           "")))))
      (λ (mode node)
        ((case mode [(index) car] [(label) cadr]) (hash-ref t node)))))
  (define max-self%
    (/ (for/fold ([m 0]) ([node (in-list nodes)]) (max m (node-self node)))
       total-time))
  (printf "digraph Profile {\n")
  (for ([node (in-list nodes)])
    (define self% (/ (node-self node) total-time))
    (printf "~a [" (node-> 'index node))
    (printf "label=~s, " (node-> 'label node))
    (printf "fillcolor=\"1,~a,1\", " (exact->inexact (/ self% max-self%)))
    (printf "style=filled];\n")
    (for ([edge (in-list (node-callees node))])
      (define callee (edge-callee edge))
      (unless (or (eq? *-node callee) (memq callee hidden))
        (printf "~a -> ~a;\n" (node-> 'index node) (node-> 'index callee)))))
  (printf "}\n"))
