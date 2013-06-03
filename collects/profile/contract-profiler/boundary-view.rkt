#lang racket/base

(require racket/list unstable/list racket/match racket/contract racket/string
         racket/set racket/dict
         "../structs.rkt" "../utils.rkt"
         "utils.rkt" "dot.rkt")

(provide boundary-view)

;; Boundary View
;; Shows contract costs associated with each each boundary (see definition
;; below) and, when possible, which call edges cross contract boundaries.
;; Because of the limitations of the profiler, some pieces of information
;; are conservative / best effort.


(define (prune-module-name name)
  (define (p n) (regexp-replace #rx"^.*/" n ""))
  (cond [(string? name) (p name)]
        [(path?   name) (p (path->string name))]
        ;; submodule path
        [(list?   name) (list 'submod
                              (p (path->string (first name)))
                              (second name))]))

;; A boundary is, at a high-level, a profile node (contracted function) and a
;; (potentially empty) set of profile edges (incoming call edges that cross the
;; contract boundary).
;; A given node can be part of multiple boundaries if it enters into contracts
;; with multiple parties, in which case it is be part of one boundary per
;; party, and the incoming edges all come from that party.
;; Contract checking time can be assigned to boundaries, but not to individual
;; edges (from our perspective, the edges in a boundary can't be distinguished)
;; Edges are found by looking at the regular time profile for callers to the
;; node that originate from the contract's other party. Due to limitations in
;; the profiler (tail calls, mostly), the set of edges is an approximation
;; (both over- and under-).
(struct boundary (contracted-node edges blame time)
        #:transparent) ;; TODO for debugging

;; Rendering options.
;; For large programs (e.g. Acquire), enabling module clustering when showing
;; all functions (not just contracted ones) makes renderings undecipherable.
;; Also, most non-dot Graphviz renderers cope poorly with our graphs. fdp is
;; probably the next best one.

;; only show contracted edges and related nodes
(define show-only-contracted? #t)
;; draw borders to group functions from the same module
(define show-module-clustering? #t)

(define boundary-graph-dot-file
  (string-append output-file-prefix "boundary-graph.dot"))
(define contract-key-file
  (string-append output-file-prefix "contract-key.txt"))

(define (boundary-view correlated)
  (match-define (contract-profile
                 total-time n-samples n-contract-samples
                 live-contract-samples all-blames regular-profile)
    correlated)

  (define all-contracts (remove-duplicates (map blame-contract all-blames)))

  ;; On the graph, contracts are abbreviated with a key ([1], [2], etc.).
  ;; The mapping from keys to actual contracts is printed separately.
  ;; TODO only have keys for contracts that actually show up in the profile, and only show these
  (define contracts->keys (map cons all-contracts (range (length all-contracts))))

  ;; For each blame, find the edges (from the profile graph) that cross
  ;; contract boundaries.
  ;; Each blame may belong to multiple boundaries (multiple contracted
  ;; functions from the same module called from the same other party).
  (define no-profile-nodes
    (make-hash)) ; we may want to re-use nodes for multiple blames
  (define all-boundaries
    (for/list ([b (in-list all-blames)])
      ;; First, find the contracted function in the profile graph.
      ;; Some functions are not in the profile graph. We create nodes for them,
      ;; which won't be connected to the actual profile graph, but will show up
      ;; in the output. (See no-profile-nodes above.)
      (define contracted-function
        (or (for/first ([n (in-list (profile-nodes regular-profile))]
                        ;; Matching is overly permissive (relies only on
                        ;; function name). If two functions in the same module
                        ;; have the same name, results may be bogus.
                        ;; TODO fix this
                        ;; Refining results using source locations would be
                        ;; tricky: blame struct and profiler don't store source
                        ;; information in the same format (path with <collects>
                        ;; vs full path, and pointing to defined identifier vs
                        ;; pointing to the whole definition).
                        ;; TODO check at least for inclusion of blame source in
                        ;;  profiler source. not 100%, but can get us closer
                        ;; The current matching also won't work for contracted
                        ;; anonymous functions. (Probably rare, though.)
                        #:when (equal? (blame-value b) (node-id n)))
              n)
            ;; function is not in the profile. create a node for it (that will
            ;; not be connected to the profile graph), or reuse an existing
            ;; node if we created one for that id
            (hash-ref! no-profile-nodes
                       (blame-value b)
                       (node (blame-value b)
                             #f ; no source ; TODO could we find it?
                             '(0) ; dummy thread-ids
                             0 0 ; if not in the profile, no time is a safe bet
                             '() '())))) ; no known callers or callees
      ;; Out of the callers of contracted-function, look for the ones that are
      ;; in the negative module. Those edges are the boundary edges.
      (define caller-module (blame-negative b))
      (define boundary-edges
        (for*/list ([e   (node-callers contracted-function)]
                    [src (in-value (node-src (edge-caller e)))]
                    #:when (and src (equal? caller-module (srcloc-source src))))
          e))
      (define time-spent ; TODO probably more efficient to group ahead of time
        (samples-time (for/list ([s (in-list live-contract-samples)]
                                 #:when (equal? (car s) b))
                        s)))
      (boundary contracted-function boundary-edges b time-spent)))

  (define all-contracted-edges
    (append* (map boundary-edges all-boundaries)))

  (with-output-to-file boundary-graph-dot-file
    #:exists 'replace
    (lambda () (render regular-profile all-boundaries all-contracted-edges
                       contracts->keys)))
  (render-dot boundary-graph-dot-file)
  ;; print contract key
  ;; TODO find a way to add to pdf
  ;; TODO also to add to pdf: show proportion of time spent in contracts
  ;;  otherwise we have no idea if we're looking at a case where contracts are
  ;;  bottlenecks or not
  (with-output-to-file contract-key-file
    #:exists 'replace
    (lambda ()
      (for ([contract+key (in-list contracts->keys)])
        (printf "[~a] = ~a\n"
                (cdr contract+key)
                (car contract+key))))))


;; given a profile node, return all the boundaries centered there
(define (node->boundaries node all-boundaries)
  (for/list ([b (in-list all-boundaries)]
             #:when (eq? node (boundary-contracted-node b)))
    b))

(define (node->module node) ; approximate. if no source info, useless
  (define src (node-src node))
  (and src (srcloc-source src)))

;; Inspired by ../render-graphviz.rkt
(define (render profile all-boundaries all-contracted-edges contracts->keys
                #:hide-self [hide-self% 1/100]
                #:hide-subs [hide-subs% 2/100])
  (define *-node (profile-*-node profile))
  (define hidden (get-hidden profile hide-self% hide-subs%))
  ;; TODO hiding may be useful when we show non-contracted nodes too, o/w not
  (define nodes  (remq* hidden (profile-nodes profile)))
  (define total-contract-time
    (max 1e-20 (for/sum ([b (in-list all-boundaries)]) (boundary-time b))))
  (define max-self%
    (/ (for/fold ([m 0]) ([b (in-list all-boundaries)])
         (max m (boundary-time b)))
       total-contract-time))

  (define nodes-to-show
    (if show-only-contracted?
        (set->list (for/fold ([contract-related-nodes ; boundary nodes
                               ;; need to add all boundary nodes, since some
                               ;; have no boundary edges, and wouldn't be
                               ;; found by iterating over edges
                               ;; TODO explain that in docs: contracted nodes
                               ;;  whose callers can't be found in profile
                               ;; also, some of these nodes may not be from
                               ;; the profile at all (created for boundary
                               ;; nodes that were not in the profile)
                               (for/set ([b (in-list all-boundaries)])
                                 (boundary-contracted-node b))])
                       ([e (in-list all-contracted-edges)])
                     ;; nodes on either side of boundary edges
                     (set-add (set-add contract-related-nodes
                                       (edge-caller e))
                              (edge-callee e))))
        nodes)) ;; TODO this won't have nodes on contracted edges that are not in the profile
  (define node->
    (let ([t (make-hasheq)])
      (for ([node (in-list nodes-to-show)] [idx (in-naturals 1)])
        (define id (node-id node))
        (define src (node-src node))
        (hash-set! t node
                   (list (format "node~a" idx)
                         (format "~a~a~a"
                                 (if id (format "~a" id) "")
                                 (if (and id src) "\n" "")
                                 (if src
                                     (prune-module-name (format-source (node-src node)))
                                     "")))))
      (λ (mode node)
        ((case mode [(index) car] [(label) cadr]) (hash-ref t node)))))

  (printf "digraph Profile {\n")
  (printf "splines=\"true\"\n") ; polyline kinda works too, maybe

  ;; cluster nodes per module, to show boundaries
  (for ([module-nodes (in-list (group-by equal? nodes-to-show #:key node->module))]
        [cluster-idx  (in-naturals 1)])
    (define known-module? (node->module (first module-nodes)))
    ;; don't cluster nodes for which we have no module info
    (when (and known-module? show-module-clustering?)
      (printf "subgraph cluster_~a {\n" cluster-idx)
      (printf "penwidth=3.0\n")
      (printf "graph[shape=\"ellipse\"]\n"))

    (for ([node (in-list module-nodes)])
      (define boundaries (node->boundaries node all-boundaries))
      (define self% (/ (for/sum ([b (in-list boundaries)]) (boundary-time b))
                       total-contract-time))
      (define label
        (string-append
         (node-> 'label node)
         (format "\n~ams" (node-self node))
         (if (null? boundaries)
             ""
             ;; Display a summary of each boundary, which includes which
             ;; contract is used, the negative party and contract time spent.
             (string-join
              (for/list ([b (in-list boundaries)])
                (format
                 "\n[~a] @ ~a : ~ams"
                 ;; show the contract key
                 (dict-ref contracts->keys (blame-contract (boundary-blame b)))
                 (prune-module-name (blame-negative (boundary-blame b)))
                 (boundary-time b)))
              ""))))
      (printf "~a [" (node-> 'index node))
      (printf "label=~s, " label)
      (unless (null? boundaries)
        (printf "color=\"blue\", shape=\"box\", ")
        (printf "fillcolor=\"1,~a,1\", " (exact->inexact (/ self% max-self%))))
      (printf "style=filled];\n"))
    (when (and known-module? show-module-clustering?)
      (printf "}\n")))
  ;; draw edges
  ;; needs to be done after the clusters, otherwise any node mentioned in an
  ;; edge printed inside a cluster is considered to be in the cluster, which
  ;; messes things up
  (for ([node (in-list nodes-to-show)])
    (define boundaries (node->boundaries node all-boundaries))
    ;; draw the graph backwards, from callees to callers (unlike analyze.rkt)
    ;; this makes it easy to mark boundary edges specially, since we know
    ;; which edges these are from the callee's boundaries
    (for ([edge (in-list (node-callers node))])
      (define caller (edge-caller edge))
      (define boundary-edge?
        (for/or ([b (in-list boundaries)])
          (memq edge (boundary-edges b))))
      (unless (or (eq? *-node caller) (memq caller hidden))
        (when (or (not show-only-contracted?) boundary-edge?)
          (printf "~a -> ~a" (node-> 'index caller) (node-> 'index node))
          ;; contract info for boundary edges
          (when boundary-edge?
            (printf "[color=\"red\"]"))
          (printf ";\n")))))
  (printf "}\n"))
