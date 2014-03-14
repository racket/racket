#lang racket/base

(require racket/list unstable/list racket/match racket/contract racket/string
         racket/set racket/dict
         profile/structs profile/utils
         "utils.rkt" "dot.rkt")

(provide boundary-view)

;; Boundary View
;; Shows contract costs associated with each each boundary (see definition
;; below) and, when possible, which call edges cross contract boundaries.
;; Because of the limitations of the profiler, some pieces of information
;; are conservative / best effort.


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
;; This is less of a problem now that we never show non-contracted nodes.

;; draw borders to group functions from the same module
(define show-module-clustering? #t)

(define boundary-graph-dot-file
  (string-append output-file-prefix "boundary-graph.dot"))
(define contract-key-file
  (string-append output-file-prefix "contract-key.txt"))

(define (boundary-view correlated)
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
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
                        ;; tricky: blame struct contains location of the *id*
                        ;; of where the *contract* is applied, which doesn't
                        ;; tell us anything about where the function actually
                        ;; *is*. (For `define/contract', it's the id of the
                        ;; definition, but that for `provide/contract', it's
                        ;; a location inside the `provide/contract'.) So can't
                        ;; really use source locations for correlation.
                        ;; The current matching won't work for contracted
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

  (with-output-to-report-file
   boundary-graph-dot-file
   (render all-boundaries all-blames contracts->keys))
  (render-dot boundary-graph-dot-file)
  ;; print contract key
  ;; TODO find a way to add to pdf
  ;; TODO also to add to pdf: show proportion of time spent in contracts
  ;;  otherwise we have no idea if we're looking at a case where contracts are
  ;;  bottlenecks or not
  (with-output-to-report-file contract-key-file
                              (for ([contract+key (in-list contracts->keys)])
                                (printf "[~a] = ~a\n"
                                        (cdr contract+key)
                                        (car contract+key)))))


;; given a profile node, return all the boundaries centered there
(define (node->boundaries node all-boundaries)
  (for/list ([b (in-list all-boundaries)]
             #:when (eq? node (boundary-contracted-node b)))
    b))

(define (node->module node) ; approximate. if no source info, useless
  (define src (node-src node))
  (and src (srcloc-source src)))

(define (node-location node shortened-srcloc)
  (define id (node-id node))
  (define src (node-src node))
  (format "~a~a~a"
          (if id (format "~a" id) "")
          (if (and id src) "\n" "")
          (if src (format-source shortened-srcloc) "")))


;; Inspired by ../render-graphviz.rkt
(define (render all-boundaries all-blames contracts->keys)

  (define total-contract-time
    (max 1e-20 (for/sum ([b (in-list all-boundaries)]) (boundary-time b))))
  (define max-self%
    (/ (for/fold ([m 1e-20]) ([b (in-list all-boundaries)])
         (max m (boundary-time b)))
       total-contract-time))

  ;; All boundary-related nodes, which includes both actual boundary nodes and
  ;; callers that call boundary nodes across boundary edges.
  (define nodes
    (set->list
     (for/fold ([nodes (set)])
         ([b (in-list all-boundaries)])
       (for/fold ([nodes* (set-add nodes (boundary-contracted-node b))])
           ([e (in-list (boundary-edges b))])
         (set-add nodes* (edge-caller e))))))
  (define nodes->names (for/hash ([n nodes]) (values n (gensym))))
  (define node->shortened-path
    (make-srcloc-shortener (filter node-src nodes) ; nodes with paths only
                           node-src))
  (define party->shortened-path
    (make-shortener (set-union (map blame-positive all-blames)
                               (map blame-negative all-blames))))

  (define (summarize-boundary b contracts->keys)
    (define blame (boundary-blame b))
    (format "\n[~a] @ ~a : ~ams"
            ;; show the contract key
            (dict-ref contracts->keys (blame-contract blame))
            (party->shortened-path (blame-negative blame))
            (boundary-time b)))

  (printf "digraph Profile {\n")
  (printf "splines=\"true\"\n") ; polyline kinda works too, maybe

  ;; cluster nodes per module, to show boundaries
  (for ([module-nodes (in-list (group-by node->module nodes))]
        [cluster-idx  (in-naturals 1)])
    (define known-module? (node->module (first module-nodes)))
    ;; don't cluster nodes for which we have no module info
    (when (and known-module? show-module-clustering?)
      (printf "subgraph cluster_~a {\n" cluster-idx)
      (printf "penwidth=3.0\n")
      (printf "graph[shape=\"ellipse\"]\n"))

    ;; render the cluster's nodes
    (for ([node (in-list module-nodes)])
      (define boundaries (node->boundaries node all-boundaries))
      (define self% (/ (for/sum ([b (in-list boundaries)]) (boundary-time b))
                       total-contract-time))
      (define label
        (format
         "~a\n~ams~a"
         (node-location node (node->shortened-path node))
         (node-self node) ; raw running time (not contracts)
         ;; Display a summary of each boundary, which includes which contract
         ;; is used, the negative party and contract time spent.
         (string-join
          (for/list ([b (in-list boundaries)])
            (summarize-boundary b contracts->keys))
          "")))
      (printf "~a [label=~s, style=filled" (dict-ref nodes->names node) label)
      ;; Boundary nodes are boxes, caller nodes are ovals.
      (unless (null? boundaries)
        (printf ", shape=\"box\", fillcolor=\"1,~a,1\""
                (/ self% max-self% 1.0)))
      (printf "];\n"))

    (when (and known-module? show-module-clustering?)
      (printf "}\n")))

  ;; draw edges
  ;; needs to be done after the clusters, otherwise any node mentioned in an
  ;; edge printed inside a cluster is considered to be in the cluster, which
  ;; messes things up
  (for* ([node     (in-list nodes)]
         [boundary (in-list (node->boundaries node all-boundaries))]
         [edge     (in-list (boundary-edges boundary))])
    (printf "~a -> ~a;\n"
            (dict-ref nodes->names (edge-caller edge))
            (dict-ref nodes->names node)))

  (printf "}\n"))
