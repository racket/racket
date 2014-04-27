#lang racket/base

(require racket/list unstable/list racket/match racket/set racket/format
         racket/contract racket/contract/private/blame
         profile/sampler profile/utils profile/analyzer
         "dot.rkt" "utils.rkt" "boundary-view.rkt")

;; (listof (U blame? #f)) profile-samples -> contract-profile struct
(define (correlate-contract-samples contract-samples* samples*)
  ;; car of samples* is total time, car of each sample is thread id
  ;; for now, we just assume a single thread. fix this eventually.
  (define total-time (car samples*))
  ;; reverse is there to sort samples in forward time, which get-times
  ;; needs.
  (define samples    (get-times (map cdr (reverse (cdr samples*)))))
  (define contract-samples
    (for/list ([c-s (in-list contract-samples*)])
      ;; In some cases, blame information is missing a party, in which.
      ;; case the contract system provides a pair of the incomplete blame
      ;; and the missing party. We combine the two here.
      (if (pair? c-s)
          (blame-add-missing-party (car c-s) (cdr c-s))
          c-s)))
  ;; combine blame info and stack trace info. samples should line up
  (define aug-contract-samples
    ;; If the sampler was stopped after recording a contract sample, but
    ;; before recording the corresponding time sample, the two lists may
    ;; be of different lengths. That's ok, just drop the extra sample.
    (for/list ([c-s (in-list contract-samples)]
               [s   (in-list samples)])
      (cons c-s s)))
  (define live-contract-samples (filter car aug-contract-samples))
  (define all-blames
    (set->list (for/set ([b (in-list contract-samples)]
                         #:when b)
                 ;; An original blamed and its swapped version are the same
                 ;; for our purposes.
                 (if (blame-swapped? b)
                     (blame-swap b) ; swap back
                     b))))
  (define regular-profile (analyze-samples samples*))
  ;; all blames must be complete, otherwise we get bogus profiles
  (for ([b (in-list all-blames)])
    (unless (and (blame-positive b)
                 (blame-negative b))
      (error (string-append "contract-profile: incomplete blame:\n"
                            (format-blame b)))))
  (contract-profile
   total-time live-contract-samples all-blames regular-profile))


(define (analyze-contract-samples contract-samples samples*)
  (define correlated (correlate-contract-samples contract-samples samples*))
  (with-output-to-report-file cost-breakdown-file
                              (print-breakdown correlated))
  (module-graph-view correlated)
  (boundary-view correlated))


;;---------------------------------------------------------------------------
;; Break down contract checking time by contract, then by callee and by chain
;; of callers.

(define cost-breakdown-file
  (string-append output-file-prefix "cost-breakdown.txt"))

(define (print-breakdown correlated)
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
    correlated)

  (define total-contract-time (samples-time live-contract-samples))
  (define contract-ratio (/ total-contract-time (max total-time 1) 1.0))
  (printf "Running time is ~a% contracts\n"
          (~r (* 100 contract-ratio) #:precision 2))
  (printf "~a/~a ms\n\n"
          (~r total-contract-time #:precision 0)
          total-time)

  (define shorten-source
    (make-srcloc-shortener all-blames blame-source))
  (define (print-contract/loc c)
    (printf "~a @ ~a\n" (blame-contract c) (shorten-source c)))

  (displayln "\nBY CONTRACT\n")
  (define samples-by-contract
    (sort (group-by (lambda (x) (blame-contract (car x)))
                    live-contract-samples)
          > #:key length #:cache-keys? #t))
  (for ([c (in-list samples-by-contract)])
    (define representative (caar c))
    (print-contract/loc representative)
    (printf "  ~a ms\n\n" (samples-time c)))

  (displayln "\nBY CALLEE\n")
  (for ([g (in-list samples-by-contract)])
    (define representative (caar g))
    (print-contract/loc representative)
    (for ([x (sort
              (group-by (lambda (x)
                          (blame-value (car x))) ; callee source, maybe
                        g)
              > #:key length)])
      (printf "  ~a\n  ~a ms\n"
              (blame-value (caar x))
              (samples-time x)))
    (newline))

  (define samples-by-contract-by-caller
    (for/list ([g (in-list samples-by-contract)])
      (sort (group-by cddr ; pruned stack trace
                      (map sample-prune-stack-trace g))
            > #:key length)))

  (displayln "\nBY CALLER\n")
  (for* ([g samples-by-contract-by-caller]
         [c g])
    (define representative (car c))
    (print-contract/loc (car representative))
    (for ([frame (in-list (cddr representative))])
      (printf "  ~a @ ~a\n" (car frame) (cdr frame)))
    (printf "  ~a ms\n" (samples-time c))
    (newline)))

;; Unrolls the stack until it hits a function on the negative side of the
;; contract boundary (based on module location info).
;; Will give bogus results if source location info is incomplete.
(define (sample-prune-stack-trace sample)
  (match-define (list blame timestamp stack-trace ...) sample)
  (define caller-module (blame-negative blame))
  (define new-stack-trace
    (dropf stack-trace
           (match-lambda
            [(cons name loc)
             (or (not loc)
                 (not (equal? (srcloc-source loc) caller-module)))])))
  (list* blame timestamp new-stack-trace))


;;---------------------------------------------------------------------------
;; Show graph of modules, with contract boundaries and contract costs for each
;; boundary.
;; Typed modules are in green, untyped modules are in red.

(define module-graph-dot-file
  (string-append output-file-prefix "module-graph.dot"))

(define (module-graph-view correlated)
  (match-define (contract-profile
                 total-time live-contract-samples all-blames regular-profile)
    correlated)

  ;; first, enumerate all the relevant modules
  (define-values (nodes edge-samples)
    (for/fold ([nodes (set)] ; set of modules
               ;; maps pos-neg edges (pairs) to lists of samples
               [edge-samples (hash)])
        ([s (in-list live-contract-samples)])
      (match-define (list blame sample-time stack-trace ...) s)
      (when (empty? stack-trace)
        (log-warning "contract profiler: sample had empty stack trace"))
      (define pos (blame-positive blame))
      (define neg (blame-negative blame))
      ;; We consider original blames and their swapped versions to be the same.
      (define edge-key (if (blame-swapped? blame)
                           (cons neg pos)
                           (cons pos neg)))
      (values (set-add (set-add nodes pos) neg) ; add all new modules
              (hash-update edge-samples edge-key
                           (lambda (ss) (cons s ss))
                           '()))))

  (define nodes->typed?
    (for/hash ([n nodes]
               ;; Needs to be either a file or a submodule.
               ;; I've seen 'unit and 'not-enough-info-for-blame go by here,
               ;; and we can't do anything with either.
               #:when (or (path? n) (pair? n)))
      ;; typed modules have a #%type-decl submodule
      (define submodule? (not (path? n)))
      (define filename (if submodule? (car n) n))
      (define typed?
        (with-handlers
            ([(lambda (e)
                (and (exn:fail:contract? e)
                     (or (regexp-match "^dynamic-require: unknown module"
                                       (exn-message e))
                         (regexp-match "^path->string"
                                       (exn-message e)))))
              (lambda _ #f)])
          (dynamic-require
           (append (list 'submod (list 'file (path->string filename)))
                   (if submodule? (cdr n) '())
                   '(#%type-decl))
           #f)
          #t))
      (values n typed?)))

  ;; graphviz output
  (with-output-to-report-file
   module-graph-dot-file
   (printf "digraph {\n")
   (printf "rankdir=LR\n")
   (define nodes->names (for/hash ([n nodes]) (values n (gensym))))
   (define node->labels (make-shortener nodes))
   (for ([n nodes])
     (printf "~a[label=\"~a\"][color=\"~a\"]\n"
             (hash-ref nodes->names n)
             (node->labels n)
             (if (hash-ref nodes->typed? n #f) "green" "red")))
   (for ([(k v) (in-hash edge-samples)])
     (match-define (cons pos neg) k)
     (printf "~a -> ~a[label=\"~ams\"]\n"
             (hash-ref nodes->names neg)
             (hash-ref nodes->names pos)
             (samples-time v)))
   (printf "}\n"))
  ;; render, if graphviz is installed
  (render-dot module-graph-dot-file))


;;---------------------------------------------------------------------------
;; Entry point

(provide (rename-out [contract-profile/user contract-profile])
         contract-profile-thunk
         analyze-contract-samples) ; for feature-specific profiler

;; TODO have kw args for profiler, etc.
;; TODO have kw args for output files
(define-syntax-rule (contract-profile/user body ...)
  (let ([sampler (create-sampler (current-thread) 0.005 (current-custodian)
                                 (list contract-continuation-mark-key))])
    (begin0 (begin body ...)
      (let ()
        (sampler 'stop)
        (define samples (sampler 'get-snapshots))
        (define contract-samples
          (for/list ([s (in-list (sampler 'get-custom-snapshots))])
            (and (not (empty? s)) (vector-ref (car s) 0))))
        (analyze-contract-samples contract-samples samples)))))

(define (contract-profile-thunk f)
  (contract-profile/user (f)))
