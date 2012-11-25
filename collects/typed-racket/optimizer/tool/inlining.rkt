#lang racket/base

;;;; Processing of mzc inliner logs.

(require racket/match racket/list racket/string unstable/list
         "structs.rkt" "utils.rkt" "instrumentation.rkt" "profiling.rkt")

(provide report-inlining)

(define (report-inlining log profile hot-functions)
  (define grouped-events
    (group-by equal? #:key log-entry-pos log)) ; right file, so that's enough
  (apply
   append
   (for/list ([group (in-list grouped-events)])
     (process-function group profile hot-functions))))


;;; Log processing. Interprets the log entries, and produces new ones.
;;; This is similar in spirit to the post-processing done for missed-opts in
;;; the TR logger.

(define (success?     l) (equal? success-regexp     (log-entry-kind l)))
(define (failure?     l) (equal? failure-regexp     (log-entry-kind l)))
(define (out-of-fuel? l) (equal? out-of-fuel-regexp (log-entry-kind l)))

;; f gets inlined in f (or tried to)
(define (self-inline? l)
  (match (inliner-log-entry-inlining-event l)
    [(inlining-event kind name loc where-name where-loc size threshold)
     (match* (loc where-loc)
       [((list path line col pos span)
         (list where-path where-line where-col))
        (and (equal? path where-path)
             (= col  where-col)
             (= line where-line))]
       [(hunoz hukairz) #f])])) ; we assume it is not, to be conservative

(define (unrolling? l) (and (success? l) (self-inline? l)))

(define (n-unrollings   group)    (length (filter unrolling?   group)))
(define (n-successes    group) (- (length (filter success?     group))
                                  (n-unrollings group)))
(define (n-failures     group)    (length (filter failure?     group)))
(define (n-out-of-fuels group)    (length (filter out-of-fuel? group)))

;; self out-of-fuels are not interesting, they're the end of loop unrolling
(define (self-out-of-fuel? l) (and (out-of-fuel? l) (self-inline? l)))

(define (any-self-o-o-f? group) (ormap self-out-of-fuel? group))

(define (counts-as-a-missed-opt? group)
  (or (> (n-failures group) 0) ; any straight failure is a problem
      (> (n-out-of-fuels group) (n-successes group)); fails more often than not
      ))


;; Process the inlining logs corresponding to a single function.
(define (process-function log profile hot-functions)
  (define total-time (and profile (profile-total-time profile)))
  (define produced-entries '())
  (let/ec escape
    ;; prune this entry from the logs, but return what we produced so far
    (define (prune) (escape produced-entries))
    (match (car log) ; events are grouped, first element is representative
      [(log-entry kind msg stx located-stx pos)

       ;; #f if no profiling info is available for this function
       ;; takes in either a single pos number or a pair of numbers (line col)
       (define (pos->node pos)
         (and profile
              (for/first ([p (in-list (profile-nodes profile))]
                          #:when (if (pair? pos)
                                     (and (equal? (car pos) (node-line p))
                                          (equal? (cdr pos) (node-col  p)))
                                     (equal? pos (node-pos p))))
                p)))
       (define profile-entry (pos->node pos))

       (define badness-multiplier
         (if profile-entry
             (/ (node-self profile-entry) total-time)
             1))

       ;; We consider that a function is a loop if it gets inlined in itself
       ;; at least once.
       (define is-a-loop?
         (or (any-self-o-o-f? log) (> (n-unrollings log) 0)))
       ;; From now on, we ignore self-out-of-fuels.
       (set! log (filter (lambda (l) (not (self-out-of-fuel? l))) log))

       (define inlining-sites
         (group-by equal? #:key (lambda (x)
                                  (inlining-event-where-loc
                                   (inliner-log-entry-inlining-event x)))
                   log))

       ;; We treat loops specially, mostly to avoid spurious reports.
       ;; For instance, if `f' is a loop, and gets inlined in `g' multiple
       ;; times, it's likely to be unrolling. Same for out-of-fuels in `g'.
       ;; Therefore, we don't want to report these as inlinings (or failed
       ;; inlinings). If `g' has multiple call sites for `f', we lose
       ;; precision, and may discard actual inlinings.
       ;; However, we care about `f' being unrolled at least once in `g'.
       ;; If we run out of fuel trying to inline `f' in `g' for the first
       ;; time, we report. The reason for this is that it's possible to
       ;; optimize better if `f''s body inside `g' calls `f' than if `g'
       ;; calls `f' directly. For instance, `f' may be a loop involving
       ;; floats, in which case having all calls to `f' originate from `f''s
       ;; body (as opposed to `g') may make unboxing possible.
       ;; Of course, we lose precision if `g' has multiple call sites to `f'.
       (set! inlining-sites
             (if (not is-a-loop?)
                 inlining-sites
                 ;; `f' is a loop. We ignore anything beyond the first inlining
                 ;; in `g'.
                 (for/list ([site (in-list inlining-sites)])
                   ;; If at least one inlining of `f' in `g', ignore the rest.
                   (or (for/first ([evt (in-list site)] #:when (success? evt))
                         (list evt))
                       site))))

       ;; Some callers are especially interesting if we have profile data.
       ;; If the function under consideration takes a large portion of the
       ;; total time of a given caller, we consider this case interesting.
       ;; This serves as a building block for more interesting patterns, such
       ;; as `key-sites' below.
       ;; returns: caller-profile-node OR #f
       (define interesting-callers
         (and profile-entry
              (filter values
                      (for/list ([edge (node-callers profile-entry)])
                        ;; Does this edge take a "large enough" proportion of
                        ;; the caller's total time?
                        (define caller-node (edge-caller edge))
                        (and (> (edge-caller-time edge)
                                (* (node-total caller-node) 0.3))
                             caller-node)))))

       ;; As above, but consed in front of the inlining info for that caller.
       (define interesting-callers+sites
         (and profile-entry
              ;; Can't map over `inlining-sites', since we also consider
              ;; callers that have no inlining reports at all.
              (for/list ([caller (in-list interesting-callers)])
                (cons caller
                      ;; Find the relevant inlining site information.
                      (or (for/or ([site (in-list inlining-sites)])
                            (match (inlining-event-where-loc
                                    (inliner-log-entry-inlining-event
                                     (car site)))
                              [`(,caller-path ,caller-line ,caller-col)
                               (and (eq? caller
                                         (pos->node (cons caller-line
                                                          caller-col)))
                                    site)]
                              [_ ; can't parse that, give up
                               #f]))
                          '()))))) ; no inlining reports for that caller

       ;; If the function under consideration takes a large portion of the
       ;; total time for a given call site, and is not inlined there, we can
       ;; recommend that the user take a closer look at that specific site.
       ;; returns: `(,caller-profile-node . ,call-site-log-entries) OR #f
       (define key-sites
         (and profile-entry
              (for/list ([site (in-list interesting-callers+sites)]
                         ;; Not inlined enough at that call site.
                         #:when (counts-as-a-missed-opt? (cdr site)))
                site)))

       (define pruned-log (apply append inlining-sites))

       (define recommendation
         (cond [is-a-loop?
                "Consider making this function smaller to encourage inlining."]
               [else
                ;; Non-recursive function -> macro
                "Consider turning this function into a macro to force inlining."]))

       ;; Produce as many log entries as necessary.
       (define (emit e) (set! produced-entries (cons e produced-entries)))
       (define start     (sub1 pos))
       (define end       (+ start (syntax-span stx)))
       (define (emit-near-miss msg badness)
         (emit (report-entry
                (list (missed-opt-report-entry
                       located-stx
                       (format "Missed Inlining ~a\n~a~a"
                               (format-aggregation-string pruned-log)
                               (if msg (format "~a\n" msg) "")
                               recommendation)
                       'inlining
                       badness
                       '())) ; no irritants to highlight
                start end
                badness)))
       (define (emit-success)
         (emit (report-entry
                (list (opt-report-entry
                       located-stx
                       (format "Inlining ~a"
                               (format-aggregation-string pruned-log))
                       'inlining))
                start end
                0)))

       (define inside-hot-function?
         (and profile (memq profile-entry hot-functions)))

       (define (inside-us? h)
         (pos-inside-us? (node-pos h)
                         (syntax-position located-stx)
                         (syntax-span     located-stx)))

       ;; To catch hot curried functions.
       ;; Turns out to be useful for the ray tracer, but increases false
       ;; positives for functions with hot loops inside that otherwise are
       ;; uninteresting wrt inlining.
       (define really-hot-anonymous-function-inside-us?
         (and hot-functions
              ;; list is sorted in increasing order of time
              (ormap (lambda (x) (and (inside-us? x)
                                      (not (node-id x)))) ; anonymous
                     ;; TODO try dropping 3/4
                     (drop hot-functions (quotient (length hot-functions) 2)))))

       ;; If we know which regions are hot, prune reports about cold
       ;; regions. If we don't know, err on the side of showing more.
       ;; We don't want to prune earlier, since traversing cold functions can
       ;; give us advice about hot functions.
       (when (and profile
                  (not inside-hot-function?)
                  (not really-hot-anonymous-function-inside-us?))
         (prune))

       (cond [(and profile
                   (counts-as-a-missed-opt? pruned-log)
                   is-a-loop?
                   ;; loops are hard to act upon, only report in extreme cases
                   (< (group-badness pruned-log) 50))
              (prune)]
             [(and profile
                   (counts-as-a-missed-opt? pruned-log)
                   (not is-a-loop?)
                   (not really-hot-anonymous-function-inside-us?)
                   ;; needs to have enough failures to report
                   (< (group-badness pruned-log) 6))
              (prune)]
             [(and profile-entry (not (null? key-sites)))
              ;; Inlining was not satisfactory for some call sites where we
              ;; accounted for a good portion of the caller's total time.
              (emit-near-miss
               (format "Key call site~a: ~a"
                       (if (> (length key-sites) 1) "s" "")
                       (string-join
                        (for/list ([site (in-list key-sites)])
                          (define node (car site))
                          (format "~a ~a:~a"
                                  (node-id   node)
                                  (node-line node)
                                  (node-col  node)))
                        ", "))
               ;; only compute badness for the interesting sites
               (group-badness (apply append (map cdr key-sites))))]
             [(counts-as-a-missed-opt? pruned-log)
              ;; Overall inlining ratio is not satisfactory.
              (emit-near-miss #f (group-badness pruned-log))]
             [else
              ;; Satisfactory.
              (emit-success)])

       produced-entries]))) ; return the list of new entries

(define (group-badness group)
  (+ (n-failures group) (- (n-out-of-fuels group) (n-successes group))))

(define (format-aggregation-string group)
  ;; Integer String #:suffix String -> (U Null (List String))
  ;; if n = 0, nothing, if n = 1 singular, o/w plural
  (define (pluralize n noun #:suffix [suffix "s"])
    (format "~a ~a~a" n noun (if (> n 1) suffix "")))
  (define n-u (n-unrollings group))
  (define n-s (n-successes  group))
  (format "(~a out of ~a~a)"
          (pluralize n-s "success" #:suffix "es")
          (+ n-s (n-failures group) (n-out-of-fuels group))
          (if (> n-u 0)
              (format " and ~a" (pluralize n-u "unrolling"))
              "")))
