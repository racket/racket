#lang racket/base

;;;; Processing of mzc inliner logs.

(require typed-racket/optimizer/logging
         unstable/syntax racket/match unstable/match racket/list racket/string
         unstable/list)

(provide log-message-from-mzc-opt?
         mzc-opt-log-message->log-entry
         post-process-inline-log)


;;; Low-level log parsing. Goes from strings to log-entry structs.

(define mzc-optimizer-regexp "^mzc optimizer: ")
(define success-regexp       "inlining: ")
(define failure-regexp       "no inlining: ")
(define out-of-fuel-regexp   "no inlining, out of fuel: ")
(define any-inlining-event-regexp
  (string-append mzc-optimizer-regexp
                 "("
                 (string-join (list success-regexp
                                    failure-regexp
                                    out-of-fuel-regexp)
                              "|")
                 ")"))

(define (log-message-from-mzc-opt? l)
  (regexp-match mzc-optimizer-regexp l))


(struct inliner-log-entry log-entry (inlining-event) #:prefab)

;; String (message from the mzc optimizer) -> log-entry
(define (mzc-opt-log-message->log-entry l)
  (define evt (parse-inlining-event l))
  (define forged-stx (inlining-event->forged-stx evt))
  (define kind
    (match (inlining-event-kind evt)
      [(and k (== success-regexp))     success-regexp]
      [(and k (== failure-regexp))     failure-regexp]
      [(and k (== out-of-fuel-regexp)) out-of-fuel-regexp]
      [_ (error "Unknown log message type" l)]))
  (inliner-log-entry kind kind
                     forged-stx forged-stx
                     (syntax-position forged-stx)
                     evt))

(define inlining-event-regexp
  ;; Last bit is `generated?'. We don't care about that.
  ;; The middle elements of the vector are numbers of #f.
  (string-append
   ;; Attempt at making this thing readable.
   any-inlining-event-regexp
   "involving: "
   ;; _What_ gets inlined (or not).
   (string-append ; either a vector with name and source info, or just name
    "("
    "#\\(([^ ]+) #<path:(.+)> ([^ ]+) ([^ ]+) ([^ ]+) ([^ ]+) [^ ]+\\)"
    "|"
    "([^ ]+)" ; just name, we won't be able to do much with it
    ")")
   ;; _Where_ this happens (in which function, can't get more precise info).
   (string-append
    ;; maybe full path info: path, line, col, name
    "( in: (([^ :]+):([^ :]+):([^ :]+): )?([^ ]+))?"
    ;; maybe module info, useless to us (at least for now)
    "( in module: [^ ]+)?")
   " size: ([^ ]+) threshold: ([^ ]+)"
   "$"))

(struct inlining-event (kind ; success, miss, out of fuel, ...
                        name ; _what_ gets inlined
                        loc  ; (U #f (List path line col pos span))
                        where-name ; _where_ it gets inlined (enclosing fun)
                        where-loc  ; (U #f (Line path line col))
                        size ; size of the closure being inlined
                        threshold ; how big of a closure can we inline
                        ;; the last two use the same units
                        ))
(define (parse-inlining-event l)
  (match (regexp-match inlining-event-regexp l)
    [`(,all ,kind
            ,what ,name ,path ,line ,col ,pos ,span
                  ,only-name
            ,where ,where-loc ,where-path ,where-line ,where-col ,where-name
            ,maybe-module-info
            ,size ,threshold)
     (inlining-event kind
                     (string->symbol (or name only-name))
                     (if only-name
                        #f ; no source location
                        (list path
                              (string->number line)
                              (string->number col)
                              (string->number pos)
                              (string->number span)))
                     where-name
                     (if where-loc
                         (list where-path
                               (string->number where-line)
                               (string->number where-col))
                         #f) ; no source location
                     (string->number size)
                     (string->number threshold))]
    [_ (error "ill-formed inlining log entry" l)]))


(define (inlining-event->forged-stx evt)
  (match evt
    [(inlining-event kind name loc where-name where-loc size threshold)
     (datum->syntax #'here name loc)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; self out-of-fuels are not interesting, they're the end of loop unrolling
(define (self-out-of-fuel? l) (and (out-of-fuel? l) (self-inline? l)))

;; We aggregate results for each function.
;; Log messages produced by the inliner are very raw, unlike the TR logs,
;; which have gone through some aggregation. We do the aggregation here.
(define (post-process-inline-log log)
  (define-values (inliner-logs tr-logs)
    (partition inliner-log-entry? log))
  (define grouped-events
    (group-by equal? #:key log-entry-pos ; right file, so that's enough
              inliner-logs))
  (define new-inline-log-entries
    (for*/list ([g     (in-list  grouped-events)]
                [group (in-value (filter (lambda (x)
                                           (not (self-out-of-fuel? x)))
                                         g))]
                #:when (not (null? group)))
      (define head (car group))
      (match head ; events are grouped, first element is representative
        [(log-entry kind msg stx located-stx pos)

         ;; We consider that a function is a loop if it gets inlined in itself
         ;; at least once.
         ;; We treat loops specially, mostly to avoid spurious reports.
         ;; For instance, if `f' is a loop, and gets inlined in `g' multiple
         ;; times, it's likely to be unrolling. Same for out-of-fuels in `g'.
         ;; Therefore, we don't want to report these as inlinings (or failed
         ;; inlinings).
         ;; However, we care about `f' being unrolled at least once in `g'.
         ;; If we run out of fuel trying to inline `f' in `g' for the first
         ;; time, we report. The reason for this is that it's possible to
         ;; optimize better if `f''s body inside `g' calls `f' than if `g'
         ;; calls `f' directly. For instance, `f' may be a loop involving
         ;; floats, in which case having all calls to `f' originate from `f''s
         ;; body (as opposed to `g') may make unboxing possible.
         ;; Of course, we lose precision if `g' has multiple call sites to `f'.
         (define n-unrollings   (length (filter unrolling?   group)))
         (define is-a-loop?     (> n-unrollings 0))
         (define inlining-sites
           (group-by equal? #:key (lambda (x)
                                    (inlining-event-where-loc
                                     (inliner-log-entry-inlining-event x)))
                     group))

         (define pruned-group
           (if (not is-a-loop?)
               group
               ;; `f' is a loop. We ignore anything beyond the first inlining
               ;; in `g'.
               (apply
                append
                (for/list ([site (in-list inlining-sites)]
                           #:when
                           ;; If at least one inlining of `f' in `g', prune.
                           (not (for/or ([evt (in-list site)])
                                  (success? evt))))
                  site))))

         (define n-successes (- (length (filter success?     group)) n-unrollings))
         (define n-failures     (length (filter failure?     group)))
         ;; self o-o-f are already gone at this point
         (define n-out-of-fuels (length (filter out-of-fuel? group)))

         (define aggregation-string
           (format-aggregation-string
            n-successes n-unrollings n-failures n-out-of-fuels))

         ;; This is where the interesting decisions are taken.
         (define counts-as-a-missed-opt?
           (or (> n-failures 0) ; any straight failure is a problem
               (> n-out-of-fuels n-successes) ; we fail more often than not
               ))

         (define recommendation
           (cond [is-a-loop?
                  "Consider making this function smaller to encourage inlining."]
                 [else
                  ;; Non-recursive function -> macro
                  "Consider turning this function into a macro to force inlining."]))

         (if counts-as-a-missed-opt?
             (missed-opt-log-entry
              kind
              (format "Missed Inlining ~a\n~a"
                      aggregation-string recommendation)
              stx located-stx pos
              '() '()
              (+ n-failures (- n-out-of-fuels n-successes))) ; badness
             (opt-log-entry
              kind
              (format "Inlining ~a" aggregation-string)
              stx located-stx pos))])))
  (append tr-logs new-inline-log-entries))

(define (format-aggregation-string
         n-successes n-unrollings n-failures n-out-of-fuels)
  ;; Integer String #:suffix String -> (U Null (List String))
  ;; if n = 0, nothing, if n = 1 singular, o/w plural
  (define (pluralize n noun #:suffix [suffix "s"])
    (format "~a ~a~a" n noun (if (> n 1) suffix "")))
  (format "(~a out of ~a~a)"
          (pluralize n-successes "success" #:suffix "es")
          (+ n-successes n-failures n-out-of-fuels)
          (if (> n-unrollings 0)
              (format " and ~a" (pluralize n-unrollings   "unrolling"))
              "")))
