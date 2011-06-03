#lang racket/base

(require racket/set racket/string racket/match racket/list
         unstable/syntax)

(provide log-optimization log-missed-optimization
         optimization-log-key
         print-log clear-log)

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(struct log-entry (msg line col) #:transparent)

;; to identify log messages that come from the optimizer
;; to be stored in the data section of log messages
;; external tools/scripts (like the test harness) can look for it
;; since this is used across phases, can't be a gensym
(define optimization-log-key 'log-message-coming-from-the-TR-optimizer)

;; we keep track of log entries, to avoid repetitions that would be
;; caused by traversing the same syntax multiple times (which is not
;; a problem per se)
(define log-so-far (set))

(define (gen-log-message msg stx)
  (format "~a ~a ~a -- ~a"
          (syntax-source-file-name stx)
          (line+col->string stx)
          (syntax->datum stx)
          msg))

(define (do-logging msg stx)
  (let* ([new-message (gen-log-message msg stx)]
         [new-entry (log-entry new-message
                               (syntax-line stx)
                               (syntax-column stx))])
    (unless (set-member? log-so-far new-entry)
      (set! log-so-far (set-add log-so-far new-entry)))))

;; once the optimizer is done, we sort the log according to source
;; location, then print it
(define (print-log)
  (define logger (current-logger))
  ;; add missed optimizations messages to the log, now that we know all of them
  (for-each (lambda (x) (do-logging (format-missed-optimization x)
                                    (missed-optimization-stx x)))
            missed-optimizations-log)
  (for-each (lambda (x) (log-message logger 'warning (log-entry-msg x)
                                     optimization-log-key))
            (sort (set->list log-so-far)
                  (lambda (x y)
                    (match* (x y)
                      [((log-entry msg-x line-x col-x)
                        (log-entry msg-y line-y col-y))
                       (cond [(not (or line-x line-y))
                              ;; neither have location, sort by message
                              (string<? msg-x msg-y)]
                             [(not line-y) #f]
                             [(not line-x) #t]
                             [else
                              ;; both have location
                              (let* ([loc-x (+ (* 1000 line-x) col-x)]
                                     ;; 1000 is a conservative bound
                                     [loc-y (+ (* 1000 line-y) col-y)])
                                (cond [(= loc-x loc-y)
                                       ;; same location, sort by message
                                       (string<? msg-x msg-y)]
                                      ;; sort by source location
                                      [else (< loc-x loc-y)]))])])))))
(define (clear-log)
  (set! log-so-far (set))
  (set! missed-optimizations-log '()))

(define (log-optimization kind stx) (do-logging kind stx))


;; Keep track of optimizations that "almost" happened, with the intention
;; of reporting them to the user.
;; This is meant to help users understand what hurts the performance of
;; their programs.

;; badness : Integer. crude measure of how severe the missed optimizations are
;;  currently, it's simply a count of how many missed optimizations occur
;;  within a given syntax object
;; irritants are the original, potentially indirect causes of the miss
;; merged-irritants are intermediate steps between stx and an irritant
;; they are not actual irritants anymore because they were the stx for a miss
;; that got merged into this miss. we need to keep them around to detect
;; future potential merges.
(struct missed-optimization (kind stx irritants merged-irritants badness)
        #:transparent)

(define missed-optimizations-log '())

;; is parent the "parent" missed optimization of child?
;; this determines whether they get reported together or not
;; currently, parents and children must be of the same kind of missed
;; optimization, and the child must be an irritant of the parent, or be a
;; merged irritant of the parent
(define (parent-of? parent child)
  (and (equal? (missed-optimization-kind parent)
               (missed-optimization-kind child))
       (member (missed-optimization-stx child)
               (append (missed-optimization-irritants parent)
                       (missed-optimization-merged-irritants parent)))))

;; combine reporting of two missed optimizations, increasing badness in the
;; process
(define (combine-missed-optmizations parent child)
  (missed-optimization
   (missed-optimization-kind parent) ; same as child's
   (missed-optimization-stx  parent) ; we report the outermost one
   (remove-duplicates
    (append (remove (missed-optimization-stx child)
                    (missed-optimization-irritants parent))
            (missed-optimization-irritants child)))
   (remove-duplicates
    (append (missed-optimization-merged-irritants child)
            (missed-optimization-merged-irritants parent)
            ;; we merge child in, keep it for future merges
            (list (missed-optimization-stx child))))
   (+ (missed-optimization-badness parent)
      (missed-optimization-badness child))))

(define (log-missed-optimization kind stx [irritants '()])
  ;; for convenience, if a single irritant is given, wrap it in a list
  ;; implicitly
  (let* ([irritants (if (list? irritants) irritants (list irritants))]
         [new       (missed-optimization kind stx irritants '() 1)]
         ;; check if the new one is the child of an old one, or vice versa
         ;; we check for either to do a single traversal
         [parent/child (for/first ([m (in-list missed-optimizations-log)]
                                   #:when (or (parent-of? m new)
                                              (parent-of? new m)))
                         m)]
         ;; if we found a related entry, is it our parent or our child?
         [parent? (and parent/child (parent-of? parent/child new))])
    ;; update
    (set! missed-optimizations-log
          (cond [parent/child
                 ;; we replace the related entry with a new one
                 (cons (if parent?
                           (combine-missed-optmizations parent/child new)
                           (combine-missed-optmizations new parent/child))
                       (remove parent/child missed-optimizations-log))]
                ;; no related entry, just add the new one
                [else (cons new missed-optimizations-log)]))))

(define (format-missed-optimization m)
  (let ([kind      (missed-optimization-kind      m)]
        [irritants (missed-optimization-irritants m)])
    (if (not (null? irritants))
        (format "~a -- caused by: ~a"
                kind
                (string-join
                 (map (lambda (irritant)
                        (format "~a ~a"
                                (line+col->string irritant)
                                (syntax->datum irritant)))
                      (sort irritants <
                            #:key (lambda (x)
                                    (or (syntax-position x) 0))))
                 ", "))
        kind)))
