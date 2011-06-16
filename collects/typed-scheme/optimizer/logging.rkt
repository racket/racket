#lang racket/base

(require racket/set racket/string racket/match racket/list
         unstable/syntax
         "../utils/tc-utils.rkt")

(provide log-optimization log-missed-optimization
         optimization-log-key
         print-log clear-log)

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(struct log-entry (msg stx pos) #:transparent)

;; to identify log messages that come from the optimizer
;; to be stored in the data section of log messages
;; external tools/scripts (like the test harness) can look for it
;; since this is used across phases, can't be a gensym
(define optimization-log-key 'log-message-coming-from-the-TR-optimizer)

;; we keep track of log entries, to avoid repetitions that would be
;; caused by traversing the same syntax multiple times (which is not
;; a problem per se)
(define log-so-far '())

(define (gen-log-message msg stx from)
  (let ([stx (locate-stx stx)])
    (format "~a: ~a ~a ~a -- ~a"
            from
            (syntax-source-file-name stx)
            (line+col->string stx)
            (syntax->datum stx)
            msg)))

(define (log-optimization msg stx #:from [from "TR opt"])
  (let* ([new-message (gen-log-message msg stx from)]
         [new-entry (log-entry new-message stx (syntax-position stx))])
    (set! log-so-far (cons new-entry log-so-far))))

;; once the optimizer is done, we sort the log according to source
;; location, then print it
(define (print-log)
  (define logger (current-logger))
  ;; add missed optimizations messages to the log, now that we know all of them
  (for-each (lambda (x) (log-optimization (format-missed-optimization x)
                                          (missed-optimization-stx x)
                                          #:from "TR missed opt"))
            missed-optimizations-log)
  (for-each (lambda (x) (log-message logger 'warning (log-entry-msg x)
                                     optimization-log-key))
            (sort (remove-duplicates log-so-far)
                  (match-lambda*
                   [(list (log-entry msg-x stx-x pos-x)
                          (log-entry msg-y stx-y pos-y))
                    (cond [(not (or pos-x pos-y))
                           ;; neither have location, sort by message
                           (string<? msg-x msg-y)]
                          [(not pos-y) #f]
                          [(not pos-x) #t]
                          [else
                           ;; both have location
                           (cond [(= pos-x pos-y)
                                  ;; same location, sort by message
                                  (string<? msg-x msg-y)]
                                 ;; sort by source location
                                 [else (< pos-x pos-y)])])]))))
(define (clear-log)
  (set! log-so-far '())
  (set! missed-optimizations-log '()))


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
         ;; check if the new one is the child of an old one
         ;; for/first is ok, since we can only have one parent in the list
         ;; (if we had more, one would have to be the parent of the other, so
         ;; only one would be in the list)
         [parent (for/first ([m (in-list missed-optimizations-log)]
                             #:when (parent-of? m new))
                   m)]
         ;; do we have children in the list, if so, merge with all of them
         [children (for/list ([m (in-list missed-optimizations-log)]
                              #:when (parent-of? new m))
                     m)])
    ;; update
    (set! missed-optimizations-log
          (cond [parent
                 ;; we found our parent, merge with it
                 (cons (combine-missed-optmizations parent new)
                       (remove parent missed-optimizations-log))]
                [(not (null? children))
                 ;; we found children, merge with them
                 (let ([new (for/fold ([new new])
                                ([child children])
                              (combine-missed-optmizations new child))])
                   (cons new
                         (filter (lambda (x) (not (member x children)))
                                 missed-optimizations-log)))]
                [else
                 ;; no related entry, just add the new one
                 (cons new missed-optimizations-log)]))))

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
