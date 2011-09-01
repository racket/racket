#lang racket/base

(require racket/set racket/string racket/match racket/list
         unstable/syntax unstable/logging
         "../utils/tc-utils.rkt")

(provide log-optimization log-missed-optimization
         print-log clear-log
         log-message-from-tr-opt?
         with-intercepted-tr-logging with-tr-logging-to-port
         (struct-out log-entry)
         (struct-out opt-log-entry)
         (struct-out missed-opt-log-entry))

;;--------------------------------------------------------------------

;; to identify log messages that come from the optimizer
;; to be stored in the data section of log messages
;; external tools/scripts (like the test harness) can look for it
;; since this is used across phases, can't be a gensym
(define optimization-log-key 'log-message-coming-from-the-TR-optimizer)

;; we keep track of log entries, to avoid repetitions that would be
;; caused by traversing the same syntax multiple times (which is not
;; a problem per se)
(define log-so-far '())

;; msg is for consumption by the DrRacket tool
(struct log-entry (kind msg stx located-stx pos) #:prefab)
;; for optimizations only (not missed optimizations, those are below)
(struct opt-log-entry log-entry () #:prefab)


(define (log-optimization kind msg stx)
  (let ([new-entry
         (opt-log-entry kind msg
                        stx (locate-stx stx) (syntax-position stx))])
    (set! log-so-far (cons new-entry log-so-far))))

;;--------------------------------------------------------------------

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
(struct missed-opt-log-entry log-entry
        (irritants merged-irritants badness)
        #:prefab)

(define missed-optimizations-log '())

;; is parent the "parent" missed optimization of child?
;; this determines whether they get reported together or not
;; currently, parents and children must be of the same kind of missed
;; optimization, and the child must be an irritant of the parent, or be a
;; merged irritant of the parent
(define (parent-of? parent child)
  (and (equal? (log-entry-kind parent)
               (log-entry-kind child))
       (member (log-entry-stx child)
               (append (missed-opt-log-entry-irritants parent)
                       (missed-opt-log-entry-merged-irritants parent)))))

;; combine reporting of two missed optimizations, increasing badness in the
;; process
(define (combine-missed-optmizations parent child)
  (missed-opt-log-entry
   (log-entry-kind        parent) ; same as child's
   (log-entry-msg         parent)
   (log-entry-stx         parent) ; we report the outermost one
   (log-entry-located-stx parent)
   (log-entry-pos         parent)

   (remove-duplicates
    (append (remove (log-entry-stx child)
                    (missed-opt-log-entry-irritants parent))
            (missed-opt-log-entry-irritants child)))
   (remove-duplicates
    (append (missed-opt-log-entry-merged-irritants child)
            (missed-opt-log-entry-merged-irritants parent)
            ;; we merge child in, keep it for future merges
            (list (log-entry-stx child))))
   (+ (missed-opt-log-entry-badness parent)
      (missed-opt-log-entry-badness child))))

;; Attempts to merge the incoming missed optimization with existing ones.
;; Otherwise, adds the new one to the log.
(define (log-missed-optimization kind msg stx [irritants '()])
  (let* (;; for convenience, if a single irritant is given, wrap it in a list
         ;; implicitly
         [irritants (if (list? irritants) irritants (list irritants))]
         [new
          (missed-opt-log-entry kind msg
                                stx (locate-stx stx) (syntax-position stx)
                                irritants '() 1)]
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
                 (if (member (log-entry-stx new)
                             (missed-opt-log-entry-merged-irritants
                              parent))
                     ;; we have been merged in the past, do nothing
                     missed-optimizations-log
                     ;; do the actual merge
                     (cons (combine-missed-optmizations parent new)
                           (remove parent missed-optimizations-log)))]
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

;; When we know all missed opts are known and merging has been done, we
;; can add them to the regular log.
(define (add-missed-opts-to-log)
  (set! log-so-far (append log-so-far missed-optimizations-log)))


;;--------------------------------------------------------------------

;; Sort log according to source location. Returns the sorted log.
(define (sort-log)
  (sort (remove-duplicates log-so-far)
        (match-lambda*
         [(list (log-entry kind-x msg-x stx-x loc-stx-x pos-x)
                (log-entry kind-y msg-y stx-y loc-stx-y pos-y))
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
                       [else (< pos-x pos-y)])])])))

(define (line+col->string stx)
  (let ([line (syntax-line stx)]
        [col  (syntax-column stx)])
    (if (and line col)
        (format "~a:~a" line col)
        "(no location)")))

(define (format-irritants irritants)
  (if (null? irritants)
      ""
      (format " -- caused by: ~a"
              (string-join
               (map (lambda (irritant)
                      (let ([irritant (locate-stx irritant)])
                        (format "~a ~s"
                                (line+col->string irritant)
                                (syntax->datum irritant))))
                    (sort irritants <
                          #:key (lambda (x)
                                  (or (syntax-position x) 0))))
               ", "))))

;; For command-line printing purposes.
;; Not as user friendly as what's produced by the DrRacket tool.
(define (format-log-entry entry)
  (define stx (log-entry-located-stx entry))
  (define msg
    (format "~a ~a ~s -- ~a"
            (syntax-source-file-name stx)
            (line+col->string stx)
            (syntax->datum stx)
            (log-entry-kind entry)))
  (cond [(opt-log-entry? entry)
         (format "TR opt: ~a" msg)]
        [(missed-opt-log-entry? entry)
         (define badness (missed-opt-log-entry-badness entry))
         (format "TR missed opt: ~a~a~a"
                 msg
                 (format-irritants (missed-opt-log-entry-irritants entry))
                 (if (> badness 1)
                     (format " (~a times)" badness)
                     ""))]))


;; Once the optimizer is done, we sort the log according to source
;; location, then print it.
(define (print-log)
  (define logger (current-logger))
  (add-missed-opts-to-log)
  (for ([x (sort-log)])
    (log-message logger 'debug
                 (format-log-entry x)
                 (cons optimization-log-key x))))

(define (clear-log)
  (set! log-so-far '())
  (set! missed-optimizations-log '()))

;;--------------------------------------------------------------------

(define (log-message-from-tr-opt? l)
  (let ([data (vector-ref l 2)])
    (and (pair? data)
         (eq? (car data) optimization-log-key))))

;; only intercepts TR log messages
(define (with-intercepted-tr-logging interceptor thunk)
  (with-intercepted-logging
   #:level 'debug
   (lambda (l) ;; look only for optimizer messages
     (when (log-message-from-tr-opt? l)
       (interceptor l)))
   thunk))

(define (with-tr-logging-to-port port thunk)
  (with-intercepted-tr-logging
   (lambda (l)
     (displayln (vector-ref l 1) port)) ; print log message
   thunk))
