#lang racket/base
(require "../host/thread.rkt"
         "logger.rkt"
         "receiver.rkt"
         "level.rkt")

(provide logger-wanted-level
         logger-max-wanted-level
         logger-all-levels)

(define (logger-wanted-level logger topic)
  (atomically/no-interrupts
   (cond
     [(not topic) (logger-max-wanted-level logger)]
     [else
      (cond
        [((logger-local-level-timestamp logger) . >= . (unbox (logger-root-level-timestamp-box logger)))
         ;; Cache is up-to-date, so search it
         (define cache (logger-topic-level-cache logger))
         (or (for/or ([i (in-range 0 (vector-length cache) 2)])
               (and (eq? (vector-ref cache i) topic)
                    (vector-ref cache (add1 i))))
             ;; Didn't find in cache, so update the cache
             (begin
               (update-logger-wanted-level! logger topic)
               (logger-wanted-level logger topic)))]
        [else
         ;; Update the cache and retry:
         (update-logger-wanted-level! logger topic)
         (logger-wanted-level logger topic)])])))
                         
(define (logger-max-wanted-level logger)
  (atomically/no-interrupts
   (cond
     [((logger-local-level-timestamp logger) . >= . (unbox (logger-root-level-timestamp-box logger)))
      ;; Ccahed value is up-to-date
      (logger-max-receiver-level logger)]
     [else
      ;; Traverse to set cache:
      (update-logger-wanted-level! logger #f)
      (logger-max-receiver-level logger)])))

(define (update-logger-wanted-level! logger topic)
  (unless ((logger-local-level-timestamp logger) . >= . (unbox (logger-root-level-timestamp-box logger)))
    (define cache (logger-topic-level-cache logger))
    (for/or ([i (in-range 0 (vector-length cache) 2)])
      (vector-set! cache i #f))
    (set-logger-local-level-timestamp! logger (unbox (logger-root-level-timestamp-box logger))))
  ;; As we traverse the parent chain, keep track of the "ceiling"
  ;; level as the maximum level that would be propagated; for any
  ;; receiver, clip the wanted levels to that ceiling.
  (let loop ([parent logger] [ceiling-level 'debug] [old-max-level 'none] [old-topic-max-level 'none])
    (define-values (max-level topic-max-level)
      (for/fold ([max-level old-max-level] [topic-max-level old-topic-max-level])
                ([r (in-list (logger-receivers parent))]
                 #:break (and (max-level . level>=? . ceiling-level)
                              (or (not topic)
                                  (topic-max-level . level>=? . ceiling-level))))
        (values (level-max max-level
                           (level-min (filters-max-level (log-receiver-filters r))
                                      ceiling-level))
                (and topic
                     (level-max topic-max-level
                                (level-min (filters-level-for-topic (log-receiver-filters r) topic)
                                           ceiling-level))))))
    (cond
      [(and (or (ceiling-level . level>=? . max-level)
                (and topic (ceiling-level . level>=? . topic-max-level)))
            (logger-parent parent))
       => (lambda (next-parent)
            (let ([ceiling-level (level-min ceiling-level (filters-max-level (logger-propagate-filters parent)))])
              (loop next-parent ceiling-level max-level topic-max-level)))]
      [else
       ;; No more parents, so save the result
       (set-logger-max-receiver-level! logger max-level)
       (when topic
         (define cache (logger-topic-level-cache logger))
         (or
          ;; Look for empty cache slot:
          (for/or ([i (in-range 0 (vector-length cache) 2)])
            (and (not (vector-ref cache i))
                 (begin
                   (vector-set! cache i topic)
                   (vector-set! cache (add1 i) topic-max-level))
                 #t))
          ;; Rotate cache and put new value at start
          (begin
            (for ([i (in-range 0 (- (vector-length cache) 2) 2)])
              (vector-set! cache (+ i 2) (vector-ref cache i))
              (vector-set! cache (+ i 3) (vector-ref cache (+ i 1))))
            (vector-set! cache 0 topic)
            (vector-set! cache 1 topic-max-level))))])))
  

(define (logger-all-levels logger)
  '(none #f))
