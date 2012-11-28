#lang racket/base

(require "structs.rkt" "utils.rkt" "profiling.rkt")

(provide report-hidden-costs)

(define (report-hidden-costs TR-log profile hot-functions)
  (apply
   append
   (for/list ([node (in-list (profile-nodes profile))])
     (process-profile-node node hot-functions TR-log
                           (profile-total-time profile)))))

(define (process-profile-node profile-entry hot-functions TR-log total-time)
  (define produced-entries '())
  (define (emit e) (set! produced-entries (cons e produced-entries)))

  (define inside-hot-function? (memq profile-entry hot-functions))

  (define (inside-us? pos)
    (pos-inside-us? pos (node-pos profile-entry) (node-span profile-entry)))

  (define badness-multiplier (/ (node-self profile-entry) total-time))
  ;; base values below are arbitrary
  ;; uses ceiling to never go down to 0
  ;; both badness and badness-multiplier are non-0
  (define parameter-access-badness    (ceiling (* 20 badness-multiplier)))
  (define struct-construction-badness (ceiling (* 20 badness-multiplier)))
  (define exact-real-arith-badness    (ceiling (* 20 badness-multiplier)))

  (define (check-hidden-cost kind message badness)
    (when inside-hot-function?
      (for/list ([TR-entry (in-list TR-log)]
                 #:when (info-log-entry? TR-entry)
                 #:when (equal? (log-entry-kind TR-entry) kind)
                 #:when (inside-us? (log-entry-pos TR-entry)))
        (emit (missed-opt-log-entry
               "" ; kind not used at this point
               message
               (log-entry-located-stx TR-entry) (log-entry-located-stx TR-entry)
               (log-entry-pos TR-entry) 'typed-racket
               '() '()
               badness)))))

  (check-hidden-cost
   "hidden parameter"
   (string-append "This function may implicitly dereference the "
                  "`current-output-port' parameter. " ;; TODO hard coded
                  "It may be faster to take the value of the "
                  "parameter once, outside hot code, and pass it "
                  "to this function as an argument.")
   parameter-access-badness)

  (check-hidden-cost
   "struct constructor"
   (string-append
    "This struct constructor is used in hot code. "
    "Allocating structs is expensive, consider using vectors instead. "
    "To keep the same interface, consider defining macro wrappers "
    "around the vector operations that have the same name as the "
    "struct constructor and accessors.")
   struct-construction-badness)

  (check-hidden-cost
   "exact real arith"
   (string-append
    "This expression may use exact rational arithmetic, which is inefficient. "
    "You can avoid this by using operations that don't return fractional "
    ;; TODO don't hard-code `quotient', show the right one depending on the operation
    "results, such as `quotient', or using floating-point numbers.")
   exact-real-arith-badness)

  produced-entries)
