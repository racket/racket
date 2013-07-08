#lang racket/base
(require "drracket-link.rkt")

;; Procedures which *may* be overridden by DrRacket to do useful things.
;; Or they may not be.

(provide has-backtrace?
         has-errortrace-backtrace?
         has-primitive-backtrace?
         show-errortrace-backtrace
         show-primitive-backtrace
         can-show-source?
         show-source)

;; A Backtrace is one of
;;   - exn
;;   - (listof srcloc)

(define USE-PRIMITIVE-STACKTRACE? #f)

;; has-backtrace? : exn -> boolean
(define (has-backtrace? exn)
  (or (has-errortrace-backtrace? exn)
      (has-primitive-backtrace? exn)))

;; has-errortrace-backtrace? : exn -> boolean
(define (has-errortrace-backtrace? exn)
  (not (null? (get-errortrace-backtrace exn))))

;; has-primitive-backtrace? : exn -> boolean
(define (has-primitive-backtrace? exn)
  (and USE-PRIMITIVE-STACKTRACE?
       (pair? (get-primitive-backtrace exn))))

;; get-errortrace-backtrace : exn -> Backtrace
(define (get-errortrace-backtrace exn)
  ((get-errortrace-backtrace*) exn))

;; get-primitive-backtrace : exn -> Backtrace
(define (get-primitive-backtrace exn)
  (let* ([ctx (continuation-mark-set->context
               (exn-continuation-marks exn))]
         [srclocs (map cdr ctx)])
    (filter (lambda (s)
              (and (srcloc? s)
                   (let ([src (srcloc-source s)])
                     (and (path? src)
                          (not (regexp-match?
                                (regexp-quote
                                 (path->string
                                  (collection-path "rackunit" "private" "gui")))
                                (path->string src)))))))
            srclocs)))

;; show-errortrace-backtrace : exn -> void
(define (show-errortrace-backtrace exn)
  ((show-backtrace*)
   (exn-message exn)
   (get-errortrace-backtrace exn)))

;; show-primitive-backtrace : exn -> void
(define (show-primitive-backtrace exn)
  ((show-backtrace*)
   (exn-message exn)
   (get-primitive-backtrace exn)))

;; can-show-source? : -> boolean
(define (can-show-source?)
  (can-show-source?*))

;; show-source : source number number -> void
(define (show-source src pos span)
  ((show-source*) src pos span))

;; ----

(define (get-link n)
  (let ([v (unbox link)])
    (and (vector? v) (vector-ref v n))))

(define (get-errortrace-backtrace*)
  (or (get-link 0)
      (lambda (exn) null)))

(define (show-backtrace*)
  (or (get-link 1)
      void))

(define (show-source*)
  (or (get-link 2)
      void))

(define (can-show-source?*)
  (vector? (unbox link)))
