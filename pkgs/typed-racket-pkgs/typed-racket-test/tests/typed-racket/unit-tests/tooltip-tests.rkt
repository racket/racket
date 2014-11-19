#lang racket/base

;; Tests for Typed Racket tooltips that are normally displayed in DrRacket.
;; These tests capture those results by listening to a logger and checks that
;; certain types are recorded at the right locations.

(require "test-utils.rkt"
         racket/match
         rackunit
         unstable/list
         (for-syntax racket/base))

(provide tests)
(gen-test-main)

(define-for-syntax debug? #f)
(define-syntax (debug stx)
  (syntax-case stx ()
    [(_ e) debug? #'e]
    [_ #'(void)]))

(define-logger online-check-syntax)
(define receiver
  (make-log-receiver online-check-syntax-logger 'info 'online-check-syntax))

;; This checks the given predicate on the tooltip vectors and will also
;; check that there's only one tooltip provided per location
(define-syntax-rule (check-tooltip exp pred)
  (check-true (run-tooltip-test (quote exp) pred)))

(define (run-tooltip-test sexp pred)
  (define namespace (make-base-namespace))
  (define-values (in out) (make-pipe))
  (port-count-lines! in)
  (port-count-lines! out)
  (write `(module a typed/racket ,sexp) out)
  (parameterize ([current-logger online-check-syntax-logger]
                 [current-namespace namespace])
    (eval-syntax (namespace-syntax-introduce (read-syntax 'tester in))))

  (log-message online-check-syntax-logger 'info 'online-check-syntax "done" 'done)

  (define result (process-tooltips pred))
  (clear)
  result)

(define (process-tooltips pred)
  (let loop ()
    (define result (sync receiver))
    (cond [(eq? 'done (vector-ref result 2)) 'no-tooltips]
          [else
           (define stxs (vector-ref result 2))
           (define tooltips
             (and (list? stxs)
                  (syntax? (car stxs))
                  (syntax-property (car stxs) 'mouse-over-tooltips)))
           (if tooltips
               (and (pred tooltips)
                    (unique-locations? tooltips))
               (loop))])))

(define (clear)
  (let loop ()
    (when (sync/timeout 0 receiver)
      (loop))))

;; has-type-at? : (Listof (List String Int Int)) -> (Listof Vector) -> Boolean
(define ((has-types-at? lst) tooltips)
  ;; turn debug? on to print the tooltip types and locations
  (debug
   (for ([tooltip (in-list tooltips)])
     (match-define (vector stx start* end* type*) tooltip)
     (printf "~a ~a ~a~n" start* end* (if (procedure? type*) (type*) type*))))
  (for/and ([entry (in-list lst)])
    (match-define (list regex start end) entry)
    (for/or ([tooltip (in-list tooltips)])
      (match-define (vector stx start* end* type*) tooltip)
      (and (= start start*)
           (= end end*)
           (regexp-match? regex (if (procedure? type*) (type*) type*))))))

;; ensures there are no duplicate type tooltips for a single syntax location
(define (unique-locations? tooltips)
  (define locations
    (for/list ([tooltip (in-list tooltips)])
      (match-define (vector _ start end _) tooltip)
      (list start end)))
  (if (check-duplicate locations)
      'duplicate-tooltips
      #t))

(define tests
  (test-suite "Tooltip tests"
    (check-tooltip (string-append "foo" "bar")
                   (has-types-at? (list (list "^String$" 38 43))))
    (check-tooltip (for/list : (Listof Integer) ([i (list 1 2 3)]) i)
                   (has-types-at? (list (list "^\\(Listof Integer\\)$" 23 24)
                                        (list "^\\(Listof Integer\\)$" 72 73))))
    (check-tooltip (class object% (super-new) (field [x : Integer 0]) x (set! x 3))
                   (has-types-at? (list (list "^Integer$" 74 75))))
    (check-tooltip (values 1 2)
                   (has-types-at? (list (list #rx"Value 1:.*One.*Value 2:.*Positive-Byte"
                                              23 24))))))
