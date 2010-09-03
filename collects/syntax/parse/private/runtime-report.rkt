#lang racket/base
(require racket/list
         "minimatch.rkt"
         "runtime.rkt"
         "kws.rkt")
(provide syntax-patterns-fail
         current-failure-handler)

(define ((syntax-patterns-fail stx0) fs)
  (call-with-values (lambda () ((current-failure-handler) stx0 fs))
    (lambda vals
      (error 'current-failure-handler
             "current-failure-handler: did not escape, produced ~e"
             (case (length vals)
               ((1) (car vals))
               (else (cons 'values vals)))))))

(define (default-failure-handler stx0 fs)
  (report-failureset stx0 fs))

(define current-failure-handler
  (make-parameter default-failure-handler))

;; ----

#|
Reporting
---------

First, failures with maximal (normalized) progresses are selected and
grouped into equivalence classes. In principle, each failure in an
equivalence class complains about the same term, but in practice,
special handling of failures like "unexpected term" make things more
complicated.

|#

;; report-failureset : stx FailureSet -> escapes
(define (report-failureset stx0 fs)
  (let* ([classes (maximal-failures fs)]
         [reports (apply append (map report/class classes))])
    (raise-syntax-error/reports stx0 reports)))

;; A Report is
;;   - (report string stx)
(define-struct report (message stx) #:prefab)

;; report/class : (non-empty-listof Failure) -> (listof Report)
(define (report/class fs)
  (let* ([ess (map failure-expectstack fs)]
         [ess (map normalize-expectstack ess)]
         [ess (remove-duplicates ess)]
         [ess (simplify-common-expectstacks ess)])
    (map report/expectstack ess)))

;; report/expectstack : ExpectStack -> Report
(define (report/expectstack es)
  (let ([top-frame (and (pair? es) (car es))])
    (cond [(not top-frame)
           (report "bad syntax" #f)]
          [else
           (let ([frame-expect (and top-frame (car top-frame))]
                 [frame-stx (and top-frame (cdr top-frame))])
             (cond [(equal? frame-expect (expect:atom '()))
                    (syntax-case frame-stx ()
                      [(one . more)
                       (report "unexpected term" #'one)]
                      [_
                       (report/expects (list frame-expect) frame-stx)])]
                   [(expect:disj? frame-expect)
                    (report/expects (expect:disj-expects frame-expect) frame-stx)]
                   [else
                    (report/expects (list frame-expect) frame-stx)]))])))

;; report/expects : (listof Expect) -> Report
(define (report/expects expects frame-stx)
  (report (join-sep (for/list ([expect expects])
                      (prose-for-expect expect))
                    ";" "or")
          frame-stx))

;; prose-for-expect : Expect -> string
(define (prose-for-expect e)
  (match e
    [(expect:thing description transparent?)
     (format "expected ~a" description)]
    [(expect:atom atom)
     (format "expected the literal ~a~s~a"
             (if (symbol? atom) "symbol `" "")
             atom
             (if (symbol? atom) "'" ""))]
    [(expect:literal literal)
     (format "expected the identifier `~s'" (syntax-e literal))]
    [(expect:message message)
     (format "~a" message)]))

;; == Do Report ==

(define (raise-syntax-error/reports stx0 reports)
  (cond [(= (length reports) 1)
         (raise-syntax-error/report stx0 (car reports))]
        [else
         (raise-syntax-error/report* stx0 (car reports))]))

(define (raise-syntax-error/report stx0 report)
  (raise-syntax-error #f (report-message report) stx0 (report-stx report)))

(define (raise-syntax-error/report* stx0 report)
  (let ([message
         (string-append
          "There were multiple syntax errors. The first error follows:\n"
          (report-message report))])
    (raise-syntax-error #f message stx0 (report-stx report))))

;; ====

(define (comma-list items)
  (join-sep items "," "or"))

(define (improper-stx->list stx)
  (syntax-case stx ()
    [(a . b) (cons #'a (improper-stx->list #'b))]
    [() null]
    [rest (list #'rest)]))
