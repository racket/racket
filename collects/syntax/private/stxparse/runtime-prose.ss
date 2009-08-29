#lang scheme/base
(require scheme/contract
         scheme/list
         scheme/match
         scheme/stxparam
         syntax/stx
         (for-syntax scheme/base)
         (for-syntax syntax/stx)
         (for-syntax scheme/private/sc)
         (for-syntax "rep-data.ss")
         (for-syntax "../util/error.ss")
         "runtime.ss")
(provide syntax-patterns-fail
         current-failure-handler)

;; Failure reporting parameter & default

(define (default-failure-handler stx0 f)
  (match (simplify-failure f)
    [(struct failure (x frontier frontier-stx expected))
     (report-failure stx0 x (last frontier) frontier-stx expected)]))

(define current-failure-handler
  (make-parameter default-failure-handler))

(define ((syntax-patterns-fail stx0) f)
  (let ([value ((current-failure-handler) stx0 f)])
    (error 'current-failure-handler
           "current-failure-handler: did not escape, produced ~e" value)))


;; report-failure : stx stx number stx Expectation -> (escapes)
(define (report-failure stx0 x index frontier-stx expected)
  (define (err msg stx0 stx)
    (raise-syntax-error #f msg stx0 stx))
  (cond [(expectation-of-null? expected)
         ;; FIXME: "extra term(s) after <pattern>"
         (syntax-case x ()
           [(one)
            (err "unexpected term" stx0 #'one)]
           [(first . more)
            (err "unexpected terms starting here" stx0 #'first)]
           [_
            (err "unexpected term" stx0 x)])]
        [(and expected (prose-for-expectation expected index x))
         =>
         (lambda (msg)
           (err (format "~a~a"
                        msg
                        (cond [(zero? index) ""]
                              [(= index +inf.0) " after matching main pattern"]
                              [else (format " after ~s ~a"
                                            index
                                            (if (= 1 index) "term" "terms"))]))
                stx0
                frontier-stx))]
        [else
         (err #f stx0 stx0)]))

;; FIXME: try different selection/simplification algorithms/heuristics
(define (simplify-failure f)
  (match f
    [(struct join-failures (f1 f2))
     (choose-error (simplify-failure f1) (simplify-failure f2))]
    [(struct failure (x frontier frontier-stx expectation))
     (match expectation
       [(struct expect:thing (description (and transparent? #t) chained))
        (match (simplify-failure (adjust-failure chained frontier frontier-stx))
          [(struct failure (_ _ _ (? ineffable?)))
           ;; If unfolded failure is ineffable, fall back to the one with description
           f]
          [new-f new-f])]
       [_ f])]))

(define (adjust-failure f base-frontier base-frontier-stx)
  (match f
    [(struct join-failures (f1 f2))
     (make-join-failures
      (adjust-failure f1 base-frontier base-frontier-stx)
      (adjust-failure f2 base-frontier base-frontier-stx))]
    [(struct failure (x frontier frontier-stx expectation))
     (let-values ([(frontier frontier-stx)
                   (combine-frontiers base-frontier base-frontier-stx
                                      frontier frontier-stx)])
       (make-failure x frontier frontier-stx expectation))]))

(define (combine-frontiers dfc0 stx0 dfc stx)
  (cond [(null? (cdr dfc0))
         (values (cons (+ (car dfc0) (car dfc))
                       (cdr dfc))
                 (if (null? (cdr dfc))
                     stx0
                     stx))]
        [else
         (let-values ([(f s) (combine-frontiers (cdr dfc0) stx0 dfc stx)])
           (values (cons (car dfc0) f) s))]))

;; choose-error : Failure Failure -> Result
(define (choose-error f1 f2)
  (case (compare-dfcs (failure-frontier f1) (failure-frontier f2))
    [(>) f1]
    [(<) f2]
    [(=) (merge-failures f1 f2)]))

;; merge-failures : failure failure -> failure
(define (merge-failures f1 f2)
  (make-failure (failure-stx f1)
                (failure-frontier f1)
                (failure-frontier-stx f1)
                (merge-expectations (failure-expectation f1)
                                    (failure-expectation f2))))

;; ----

;; prose-for-expectation : Expectation syntax -> string/#f
(define (prose-for-expectation e index stx)
  (cond [(expect? e)
         (let ([parts
                (for/list ([alt (expect->alternatives e)])
                  (for-alternative alt index stx))])
           (join-sep parts ";" "or"))]
        [(eq? e 'ineffable)
         #f]))

(define (for-alternative e index stx)
  (match e
    [(struct expect:thing (description transparent? chained))
     (format "expected ~a" description)]
    [(struct expect:atom (atom))
     (format "expected the literal ~s" atom)]
    [(struct expect:literal (literal))
     (format "expected the literal identifier ~s" (syntax-e literal))]
    [(struct expect:message (message))
     (format "~a" message)]
    [(struct expect:pair ())
     (cond [(= index 0)
            "expected sequence of terms"]
           [else
            (if (stx-null? stx)
                "expected more terms in sequence"
                "expected sequence of terms")])]))

(define (comma-list items)
  (join-sep items "," "or"))

(define (join-sep items sep0 ult0 [prefix ""])
  (define sep (string-append sep0 " "))
  (define ult (string-append ult0 " "))
  (define (loop items)
    (cond [(null? items)
           null]
          [(null? (cdr items))
           (list sep ult (car items))]
          [else
           (list* sep (car items) (loop (cdr items)))]))
  (case (length items)
    [(0) #f]
    [(1) (string-append prefix (car items))]
    [(2) (format "~a~a ~a~a" prefix (car items) ult (cadr items))]
    [else (let ([strings (list* (car items) (loop (cdr items)))])
            (apply string-append prefix strings))]))
