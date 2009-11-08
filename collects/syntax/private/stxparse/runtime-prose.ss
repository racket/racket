#lang scheme/base
(require scheme/contract/base
         scheme/list
         "minimatch.ss"
         scheme/stxparam
         syntax/stx
         (for-syntax scheme/base)
         (for-syntax syntax/stx)
         (for-syntax scheme/private/sc)
         (for-syntax "rep-data.ss")
         (for-syntax "../util.ss")
         "runtime.ss")
(provide syntax-patterns-fail
         current-failure-handler
         simplify-failure)

;; Failure reporting parameter & default

(define (default-failure-handler stx0 f)
  (match (simplify-failure f)
    [#s(failure x frontier expectation)
     (report-failure stx0 x (dfc->index frontier) (dfc->stx frontier) expectation)]))

(define current-failure-handler
  (make-parameter default-failure-handler))

(define ((syntax-patterns-fail stx0) f)
  (call-with-values (lambda () ((current-failure-handler) stx0 f))
    (lambda vals
      (error 'current-failure-handler
             "current-failure-handler: did not escape, produced ~e"
             (case (length vals)
               ((1) (car vals))
               (else (cons 'values vals)))))))

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
            ;; TODO: report error with all elements (use improper-stx->list)
            (err "unexpected terms starting here" stx0 #'first)]
           [_
            (err "unexpected term" stx0 x)])]
        [(and expected (prose-for-expectation expected index x))
         =>
         (lambda (msg)
           (err (format "~a~a"
                        msg
                        (cond [(zero? index) ""]
                              [else (format " after ~s ~a"
                                            index
                                            (if (= 1 index) "term" "terms"))]))
                stx0
                frontier-stx))]
        [else
         (err "bad syntax" stx0 stx0)]))

;; simplify-failure : Failure -> SimpleFailure
(define (simplify-failure f)
  (simplify* f))

;; simplify* : Failure -> SimpleFailure
(define (simplify* f)
  (match f
    [#s(join-failures f1 f2)
     (choose-error (simplify* f1) (simplify* f2))]
    [#s(failure x frontier expectation)
     (match expectation
       [#s(expect:thing description '#t chained)
        (let ([chained* (simplify* chained)])
          (match chained*
            [#s(failure _ chained*-frontier chained*-expectation)
             (cond [(ineffable? chained*-expectation)
                    ;; If simplified chained failure is ineffable,
                    ;; keep (& adjust) its frontier
                    ;; and attach enclosing description
                    (adjust-failure
                     (make-failure x chained*-frontier
                                   (make-expect:thing description #f #f))
                     frontier)]
                   [else
                    ;; Otherwise, "expose" the chained failure and
                    ;; adjust its frontier
                    (adjust-failure chained* frontier)])]))]
       [_ f])]))

;; FIXME: try different selection/simplification algorithms/heuristics
(define (simplify-failure0 f)
  (match f
    [#s(join-failures f1 f2)
     (choose-error (simplify-failure0 f1) (simplify-failure0 f2))]
    [#s(failure x frontier expectation)
     (match expectation
       [#s(expect:thing description '#t chained)
        (let ([chained* (simplify-failure0 chained)])
          (match chained*
            [#s(failure _ _ chained*-expectation)
             (cond [(ineffable? chained*-expectation)
                    ;; If simplified chained failure is ineffable, ignore it
                    ;; and stick to the one with the description
                    f]
                   [else
                    ;; Otherwise, "expose" the chained failure
                    ;; and adjust its frontier
                    (adjust-failure chained* frontier)])]))]
       [_ f])]))

(define (adjust-failure f base-frontier)
  (match f
    [#s(failure x frontier expectation)
     (let ([frontier (dfc-append base-frontier frontier)])
       (make-failure x frontier expectation))]))

;; choose-error : Failure Failure -> Failure
(define (choose-error f1 f2)
  (case (compare-idfcs (invert-dfc (failure-frontier f1))
                       (invert-dfc (failure-frontier f2)))
    [(>) f1]
    [(<) f2]
    [(=) (merge-failures f1 f2)]))

;; merge-failures : failure failure -> failure
(define (merge-failures f1 f2)
  (make-failure (failure-stx f1)
                (failure-frontier f1)
                (merge-expectations (failure-expectation f1)
                                    (failure-expectation f2))))

;; ----

;; prose-for-expectation : Expectation syntax -> string/#f
(define (prose-for-expectation e index stx)
  (cond [(expect? e)
         (let ([alts (expect->alternatives e)])
           (and alts
                (join-sep (for/list ([alt alts])
                            (for-alternative alt index stx))
                          ";" "or")))]
        [(eq? e 'ineffable)
         #f]))

(define (for-alternative e index stx)
  (match e
    [#s(expect:thing description transparent? chained)
     (format "expected ~a" description)]
    [#s(expect:atom atom)
     (format "expected the literal ~s" atom)]
    [#s(expect:literal literal)
     (format "expected the literal identifier ~s" (syntax-e literal))]
    [#s(expect:message message)
     (format "~a" message)]
    [#s(expect:pair)
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

(define (improper-stx->list stx)
  (syntax-case stx ()
    [(a . b) (cons #'a (improper-stx->list #'b))]
    [() null]
    [rest (list #'rest)]))
