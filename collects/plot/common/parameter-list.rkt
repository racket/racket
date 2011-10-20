#lang racket/base

;; Provides a way to treat a list of parameters as a parameter itself

(require racket/match racket/list
         (for-syntax racket/base)
         ;; Can't make parameter lists first-class values without these:
         (only-in '#%paramz parameterization-key extend-parameterization))

(provide parameter-list parameter-list? parameter-list* parameter-list-append
         parameterize/list parameterize*/list)

(define (check-values! name n v)
  (unless (and (list? v) (= n (length v)))
    (raise-type-error name (format "list of ~a values" n) v)))

;; A wrapper for a list of parameters that acts like a parameter-procedure
(struct parameter-list-procedure (params)
  #:property prop:procedure
  (case-lambda
    [(p)    (map (λ (param) (param)) (parameter-list-procedure-params p))]
    [(p v)  (define params (parameter-list-procedure-params p))
            (define n (length params))
            (check-values! 'parameter-list-procedure n v)
            (for ([param  (in-list params)] [val  (in-list v)])
              (param val))]))

(define parameter-list? parameter-list-procedure?)

;; Raises a type error when one of 'params' isn't a parameter or parameter list
(define (check-parameters! name params)
  (for ([param  (in-list params)] [i  (in-naturals)])
    (unless (or (parameter? param) (parameter-list? param))
      (apply raise-type-error name "parameter or parameter-list" i params))))

;; Main constructor for a parameter list
(define (parameter-list . params)
  (check-parameters! 'parameter-list params)
  (parameter-list-procedure params))

;; Corresponds to list*
(define (parameter-list* fst . rst)
  (match-define (list params ... p) (cons fst rst))
  (check-parameters! 'parameter-list params)
  (unless (parameter-list? p) (raise-type-error 'parameter-list* "parameter-list" p))
  (parameter-list-procedure (append params (parameter-list-procedure-params p))))

;; Corresponds to append
(define (parameter-list-append . ps)
  (for ([p  (in-list ps)] [i  (in-naturals)])
    (unless (parameter-list? p)
      (apply raise-type-error 'parameter-list-append "parameter-list" i ps)))
  (parameter-list-procedure (append* (map parameter-list-procedure-params ps))))

;; Given the left and right side of a 'parameterize' binding, returns a list of alternating
;; parameters and parameter values
(define (extract-parameterization p v)
  (cond [(parameter? p)       (list p v)]
        [(parameter-list? p)  (define params (parameter-list-procedure-params p))
                              (define n (length params))
                              (check-values! 'parameterize n v)
                              (append* (map extract-parameterization params v))]
        [else  (raise-type-error 'parameterize/list "parameter or parameter-list" p)]))

;; Corresponds to parameterize
(define-syntax (parameterize/list stx)
  (syntax-case stx ()
    [(_ () expr1 expr ...)
     (syntax-protect (syntax/loc stx (let () expr1 expr ...)))]
    [(_ ([p v] ...) expr1 expr ...)
     (syntax-protect
      (syntax/loc stx
        (with-continuation-mark parameterization-key
          (apply extend-parameterization
                 (continuation-mark-set-first #f parameterization-key)
                 (append (extract-parameterization p v) ...))
          (let () expr1 expr ...))))]))

;; Corresponds to parameterize*
(define-syntax parameterize*/list
  (syntax-rules ()
      [(_ () body1 body ...)
       (let () body1 body ...)]
      [(_ ([lhs1 rhs1] [lhs rhs] ...) body1 body ...)
       (parameterize/list
        ([lhs1 rhs1])
        (parameterize*/list
         ([lhs rhs] ...)
         body1 body ...))]))
