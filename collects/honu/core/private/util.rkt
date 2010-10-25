#lang racket/base

(provide (except-out (all-defined-out) test-delimiter))
(require "debug.rkt"
         tests/eli-tester
         racket/match
         (for-syntax racket/base)
         syntax/stx
         racket/list)

#;
(provide delim-identifier=?
         extract-until
         call-values)


(define (delim-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define extract-until
  (case-lambda
    [(r ids keep?)
     (let loop ([r r][val-stxs null])
       (cond
         [(stx-null? r)
          (values #f #f #f)]
         [(and (identifier? (stx-car r))
               (ormap (lambda (id)
                        (delim-identifier=? id (stx-car r)))
                      ids))
          (values (reverse (if keep?
                             (cons (stx-car r) val-stxs) 
                             val-stxs))
                  r
                  (stx-car r))]
         [else
           (loop (stx-cdr r) (cons (stx-car r) val-stxs))]))]
    [(r ids) (extract-until r ids #f)]))

(define-syntax-rule (call-values values-producing function)
  (call-with-values (lambda () values-producing) function))

;; shortcut for treating arguments as syntax objects
(define-syntax (syntax-lambda stx)
  (syntax-case stx ()
    [(_ (arg ...) body ...)
     (with-syntax ([(temp ...) (generate-temporaries #'(arg ...))])
       #'(lambda (temp ...)
           (with-syntax ([arg temp] ...)
             body ...)))]))

;; removes the last element of a list
(define (drop-last lst)
  (take lst (sub1 (length lst))))

(define (test-delimiter)
  (let* ([original #'(a b c d e)]
         [delimiter #'c]
         [expected-before #'(a b)]
         [expected-rest #'(c d e)]
         [expected-delimiter #'c]
         )
    (let-values ([(before rest hit) (extract-until original (list delimiter))])
      ;; is there a better way to test equality between two syntaxes?
      (when (not (and (equal? (syntax->datum expected-before)
                              (map syntax->datum before))
                      (equal? (syntax->datum expected-rest)
                              (map syntax->datum rest))
                      (equal? (syntax->datum expected-delimiter)
                              (syntax->datum hit))))
        (debug "failure: original ~a until ~a\n" (syntax->datum original) (map syntax->datum (list delimiter)))
        (debug " before expected ~a actual ~a\n" (syntax->datum expected-before) (map syntax->datum before))
        (debug " rest expected ~a actual ~a\n" (syntax->datum expected-rest) (map syntax->datum rest))
        (debug " delimiter expected ~a actual ~a\n" (syntax->datum expected-delimiter) (syntax->datum hit))
        ))))

#;
(test)

;; better version of caddadadr-type functions
(define-syntax (list-match stx)
  (define (convert-pattern pattern)
    (syntax-case pattern ()
      [(x ...) (with-syntax ([(x* ...) (map convert-pattern (syntax->list #'(x ...)))])
                 #'(list x* ...))]
      [x #'x]))
  (define (extract-variable pattern)
    (define (non-wildcard-identifier? x)
      (and (identifier? x)
           (not (eq? (syntax-e x) '_))))
    (syntax-case pattern ()
      [(a x ...)
       (if (non-wildcard-identifier? #'a)
         #'a
         (or (extract-variable #'a)
             (extract-variable #'(x ...))))]
      [x (if (non-wildcard-identifier? #'x)
           #'x
           #f)]))
  (syntax-case stx ()
    [(_ pattern expression)
     (with-syntax ([match-pattern (convert-pattern #'pattern)]
                   [match-variable (extract-variable #'pattern)])
       #'(match expression
           [match-pattern match-variable]))]))

#;
(test
  (list-match a '(1 2 3)) => '(1 2 3)
  (list-match (a _ ...) '(1 2 3)) => 1
  (list-match (_ _ a ...) '(1 2 3 4)) => '(3 4)
  (list-match ((_ a _ ...) _ ...) '((1 2 3 4) 5 6)) => 2
  (list-match ((_ _ a _ ...) _ ...) '((7 6 5 4 3 2 1) 8 9)) => 5
  )
