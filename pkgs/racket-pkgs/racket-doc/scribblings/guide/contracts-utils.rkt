#lang racket

(require scribble/basic
         (for-syntax racket/port)
         (for-label racket rackunit rackunit/text-ui)
         racket/include
         scribble/eval
         (except-in scribble/manual link))

(provide ctc-section
         ctc-link
         exercise
         solution
         external-file
         contract-eval)

(define (ctc-section #:tag [tag #f] . rest)
  (keyword-apply section
                 '(#:tag)
                 (list (and tag (str->tag tag)))
                 rest))

(define (ctc-link tag . rest) (apply seclink (str->tag tag) rest))

(define (str->tag tag) (format "contracts-~a" tag))

(define exercise-number 0)
(define (exercise)
  (set! exercise-number (+ exercise-number 1))
  (bold (format "Exercise ~a" exercise-number)))

(define (solution)
  (bold (format "Solution to exercise ~a" exercise-number)))

(require (for-syntax (only-in scribble/comment-reader [read-syntax comment-reader])))
(define-for-syntax (comment-racketmod-reader path port)
  (let ([pb (peek-byte port)])
    (if (eof-object? pb)
        pb
        (let ([m (regexp-match #rx"^#lang " port)])
          (unless m
            (raise-syntax-error 'comment-racket-reader "expected a #lang to begin file ~s" path))
          (let ([np (let-values ([(line col pos) (port-next-location port)])
                      (relocate-input-port port line 0 pos))])
            (port-count-lines! np)
            (let loop ([objects '()])
              (let ([next (comment-reader path np)])
                (cond
                  [(eof-object? next)
                   #`(racketmod #,@(reverse objects))]
                  [else
                   (loop (cons next objects))]))))))))

(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename)
     #`(include/reader #,(format "contracts-examples/~a.rkt" (syntax-e #'filename))
                       comment-racketmod-reader)]))

(define contract-eval (make-base-eval))
(contract-eval '(require racket/contract))
