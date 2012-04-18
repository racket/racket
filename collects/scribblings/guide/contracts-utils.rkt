#lang racket

(require scribble/basic
         (for-syntax racket/port)
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

#;
(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename)
     (call-with-input-file (build-path "contracts-examples" (format "~a.rkt" (syntax-e #'filename)))
       (λ (port)
         (define prefix "#reader scribble/comment-reader\n[racketmod\nracket\n")
         (define suffix "]")
         (with-syntax ([s (parameterize ([read-accept-reader #t])
                            (read-syntax 'contract-examples
                                         (input-port-append #f 
                                                            (open-input-string prefix)
                                                            port
                                                            (open-input-string suffix))))])
           #'s)))]))

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
