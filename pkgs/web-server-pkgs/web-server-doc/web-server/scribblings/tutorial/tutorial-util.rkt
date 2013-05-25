#lang racket/base
(require scribble/basic
         (for-syntax racket/base
                     racket/port)
         racket/include
         (except-in scribble/manual link))
(provide external-file)

; Copied from guide/scribblings/contracts-utils
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
     #`(include/reader #,(format "examples/~a" (syntax-e #'filename))
                       comment-racketmod-reader)]))
