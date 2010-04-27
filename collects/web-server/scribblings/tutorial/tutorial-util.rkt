#lang scheme
(require scribble/basic
         (for-syntax scheme/port)
         scheme/include
         (except-in scribble/manual link))
(provide external-file)

; Copied from guide/scribblings/contracts-utils
(require (for-syntax (only-in scribble/comment-reader [read-syntax comment-reader])))
(define-for-syntax (comment-schememod-reader path port)
  (let ([pb (peek-byte port)])
    (if (eof-object? pb)
        pb
        (let ([m (regexp-match #rx"^#lang " port)])
          (unless m
            (raise-syntax-error 'comment-scheme-reader "expected a #lang to begin file ~s" path))
          (let ([np (let-values ([(line col pos) (port-next-location port)])
                      (relocate-input-port port line 0 pos))])
            (port-count-lines! np)
            (let loop ([objects '()])
              (let ([next (comment-reader path np)])
                (cond
                  [(eof-object? next)
                   #`(schememod #,@(reverse objects))]
                  [else
                   (loop (cons next objects))]))))))))

(define-syntax (external-file stx)
  (syntax-case stx ()
    [(_ filename)
     #`(include/reader #,(format "examples/~a" (syntax-e #'filename))
                       comment-schememod-reader)]))
