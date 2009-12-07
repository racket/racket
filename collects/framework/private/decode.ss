#lang scheme/base
(require (for-syntax file/gunzip scheme/base))
(provide decode)

(define-syntax (decode stx)
  (define (decode stxs)
    (define str
      (apply string-append (map (λ (x) (symbol->string (syntax-e x))) stxs)))
    (define loc
      (if (even? (string-length str))
        (for/list ([i (in-range 0 (string-length str) 2)])
          (string->number (substring str i (+ i 2)) 16))
        (error 'decode "missing digit somewhere")))
    (define-values (p-in p-out) (make-pipe))
    (inflate (open-input-bytes (apply bytes loc)) p-out)
    (read p-in))
  (syntax-case stx ()
    [(_ x ...)
     (andmap identifier? (syntax->list #'(x ...)))
     (datum->syntax stx (decode (syntax->list #'(x ...))) stx)]))
