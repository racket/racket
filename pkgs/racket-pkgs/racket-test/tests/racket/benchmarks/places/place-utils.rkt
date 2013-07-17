#lang racket/base
(require racket/place
         racket/file
         (for-syntax racket/base
                     racket/file))

(provide splat
         splat-tmp
         barrier-m
         barrier
         places-wait
         place/base
         here-submod
         time-n)


(define (splat txt fn)
  (call-with-output-file fn #:exists 'replace
      (lambda (out)
        (fprintf out "~a" txt))))

(define (splat-tmp txt)
  (define fn (make-temporary-file "place-benchmark~a" #f (current-directory)))
  (splat txt fn)
  fn)

(define (barrier-m pls)
  (for ([ch pls]) (place-channel-get ch))
  (for ([ch pls]) (place-channel-put ch 1)))

(define (barrier ch)
  (place-channel-put ch 0)
  (place-channel-get ch))

(define (places-wait pls)
  (for ([p pls]) (place-wait p)))

(define-syntax (place/base stx)
  (syntax-case stx ()
    [(_ module-name (name ch) body ...)
     #'(module module-name racket/base
         (require racket/place)
         (provide name)
         (define (name ch)
           body ...))]))

(define-syntax-rule (here-submod id)
  `(submod ,(resolved-module-path-name
             (variable-reference->resolved-module-path
             (#%variable-reference)))
           id))

(define-syntax (time-n stx)
  (syntax-case stx ()
    [(_ msg cnt body ...)
      #'(let-values ([(r ct rt gct)
                      (time-apply (lambda () body ...) null)])
          (displayln (list msg cnt ct rt gct))
          (if (pair? r) (car r) r))
#|
      #'(time body ...)
|#
]))
          
