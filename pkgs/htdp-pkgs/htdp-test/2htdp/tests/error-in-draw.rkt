#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define (f x)
  (cond
    [(= x 0) (circle 10 'solid 'red)]
    [(= x 1) (circle 20 'solid 'red)]
    [else (error txt)]))

(define txt "all questions were #f")

(with-handlers ([exn? (lambda (e) (unless (string=? (exn-message e) txt) (raise e)))])
  (big-bang 0 (on-tick add1) (to-draw f))
  (error 'error-in-draw "test failed"))


(let ([exn (with-handlers ([exn:fail? values])
             (big-bang #f 
                       [to-draw (λ (a b) #f)])
             "no error raised")])
(unless (regexp-match #rx"^to-draw:" (exn-message exn))
  (eprintf "expected a error message beginning with to-draw:\n")
  (raise exn)))

(let ([exn (with-handlers ([exn:fail? values])
             (big-bang #f 
                       [on-draw (λ (a b) #f)])
             "no error raised")])
(unless (regexp-match #rx"^on-draw:" (exn-message exn))
  (eprintf "expected a error message beginning with on-draw:\n")
  (raise exn)))
