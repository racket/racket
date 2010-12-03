#lang racket/base
(require web-server/dispatchers/dispatch
         racket/list
         racket/contract
         racket/match
         "syntax.rkt")

(struct container (bunches) #:mutable)
(struct bunch (dispatch url))

(define (container-dispatch c)
  (λ (req)
    (let/ec esc
      (for ([d*u (in-list (container-bunches c))])
        (with-handlers ([exn:dispatcher? void])
          (esc ((bunch-dispatch d*u) req))))
      (next-dispatcher))))

(define (container-url c)
  (λ args
    (let/ec esc
      (for ([d*u (in-list (container-bunches c))])
        (with-handlers ([exn:misc:match? void])
          (esc (apply (bunch-url d*u) args))))
      (match args))))

(define-syntax-rule (define-container container-id (container-dispatch-id container-url-id))
  (begin
    (define container-id
      (container empty))
    (define container-dispatch-id
      (container-dispatch container-id))
    (define container-url-id
      (container-url container-id))))

(define (container-cons! c d u)
  (set-container-bunches! 
   c
   (cons (bunch d u) (container-bunches c))))

#;(define (snoc l x) (append l (list x)))
#;(define (container-snoc! c d u)
    (set-container-bunches! 
     c
     (snoc (container-bunches c) (bunch d u))))

(define-syntax-rule (dispatch-rules! container-expr [pat fun] ...)
  (let-values ([(dispatch url) (dispatch-rules [pat fun] ...)])
    (container-cons! container-expr
                     dispatch url)))

(provide
 define-container
 dispatch-rules!)
(provide/contract
 [container? (any/c . -> . boolean?)])
