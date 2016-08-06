#lang racket/load

(require racket/base racket/stream racket/syntax)

(define (make-mod name required-name)
  #`(module #,name typed/racket/base
      (require '#,required-name)))



(define (make-program N)
  (define the-names
    (let ()
      (define (names) (stream-cons (generate-temporary) (names)))
      (names)))
  #`(begin
      (module #,(stream-first the-names) typed/racket/base)
      #,@(for/list ((_ N)
                    (name (stream-rest the-names))
                    (required-name the-names))
          #`(begin
              #,(make-mod name required-name)
              (require '#,name)))))

(time (eval (make-program 40)))
