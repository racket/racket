#lang racket/base

(require (for-syntax racket/base))

(provide define-struct/reg-mk
         id->constructor
         (struct-out point)
         (struct-out bb))

(define id-constructor-pairs '())
(define (add-id-constructor-pair a b)
  (set! id-constructor-pairs (cons (list a b) id-constructor-pairs)))

(define-syntax (define-struct/reg-mk stx)
  (syntax-case stx ()
    [(_ id #:reflect-id reflect-id rest ...)
     (let ([build-name
            (λ (fmt id)
              (datum->syntax id (string->symbol (format fmt (syntax->datum id)))))])
       #`(begin
           (define-struct id rest ... #:reflection-name 'reflect-id)
           (add-id-constructor-pair '#,(build-name "struct:~a" #'reflect-id)
                                    #,(build-name "make-~a" #'id))))]
    [(_ id . rest) #'(define-struct/reg-mk id #:reflect-id id . rest)]))

(define (id->constructor id)
  (let ([line (assoc id id-constructor-pairs)])
    (and line
         (list-ref line 1))))

(define-struct/reg-mk point (x y) #:transparent)

;; a bb is  (bounding box)
;;  (make-bb number number number)
(define-struct/reg-mk bb (right bottom baseline) #:transparent)
