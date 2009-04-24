#lang scheme/base

(provide (all-defined-out))

;; Structures --------------------------------------------------

;; struct check-info : symbol any
(define-struct check-info (name value))


;; Infrastructure ----------------------------------------------

;; parameter check-info-stack : (listof check-info)
(define check-info-stack
  (make-parameter
   (list)
   (lambda (v)
     (if (list? v)
         v
         (raise-type-error 'check-info-stack "list" v)))))

;; with-check-info* : (list-of check-info) thunk -> any
(define (with-check-info* info thunk)
  (parameterize
      ((check-info-stack (append (check-info-stack) info)))
    (thunk)))

(define-syntax with-check-info
  (syntax-rules ()
    ((_ ((name val) ...) body ...)
     (with-check-info*
      (list (make-check-info name val) ...)
      (lambda ()
        body ...)))))


(define (make-check-name name)
  (make-check-info 'name name))
(define (make-check-params params)
  (make-check-info 'params params))
(define (make-check-location stx)
  (make-check-info 'location stx))
(define (make-check-expression msg)
  (make-check-info 'expression msg))
(define (make-check-message msg)
  (make-check-info 'message msg))
(define (make-check-actual param)
  (make-check-info 'actual param))
(define (make-check-expected param)
  (make-check-info 'expected param))

(define (check-name? info)
  (eq? (check-info-name info) 'name))
(define (check-params? info)
  (eq? (check-info-name info) 'params))
(define (check-location? info)
  (eq? (check-info-name info) 'location))
(define (check-expression? info)
  (eq? (check-info-name info) 'expression))
(define (check-message? info)
  (eq? (check-info-name info) 'message))
(define (check-actual? info)
  (eq? (check-info-name info) 'actual))
(define (check-expected? info)
  (eq? (check-info-name info) 'expected))
