#reader scribble/reader
#lang scheme/base

(provide spec->host-method spec->surrogate-method)
(require (for-template scribble/manual scheme/base))
(require scheme/class)
(require (for-label framework))

(define (spec->host-method spec)
  (syntax-case* spec (override augment) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
    [(override mtd (x ...) ...)
     #'@defmethod*[(((mtd (x any/c) ...) any) ...)]{
        Delegates to the result of @method[mode:host-text<%> get-surrogate] if it is not @racket[#f].}]
    [(augment default mtd (x ...) ...)
     #'@defmethod*[(((mtd (x any/c) ...) any) ...)]{
        Delegates to the result of @method[mode:host-text<%> get-surrogate] if it is not @racket[#f].}]))

(define (spec->surrogate-method spec)
  (syntax-case* spec (override augment) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
    [(override method (x ...) ...)
     #'@defmethod*[(((method (orig (is-a?/c text%)) (call-super (-> any)) (x any/c) ...) any) ...)]{
          Returns the result of invoking @racket[call-super].
    }]
    [(augment default method (x ...) ...)
     #'@defmethod*[(((method (orig (is-a?/c text%)) (call-inner (-> any)) (x any/c) ...) any) ...)]{
          Returns the result of invoking @racket[call-super].
    }]))
