
#lang scheme/base
(require (for-syntax scheme/base)
         scheme/class
         framework/framework)
(provide closure-mixin
         pref:get/set)

(define-syntax pref:get/set
  (syntax-rules ()
    [(_ get/set prop)
     (define get/set
       (case-lambda
         [() (preferences:get 'prop)]
         [(newval) (preferences:set 'prop newval)]))]))

(define-syntax (closure-mixin stx)
  (syntax-case stx ()
    [(closure-mixin interfaces [name proc] ...)
     (with-syntax ([(iname ...) (generate-temporaries #'(name ...))])
       #'(let ([iname proc] ...)
           (mixin () interfaces
             (define/public (name . args) (apply iname args))
             ...
             (super-new))))]))
