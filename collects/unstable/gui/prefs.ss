
#lang scheme/base
(require (for-syntax scheme/base)
         framework/framework)
(provide pref:get/set)

(define-syntax pref:get/set
  (syntax-rules ()
    [(_ get/set prop)
     (define get/set
       (case-lambda
         [() (preferences:get 'prop)]
         [(newval) (preferences:set 'prop newval)]))]))
