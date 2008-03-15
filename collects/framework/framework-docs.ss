#lang scheme/base

(provide def-fw-procs)

(require (for-label scheme/contract)
         (for-label framework/framework))

(require "private/framework-exports.ss"
         (for-syntax scheme/base)
         scribble/manual)

(define-syntax (fw-doc-form stx)
  (syntax-case stx (->)
    [(_ id (-> a ... b) (arg ...) docs ...)
     #'(defproc (id (arg a) ...)
         b)]

    [(_ id b () docs ...)
     #'(defthing id b)]

    [(_ id whatever ...)
     #'(defthing id any/c)]))

(define-syntax (export/docs stx)
  (syntax-case stx ()
    [(_ (id ctc argspec docs ...) ...)
     #'(begin (fw-doc-form id ctc argspec docs ...) ...)]))

(define-syntax (conv/export/docs stx)
  (define-struct faux-stx (obj vec) #:prefab)
  (syntax-case stx ()
    [(_ arg)
     #`(export/docs
        #,@(let loop ([f-stx (syntax->datum #'arg)])
             (cond
               [(faux-stx? f-stx) 
                (datum->syntax stx
                               (loop (faux-stx-obj f-stx))
                               (faux-stx-vec f-stx))]
               [(pair? f-stx) (cons (loop (car f-stx)) (loop (cdr f-stx)))]
               [else f-stx])))]))

(define-syntax (def-fw-procs stx)
  #'(framework-exports/srcloc-preserved conv/export/docs))
