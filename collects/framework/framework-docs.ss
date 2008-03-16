#lang scheme/base

(provide def-fw-procs)

(require (for-label scheme/contract)
         (for-label framework/framework))

(require "private/framework-exports.ss"
         (for-syntax scheme/base)
         scribble/decode
         scribble/manual)

(define-syntax (fw-doc-form stx)
  (syntax-case stx (-> ->*)
    [(_ id (-> a ... b) (arg ...) (docs ...))
     #'(defproc (id (arg a) ...)
         b
         docs ...)]
    [(_ id (->* (mandatory-ctc ...) (optional-ctc ...) range) 
        ((mandatory-arg ...) ((optional-arg default) ...))
        (docs ...))
     #'(defproc (id (mandatory-arg mandatory-ctc) ...
                    (optional-arg optional-ctc default) ...)
         range
         docs ...)]
    
    [(_ id ctc () (docs ...))
     #'(defthing id ctc docs ...)]
    
    [(_ id whatever ...)
     (begin
       (fprintf (current-error-port) "Cannot parse docs for ~a\n" (syntax->datum #'id))
       #'(defthing id any/c))]))

(define-syntax (mapdesc stx)
  (syntax-case stx ()
    [(_ cmd events)
     #'(make-splice (list (index (symbol->string 'cmd))
                          (symbol->string 'cmd)
                          " ("
                          (symbol->string 'events)
                          " events)"))]))

(define-syntax (export/docs stx)
  (syntax-case stx ()
    [(_ (id ctc argspec docs ...) ...)
     #'(begin (fw-doc-form id ctc argspec docs ...) ...)]))

(define-syntax (conv/export/docs stx)
  (define-struct faux-stx (obj vec) #:prefab)
  (syntax-case stx ()
    [(id arg)
     #`(export/docs
        #,@(let loop ([f-stx (syntax->datum #'arg)])
             (cond
               [(faux-stx? f-stx) 
                (datum->syntax #'id
                               (loop (faux-stx-obj f-stx))
                               (faux-stx-vec f-stx))]
               [(pair? f-stx) (cons (loop (car f-stx)) (loop (cdr f-stx)))]
               [else f-stx])))]))

(define-syntax (def-fw-procs stx)
  #'(framework-exports/srcloc-preserved conv/export/docs))
