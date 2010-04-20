
;; A sane "core" for finishing up the "scheme/base" library

(module pre-base '#%kernel
  (#%require (for-syntax '#%kernel))
  (#%require "more-scheme.rkt"
             "misc.rkt"
             (all-except "define.rkt" define)
             "letstx-scheme.rkt"
             "kw.rkt"
             "define-struct.rkt"
             "reqprov.rkt"
             "modbeg.rkt"
             "for.rkt"
             "map.rkt" ; shadows #%kernel bindings
             "kernstruct.rkt"
             "norm-arity.rkt"
             '#%builtin) ; so it's attached

  (define-syntaxes (#%top-interaction)
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (datum->syntax stx (cdr (syntax-e stx)) stx stx)))

  (define-values (new-apply)
    (make-keyword-procedure
     (lambda (kws kw-args proc args . rest)
       (keyword-apply proc kws kw-args (apply list* args rest)))
     apply))

  (define-values (new-keyword-apply)
    (make-keyword-procedure
     (lambda (kws kw-args proc orig-kws orig-kw-args args . rest)
       (let-values ([(kws kw-args)
                     (let loop ([kws kws] [kw-args kw-args]
                                [kws2 orig-kws] [kw-args2 orig-kw-args]
                                [swapped? #f])
                       (cond
                        [(null? kws) (values kws2 kw-args2)]
                        [(null? kws2) (values kws kw-args)]
                        [(keyword<? (car kws) (car kws2))
                         (let-values ([(res-kws res-kw-args)
                                       (loop (cdr kws) (cdr kw-args) kws2 kw-args2 #f)])
                           (values (cons (car kws) res-kws)
                                   (cons (car kw-args) res-kw-args)))]
                        [swapped?
                         (raise-mismatch-error
                          'keyword-apply
                          "keyword duplicated in list and direct keyword arguments: "
                          (car kws))]
                        [else (loop kws2 kw-args2 kws kw-args #t)]))])
         (keyword-apply proc kws kw-args (apply list* args rest))))
     keyword-apply))

  (#%provide (all-from-except "more-scheme.rkt" old-case fluid-let)
             (all-from "misc.rkt")
             (all-from "define.rkt")
             (all-from-except "letstx-scheme.rkt" -define -define-syntax -define-struct old-cond)
             (rename new-lambda lambda)
             (rename new-λ λ)
             (rename new-define define)
             (rename new-app #%app)
             (rename new-apply apply)
             (rename new-prop:procedure prop:procedure)
             (rename #%app #%plain-app)
             (rename lambda #%plain-lambda)
             (rename #%module-begin #%plain-module-begin)
             (rename module-begin #%module-begin)
             (rename norm:procedure-arity procedure-arity)
             (rename norm:raise-arity-error raise-arity-error)
             (rename new:procedure-reduce-arity procedure-reduce-arity)
             (rename new:procedure->method procedure->method)
             (rename new:procedure-rename procedure-rename)
             (rename new:chaperone-procedure chaperone-procedure)
             (all-from-except '#%kernel lambda λ #%app #%module-begin apply prop:procedure 
                              procedure-arity procedure-reduce-arity raise-arity-error
                              procedure->method procedure-rename
                              chaperone-procedure)
             (all-from "reqprov.rkt")
             (all-from "for.rkt")
             (all-from "kernstruct.rkt")
             #%top-interaction

             map for-each andmap ormap
             make-keyword-procedure
             (rename new-keyword-apply keyword-apply)
             procedure-keywords
             procedure-reduce-keyword-arity
             (rename define-struct* define-struct)
             define-struct/derived
             struct-field-index
             struct-copy))
