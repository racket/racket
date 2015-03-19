#lang racket/base

(require "private/performance-hint.rkt")

(provide begin-encourage-inline
         define-inline)

(module begin-encourage-inline '#%kernel
  (#%require "private/performance-hint.rkt")
  (#%provide begin-encourage-inline))

(require (for-syntax syntax/parse syntax/define
                     racket/syntax racket/base)
         racket/stxparam)

(begin-for-syntax

 (define-splicing-syntax-class actual
   (pattern (~seq (~optional kw:keyword) arg:expr)
            #:with tmp (generate-temporary #'arg)
            #:attr for-aux (list (attribute kw) (attribute tmp))))

 (define-syntax-class formals
   (pattern () ; done
            #:with aux-args #'()
            #:with ids      #'()
            #:with rest-arg #'()) ; rest-arg is () for no rest arg or (id)
   (pattern rest:id
            #:with aux-args #'rest
            #:with ids      #'()
            #:with rest-arg #'(rest))
   (pattern (x:id . rest:formals)
            #:with aux-args #'(x . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern ([x:id default:expr] . rest:formals)
            ;; for aux-args, wrap defaults in syntax
            #:with aux-args #'([x #'default] . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern (kw:keyword x:id . rest:formals)
            #:with aux-args #'(kw x . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern (kw:keyword [x:id default:expr] . rest:formals)
            #:with aux-args #'(kw [x #'default] . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)))

(define-syntax (define-inline stx)
  (syntax-parse stx
    [(_ (name . formals) . body)
     (define-values (name lam) (normalize-definition stx #'lambda #t #t))
     (syntax-parse lam
       [(_ args . body)
        #`(define-inline-helper (#,name . args) . body)])]
    [_
     (raise-syntax-error
      'define-inline "only supports definitions with function headers" stx)]))

(define-syntax (define-inline-helper stx)
  (syntax-parse stx
    [(_ (name:id . args:formals) . body)
     (with-syntax* ([internal-name     (format-id #'name "~a-internal" #'name)]
                    [inline-name       (format-id #'name "~a-inline"   #'name)]
                    [function-aux      (format-id #'name "~a-aux"      #'name)]
                    [(arg-id ...)      #'args.ids]
                    [(rest-arg-id ...) #'args.rest-arg]
                    [(tmp-arg-id ...)  (generate-temporaries #'(arg-id ...))]
                    [(tmp-rest-arg-id ...)
                     (generate-temporaries #'(rest-arg-id ...))]
                    [body*
                     #'(let-syntax ([name (make-rename-transformer
                                           (quote-syntax internal-name))])
                         . body)])
       #`(begin
           ;; define a function version that recursive calls fall back to, to
           ;; avoid infinite expansion
           (define (internal-name . args) body*)
           (define-syntax-parameter name
             (syntax-id-rules ()
               [(_ . rest) (inline-name . rest)]
               [_ internal-name])) ; higher-order use
           (define-syntax (inline-name stx*)
             ;; generate a compile-time function that takes care of linking
             ;; formals and actuals (so we don't have to handle keyword
             ;; arguments manually)
             (define (function-aux . args.aux-args)
               ;; default values for optional arguments can refer to previous
               ;; arguments, which makes things tricky
               (with-syntax* ([tmp-arg-id      arg-id] ...
                              [tmp-rest-arg-id rest-arg-id] ...)
                 #'(let* ([arg-id tmp-arg-id] ...
                          [rest-arg-id
                           (list . tmp-rest-arg-id)] ...)
                     body*)))
             (...
              (syntax-parse stx*
                [(_ arg*:actual ...)
                 ;; let*-bind the actuals, to ensure that they're evaluated
                 ;; only once, and in order
                 #`(syntax-parameterize
                    ([name (make-rename-transformer #'internal-name)])
                    (let* ([arg*.tmp arg*.arg] ...)
                      #,(let* ([arg-entries     (attribute arg*.for-aux)]
                               [keyword-entries (filter car arg-entries)]
                               [positional-entries
                                (filter (lambda (x) (not (car x)))
                                        arg-entries)]
                               [sorted-keyword-entries
                                (sort keyword-entries
                                      string<?
                                      #:key (lambda (kw)
                                              (keyword->string
                                               (syntax-e kw))))])
                          (keyword-apply
                           function-aux
                           (map (lambda (x) (syntax-e (car x)))
                                sorted-keyword-entries)
                           (map cadr sorted-keyword-entries)
                           (map cadr positional-entries)))))])))))]))
