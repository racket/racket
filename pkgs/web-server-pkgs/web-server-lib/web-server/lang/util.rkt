#lang racket/base
(require (for-template racket/base)
         racket/list
         racket/contract)
(provide/contract
 [transformer? (parameter/c boolean?)]
 [disarm (syntax? . -> . syntax?)]
 [rearm (syntax? syntax? . -> . syntax?)]
 [current-code-labeling (parameter/c (syntax? . -> . syntax?))]
 [generate-formal ((symbol?) ((or/c false/c syntax?)) . ->* . (values syntax? syntax?))]
 [formals-list (syntax? . -> . (listof syntax?))]
 [make-define-case ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [make-module-case ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [make-lang-module-begin ((bytes? . -> . (-> symbol?)) (syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))]
 [bound-identifier-member? (syntax? (listof syntax?) . -> . boolean?)])

(define transformer? (make-parameter #f))

(define code-insp
  (variable-reference->module-declaration-inspector
   (#%variable-reference)))

(define (disarm expr)
  (syntax-disarm expr code-insp))
(define (rearm old-expr expr)
  (syntax-rearm expr old-expr))

(define current-code-labeling
  (make-parameter
   (lambda (stx)
     (datum->syntax stx 'error))))

(define (generate-formal sym-name [stx-base #f])
  (let ([name (datum->syntax (and stx-base (disarm stx-base)) (gensym sym-name))])
    (with-syntax ([(#%plain-lambda (formal) ref-to-formal)
                   (if (syntax-transforming?)
                       (local-expand #`(#%plain-lambda (#,name) #,name) 'expression empty)
                       #`(#%plain-lambda (#,name) #,name))])
      (values #'formal #'ref-to-formal))))

(define (formals-list stx)
  (syntax-case stx ()
    [v (identifier? #'v)
       (list #'v)]
    [(v ...)
     (syntax->list #'(v ...))]
    [(v ... . rv)
     (list* #'rv (syntax->list #'(v ...)))]))

(define ((make-define-case inner) stx)
  (syntax-case stx (define-values define-syntaxes #%require)
    [(define-syntaxes . ds) stx]      
    [(#%require . r) stx]
    [(define-values (v ...) ve)
     (let-values ([(nve) (inner #'ve)])
       (quasisyntax/loc stx
         (define-values (v ...) #,nve)))]
    [expr
     (inner #'expr)]))

(define ((make-module-case inner) stx)
  (syntax-case* stx (#%provide #%declare begin-for-syntax module module*) free-identifier=?     
    [(#%provide . p) stx]
    [(#%declare . d) stx]
    [(module* . m) stx]
    [(module . m) stx]
    [(begin-for-syntax . e) stx]
    [_
     (inner stx)]))

(define ((make-lang-module-begin make-labeling transform) stx)
  (rearm
   stx
   (syntax-case (disarm stx) ()
     [(mb forms ...)
      (let ([stx (local-expand (quasisyntax/loc stx
                                 (#%module-begin (require web-server/lang/lang-api) forms ...))
                               'module-begin 
                               (list #'module*))])
        (with-syntax ([(pmb body ...) (disarm stx)])
          (define base-labeling (make-labeling (string->bytes/utf-8 (format "~a" (syntax->datum stx)))))      
          (define new-defs 
            (parameterize ([current-code-labeling
                            (lambda (stx) (datum->syntax stx (base-labeling)))])
              (map transform (syntax->list #'(body ...)))))
          (quasisyntax/loc stx
            (pmb #,@new-defs))))])))

(define (bound-identifier-member? id ids)
  (ormap
   (lambda (an-id)
     (bound-identifier=? id an-id))
   ids))
