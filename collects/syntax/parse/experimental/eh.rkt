#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     unstable/syntax
                     "../private/minimatch.rkt"
                     "../private/rep.rkt"
                     "../private/rep-data.rkt"
                     "../private/rep-patterns.rkt")
         "../private/keywords.rkt"
         "../private/sc.rkt")

(provide ~eh-var
         define-eh-alternative-set)

(define-syntax (define-eh-alternative-set stx)
  (define-syntax-class alt
    #:description "eh-alternate-set alternative"
    #:literals (pattern)
    (pattern (pattern alt)))
  (syntax-parse stx
    #:literals (pattern)
    [(_ name:id a:alt ...)
     (parameterize ((current-syntax-context stx))
       (let* ([decls (new-declenv null #:conventions null)]
              [ehpat+hstx-list
               (apply append
                      (for/list ([alt (in-list (syntax->list #'(a.alt ...)))])
                        (parse*-ellipsis-head-pattern alt decls #t #:context stx)))]
              [eh-alt+defs-list
               (for/list ([ehpat+hstx (in-list ehpat+hstx-list)])
                 (let ([ehpat (car ehpat+hstx)]
                       [hstx (cadr ehpat+hstx)])
                   (cond [(syntax? hstx)
                          (with-syntax ([(parser) (generate-temporaries '(eh-alt-parser))])
                            (let ([attrs (iattrs->sattrs (pattern-attrs (ehpat-head ehpat)))])
                              (list (eh-alternative (ehpat-repc ehpat) attrs #'parser)
                                    (list #`(define parser
                                              (parser/rhs parser () #,attrs
                                                          [#:description #f (pattern #,hstx)]
                                                          #t
                                                          #,stx))))))]
                         [(eh-alternative? hstx)
                          (list hstx null)]
                         [else
                          (error 'define-eh-alternative-set "internal error: unexpected ~e"
                                 hstx)])))]
              [eh-alts (map car eh-alt+defs-list)]
              [defs (apply append (map cadr eh-alt+defs-list))])
         (with-syntax ([(def ...) defs]
                       [(alt-expr ...)
                        (for/list ([alt (in-list eh-alts)])
                          (with-syntax ([repc-expr
                                         (match (eh-alternative-repc alt)
                                           ['#f
                                            #'(quote #f)]
                                           [(rep:once n u o)
                                            #`(rep:once (quote-syntax #,n)
                                                        (quote-syntax #,u)
                                                        (quote-syntax #,o))]
                                           [(rep:optional n o d)
                                            #`(rep:optional (quote-syntax #,n)
                                                            (quote-syntax #,o)
                                                            (quote-syntax #,d))]
                                           [(rep:bounds min max n u o)
                                            #`(rep:bounds (quote #,min)
                                                          (quote #,max)
                                                          (quote-syntax #,n)
                                                          (quote-syntax #,u)
                                                          (quote-syntax #,o))])]
                                        [attrs-expr
                                         #`(quote #,(eh-alternative-attrs alt))]
                                        [parser-expr
                                         #`(quote-syntax #,(eh-alternative-parser alt))])
                            #'(eh-alternative repc-expr attrs-expr parser-expr)))])
           #'(begin def ...
                    (define-syntax name
                      (eh-alternative-set (list alt-expr ...)))))))]))
