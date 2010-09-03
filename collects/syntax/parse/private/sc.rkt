#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     unstable/syntax
                     "rep-data.rkt"
                     "rep.rkt")
         "parse.rkt"
         "keywords.rkt"
         "runtime.rkt")

(provide define-syntax-class
         define-splicing-syntax-class

         syntax-parse
         syntax-parser

         (except-out (all-from-out "keywords.rkt")
                     ~do
                     ~reflect
                     ~splicing-reflect
                     ~eh-var)

         attribute
         this-syntax

         ;;----
         parser/rhs)

(begin-for-syntax
 (define (defstxclass stx name formals rhss splicing?)
   (parameterize ((current-syntax-context stx))
     (with-syntax ([name name]
                   [formals formals]
                   [rhss rhss])
       (let* ([the-rhs (parse-rhs #'rhss #f splicing? #:context stx)]
              [arity (parse-kw-formals #'formals #:context stx)]
              [opt-rhs+def
               (and (stx-list? #'formals) (andmap identifier? (syntax->list #'formals))
                    (optimize-rhs the-rhs (syntax->list #'formals)))]
              [the-rhs (if opt-rhs+def (car opt-rhs+def) the-rhs)])
         (with-syntax ([parser (generate-temporary
                                (format-symbol "parse-~a" (syntax-e #'name)))]
                       [arity arity]
                       [attrs (rhs-attrs the-rhs)]
                       [(opt-def ...)
                        (if opt-rhs+def
                            (list (cadr opt-rhs+def))
                            '())]
                       [options (rhs-options the-rhs)]
                       [integrate-expr
                        (syntax-case (rhs-integrate the-rhs) ()
                          [#s(integrate predicate description)
                             #'(integrate (quote-syntax predicate)
                                          'description)]
                          [#f
                           #''#f])])
           #`(begin (define-syntax name
                      (stxclass 'name 'arity
                                'attrs
                                (quote-syntax parser)
                                '#,splicing?
                                options
                                integrate-expr))
                    opt-def ...
                    (define-values (parser)
                      ;; If opt-rhs, do not reparse:
                      ;; need to keep same generated predicate name
                      #,(if opt-rhs+def
                            (begin
                              ;; (printf "Integrable syntax class: ~s\n" (syntax->datum #'name))
                              #`(parser/rhs/parsed
                                 name formals attrs #,the-rhs
                                 #,(and (rhs-description the-rhs) #t)
                                 #,splicing? #,stx))
                            #`(parser/rhs
                               name formals attrs rhss #,splicing? #,stx))))))))))

(define-syntax (define-syntax-class stx)
  (syntax-case stx ()
    [(define-syntax-class name . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'() #'rhss #f)]
    [(define-syntax-class (name . formals) . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'formals #'rhss #f)]))

(define-syntax (define-splicing-syntax-class stx)
  (syntax-case stx ()
    [(define-splicing-syntax-class name . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'() #'rhss #t)]
    [(define-splicing-syntax-class (name . formals) . rhss)
     (identifier? #'name)
     (defstxclass stx #'name #'formals #'rhss #t)]))

;; ----

(define-syntax (parser/rhs stx)
  (syntax-case stx ()
    [(parser/rhs name formals attrs rhss splicing? ctx)
     (with-disappeared-uses
      (let ([rhs
             (parameterize ((current-syntax-context #'ctx))
               (parse-rhs #'rhss (syntax->datum #'attrs) (syntax-e #'splicing?)
                          #:context #'ctx))])
        #`(parser/rhs/parsed name formals attrs
                             #,rhs #,(and (rhs-description rhs) #t)
                             splicing? ctx)))]))

(define-syntax (parser/rhs/parsed stx)
  (syntax-case stx ()
    [(prp name formals attrs rhs rhs-has-description? splicing? ctx)
     #`(let ([get-description
              (lambda formals
                (if 'rhs-has-description?
                    #,(rhs-description (syntax-e #'rhs))
                    (symbol->string 'name)))])
         (parse:rhs rhs attrs formals splicing?
                    (if 'rhs-has-description?
                        #,(rhs-description (syntax-e #'rhs))
                        (symbol->string 'name))))]))

;; ====

(define-syntax (syntax-parse stx)
  (syntax-case stx ()
    [(syntax-parse stx-expr . clauses)
     (quasisyntax/loc stx
       (let ([x (datum->syntax #f stx-expr)])
         (parse:clauses x clauses #,((make-syntax-introducer) stx))))]))

(define-syntax (syntax-parser stx)
  (syntax-case stx ()
    [(syntax-parser . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (parse:clauses x clauses #,((make-syntax-introducer) stx)))))]))
