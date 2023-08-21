#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     syntax/keyword
                     racket/syntax
                     "datum-to-expr.rkt"
                     "rep-attrs.rkt"
                     "rep-data.rkt"
                     "rep-patterns.rkt"
                     "rep.rkt"
                     "kws.rkt"
                     "const-expr.rkt"
                     "txlift.rkt")
         "keywords.rkt"
         "residual.rkt"
         "runtime.rkt")

;; ----------------------------------------
;; Backend selection

;; Interpreter backend
#; (require "parse-interp.rkt")

;; Classic backend (also disable require of "residual-interp.rkt" in "sc.rkt")
#; (require "parse-classic.rkt")

;; Hybrid backend
(begin
  (require (prefix-in c: "parse-classic.rkt")
           (prefix-in i: "parse-interp.rkt"))
  (begin-for-syntax
    (provide interp-clauses? interp-rhs?)
    (define interp-clauses? (make-parameter #t))  ;; for syntax-parse, etc
    (define interp-rhs? (make-parameter #f))      ;; for define-syntax-class, etc
    (define (codegen-clauses who context x all-defs patterns body-exprs ctx track-literals?)
      (define (call f) (f who context x all-defs patterns body-exprs ctx track-literals?))
      (cond [(interp-clauses?) (call i:codegen-clauses)]
            [else (call c:codegen-clauses)]))
    (define (codegen-rhs name formals relsattrs rhs splicing? ctx)
      (define (call f) (f name formals relsattrs rhs splicing? ctx))
      (cond [(interp-rhs?) (call i:codegen-rhs)]
            [else (call c:codegen-rhs)]))))

;; ============================================================

(provide define-syntax-class
         define-splicing-syntax-class
         define-integrable-syntax-class
         syntax-parse
         syntax-parser
         define/syntax-parse
         syntax-parser/template
         define-eh-alternative-set)

(define-syntax define-syntax-class
  (lambda (stx) (tx:define-*-syntax-class stx #f)))
(define-syntax define-splicing-syntax-class
  (lambda (stx) (tx:define-*-syntax-class stx #t)))

(begin-for-syntax
 (define (tx:define-*-syntax-class stx splicing?)
   (syntax-case stx ()
     [(_ header . rhss)
      (parameterize ((current-syntax-context stx))
        (let-values ([(name formals arity)
                      (let ([p (check-stxclass-header #'header stx)])
                        (values (car p) (cadr p) (caddr p)))])
          (let ([the-rhs (parse-rhs #'rhss splicing? #:context stx
                                    #:default-description (symbol->string (syntax-e name)))])
            (with-syntax ([name name]
                          [formals formals]
                          [splicing? splicing?]
                          [desc (cond [(rhs-description the-rhs) => string-expr-value] [else #f])]
                          [parser (generate-temporary (format-symbol "parse-~a" name))]
                          [arity arity]
                          [attrs (rhs-attrs the-rhs)]
                          [commit? (rhs-commit? the-rhs)]
                          [delimit-cut? (rhs-delimit-cut? the-rhs)]
                          [the-rhs-expr (datum->expression the-rhs)])
              #`(begin (define-syntax name
                         (stxclass (quote name) (quote arity) (quote attrs)
                                   (quote-syntax parser)
                                   (quote splicing?)
                                   (scopts (length 'attrs) 'commit? 'delimit-cut? desc)
                                   #f))
                       (define-values (parser)
                         (parser/rhs name formals attrs the-rhs-expr splicing? #,stx)))))))])))

(define-syntax (parser/rhs stx)
  (syntax-case stx ()
    [(parser/rhs name formals relsattrs the-rhs-expr splicing? ctx)
     (with-disappeared-uses
       (let ([splicing? (syntax-e #'splicing?)]
             [relsattrs (syntax->datum #'relsattrs)])
         (define the-rhs
           (parameterize ((current-syntax-context #'ctx))
             (fixup-rhs (syntax-local-eval
                         (syntax-local-introduce #'the-rhs-expr))
                        splicing? relsattrs)))
         (codegen-rhs #'name #'formals relsattrs the-rhs splicing? #'ctx)))]))

(define-syntax (define-integrable-syntax-class stx)
  (syntax-case stx (quote)
    [(_ name (quote description) predicate)
     (with-syntax ([parser (generate-temporary (format-symbol "parse-~a" (syntax-e #'name)))]
                   [no-arity no-arity]
                   [rhs-expr (datum->expression
                              (make-predicate-rhs #'name #'description #'predicate))])
       #'(begin (define-syntax name
                  (stxclass 'name no-arity '()
                            (quote-syntax parser)
                            #f
                            (scopts 0 #t #t 'description)
                            (quote-syntax predicate)))
                (define-values (parser)
                  (parser/rhs name () () rhs-expr #f #f))
                #;
                (define (parser x cx pr es undos fh0 cp0 rl success)
                  (if (predicate x)
                      (success fh0 undos)
                      (let ([es (es-add-thing pr 'description #t rl es)])
                        (fh0 undos (failure* pr es)))))))]))

(begin-for-syntax
  ;; make-predicate-rhs : Id Id Id -> RHS
  (define (make-predicate-rhs name description predicate)
    (define p (pat:and (list (pat:svar #'s)
                             (pat:action (action:fail #`(not (#,predicate #'s)) #''#f)
                                         (pat:any)))))
    (define variants (list (variant #f null p null)))
    (rhs null #t #''description variants null #t #t)))

(define-syntax (syntax-parse stx)
  (syntax-case stx ()
    [(syntax-parse stx-expr . clauses)
     (quasisyntax/loc stx
       (let ([x (datum->syntax #f stx-expr)])
         (with ([this-syntax x])
           (parse:clauses x clauses body-sequence #,((make-syntax-introducer) stx)))))]))

(define-syntax (syntax-parser stx)
  (syntax-case stx ()
    [(syntax-parser . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (with ([this-syntax x])
             (parse:clauses x clauses body-sequence #,((make-syntax-introducer) stx))))))]))

(define-syntax (syntax-parser/template stx)
  (syntax-case stx ()
    [(syntax-parser/template ctx . clauses)
     (quasisyntax/loc stx
       (lambda (x)
         (let ([x (datum->syntax #f x)])
           (with ([this-syntax x])
             (parse:clauses x clauses one-template ctx)))))]))

;; (parse:clauses x clauses ctx)
(define-syntax (parse:clauses stx)
  (syntax-case stx ()
    [(parse:clauses x clauses body-mode ctx)
     ;; if templates? is true, expect one form after kwargs in clause, wrap it with syntax
     ;; otherwise, expect non-empty body sequence (defs and exprs)
     (with-disappeared-uses
       (with-txlifts
         (lambda ()
           (define who
             (syntax-case #'ctx ()
               [(m . _) (identifier? #'m) #'m]
               [_ 'syntax-parse]))
           (define-values (chunks clauses-stx)
             (parse-keyword-options #'clauses parse-directive-table
                                    #:context #'ctx
                                    #:no-duplicates? #t))
           (define context
             (options-select-value chunks '#:context #:default #'x))
           (define colon-notation?
             (not (assq '#:disable-colon-notation chunks)))
           (define track-literals?
             (or (assq '#:track-literals chunks)
                 (eq? (syntax-e #'body-mode) 'one-template)))
           (define-values (decls0 defs)
             (get-decls+defs chunks #:context #'ctx))
           ;; for-clause : stx -> (values SinglePattern Expr (Listof Defn))
           (define (for-clause clause)
             (syntax-case clause ()
               [[p . rest]
                (let-values ([(rest pattern defs2)
                              (parameterize ((stxclass-colon-notation? colon-notation?))
                                (parse-pattern+sides #'p #'rest
                                                     #:splicing? #f
                                                     #:decls decls0
                                                     #:context #'ctx))])
                  (define body-expr
                    (case (syntax-e #'body-mode)
                      ((one-template)
                       (syntax-case rest ()
                         [(template)
                          #'(syntax template)]
                         [_ (raise-syntax-error #f "expected exactly one template" #'ctx)]))
                      ((body-sequence)
                       (syntax-case rest ()
                         [(e0 e ...)
                          #'(let () e0 e ...)]
                         [_ (raise-syntax-error #f "expected non-empty clause body"
                                                #'ctx clause)]))
                      (else
                       (raise-syntax-error #f "internal error: unknown body mode"
                                           #'ctx #'body-mode))))
                  (values pattern body-expr defs2))]
               [_ (raise-syntax-error #f "expected clause" #'ctx clause)]))
           (unless (stx-list? clauses-stx)
             (raise-syntax-error #f "expected sequence of clauses" #'ctx))
           (define-values (patterns body-exprs defs2s)
             (for/lists (patterns body-exprs defs2s) ([clause (in-list (stx->list clauses-stx))])
               (for-clause clause)))
           (define all-defs (apply append (get-txlifts-as-definitions) defs defs2s))
           (codegen-clauses who context #'x all-defs patterns body-exprs #'ctx track-literals?))))]))

(define-syntax (define/syntax-parse stx)
  (syntax-case stx ()
    [(define/syntax-parse pattern . rest)
     (with-disappeared-uses
       (let-values ([(rest pattern defs)
                     (parse-pattern+sides #'pattern
                                          #'rest
                                          #:splicing? #f
                                          #:decls (new-declenv null)
                                          #:context stx)])
         (define no-fail? (patterns-cannot-fail? (list pattern)))
         (define expr
           (syntax-case rest ()
             [(expr) #'expr]
             [_ (raise-syntax-error #f "bad syntax" stx)]))
         (define attrs (pattern-attrs pattern))
         (with-syntax ([expr expr]
                       [(a ...) attrs]
                       [(#s(attr name _ _) ...) attrs])
           #`(defattrs/unpack (a ...)
               (let ([x (datum->syntax #f expr)])
                 #,(codegen-clauses 'define/syntax-parse #'x #'x defs
                                    (list pattern) (list #'(list (attribute name) ...)) stx #f))))
         #;
         (with-syntax ([(a ...) attrs]
                       [(#s(attr name _ _) ...) attrs]
                       [pattern pattern]
                       [es0 (if no-fail? #'#f #'#t)]
                       [(def ...) defs]
                       [expr expr])
           #'(defattrs/unpack (a ...)
               (let* ([x (datum->syntax #f expr)]
                      [cx x]
                      [pr (ps-empty x x)]
                      [es es0]
                      [fh0 (syntax-patterns-fail
                            (normalize-context 'define/syntax-parse
                                               '|define/syntax-parse pattern|
                                               x))])
                 (parameterize ((current-syntax-context x))
                   def ...
                   (#%expression
                    (with ([fail-handler fh0]
                           [cut-prompt fh0]
                           [undo-stack null])
                      (parse:S x cx pattern pr es
                               (list (attribute name) ...))))))))))]))

(define-syntax (define-eh-alternative-set stx)
  (define (parse-alt x)
    (syntax-case x (pattern)
      [(pattern alt)
       #'alt]
      [else
       (wrong-syntax x "expected eh-alternative-set alternative")]))
  (parameterize ((current-syntax-context stx))
    (syntax-case stx ()
      [(_ name a ...)
       (unless (identifier? #'name)
         (wrong-syntax #'name "expected identifier"))
       (let* ([alts (map parse-alt (syntax->list #'(a ...)))]
              [decls (new-declenv null #:conventions null)]
              [ehpat+hstx-list
               (apply append
                      (for/list ([alt (in-list alts)])
                        (parse-EH-variant alt decls #t #:context stx)))]
              [eh-alt+defs-list
               (for/list ([ehpat+hstx (in-list ehpat+hstx-list)])
                 (let ([ehpat (car ehpat+hstx)]
                       [hstx (cadr ehpat+hstx)])
                   (cond [(syntax? hstx)
                          (define the-pattern (ehpat-head ehpat))
                          (define attrs (iattrs->sattrs (pattern-attrs the-pattern)))
                          (define the-variant (variant hstx attrs the-pattern null))
                          (define the-rhs (rhs attrs #t #f (list the-variant) null #f #f))
                          (with-syntax ([(parser) (generate-temporaries '(eh-alt-parser))]
                                        [the-rhs-expr (datum->expression the-rhs)])
                            (list (eh-alternative (ehpat-repc ehpat) attrs #'parser)
                                  (list #`(define parser
                                            (parser/rhs parser () #,attrs
                                                        the-rhs-expr #t #,stx)))))]
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
                                         (datum->expression (eh-alternative-repc alt))]
                                        [attrs-expr
                                         #`(quote #,(eh-alternative-attrs alt))]
                                        [parser-expr
                                         #`(quote-syntax #,(eh-alternative-parser alt))])
                            #'(eh-alternative repc-expr attrs-expr parser-expr)))])
           #'(begin def ...
                    (define-syntax name
                      (eh-alternative-set (list alt-expr ...))))))])))
