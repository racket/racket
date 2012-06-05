#lang racket/base

;; (require (rename-in typed-scheme (#%module-begin #%module-begin-typed-scheme)))
(require (for-syntax scheme/base
                     syntax/stx
                     syntax/name
                     syntax/define
                     syntax/parse
                     syntax/parse/experimental/splicing
                     scheme/splicing
                     macro-debugger/emit
                     racket/pretty
                     "compile.rkt"
                     "debug.rkt"
                     "contexts.rkt"
                     "util.rkt"
                     "ops.rkt"
                     "syntax.rkt"
                     ;; "parse.rkt"
                     "parse2.rkt"
                     "literals.rkt"
                     )
         syntax/parse
         "literals.rkt"
         "debug.rkt"
         ;; (prefix-in honu: "honu.rkt")
         ;; (prefix-in honu: "macro2.rkt")
         ;; "typed-utils.ss"
         )

(require (for-meta 2 racket/base "util.rkt"))
(require (for-meta 3 racket/base))

(provide (all-defined-out))

(begin-for-syntax

#;
(define parse-expr
  ;; The given syntax sequence must not be empty
  (let ()
    (define (parse-expr-seq stx)
      (define (start-expr stx) 
        (let ([trans (get-transformer stx)])
          (if trans
            (let-values ([(expr rest) (trans stx the-expression-context)])
              (if (stx-null? rest)
                (list expr)
                (cons expr (start-operator rest))))
            (syntax-case* stx (#%parens #%braces #%angles) delim-identifier=?
                          [(v)
                           (or (number? (syntax-e #'v))
                               (identifier? #'v)
                               (string? (syntax-e #'v)))
                           (if (operator? #'v)
                             (raise-syntax-error
                               #f
                               "operator alone is not an expression and cannot start an expression"
                               #'v)
                             (list #'v))]
                          [((#%parens . pexpr))
                           ;; parens as an expression
                           (if (stx-null? #'pexpr)
                             (raise-syntax-error
                               #f
                               "missing expression inside parentheses as expression"
                               (stx-car stx))
                             (list (parse-expr #'pexpr)))]
                          [((#%parens . pexpr) expr . more)
                           (get-transformer #'pexpr)
                           ;; Expand pexpr in an expression-or-type context, and make a cast 
                           ;; if it's a type.
                           (let ([trans (get-transformer #'pexpr)])
                             (let-values ([(expr-or-type rest) (trans #'pexpr the-type-or-expression-context)])
                               (cons expr-or-type
                                     (start-operator #'(expr . more)))))]
                          [((#%braces . pexpr))
                           (if (stx-null? #'pexpr)
                             (raise-syntax-error
                               #f
                               "missing expression inside braces as expression"
                               (stx-car stx))
                             (list #'(honu-unparsed-block #f obj 'obj #f #f . pexpr)))]
                          [(op . more)
                           (and (identifier? #'op)
                                (memq (syntax-e #'op) unary-prefix-ops))
                           (cons (make-prefix (stx-car stx)) (start-expr #'more))]
                          [(expr then . more)
                           (append (start-expr (list #'expr))
                                   (start-operator #'(then . more)))]
                          [(bad . rest)
                           (raise-syntax-error
                             'expression
                             "unknown expression form"
                             #'bad)]))))
      (define (start-operator stx)
        (unless (or (and (stx-pair? (stx-car stx))
                         (let ([id (stx-car (stx-car stx))])
                           (or (delim-identifier=? #'#%brackets id)
                               (delim-identifier=? #'#%parens id)
                               (delim-identifier=? #'#%angles id))))
                    (and (identifier? (stx-car stx))
                         (hash-ref op-table
                                   (syntax-e (stx-car stx))
                                   (lambda () #f))))
          (raise-syntax-error
            'expression
            "expected an operator, but found something else"
            (stx-car stx)))
        ;; Check for postfix operator, first (or parens as a
        ;;  an "infix" operator)
        (cond
          [(stx-pair? (stx-car stx))
           ;; Convert vector index or application to a binary operator:
           (let ([opl (let ([id (stx-car (stx-car stx))])
                        ;; Note that we don't check for whether #%brackets, etc. is
                        ;;  bound as a transformer, which means that you can't
                        ;;  change the parsing of [], (), or <> as an "infix" operator.
                        (cond
                          [(delim-identifier=? #'#%brackets id)
                           (let ([index-expr (parse-expr (stx-cdr (stx-car stx)))])
                             (list (make-infix id)
                                   index-expr))]
                          [(delim-identifier=? #'#%parens id)
                           (let ([arg-exprs (parse-arg-list (stx-cdr (stx-car stx)))])
                             (list (make-infix id)
                                   arg-exprs))]
                          [(delim-identifier=? #'#%angles id)
                           (list (make-infix id)
                                 ;; These are normally type expressions, so
                                 ;;  leave parsing to honu-type-ap:
                                 (stx-cdr (stx-car stx)))]
                          [else (error "internal error parsing expr")]))])
             (if (stx-null? (stx-cdr stx))
               opl
               (append opl (start-operator (stx-cdr stx)))))]
          [(memq (syntax-e (stx-car stx)) unary-postfix-ops)
           (if (stx-null? (stx-cdr stx))
             (list (make-postfix (stx-car stx)))
             (cons (make-postfix (stx-car stx))
                   (start-operator (stx-cdr stx))))]
          [else
            ;; Otherwise, must be infix
            (cons (make-infix (stx-car stx))
                  (start-expr (stx-cdr stx)))]))
      (start-expr stx))

    (define (parse-expr stx)
      (let group ([seq (parse-expr-seq stx)])
        ;; seq is a list that mixes exprs with ops.
        ;; Find leftmost oper with maximal precedence
        (if (null? (cdr seq))
          (car seq)
          (let loop ([seq seq][before null][op #f][since null])
            (cond
              [(null? seq)
               (cond
                 [(prefix? op)
                  (let ([after (reverse since)])
                    (group (append (reverse before)
                                   (list (quasisyntax/loc (op-id op)
                                                          (op-app #,(op-id op) #%prefix #,(car after))))
                                   (cdr after))))]
                 [(postfix? op)
                  (let ([after (reverse since)]
                        [before (reverse before)])
                    (group (append (cdr before)
                                   (list (quasisyntax/loc (op-id op)
                                                          (op-app #,(op-id op) #%postfix #,(car before))))
                                   after)))]
                 [(infix? op)
                  (let ([after (reverse since)])
                    (group (append (reverse (cdr before))
                                   (list (quasisyntax/loc (op-id op)
                                                          (op-app #,(op-id op) #,(car before) #,(car after))))
                                   (cdr after))))]
                 [else (error 'parse-expr "not an op!: ~s ~s ~s" op before since)])]
              [(not (op? (stx-car seq)))
               (loop (cdr seq) before op (cons (car seq) since))]
              [((if (prefix? op) >= >)
                (hash-ref precedence-table (prec-key (car seq)) (lambda () 0))
                (hash-ref precedence-table (prec-key op) (lambda () 0)))
               (loop (cdr seq) 
                     (if op
                       (append since (list op) before)
                       since)
                     (car seq) null)]
              [else
                (loop (cdr seq) before op (cons (car seq) since))])))))

    (define (parse-arg-list stxs)
      (if (stx-null? stxs)
        stxs
        (let-values ([(val-stxs after-expr terminator) (extract-until stxs (list #'\,))])
          (when (and val-stxs
                     (stx-null? (stx-cdr after-expr)))
            (raise-syntax-error
              'procedure\ call
              "missing expression after comma"
              (stx-car after-expr)))
          (when (null? val-stxs)
            (raise-syntax-error
              'procedure\ call
              "missing expression before token"
              (stx-car after-expr)))
          (if val-stxs
            (cons (parse-expr val-stxs)
                  (parse-arg-list (stx-cdr after-expr)))
            (list (parse-expr stxs))))))

    parse-expr))

#;
(define (parse-tail-expr expr-stxs)
  (syntax-parse expr-stxs
    #:literals (honu-return)
    [(honu-return expr ...)
     #:fail-when (stx-null? #'(expr ...)) "missing expression"
     (parse-expr (syntax->list #'(expr ...)))]
    [else (parse-expr expr-stxs)]))


#;
(define (parse-block-one context body combine-k done-k)
  (define (parse-one expr-stxs after-expr terminator)
    (define (checks)
      (unless expr-stxs
        (raise-syntax-error
          #f
          "expected a semicolon to terminate form"
          (stx-car body)))
      (when (null? expr-stxs)
        (raise-syntax-error
          #f
          "missing expression before terminator"
          terminator)))
    (checks)
    (let* ([parser (if (block-context-return? context)
                     parse-tail-expr
                     parse-expr)]
           [code (parser expr-stxs)])
      (with-syntax ([code code])
        (with-syntax ([top-expr (if (top-block-context? context) 
                                   #'(let ([v code])
                                       (unless (void? v)
                                         (debug "~s\n" v)))
                                   #'code)])
          (combine-k #'(#%expression top-expr)
                     (stx-cdr after-expr))))))
  (cond
    [(stx-null? body) (done-k)]
    [(get-transformer body) =>
     (lambda (transformer)
       (let-values ([(code rest) (transformer body context)])
         (combine-k code rest)))]
    [else (call-values (extract-until body (list #'\;
                                                 ))
                       parse-one )]))

#;
(define (parse-block stx ctx)
  (let loop ([stx stx])
    (parse-block-one ctx
                     stx 
                     (lambda (code rest)
                       (cons code (loop rest)))
                     (lambda ()
                       null))))

(define (expression-result ctx expr rest)
     (if (top-block-context? ctx)
         (values #`(#%expression (show-top-result #,expr)) rest)
         (values #`(#%expression #,expr) rest)))

)

#|

Yes, check out head patterns and splicing syntax classes.

For example, if 'display' is a special kind of statement, you might have something like this:

(define-splicing-syntax-class statement
      (pattern (~seq (~literal display) something (~literal \;)))
      ___ <other alternatives> ___)

Then, in the pattern above for 'if', 'then' would be bound to the following syntax list:
  (display (#%braces "hello world") \;) 

(if expr block else statement rest)
(if expr block rest)

|#

(define-syntax (define-honu-infix-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-infix-transformer rhs))))))

#;
(honu:define-honu-syntax honu-macro-item
  (lambda (stx ctx)
    (syntax-parse stx
      #:literals (#%braces)
      [(_ name:id (#%braces literals (#%braces literal ...)
                   items ...) . rest)
       (values #'(define-syntax-class name [pattern x])
               #'rest)])))

#;
(honu:define-honu-syntax honu-scheme
  (lambda (stx ctx)
    (syntax-parse stx #:literals (semicolon)
      [(_ template semicolon rest ...)
       (values (lambda () #'(lambda () template)) #'(rest ...))]
      [else (raise-syntax-error 'scheme "need a semicolon probably" stx)]
      )))

#;
(honu:define-honu-syntax honu-keywords
  (lambda (stx ctx)
    (syntax-parse stx #:literals (semicolon)
      [(_ keyword:honu-identifier ... semicolon . rest)
       (values (lambda () (apply-scheme-syntax
                            (syntax/loc stx
                                        (begin
                                          (define-syntax keyword.x (lambda (xx) (raise-syntax-error 'keyword.x "dont use this")))
                                          ...))))
               #'rest)])))

#|
(honu:define-honu-syntax honu-if
  (lambda (stx ctx)
    (define (parse-complete-block stx)
      ;; (debug "Parsing complete block ~a\n" (syntax->datum stx))
      (with-syntax ([(exprs ...) (parse-block stx the-expression-block-context)])
        #'(begin exprs ...)))
    ;; TODO: move these syntax classes to a module
    (define-syntax-class expr
      [pattern e])
    (define-syntax-class paren-expr
                         #:literals (#%parens)
      [pattern (#%parens expr:expression) #:with result #'expr.result])
    (define-syntax-class block
                         [pattern (#%braces statement ...)
                                  #:with line #'(honu-unparsed-begin statement ...)])
    ;; (debug "Original syntax ~a\n" (syntax->datum stx))
    (syntax-parse stx
      #:literals (else)
      [(_ condition:paren-expr on-true:block else on-false:block . rest)
       ;; (debug "Condition expr is ~a\n" #'condition.expr)
       ;; (debug "used if with else\n")
       (let ([result #'(if condition.result on-true.line on-false.line)])
         (values
           (lambda () result)
           #'rest))]
      [(_ condition:paren-expr on-true:block . rest)
       ;; (debug "used if with no else\n")
       (let ([result #'(when condition.result on-true.line)])
         (values
           (lambda () result)
           #'rest))])))
|#


(define (show-top-result v)
  (unless (void? v)
    (debug "~s\n" v)))

(define-syntax (op-app stx)
    (syntax-case stx (#%parens #%angles)
      [(_ #%parens a (b ...))
       #'(a b ...)
       #;
       #'(honu-app a b ...)]
      [(_ #%angles a (b ...))
       #'(honu-type-app a b ...)]
      [(_ a b ...) 
       (datum->syntax #'a
                      (cons #'a #'(b ...))
                      #'a)]))
  
(define-syntax (honu-top stx)
  (debug "Honu ~a\n" (syntax->datum stx))
  (raise-syntax-error #f "interactive use is not yet supported"))

(define (display2 x y)
  (debug "~a ~a" x y))

#;
(define-syntax (honu-unparsed-expr stx)
  (syntax-parse stx
    [(_ expr ...)
     (emit-remark "Parse an expression" #'(expr ...))
     (parse-an-expr #'(expr ...))]
    [else (raise-syntax-error 'honu-unparsed-expr "Invalid expression syntax" stx)]))

#;
(honu:define-honu-syntax scheme-syntax
  (lambda (body ctx)
    (syntax-parse body
      [(_ template . rest)
       (values
         (lambda ()
           (debug "Applying syntax to ~a\n" (quote-syntax template))
           (apply-scheme-syntax #'#'template))
         #'rest)])))

#;
(honu:define-honu-syntax honu-provide
  (lambda (body ctx)
    (syntax-parse body #:literals (semicolon)
      [(_ x:honu-identifier ... semicolon . rest)
       (values
         (lambda ()
           (debug "Providing ~a\n" #'(x ...))
           #'(provide x.x ...))
         #'rest)])))

#;
(honu:define-honu-syntax honu-require
  (lambda (body ctx)
    (define-syntax-class for-syntax-form
                         #:literals (#%parens honu-for-syntax)
      [pattern (#%parens honu-for-syntax spec)
               #:with result
               (datum->syntax #'spec (cons #'for-syntax (cons #'spec #'()))
                              #'spec #'spec)
               #;
               (datum->syntax body (cons #'for-syntax (cons #'spec #'()))
                                            body body)])
    (define-syntax-class for-template-form
                         #:literals (#%parens honu-for-template)
      [pattern (#%parens honu-for-template spec)
               #:with result
               (datum->syntax #'spec (cons #'for-template (cons #'spec #'()))
                              #'spec #'spec)])
    (define-syntax-class normal-form
      [pattern x:str #:with result #'x])
    (define-syntax-class form
      [pattern x:for-syntax-form #:with result #'x.result]
      [pattern x:for-template-form #:with result #'x.result]
      [pattern x:normal-form #:with result #'x.result])
    (syntax-parse body #:literals (semicolon)
      [(_ form:form ... semicolon . rest)
       (values
         (lambda ()
           (datum->syntax
             body
             (cons #'require
                   #'(form.result ...))
             body
             body))
         #'rest)])))

(define-for-syntax (honu-expand forms)
  (parse-one forms))

(define-for-syntax (honu-compile forms)
  #'(void))

(provide honu-unparsed-begin)
(define-syntax (honu-unparsed-begin stx)
  (emit-remark "Honu unparsed begin!" stx)
  (debug "honu unparsed begin: ~a at phase ~a\n" (syntax->datum stx) (syntax-local-phase-level))
  (syntax-parse stx
    [(_) #'(void)]
    [(_ forms ...)
     #'(begin
         (define-syntax (parse-more stx)
           (syntax-case stx ()
             [(_ stuff (... ...))
              (do-parse-rest #'(stuff (... ...)) #'parse-more)]))
         (parse-more forms ...))
     ;; if parsed is #f then we don't want to expand to anything that will print
     ;; so use an empty form, begin, `parsed' could be #f becuase there was no expression
     ;; in the input such as parsing just ";".
     #;
     (begin
     (debug "expanded ~a unexpanded ~a\n"
            (if parsed (syntax->datum parsed) parsed)
            (syntax->datum unparsed))(define-values (parsed unparsed) (honu-expand #'(forms ...)))
     (with-syntax ([parsed (if (not parsed) #'(begin)
                             (remove-repeats parsed)
                             #;
                             (honu->racket parsed))]
                   [(unparsed ...) unparsed])
       (debug "Final parsed syntax\n~a\n" (pretty-format (syntax->datum #'parsed)))
       (debug "Unparsed syntax ~a\n" #'(unparsed ...))
       (if (null? (syntax->datum #'(unparsed ...)))
         (if (parsed-syntax? #'parsed)
           #'parsed
           (with-syntax ([(out ...) #'parsed])
             #'(honu-unparsed-begin out ...)))
         (if (parsed-syntax? #'parsed)
           #'(begin parsed (honu-unparsed-begin unparsed ...))
           (with-syntax ([(out ...) #'parsed])
             #'(honu-unparsed-begin out ... unparsed ...))))))]))

(define-syntax (#%dynamic-honu-module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (begin
       (debug "Module begin ~a\n" (pretty-format (syntax->datum #'(forms ...))))
       #'(#%module-begin (honu-unparsed-begin forms ...)))]))

(provide honu-top-interaction)
(define-syntax (honu-top-interaction stx)
  (syntax-case stx ()
    [(_ rest ...)
     #'(#%top-interaction . (honu-unparsed-begin rest ...))]))

