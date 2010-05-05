#lang scheme/base

(require (rename-in typed-scheme (#%module-begin #%module-begin-typed-scheme)))
(require (for-syntax scheme/base
                     syntax/stx
                     syntax/name
                     syntax/define
                     syntax/parse
                     syntax/parse/experimental/splicing
                     scheme/splicing
                     "contexts.ss"
                     "util.ss"
                     "ops.ss"
                     "parse.ss"
                     )
         "literals.ss"
         ;; "typed-utils.ss"
         )

(require (for-meta 2 scheme/base "util.ss"))
(require (for-meta 3 scheme/base))

(provide (all-defined-out))


;; (define-syntax (\; stx) (raise-syntax-error '\; "out of context" stx))

(begin-for-syntax

;; these functions use parse-block-one 
;; (define parse-a-tail-expr #f)
;; (define parse-an-expr #f)
;; (set! parse-a-tail-expr parse-tail-expr)
;; (set! parse-an-expr parse-expr)

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
                                       (start-operator #'(expr . more)))
                               #;
                               (if (honu-type? expr-or-type)
                                 ;; parens as a unary prefix operator
                                 (cons (make-cast-prefix (stx-car (stx-car stx)) expr-or-type)
                                       (start-expr #'(expr . more)))
                                 ;; must have been an expression
                                 (cons expr-or-type
                                       (start-operator #'(expr . more))))))]
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
                 #;
                 [(cast-prefix? op)
                  (let ([after (reverse since)])
                    (group (append (reverse before)
                                   (list (quasisyntax/loc (op-id op)
                                                          (op-cast #,(op-id op) 
                                                                   #,(let ([t (cast-prefix-type op)])
                                                                       (list (honu-type-stx t)
                                                                             (honu-type-name-stx t)
                                                                             (honu-type-pred-stx t)
                                                                             (honu-type-protect-stx t)))
                                                                   #,(car after))))
                                   (cdr after))))]
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

(define (parse-tail-expr expr-stxs)
  (syntax-parse expr-stxs
    #:literals (honu-return)
    [(honu-return expr ...)
     #:fail-when (stx-null? #'(expr ...)) "missing expression"
     (parse-expr (syntax->list #'(expr ...)))]
    [else (parse-expr expr-stxs)]))


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
                                         (printf "~s\n" v)))
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

#|
(define-honu-macro (e ... * e ... \;))

(foo . bar ())
x(2)
|#

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

(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))


(define-honu-syntax honu-provide
  (lambda (stx ctx)
    (syntax-parse stx
      #:literals (semicolon)
      [(_ something:id semicolon . rest)
       (values #'(provide something)
               #'rest)])))

;; (honu-syntax ...)

(define-honu-syntax honu-macro-item
  (lambda (stx ctx)
    (syntax-parse stx
      #:literals (#%braces)
      [(_ name:id (#%braces literals (#%braces literal ...)
                   items ...) . rest)
       (values #'(define-syntax-class name [pattern x])
               #'rest)])))

(define-honu-syntax honu-if
  (lambda (stx ctx)
    (define (parse-complete-block stx)
      ;; (printf "Parsing complete block ~a\n" (syntax->datum stx))
      (with-syntax ([(exprs ...) (parse-block stx the-expression-block-context)])
        #'(begin exprs ...))
      #;
      (let-values ([(a b)
                    (parse-block-one
                      (if (block-context-return? ctx)
                        the-expression-return-block-context
                        the-expression-block-context)
                      stx
                      (lambda (expr rest)
                        (values expr rest))
                      (lambda ()
                        (raise-syntax-error
                          #f
                          "expected a braced block or a statement"
                          )))])
        (printf "Result is ~a and ~a\n" a b)
        a))
    ;; TODO: move these syntax classes to a module
    (define-syntax-class expr
      [pattern e])
    (define-syntax-class paren-expr
      [pattern (#%parens expr:expr)])
    (define-syntax-class block
                         [pattern (#%braces statement ...)
                                  #:with line (parse-complete-block #'(statement ...))])
    ;; (printf "Original syntax ~a\n" (syntax->datum stx))
    (syntax-parse stx
      #:literals (else)
      [(_ condition:paren-expr on-true:block else on-false:block . rest)
       ;; (printf "used if with else\n")
       (let ([result #'(if condition.expr on-true.line on-false.line)])
         (expression-result ctx result (syntax/loc #'rest rest)))]
      [(_ condition:paren-expr on-true:block . rest)
       ;; (printf "used if with no else\n")
       (let ([result #'(when condition.expr on-true.line)])
         (expression-result ctx result #'rest))])))

#|
if (foo){
  blah..
} else {
}

|#

#;
(define-honu-syntax honu-if
  (lambda (stx ctx)
    (define (get-block-or-statement kw rest)
      (syntax-parse rest (#%braces)
        [((#%braces then ...) . rrest)
         (values
           #`(honu-unparsed-block #f obj 'obj #f #,(and (block-context-return? ctx)
                                                        (stx-null? rest))
                                  . #,(stx-cdr (stx-car rest)))
           #'rrest)]
        [else
          (parse-block-one (if (block-context-return? ctx)
                             the-expression-return-block-context
                             the-expression-block-context)
                           rest
                           (lambda (expr rest)
                             (values expr rest))
                           (lambda ()
                             (raise-syntax-error
                               #f
                               "expected a braced block or a statement"
                               kw)))]))

    (unless (block-context? ctx)
      (raise-syntax-error
        #f
        "allowed only in a block context"
        (stx-car stx)))

    (syntax-parse stx (#%parens)
      [(_ (#%parens test ...) . rest)
       (let* ([tests #'(test ...)])
         (when (stx-null? tests)
           (raise-syntax-error
             #f
             "missing test expression"
             (stx-car stx)
             (stx-car (stx-cdr stx))))
         (let ([test-expr (parse-expr (syntax->list tests))])
           (let-values ([(then-exprs rest) (get-block-or-statement (stx-car stx) #'rest)])
             (syntax-case rest (else)
               [(else . rest2)
                (let-values ([(else-exprs rest) (get-block-or-statement (stx-car rest) #'rest2)])
                  (expression-result ctx
                                     #`(if (as-test #,test-expr) #,then-exprs #,else-exprs)
                                     rest))]
               [_else
                 (expression-result ctx #`(if (as-test #,test-expr) #,then-exprs (void)) rest)]))))]
      [_else
        (raise-syntax-error
          #f
          "expected a parenthesized test after `if' keyword"
          (stx-car stx))])))

(define true #t)
(define false #f)

(define (show-top-result v)
  (unless (void? v)
    (printf "~s\n" v)))

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
  (raise-syntax-error #f "interactive use is not yet supported"))

(define-syntax (foobar2000 stx)
  (printf "Called foobar2000 on ~a\n" (syntax->datum stx))
  (syntax-case stx ()
    [(_ x y ...) #'(printf "foobar2000 ~a\n" x)]))

(define (display2 x y)
  (printf "~a ~a" x y))



(define-syntax (honu-unparsed-begin stx)
  (printf "honu unparsed begin: ~a\n" (syntax->datum stx))
  (syntax-case stx ()
    [(_) #'(begin (void))]
    [(_ . body) (let-values ([(code rest) (parse-block-one/2 #'body
                                                             the-expression-context
                                                             #;
                                                             the-top-block-context)])
                  ;; (printf "Rest is ~a\n" (syntax->datum rest))
                  (with-syntax ([code code]
                                [(rest ...) rest])
                    (syntax/loc stx
                                (begin code (honu-unparsed-begin rest ...)))))]
    #;
    [(_ . body) (let-values ([(code rest) (parse-block-one the-top-block-context
                                                           #'body 
                                                           values
                                                           (lambda ()
                                                             (values #'(void) null)))])
                  (with-syntax ([code code]
                                [(rest ...) rest])
                    #'(begin code (honu-unparsed-begin rest ...))))]))

#;
(define-syntax-rule (#%dynamic-honu-module-begin forms ...)
                    #;
                    (#%module-begin-typed-scheme
                     ;; (require honu/private/typed-utils)
                     (honu-unparsed-begin forms ...))
                    (#%plain-module-begin (honu-unparsed-begin forms ...)))

(define-syntax (#%dynamic-honu-module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (begin
       (printf "Module begin ~a\n" (syntax->datum #'(forms ...)))
       #'(#%plain-module-begin (honu-unparsed-begin forms ...)))]))
