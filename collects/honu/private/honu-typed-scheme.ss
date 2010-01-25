#lang scheme/base

(require (rename-in typed-scheme (#%module-begin #%module-begin-typed-scheme)))
(require (for-syntax scheme/base
                     syntax/stx
                     syntax/name
                     syntax/define
                     syntax/parse
                     scheme/splicing
                     "contexts.ss"
                     "util.ss"
                     "ops.ss"
                     )
         ;; "typed-utils.ss"
         )

(require (for-meta 2 scheme/base "util.ss"))
(require (for-meta 3 scheme/base))

(provide (all-defined-out))

;; macro for defining literal tokens that can be used in macros
(define-syntax-rule (define-literal name ...)
  (begin
   (define-syntax name (lambda (stx)
                         (raise-syntax-error 'name
                                             "this is a literal and cannot be used outside a macro")))
   ...))

(define-literal honu-return)
(define-literal semicolon)
(define-literal honu-+ honu-* honu-/ honu-- honu-|| honu-%
                honu-= honu-+= honu--= honu-*= honu-/= honu-%=
                honu-&= honu-^= honu-\|= honu-<<= honu->>= honu->>>=
                honu->> honu-<< honu->>> honu-< honu-> honu-<= honu->=)

;; (define-syntax (\; stx) (raise-syntax-error '\; "out of context" stx))

(begin-for-syntax

(define-values (prop:honu-transformer honu-transformer? honu-transformer-ref)
               (make-struct-type-property 'honu-transformer))


(define-values (struct:honu-trans make-honu-trans honu-trans? honu-trans-ref honu-trans-set!)
  (make-struct-type 'honu-trans #f 1 0 #f
                    (list (list prop:honu-transformer #t))
                    (current-inspector) 0))

(define (make-honu-transformer proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-type-error
      'define-honu-syntax
      "procedure (arity 2)"
      proc))
  (make-honu-trans proc))



(define operator? 
  (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
    (lambda (stx)
      (and (identifier? stx)
           (let ([str (symbol->string (syntax-e stx))])
             (and (positive? (string-length str))
                  (memq (string-ref str 0) sym-chars)))))))

(define (get-transformer stx)
  ;; if its an identifier and bound to a transformer return it
  (define (bound-transformer stx)
    (and (stx-pair? stx)
         (identifier? (stx-car stx))
         (let ([v (syntax-local-value (stx-car stx) (lambda () #f))])
           (and (honu-transformer? v) v))))
  (define (special-transformer stx) 
    (and (stx-pair? stx)
         (let ([first (stx-car stx)])
           (cond
             [(and (stx-pair? first)
                   (identifier? (stx-car first))
                   (delim-identifier=? #'#%parens (stx-car first)))
              ;; If the stx-car is a list with just one operator symbol,
              ;;  try using the operator as a transformer
              (let ([l (cdr (stx->list first))])
                (let loop ([l l])
                  (cond
                    [(null? l) #f]
                    [(operator? (car l))
                     (if (ormap operator? (cdr l))
                       #f
                       (let ([v (syntax-local-value (car l) (lambda () #f))])
                         (and (honu-transformer? v)
                              v)))]
                    [else (loop (cdr l))])))]
             [(and (stx-pair? first)
                   (identifier? (stx-car first))
                   (free-identifier=? #'#%angles (stx-car first)))
              (let ([v (syntax-local-value (stx-car first) (lambda () #f))])
                (and (honu-transformer? v) v))]
             [else #f]))))
  ;; (printf "~a bound transformer? ~a\n" stx (bound-transformer stx))
  (or (bound-transformer stx)
      (special-transformer stx)))

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
    [else (call-values parse-one (extract-until body (list #'\;
                                                           )))]))

#|
(define-honu-macro (e ... * e ... \;))

(foo . bar ())
x(2)
|#

(define (parse-block-one/2 stx context)
  (define (parse-one stx context)
    (define-syntax-class block
                         [pattern (#%braces statement ...)
                                  #:with result #'(honu-unparsed-begin statement ...)])
    (define-syntax-class function
                         [pattern (type:id name:id (#%parens args ...) body:block . rest)
                                  #:with result #'(define (name args ...)
                                                    body.result)])
    (define-syntax-class expr
                         [pattern f])

    (define-splicing-syntax-class call
                         [pattern (~seq e:expr (#%parens arg:expression-1))
                                  #:with call #'(e arg.result)])
    (define-splicing-syntax-class expression-last
                         [pattern (~seq call:call) #:with result #'call.call]
                         [pattern (~seq x:number) #:with result #'x]
                         )

    (define-syntax-rule (define-infix-operator name next [operator reducer] ...)
      (begin
       (define-syntax-class operator-class
         #:literals (operator ...)
         (pattern operator #:attr func reducer)
         ...)
       (define-splicing-syntax-class name
         (pattern (~seq (~var left next)
                        (~optional (~seq (~var op operator-class) (~var right name))))
                  #:with result
                  (cond [(attribute right)
                         ((attribute op.func) #'left.result #'right.result)]
                        [else
                         #'left.result])))))

    #;
    (define-syntax-rule (define-infix-operator name next [operator reducer] ...)
      (define-splicing-syntax-class name
                                    #:literals (operator ...)
                                    [pattern (~seq (~var left next) operator (~var right name))
                                             #:with result (reducer #'left.result #'right.result)]
                                    ...
                                    [pattern (~seq (~var exp next))
                                             #:with result #'exp.result]
                                    ))

    ;; TODO: maybe just have a precedence macro that creates all these constructs
    ;;   (infix-operators ([honu-* ...]
    ;;                     [honu-- ...])
    ;;                    ([honu-+ ...]
    ;;                     [honu-- ...]))
    ;; Where operators defined higher in the table have higher precedence.
    (define-syntax (infix-operators stx)
      (define (create-stuff names operator-stuff)
        (define make (syntax-lambda (expression next-expression operator-stuff)
                                    #;
                                    (printf "Make infix ~a ~a\n" (syntax->datum #'expression) (syntax->datum #'next-expression))
                                    (with-syntax ([(ops ...) #'operator-stuff])
                                      #'(define-infix-operator expression next-expression ops ...))))
        (for/list ([name1 (drop-last names)]
                   [name2 (cdr names)]
                   [operator operator-stuff])
                  (make name1 name2 operator)))
      (syntax-case stx ()
        [(_ first last operator-stuff ...)
         (with-syntax ([(name ...) (generate-temporaries #'(operator-stuff ...))])
           (with-syntax ([(result ...) (create-stuff (cons #'first
                                                           (append
                                                             (drop-last (syntax->list #'(name ...)))
                                                             (list #'last)))

                                                     (syntax->list #'(operator-stuff ...)))])
             #'(begin
                 result ...)))]))

    #;
    (infix-operators expression-1 expression-last
                       ([honu-+ (syntax-lambda (left right)
                                    #'(+ left right))]
                        [honu-- (syntax-lambda (left right)
                                               #'(- left right))])
                       ([honu-* (syntax-lambda (left right)
                                               #'(* left right))]
                        [honu-/ (syntax-lambda (left right)
                                               #'(/ left right))]))
    

    (define-syntax-class expression-top
                         [pattern (e:expression-1 semicolon . rest)
                                  #:with result #'e.result])


    ;; infix operators in the appropriate precedence level
    ;; things defined lower in the table have a higher precedence.
    ;; the first set of operators is `expression-1'
    (splicing-let-syntax ([sl (make-rename-transformer #'syntax-lambda)])
      (infix-operators expression-1 expression-last
        ([honu-= (sl (left right) #'(= left right))]
         [honu-+= (sl (left right) #'(+ left right))]
         [honu--= (sl (left right) #'(- left right))]
         [honu-*= (sl (left right) #'(* left right))]
         [honu-/= (sl (left right) #'(/ left right))]
         [honu-%= (sl (left right) #'(modulo left right))]
         [honu-&= (sl (left right) #'(+ left right))]
         [honu-^= (sl (left right) #'(+ left right))]
         [honu-\|= (sl (left right) #'(+ left right))]
         [honu-<<= (sl (left right) #'(+ left right))]
         [honu->>= (sl (left right) #'(+ left right))]
         [honu->>>= (sl (left right) #'(+ left right))])
        ([honu-|| (sl (left right) #'(+ left right))])
        ([honu->> (sl (left right) #'(+ left right))]
         [honu-<< (sl (left right) #'(+ left right))]
         [honu->>> (sl (left right) #'(+ left right))]
         [honu-< (sl (left right) #'(< left right))]
         [honu-> (sl (left right) #'(> left right))]
         [honu-<= (sl (left right) #'(<= left right))]
         [honu->= (sl (left right) #'(>= left right))])
        ([honu-+ (sl (left right) #'(+ left right))]
         [honu-- (sl (left right) #'(- left right))])
        ([honu-* (sl (left right) #'(* left right))]
         [honu-% (sl (left right) #'(modulo left right))]
         [honu-/ (sl (left right) #'(/ left right))])))

    ;; (printf "~a\n" (syntax-class-parse function stx))
    (syntax-parse stx
      [function:function (values #'function.result #'function.rest)]
      [expr:expression-top (values #'expr.result #'expr.rest)]
      [(x:number . rest) (values #'x #'rest)]
      ))
  (cond
    [(stx-null? stx) (values stx '())]
    [(get-transformer stx) => (lambda (transformer)
                                (transformer stx context))]
    [else (parse-one stx context)]))

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

(define-syntax (define-honu-syntax stx)
    (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
      (with-syntax ([id id]
		    [rhs rhs])
	#'(define-syntax id (make-honu-transformer rhs)))))


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

(define-honu-syntax honu-if
  (lambda (stx ctx)
    (define (parse-complete-block stx)
      ;; (printf "Parsing complete block ~a\n" (syntax->datum stx))
      (with-syntax ([(exprs ...) (parse-block stx ctx)])
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
         (expression-result ctx result #'rest))]
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

(define-syntax (honu-unparsed-begin stx)
  ;; (printf "honu unparsed begin: ~a\n" (syntax->datum stx))
  (syntax-case stx ()
    [(_) #'(begin (void))]
    [(_ . body) (let-values ([(code rest) (parse-block-one/2 #'body
                                                             the-top-block-context)])
                  ;; (printf "Rest is ~a\n" (syntax->datum rest))
                  (with-syntax ([code code]
                                [(rest ...) rest])
                    #'(begin code (honu-unparsed-begin rest ...))))]
    #;
    [(_ . body) (let-values ([(code rest) (parse-block-one the-top-block-context
                                                           #'body 
                                                           values
                                                           (lambda ()
                                                             (values #'(void) null)))])
                  (with-syntax ([code code]
                                [(rest ...) rest])
                    #'(begin code (honu-unparsed-begin rest ...))))]))

(define-syntax-rule (#%dynamic-honu-module-begin forms ...)
                    #;
                    (#%module-begin-typed-scheme
                     ;; (require honu/private/typed-utils)
                     (honu-unparsed-begin forms ...))
                    (#%plain-module-begin (honu-unparsed-begin forms ...)))
