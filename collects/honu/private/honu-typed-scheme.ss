#lang scheme/base

(require (for-syntax scheme/base
                     syntax/stx
                     syntax/parse
                     "contexts.ss"
                     "util.ss"
                     "ops.ss"
                     ))

(provide (all-defined-out))

;; macro for defining literal tokens that can be used in macros
(define-syntax-rule (define-literal name)
                    (define-syntax name (lambda (stx)
                                          (raise-syntax-error 'name
                                                              "this is a literal and cannot be used outside a macro"))))

(define-literal honu-return)

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

(define (call-values function values-producing)
  (call-with-values (lambda () values-producing) function))

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
      (with-syntax ([top-expr ((if (top-block-context? context) 
                                      (lambda (x) 
                                        `(show-top-result ,x))
                                      values)
                                    code)])
        (combine-k #'(#%expression top-expr)
                   (stx-cdr after-expr)))))
  (cond
    [(stx-null? body) (done-k)]
    [(get-transformer body) =>
     (lambda (transformer)
       (let-values ([(code rest) (transformer body context)])
         (combine-k code rest)))]
    [else (call-values parse-one (extract-until body (list #'\;
                                                           )))]))

)

(define-syntax (honu-unparsed-begin stx)
  (syntax-case stx ()
    [(_) #'(begin)]
    [(_ . body) (let-values ([(code rest) (parse-block-one the-top-block-context
                                                           #'body 
                                                           values
                                                           (lambda ()
                                                             (values #'(void) null)))])
                  (with-syntax ([code code]
                                [(rest ...) rest])
                    #'(begin code (honu-unparsed-begin rest ...))))]))

(define-syntax-rule (#%dynamic-honu-module-begin forms ...)
                    (#%plain-module-begin (honu-unparsed-begin forms ...)))
