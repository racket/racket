#lang racket/base

(require "contexts.ss"
         "util.ss"
         (for-template "literals.ss"
                       "language.ss"
                       "syntax.ss"
                       racket/class)
         syntax/parse
         syntax/parse/experimental/splicing
         "syntax.ss"
         (for-syntax syntax/parse
                     racket/base)
         macro-debugger/emit
         scheme/splicing
         (for-syntax syntax/define)
         syntax/name
         racket/match
         syntax/stx
         (for-syntax "util.ss"
                     macro-debugger/emit)
         (for-syntax syntax/private/stxparse/runtime-prose
                     syntax/private/stxparse/runtime
                     )
         (for-template racket/base))

(provide (all-defined-out))

#;
(begin-for-syntax 
  (current-failure-handler
                         (lambda (_ f)
                           (printf "Failure is ~a\n" (failure->sexpr (simplify-failure f)))
                           (error 'failed "whatever"))))

(define-syntax-class block
                     #:literals (#%braces)
  [pattern (#%braces statement ...)
           #:with result (let-values ([(body rest) (parse-block-one/2 #'(statement ...) the-block-context)])
                           body)])

(define-syntax-class function
  #:literals (#%parens)
  [pattern (_ name:id (#%parens args ...) body:block . rest)
           #:with result #'(define (name args ...)
                            body.result)])

(define (syntax-object-position mstart end)
  (- (length (syntax->list mstart)) (length (syntax->list end)))
  #;
  (if (stx-null? end)
      (length (syntax->list mstart))
    (let loop ([start mstart]
               [count 0])
      ;; (printf "Checking ~a vs ~a\n" start end)
      (cond
       [(stx-null? start) (raise-syntax-error 'honu-macro "the `rest' syntax returned by a honu macro did not return objects at the same syntactic nesting level as the head of the pattern. this is probably because it returned syntax from some inner nesting level such as (if (x + 1 2) more-stuff) where `rest' was (+ 1 2) instead of `more-stuff'" end mstart)]
       [(equal? (stx-car start) (stx-car end)) count]
       ;; [(equal? start end) count]
       [else (loop (stx-cdr start) (add1 count))]))))

(define-primitive-splicing-syntax-class (honu-transformer context)
  #:attrs (result)
  #:description "honu-expr"
  (lambda (stx fail)
    (printf "Honu expr from transformer `~a' in context ~a transformer ~a\n" (syntax->datum stx) context (get-transformer stx))
    (cond
     [(stx-null? stx) (fail)]
     #;
     [(syntax-parse stx #:literals (honu-syntax #%parens semicolon)
        [(honu-syntax (#%parens expr ...) semicolon . rest)
         (printf "Parsed honu-syntax rest ~a position ~a out ~a\n"
                 #'rest (syntax-object-position stx #'rest)
                 #'(honu-unparsed-begin expr ...))
         (list #'rest (syntax-object-position stx #'rest)
               #'(honu-unparsed-begin expr ...))]
        [else #f]
         #;
        [else #f => (lambda (exprs)
                               (printf "Ignoring honu-syntax 1!\n")
                               (list  0 #''()))]
        )]
     [(get-transformer stx) => (lambda (transformer)
                                 (define introducer (make-syntax-introducer))
                                 (printf "Transforming honu macro ~a\n" (stx-car stx))
                                 (let-values ([(used rest)
                                               (transformer (introducer stx) context)])
                                   (printf "Result is ~a. Object position is ~a\n" used (syntax-object-position stx (introducer rest)))
                                   (list (introducer rest) (syntax-object-position stx (introducer rest))
                                         (introducer (used)))))]
     
     [else (fail)])))

(define-primitive-splicing-syntax-class (honu-expr context)
  #:attributes (result)
  #:description "honu-expr"
  (lambda (stx fail)
    (printf "Honu expr ~a\n" stx)
    (cond
     [(stx-null? stx) (fail)]
     #;
     [(syntax-parse stx #:literals (honu-syntax #%parens semicolon)
        [(honu-syntax (#%parens expr ...) semicolon . rest)
         (list #'rest (syntax-object-position stx #'rest)
               #'(honu-unparsed-begin expr ...))]
        [else #f]
         #;
        [else #f => (lambda (exprs)
                               (printf "Ignoring honu-syntax 1!\n")
                               (list  0 #''()))]
        )]
     #;
     [(syntax-parse stx #:literals (honu-syntax)
        [(honu-syntax expr ...) #'(expr ...)]
        [else #f]) => (lambda (exprs)
                               (printf "Ignoring honu-syntax 2!\n")
                               (list '() 0 exprs))]
     [(get-transformer stx) => (lambda (transformer)
                                 (define introducer (make-syntax-introducer))
                                 (printf "Transforming honu macro ~a\n" (car stx))
                                 (let-values ([(used rest)
                                               (transformer (introducer stx) context)])
                                   (list (introducer rest) (syntax-object-position stx rest)
                                         (introducer (used)))))]
     
     [else (syntax-case stx ()
                        [(f . rest) (list 1 #'f)])])))

         #;
    (define-splicing-syntax-class expr
      [pattern (~seq f ...) #:with result])
    
(define-splicing-syntax-class (call context)
  #:literals (honu-comma #%parens)

  #;
  [pattern (~seq (~var e identifier)
                 (x (~var arg (expression-1 context)) ...)
                 #;
                 (#%parens (~var arg (expression-1 context)) ...))
           #:with call 
           (begin
             (printf "Resulting call. e is ~a -- ~a\n" #'e (syntax->datum #'(e arg.result ...)))
           #'(e.x arg.result ...))]

  [pattern (~seq (~var e honu-identifier) (#%parens rest ...)) #:with call #f
           #:when (begin
                    (printf "Trying a call on ~a and ~a\n" #'e #'(rest ...))
                    #f)]

  [pattern (~seq (~var e (expression-simple context))
                 (~var dx (debug-here (format "call 1 ~a" #'e)))
                 (#%parens 
                  (~seq (~var dz (debug-here (format "call 2")))
                        (~var arg (ternary context))
                        (~var d3 (debug-here (format "call 3 ~a" #'arg)))
                        (~optional honu-comma))
                  ...))
           #:with call
             (begin
               (printf "Resulting call is ~a\n" (syntax->datum #'(e.result arg.result ...)))
               #'(e.result arg.result ...))]

  #;
  [pattern (~seq (~var e honu-identifier
                       #;
                       (honu-expr context))
                 (~var d1 (debug-here (format "call 1 ~a\n" #'e)))
                 (x
                   (~var d2 (debug-here (format "call 2 ~a\n" #'x)))
                  ;;#%parens
                       (~seq (~var arg (ternary context))
                             (~var d3 (debug-here (format "call 3 ~a\n" #'arg)))
                             (~optional honu-comma))
                       ...))
           #:with call 
           (begin
             (printf "Resulting call is ~a\n" (syntax->datum #'(e.x arg.result ...)))
             #'(e.x arg.result ...))])

(define-splicing-syntax-class honu-identifier
  [pattern (~seq x:identifier) #:when (not (or (free-identifier=? #'honu-comma #'x)
                                               (free-identifier=? #'semicolon #'x))
                                           )
                               #:with result #'x])

(define-splicing-syntax-class (expression-simple context)
                              #:literals (#%parens)
  [pattern (~seq (#%parens (~var e (expression-1 context)))) #:with result #'e.result]
  [pattern (~seq (~var e (honu-transformer
                           the-expression-context
                           #;
                           context))) #:with result #'e.result]
  [pattern (~seq x:number) #:with result (begin (printf "got a number ~a\n" #'x) #'x)]
  [pattern (~seq x:str) #:with result #'x]
  [pattern (~seq x:honu-identifier) #:with result #'x.x])

(define-splicing-syntax-class (expression-last context)
                              #:literals (#%parens)

                              #;
  [pattern (~seq a 1 2 3 b 4 5 6)]

  [pattern (~seq x) #:with result #f #:when (begin (printf "Expression last ~a. Raw? ~a\n" #'x (raw-scheme? #'x)) #f)]

  [pattern (~seq raw:raw-scheme-syntax) #:with result #'raw.x
           #;
           (begin (printf "raw syntax ~a\n" #'raw)
                  (if (stx-pair? #'raw)
                    (stx-car #'raw)
                    #'raw))]

  [pattern (~seq (#%parens (~var e (expression-1 context)))) #:with result #'e.result]
  [pattern (~seq (~var call (call context))) #:with result #'call.call]
  [pattern (~seq (~var e (honu-transformer
                           the-expression-context
                           #;
                           context)))
           #:with result #'e.result
           #:with rest #'e.rest]
  [pattern (~seq x:number) #:with result (begin (printf "got a number ~a\n" #'x) #'x)]
  [pattern (~seq x:str) #:with result #'x]
  [pattern (~seq x:honu-identifier) #:with result #'x.x]
  #;
  [pattern (~seq (~var e (honu-expr context))) #:with result #'e.result]
  )

(define-syntax-rule (define-infix-operator name next [operator reducer] ...)
  (begin
   (define-syntax-class operator-class
     #:literals (operator ...)
     (pattern operator #:attr func reducer)
     ...)
   (define-splicing-syntax-class (do-rest context left)
     (pattern (~seq (~var op operator-class)
                    (~var right (next context))

                    (~var new-right (do-rest context ((attribute op.func) left (attribute right.result)))))
              #:with result 
              (begin
                (printf "Left was ~a\n" left)
                #;
                (attribute new-right.result)
                (apply-scheme-syntax (attribute new-right.result))))

     (pattern (~seq) #:with result (begin #;(printf "Left is still ~a\n" left)
                                          left)))

   (define-splicing-syntax-class (name context)
     (pattern (~seq (~var left2 (next context))
                    (~var rest (do-rest context (attribute left2.result))))
              #:with result
              (attribute rest.result)))
   #;
   (define-splicing-syntax-class (name context)
     (pattern (~seq (~var left (next context))
                    (~var op operator-class)
                    (~var right (name context)))
              #:with result
              (cond [(attribute right)
                     ((attribute op.func) #'left.result #'right.result)]
                    [else
                     #'left.result]))

         #;
     (pattern (~seq (~var left (next context))
                    (~optional (~seq (~var op operator-class) (~var right (name context)))))
              #:with result
              (cond [(attribute right)
                     ((attribute op.func) #'left.result #'right.result)]
                    [else
                     #'left.result])))))



;;   (infix-operators ([honu-* ...]
;;                     [honu-- ...])
;;                    ([honu-+ ...]
;;                     [honu-- ...]))
;; Where operators defined higher in the table have higher precedence.
(define-syntax (infix-operators stx)
  (define (create-stuff names operator-stuff)
    (define make (syntax-lambda (expression next-expression (ops ...))
                                #;
                                (printf "Make infix ~a ~a\n" (syntax->datum #'expression) (syntax->datum #'next-expression))
                                #'(define-infix-operator expression next-expression ops ...)))
    (for/list ([name1 (drop-last names)]
               [name2 (cdr names)]
               [operator operator-stuff])
              (make name1 name2 operator)))
  (syntax-case stx ()
    [(_ first last operator-stuff ...)
     (with-syntax ([(name ...) (generate-temporaries #'(operator-stuff ...))])
       (with-syntax ([(result ...)
                      (create-stuff (cons #'first
                                          (append
                                            (drop-last (syntax->list #'(name ...)))
                                            (list #'last)))

                                    (syntax->list #'(operator-stuff ...)))])
         #'(begin
             result ...)))]))

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
                                       [honu-/ (sl (left right) #'(/ left right))])
                                      ([honu-. (sl (left right) #'(get-field right left))])
                                      ))

(define-splicing-syntax-class (ternary context)
                              #:literals (honu-? honu-:)
                              [pattern (~seq (~var condition (expression-1 context))
                                             (~var x1 (debug-here (format "ternary 1 ~a\n" #'condition.result)))
                                             (~optional (~seq honu-? (~var on-true (ternary context))
                                                              honu-: (~var on-false (ternary context))))
                                             (~var x2 (debug-here "ternary 2"))
                                             )
                                       #:with result
                                       (cond [(attribute on-true)
                                              #'(if condition.result on-true.result on-false.result)]
                                             [else #'condition.result])])

(define-splicing-syntax-class (debug-here d)
  [pattern (~seq) #:when (begin
                           (printf "Debug parse I got here ~a\n" d)
                           #t)])

(define (make-assignment left right)
  (match (identifier-binding left)
    ['lexical (with-syntax ([left left] [right right])
                #'(set! left right))]
    [#f (with-syntax ([left left] [right right])
          #'(define left right))]
    [(list source-mod source-id nominal-source-mod nominal-source-id source-phase import-phase nominal-export-phase) (with-syntax ([left left] [right right])
                                                                                                                       #'(set! left right))]
    [else (raise-syntax-error 'assignment "failed to assign" left right)]
    ))

(define-syntax-class (assignment context)
  #:literals (semicolon honu-=)
  [pattern ((~var left honu-identifier)
            honu-=
            (~var right (ternary context))
            semicolon
            . rest)
           ;; FIXME! 1 isn't the right result
           ;; either `set!' or `define' the variable
           #:with result (make-assignment #'left.result #'right.result)])

(define-syntax-class (expression-top context)
                     #:literals (semicolon #%braces)
                     [pattern (~var assignment (assignment context))
                              #:with result #'assignment.result
                              #:with rest #'assignment.rest]
                     [pattern ((#%braces stuff ...) . rest)
                              #:with result
                              (do-parse-block #'(stuff ...))
                              #;
                              (let-values ([(parsed dont-care)
                                                          (parse-block-one/2 #'(stuff ...) context)])
                                              (printf "Parsed ~a. Dont care rest ~a\n" parsed dont-care)
                                              parsed)]
                     [pattern ((~var x0 (debug-here (format "expression top\n")))
                               (~var e (ternary context))
                               (~var x1 (debug-here (format "expression top 1 ~a\n" (syntax->datum #'e))))
                               semicolon ...
                               (~var x2 (debug-here "expression top 2"))
                               . rest)
                              #:with result #'e.result])

(define-splicing-syntax-class raw-scheme-syntax
  [pattern (~seq x) #:when (raw-scheme? #'x)])

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



#;
(define-honu-syntax honu-scheme
  (lambda (stx ctx)
    (syntax-parse stx
      [(_ template rest ...) (values #'template #'(rest ...))])))

(define (fix-output stx)
  #f
  #|
  (printf "Fix output ~a\n" (syntax->datum stx))
  (when (and (stx-pair? stx) (equal? 'honu-syntax (syntax->datum (stx-car stx))))
    (printf "syntax == honu-syntax? ~a\n" (free-identifier=? (stx-car stx) #'honu-syntax)))
  (when (identifier? stx)
    (printf "Current phase ~a stx at ~a honu-scheme ~a same? ~a\n" (syntax-local-phase-level) (identifier-binding stx)
            (identifier-transformer-binding #'honu-scheme)
            (free-identifier=? stx #'honu-scheme)
            ))
  |#
  #;
  (syntax-parse stx #:literals (honu-syntax #%parens syntax)
    #;
    [((honu-syntax (#%parens x ...) y ...) rest ...)
     #;
     (printf "a1\n")
     (with-syntax ([(y* ...) (fix-output #'(y ... rest ...))])
       (syntax/loc stx 
       ((honu-syntax x ...) y* ...)))]
    #;
    [(start ... (honu-scheme code ...) rest ...)
     (with-syntax ([(rest* ...) (fix-output #'(rest ...))])
       (syntax/loc stx
                   (start ... honu-scheme (code ...) rest* ...)))]
    #;
    [(honu-syntax (#%parens x ...) y ...)
     #;
     (printf "a2\n")
     (with-syntax ([(y* ...) (fix-output #'(y ...))])
       (syntax/loc stx
                   (x ... y* ...)))]
    ;; dont touch real syntax
    [(syntax stuff ...)
     #;
     (printf " aa\n")
     stx]
    #;
    [honu-scheme (raise-syntax-error 'asdfioj "got honu-scheme")]
    [(z x ...)
     #;
     (printf "a3\n")
     (datum->syntax stx (cons (fix-output #'z)
                              (fix-output #'(x ...)))
                    stx)

     #;
     (with-syntax ([z* (fix-output #'z)]
                   [(x* ...) (fix-output #'(x ...))])
       (syntax/loc stx
                   (z* x* ...)))]
    #;
    [(honu-syntax . rest)
     (raise-syntax-error 'fix-output "invalid use of honu-syntax")]
    [else
      #;
      (printf " no change\n")
      stx]))

(define-splicing-syntax-class expression
  [pattern (~seq (~var x (expression-1 the-expression-context)))
           #:with result (apply-scheme-syntax #'x.result)])

(define-splicing-syntax-class (whats-here? hm)
  [pattern (~seq x ...)
           #:when (begin (printf "Whats at `~a': `~a'\n" hm (syntax->datum #'(x ...)))
                         #f)])

#;
(define-syntax-class statement
  [pattern ((~var f (whats-here? "statement1"))
            (~var x (expression-top the-top-block-context)))
           #:with result (apply-scheme-syntax (attribute x.result))
           #:with rest #'x.rest])

(define-splicing-syntax-class statement
                              #:literals (semicolon)
  [pattern (~seq (~var x (ternary the-top-block-context))
                 (~var q (debug-here "statement 2"))
                 #;
                 (~var qq (whats-here? "statement 2.1"))
                 (~var z (debug-here "statement 3"))
                 )
           #:with result (apply-scheme-syntax (attribute x.result))
           #:with rest #'x.rest]
                     #;
  [pattern ((~var f (debug-here "statement1"))
                 (~var x (expression-top the-top-block-context)))
           #:with result (apply-scheme-syntax (attribute x.result))
           #:with rest #'x.rest]

                              #;
  [pattern (~seq (~var f (whats-here? "statement1"))
                 (~var f1 (whats-here? "statement2"))
                 (~seq 
                 (~var x (expression-top the-top-block-context))))
           #:with result (apply-scheme-syntax (attribute x.result))
           #:with rest #'x.rest])

#;
(define-splicing-syntax-class statement
  [pattern (~seq
             (~optional (~var zz (whats-here? "statement")))
             (~var d1 (debug-here (format "statement 1\n")))
             (~var x (expression-top the-top-block-context))
             (~var d2 (debug-here (format "statement 2\n")))
             )
           #:with result (apply-scheme-syntax #'x.result)
           #:with rest #'x.rest])

(define-splicing-syntax-class expression-comma
  #:literals (honu-comma)
  #;
  [pattern ;; ((~seq x) ...)
           (x ...)
           #:with (expr ...) (filter (lambda (n)
                                       (not (free-identifier=? #'honu-comma n)))
                                     (syntax->list #'(x ...)))]
  #;
  [pattern ((~seq (~var expr honu-identifier) (~optional honu-comma)) ...)]
  
  #;
  [pattern (~seq (~var expr honu-identifier) (~optional honu-comma))]

  [pattern (~seq (~var expr (expression-1 the-expression-context)) (~optional honu-comma)) #:with result (apply-scheme-syntax #'expr.result)]

  #;
  [pattern ((~seq (~var expr (expression-1 the-expression-context)) (~optional honu-comma)) ...)])

(define (parse-an-expr stx)
  (printf "Parse an expr ~a\n" (syntax->datum stx))
  (syntax-parse (with-syntax ([(s ...) stx])
                  #'(s ...))
    #;
    [(raw:raw-scheme-syntax . rest) #'raw]
    [((~var expr (expression-1 the-expression-context)) . rest) #'expr.result]
    [else (raise-syntax-error 'parse-an-expr "cant parse" stx)]
    ))

(define-splicing-syntax-class honu-body:class
                     #:literals (#%braces)
  [pattern (~seq (#%braces code ...))])

(define (parse-block-one/2 stx context)
  (define (parse-one stx context)
    #;
    (let-values ([(a b) (debug-parse #'(SQL_create_insert) ((~seq x:expression)))])
      (printf "debug parse for ~a is ~a and ~a\n" 'SQL_create_insert a b))
    #;
    (let-values ([(a b) (debug-parse stx ((~seq (~var x (expression-top context)))))])
      (printf "debug parse for ~a is ~a and ~a\n" (syntax->datum stx) a b))
    
    ;; (printf "~a\n" (syntax-class-parse function stx))
    (syntax-parse stx
      #;
      [(raw:raw-scheme-syntax . rest) (values #'raw #'rest)]
      #;
      [function:function (values #'function.result #'function.rest)]
      [(~var expr (expression-top context)) (values #'expr.result #'expr.rest)]
      #;
      [(x:number . rest) (values #'x #'rest)]
      ))
  (printf "Parsing ~a\n" (syntax->datum stx))
  (cond
    [(stx-null? stx) (values stx '())]
    #;
    [(syntax-parse stx #:literals (honu-syntax #%parens semicolon)
       [(honu-syntax (#%parens expr ...) semicolon . rest)
        (list #'(expr ...)
                #'rest)
     #;
         (printf "Parsed honu-syntax rest ~a position ~a out ~a\n"
                 #'rest (syntax-object-position stx #'rest)
                 #'(honu-unparsed-begin expr ...))
         #;
         (list #'rest (syntax-object-position stx #'rest)
               #'(honu-unparsed-begin expr ...))]
        [else #f]
         #;
        [else #f => (lambda (exprs)
                               (printf "Ignoring honu-syntax 1!\n")
                               (list  0 #''()))]
        ) => (lambda (all)
               (let ([to-parse (car all)]
                     [rest (cadr all)])
                 (let-values ([(out rest2)
                               (with-syntax ([(more ...) rest]
                                             [(stuff ...) to-parse])
                                 (parse-block-one/2 #'(stuff ... more ...) context))])
                   (values out rest2))))
        ]
    #;
    [(get-transformer stx) => (lambda (transformer)
                                (define introducer (make-syntax-introducer))
                                (define introduce introducer)
                                (define unintroduce introducer)
                                #;
                                (define introduce (compose introducer syntax-local-introduce))
                                #;
                                (define unintroduce (compose syntax-local-introduce introducer))
                                (printf "Parse one: execute transformer ~a ~a\n" (stx-car stx) transformer)
                                #;
                                (printf "output of transformer is ~a\n" (let-values ([(a b) (transformer stx context)]) (list a b)))
                                (let-values ([(output rest)
                                              (transformer (introduce stx) context)])
                                  (values (unintroduce (output)) (unintroduce rest)))
                                #;
                                (call-values (transformer stx context)
                                             (lambda (reparse rest)
                                               ;; (define fixed (fix-output reparse))
                                               (define fixed reparse)
                                               (printf "Transformer gave us ~a\n" (syntax->datum reparse))
                                               #;
                                               (values reparse rest)
                                               #;
                                               (values (fix-output reparse) rest)
                                               #;
                                               (printf "Macroized ~a and ~a\n" reparse rest)
                                               (printf "Fixed syntax ~a\n" (syntax->datum fixed))
                                               (syntax-parse fixed #:literals (honu-unparsed-expr)
                                                 [(honu-unparsed-expr stuff ...)
                                                  (let-values ([(out rest2)
                                                                (with-syntax ([(more ...) rest])
                                                                  (parse-block-one/2 #'(stuff ... more ...) context))])
                                                    (values out rest2))]
                                                 [else (values fixed rest)]))
                                             ))]
    [else (parse-one stx context)]
    #;
    [else (let-values ([(a b) (parse-one stx context)])
            (values (apply-scheme-syntax a) b))]
            ))

(define operator? 
  (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
    (lambda (stx)
      (and (identifier? stx)
           (let ([str (symbol->string (syntax-e stx))])
             (and (positive? (string-length str))
                  (memq (string-ref str 0) sym-chars)))))))

;; returns a transformer or #f
(define (get-transformer stx)
  ;; if its an identifier and bound to a transformer return it
  (define (bound-transformer stx)
    (and (stx-pair? stx)
         (identifier? (stx-car stx))
         (let ([v (begin
                    (printf "Transformer is ~a. Local value is ~a\n" (stx-car stx) (syntax-local-value (stx-car stx) (lambda () #f)))
                    (syntax-local-value (stx-car stx) (lambda () #f)))])
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
  #;
  (printf "~a bound transformer? ~a at phase level ~a identifiers: ~a\n" stx (bound-transformer stx) (syntax-local-phase-level)
          (if (and (stx-pair? stx)
                   (identifier? (stx-car stx)))
            (let ([id (stx-car stx)])
              (for/list ([phase (in-range -2 2)])
                        (format "~a : ~a." phase (identifier-binding id phase))))
            'not-an-id))
  (bound-transformer stx)
  #;
  (or (bound-transformer stx)
      (special-transformer stx)))

#;
(define-honu-syntax honu-syntax
  (lambda (stx ctx)
  (syntax-case stx ()
    [(_ expr ...)
     (begin
       (printf "Honu syntax on ~a\n" #'(expr ...))
       (raise-syntax-error 'honu-syntax "should have been handled already")
       #;
       (parse-block-one/2 #'(expr ...) the-expression-context))])))

(define (do-parse-block block)
  (define parsed
    (let loop ([out '()]
               [rest block])
       (if (stx-null? rest)
         out
         (let-values ([(out* rest*) (parse-block-one/2 rest the-top-block-context)])
           (loop (cons out* out)
                 rest*)))))
  (with-syntax ([(out ...) (reverse parsed)])
    #'(begin out ...)))
