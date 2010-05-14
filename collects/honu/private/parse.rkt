#lang scheme

(require "contexts.ss"
         "util.ss"
         (for-template "literals.ss"
                       "language.ss"
                       "syntax.ss")
         syntax/parse
         syntax/parse/experimental/splicing
         "syntax.ss"
         (for-syntax syntax/parse)
         scheme/splicing
         (for-syntax syntax/define)
         syntax/name
         syntax/stx
         (for-syntax "util.ss")
         (for-template scheme/base))

(provide (all-defined-out))

(define-syntax-class block
  [pattern (#%braces statement ...)
           #:with result (let-values ([(body rest) (parse-block-one/2 #'(statement ...) the-block-context)])
                           body)])

(define-syntax-class function
  [pattern (type:id name:id (#%parens args ...) body:block . rest)
           #:with result #'(define (name args ...)
                            body.result)])

(define (syntax-object-position mstart end)
  (if (stx-null? end)
      (length (syntax->list mstart))
    (let loop ([start mstart]
               [count 0])
      ;; (printf "Checking ~a vs ~a\n" start end)
      (cond
       [(stx-null? start) (raise-syntax-error 'honu-macro "the `rest' syntax returned by a honu macro did not return objects at the same syntactic nesting level as the head of the pattern. this is probably because it returned syntax from some inner nesting level such as (if (x + 1 2) more-stuff) where `rest' was (+ 1 2) instead of `more-stuff'" end mstart)]
       [(eq? (stx-car start) (stx-car end)) count]
       ;; [(equal? start end) count]
       [else (loop (stx-cdr start) (add1 count))]))))

(define-primitive-splicing-syntax-class (honu-transformer context)
  #:attrs (result)
  #:description "honu-expr"
  (lambda (stx fail)
    (printf "Honu expr from transformer ~a\n" (syntax->datum stx))
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
                                 (printf "Transforming honu macro ~a\n" (stx-car stx))
                                 (let-values ([(used rest)
                                               (transformer stx context)])
                                   (printf "Result is ~a\n" used)
                                   (list rest (syntax-object-position stx rest)
                                         (used))))]
     
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
                                 (printf "Transforming honu macro ~a\n" (car stx))
                                 (let-values ([(used rest)
                                               (transformer stx context)])
<<<<<<< HEAD
                                   (list (syntax-object-position stx rest)
                                         used)))]
     
     [else (syntax-case stx ()
                        [(f . rest) (list 1 #'f)])])))
=======
                                   (list rest (syntax-object-position stx rest)
                                         (used))))]
     
     [else (syntax-parse stx
                        [(f . rest) (list #'rest 1 #'f)]
                        [x:number (list #''() 0 #'x)]
                        )])))
>>>>>>> allow macros to reparse their input

    #;
    (define-splicing-syntax-class expr
      [pattern (~seq f ...) #:with result])
    
(define-splicing-syntax-class (call context)
  #:literals (honu-comma)
  [pattern (~seq (~var e (honu-expr context)) (#%parens (~seq (~var arg (ternary context))
                                                              (~optional honu-comma)) ...))
           #:with call #'(e.result arg.result ...)])

(define-splicing-syntax-class (expression-last context)
                              #:literals (#%parens)
  [pattern (~seq (#%parens (~var e (expression-1 context)))) #:with result #'e.result]
  [pattern (~seq (~var e (honu-transformer context))) #:with result #'e.result]

  [pattern (~seq (~var call (call context))) #:with result #'call.call]
  [pattern (~seq x:number) #:with result #'x]
  [pattern (~seq x:str) #:with result #'x]
  [pattern (~seq x:id) #:with result #'x]
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

                    (~var new-right (do-rest context ((attribute op.func) left #'right.result))))
              #:with result (attribute new-right.result))
     (pattern (~seq) #:with result left))
   (define-splicing-syntax-class (name context)
     (pattern (~seq (~var left (next context))
                    (~var rest (do-rest context #'left.result)))
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
                                      ([honu-. (sl (left right) #'(field-access right left))])
                                      ))

(define-splicing-syntax-class (ternary context)
                              #:literals (honu-? honu-:)
                              [pattern (~seq (~var condition (expression-1 context))
                                             (~optional (~seq honu-? (~var on-true (ternary context))
                                                              honu-: (~var on-false (ternary context)))))
                                       #:with result
                                       (cond [(attribute on-true)
                                              #'(if condition.result on-true.result on-false.result)]
                                             [else #'condition.result])])

(define-syntax-class (expression-top context)
                     #:literals (semicolon)
                     [pattern ((~var e (ternary context)) semicolon . rest)
                              #:with result #'e.result])


(define-syntax-class raw-scheme-syntax
  [pattern x #:when (syntax-property #'x honu-scheme-syntax)])

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

(define (parse-an-expr stx)
  (printf "Parse an expr ~a\n" (syntax->datum stx))
  (syntax-parse (with-syntax ([s stx])
                  #'(s semicolon))
    [(raw:raw-scheme-syntax . rest) #'raw]
    [((~var expr (expression-1 the-expression-context)) . rest) #'expr.result]
    [else (raise-syntax-error 'parse-an-expr "cant parse" stx)]
    ))

(define (parse-block-one/2 stx context)
  (define (parse-one stx context)
    
    ;; (printf "~a\n" (syntax-class-parse function stx))
    (syntax-parse stx
      [(raw:raw-scheme-syntax . rest) (values #'raw #'rest)]
      [function:function (values #'function.result #'function.rest)]
      [(~var expr (expression-top context)) (values #'expr.result #'expr.rest)]
      #;
      [(x:number . rest) (values #'x #'rest)]
      ))
  (printf "Parsing ~a\n" stx)
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
    [(get-transformer stx) => (lambda (transformer)
                                (printf "Parse one: execute transformer ~a ~a\n" (stx-car stx) transformer)
                                #;
                                (printf "output of transformer is ~a\n" (let-values ([(a b) (transformer stx context)]) (list a b)))
                                (let-values ([(output rest)
                                              (transformer stx context)])
                                  (values (output) rest))
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
    [else (parse-one stx context)]))



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
                    #;
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
  (printf "~a bound transformer? ~a\n" stx (bound-transformer stx))
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
