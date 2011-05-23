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
         "debug.rkt"
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
         (for-syntax syntax/parse/private/runtime-report
                     syntax/parse/private/runtime
                     )
         (for-template racket/base))

(provide (all-defined-out))

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
  (- (length (syntax->list mstart)) (length (syntax->list end))))

(define-primitive-splicing-syntax-class (infix-macro-class left-expression context)
  #:attributes (result)
  #:description "infix-macro"
  (lambda (stx fail)
    (cond
      [(stx-null? stx) (fail)]
      [(get-infix-transformer stx) => (lambda (transformer)
                                  (define full-stx (datum->syntax left-expression (cons left-expression stx)))
                                  (define introducer (make-syntax-introducer))
                                  (debug "Transforming honu infix macro ~a\n" (stx-car stx))
                                  (let-values ([(used rest)
                                                (transformer (introducer full-stx) context)])
                                    (let ([rest (introducer rest)]
                                          [position 
                                            (sub1 (syntax-object-position full-stx (introducer rest)))]
                                          [parsed (introducer (used))])
                                    (debug "Result is ~a. Object position is ~a out of expression ~a\n" parsed position (syntax->datum full-stx))
                                    (list position parsed))))]
      [else (fail)])))

(define-primitive-splicing-syntax-class (honu-transformer context)
  #:attributes (result)
  #:description "honu-expr"
  (lambda (stx fail)
    (debug "Honu expr from transformer `~a' in context ~a transformer ~a\n" (syntax->datum stx) context (get-transformer stx))
    (cond
     [(stx-null? stx) (fail)]
     [(get-transformer stx) => (lambda (transformer)
                                 (define introducer (make-syntax-introducer))
                                 (debug "Transforming honu macro ~a\n" (stx-car stx))
                                 (let-values ([(used rest)
                                               (transformer (introducer stx) context)])
                                   (debug "Result is ~a. Object position is ~a out of expression ~a\n\n" used (syntax-object-position stx (introducer rest)) (syntax->datum stx))
                                   (debug "Used is ~a\n" (syntax->datum (introducer (used))))
                                   (list (syntax-object-position stx (introducer rest))
                                         (list #f))))]
     
     [else (fail)])))

(define-primitive-splicing-syntax-class (honu-expr context)
  #:attributes (result)
  #:description "honu-expr"
  (lambda (stx fail)
    (debug "Honu expr ~a\n" stx)
    (cond
     [(stx-null? stx) (fail)]
     [(get-transformer stx) => (lambda (transformer)
                                 (define introducer (make-syntax-introducer))
                                 (debug "Transforming honu macro ~a\n" (car stx))
                                 (let-values ([(used rest)
                                               (transformer (introducer stx) context)])
                                   (list (syntax-object-position stx rest)
                                         (introducer (used)))))]
     
     [else (syntax-case stx ()
                        [(f . rest) (list 1 #'f)])])))
    
(define-splicing-syntax-class (call context)
  #:literals (honu-comma #%parens)

  [pattern (~seq (~var e honu-identifier) (#%parens rest ...)) #:with call #f
           #:when (begin
                    (debug "Trying a call on ~a and ~a\n" #'e #'(rest ...))
                    #f)]

  [pattern (~seq (~var e (expression-simple context))
                 (~var dx (debug-here (format "call 1 ~a" (syntax->datum #'e))))
                 (#%parens 
                  (~seq (~var dz (debug-here (format "call 2")))
                        (~var arg (ternary context))
                        (~var d3 (debug-here (format "call 3 ~a" #'arg)))
                        (~optional honu-comma))
                  ...))
           #:with call
             (begin
               (debug "Resulting call is ~a\n" (syntax->datum #'(e.result arg.result ...)))
               #'(e.result arg.result ...))])

(define-splicing-syntax-class honu-identifier
  [pattern (~seq x:identifier) #:when (not (or (free-identifier=? #'honu-comma #'x)
                                               (free-identifier=? #'semicolon #'x)))
                               #:with result #'x])

(define-splicing-syntax-class (expression-simple context)
                              #:literals (#%parens)
  [pattern (~seq (#%parens (~var e (expression-1 context)))) #:with result #'e.result]
  [pattern (~seq (~var e (honu-transformer
                           the-expression-context))) #:with result #'e.result]
  [pattern (~seq x:number) #:with result (begin (debug "got a number ~a\n" #'x) #'x)]
  [pattern (~seq x:str) #:with result #'x]
  [pattern (~seq x:honu-identifier) #:with result #'x.x])

(define-splicing-syntax-class (expression-last context)
                              #:literals (#%parens honu-:)

  [pattern (~seq x) #:with result #f #:when (begin (debug "Expression last ~a. Raw? ~a\n" #'x (raw-scheme? #'x)) #f)]

  [pattern (~seq raw:raw-scheme-syntax) #:with result #'raw.x]

  [pattern (~seq (#%braces code:statement))
           #:with result #'(begin code.result)]

  [pattern (~seq (#%parens (~var e (expression-1 context)))) #:with result #'e.result]
  [pattern (~seq (~var call (call context))) #:with result #'call.call]
  [pattern (~seq (~var e (honu-transformer
                           the-expression-context)))
           #:with result #'e.result
           #:with rest #'e.rest]
  [pattern (~seq x:number) #:with result (begin (debug "got a number ~a\n" #'x) #'x)]
  [pattern (~seq honu-: id:honu-identifier) #:with result #''id.result]
  [pattern (~seq x:str) #:with result #'x]
  [pattern (~seq x:honu-identifier) #:with result #'x.x])

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
                (debug "Left was ~a\n" left)
                (apply-scheme-syntax (attribute new-right.result))))

     (pattern (~seq) #:with result left))

   (define-splicing-syntax-class (name context)
     (pattern (~seq (~var left2 (next context))
                    (~var rest (do-rest context (attribute left2.result))))
              #:with result
              (attribute rest.result)))))



;;   (infix-operators ([honu-* ...]
;;                     [honu-- ...])
;;                    ([honu-+ ...]
;;                     [honu-- ...]))
;; Where operators defined higher in the table have higher precedence.
(define-syntax (infix-operators stx)
  (define (create-stuff names operator-stuff)
    (define make (syntax-lambda (expression next-expression (ops ...))
                                #;
                                (debug "Make infix ~a ~a\n" (syntax->datum #'expression) (syntax->datum #'next-expression))
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
                                      ([honu-and (sl (left right) #'(and left right))])
                                      (
                                       #;
                                       [honu-= (sl (left right) #'(= left right))]
                                       [honu-== (sl (left right) #'(equal? left right))]
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
                                       [honu-!= (sl (left right) #'(not (equal? left right)))]
                                       [honu-<= (sl (left right) #'(<= left right))]
                                       [honu->= (sl (left right) #'(>= left right))])
                                      ([honu-+ (sl (left right) #'(+ left right))]
                                       [honu-- (sl (left right) #'(- left right))])
                                      ([honu-* (sl (left right) #'(* left right))]
                                       [honu-% (sl (left right) #'(modulo left right))]
                                       [honu-/ (sl (left right) #'(/ left right))])
                                      ([honu-. (sl (left right) #'(get-field right left))])
                                      ))

(define-splicing-syntax-class (infix-macro context)
  [pattern (~seq (~var e (expression-1 context))
                 (~var infix (infix-macro-class #'e.result context)))
           #:with result #'infix.result]
  [pattern (~seq (~var e (expression-1 context))) #:with result #'e.result])

(define-splicing-syntax-class (ternary context)
                              #:literals (honu-? honu-:)
                              [pattern (~seq (~var condition
                                                   (infix-macro context))
                                             (~var x1 (debug-here (format "ternary 1 ~a\n" (syntax->datum #'condition.result))))
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
                           (debug "Debug parse I got here ~a\n" d)
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
                              (do-parse-block #'(stuff ...))]
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

(define-values (prop:honu-infix-transformer honu-infix-transformer? honu-infix-transformer-ref)
               (make-struct-type-property 'honu-infix-transformer))

(define-values (struct:honu-infix-trans make-honu-infix-trans honu-infix-trans? honu-infix-trans-ref honu-infix-trans-set!)
  (make-struct-type 'honu-infix-trans #f 1 0 #f
                    (list (list prop:honu-infix-transformer #t))
                    (current-inspector) 0))

(define (make-honu-infix-transformer proc)
  (unless (and (procedure? proc)
               (procedure-arity-includes? proc 2))
    (raise-type-error
      'define-honu-syntax
      "procedure (arity 2)"
      proc))
  (make-honu-infix-trans proc))

(define-splicing-syntax-class expression
  [pattern (~seq (~var x (expression-1 the-expression-context)))
           #:with result (apply-scheme-syntax #'x.result)])

(define-splicing-syntax-class statement
                              #:literals (semicolon)
  [pattern (~seq (~var x (ternary the-top-block-context)))
           #:with result (apply-scheme-syntax (attribute x.result))
           #:with rest #'x.rest])

(define-splicing-syntax-class expression-comma
  #:literals (honu-comma)
  [pattern (~seq (~var expr (expression-1 the-expression-context))
                 (~optional honu-comma))
           #:with result (apply-scheme-syntax #'expr.result)])

(define (parse-an-expr stx)
  (debug "Parse an expr ~a\n" (syntax->datum stx))
  (syntax-parse (with-syntax ([(s ...) stx])
                  #'(s ...))
    [((~var expr (expression-1 the-expression-context)) . rest) #'expr.result]
    [else (raise-syntax-error 'parse-an-expr "can't parse" stx)]))

(define-splicing-syntax-class honu-body:class
                     #:literals (#%braces)
  [pattern (~seq (#%braces code ...))])

(define (parse-block-one/2 stx context)
  (define (parse-one stx context)
    (syntax-parse stx
      [(~var expr (expression-top context)) (values #'expr.result #'expr.rest)]))
  (debug "Parsing ~a\n" (syntax->datum stx))
  (cond
    [(stx-null? stx) (values stx '())]
    [else (parse-one stx context)]))

(define operator? 
  (let ([sym-chars (string->list "+-_=?:<>.!%^&*/~|")])
    (lambda (stx)
      (and (identifier? stx)
           (let ([str (symbol->string (syntax-e stx))])
             (and (positive? (string-length str))
                  (memq (string-ref str 0) sym-chars)))))))

(define (get-infix-transformer stx)
  (let ([check (stx-car stx)])
    (and (identifier? check)
         (let ([value (syntax-local-value check (lambda () #f))])
           (and (honu-infix-transformer? value) value)))))

;; returns a transformer or #f
(define (get-transformer stx)
  ;; if its an identifier and bound to a transformer return it
  (define (bound-transformer stx)
    (and (stx-pair? stx)
         (identifier? (stx-car stx))
         (let ([v (begin
                    (debug "Transformer is ~a. Local value is ~a\n" (stx-car stx) (syntax-local-value (stx-car stx) (lambda () #f)))
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
  (bound-transformer stx))

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
