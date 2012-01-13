#lang racket/base

(require "macro2.rkt"
         "operator.rkt"
         "struct.rkt"
         "honu-typed-scheme.rkt"
         racket/match
         racket/class
         racket/require
         (only-in "literals.rkt"
                  honu-then
                  honu-in
                  honu-prefix
                  semicolon
                  honu-comma
                  define-literal
                  %racket)
         (for-syntax syntax/parse
                     syntax/parse/experimental/reflect
                     syntax/parse/experimental/splicing
                     racket/syntax
                     racket/pretty
                     "compile.rkt"
                     "util.rkt"
                     "debug.rkt"
                     "literals.rkt"
                     "parse2.rkt"
                     racket/base)
         (for-meta 2 racket/base
                     syntax/parse
                     racket/pretty
                     macro-debugger/emit
                     "compile.rkt"
                     "debug.rkt"
                     "parse2.rkt"))

(provide (all-from-out "struct.rkt"))

(define-syntax (parse-body stx)
  (syntax-parse stx
    [(_ stuff ...)
     (honu->racket (parse-all #'(stuff ...)))]))

(provide honu-function)
(define-honu-syntax honu-function
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens arg:identifier ...)
          (#%braces code ...)
          . rest)
       (values
         #'(%racket (lambda (arg ...)
                      (parse-body code ...)))
         #'rest
         #f)])))



(provide honu-if)
(define-honu-syntax honu-if
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (else honu-then)
      [(_ condition:honu-expression honu-then true:honu-expression else false:honu-expression . rest)
       (values
         #'(%racket (if condition.result true.result false.result))
         #'rest
         #f)])))

(provide honu-val)
(define-honu-syntax honu-val
  (lambda (code context)
    (syntax-parse code
      [(_ rest ...)
       (define-values (parsed unparsed)
                      (parse #'(rest ...)))
       (values parsed unparsed #t)])))

(provide honu-quote)
(define-honu-syntax honu-quote
  (lambda (code context)
    (syntax-parse code
      [(_ expression rest ...)
       (values #'(%racket (quote expression)) #'(rest ...) #f)])))

(provide honu-quasiquote)
(define-honu-syntax honu-quasiquote
  (lambda (code context)
    (syntax-parse code
      [(_ expression rest ...)
       (values #'(%racket (quasiquote expression))
               #'(rest ...)
               #f)])))

(define-syntax-rule (define-binary-operator name precedence associativity operator)
                    (begin
                      (provide name)
                      (define-honu-operator/syntax name precedence associativity
                                                   ;; binary
                                                   (lambda (left right)
                                                     (with-syntax ([left left]
                                                                   [right right])
                                                       #'(%racket (operator left right))))
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument argument])
                                                       #'(%racket (operator argument)))))))

(define-syntax-rule (define-unary-operator name precedence associativity operator)
                    (begin
                      (provide name)
                      (define-honu-operator/syntax name precedence associativity
                                                   #f
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument argument])
                                                       #'(%racket (operator argument)))))))
(begin-for-syntax

(define-syntax (parse-expression stx)
  (syntax-parse stx
    [(_ (syntax-ignore (stuff ...)))
     (debug "Parse expression ~a\n" (pretty-format (syntax->datum #'(stuff ...))))
     (define-values (parsed unparsed)
                    (parse #'(stuff ...)))
     (with-syntax ([parsed* (honu->racket parsed)]
                   [unparsed unparsed])
       (emit-local-step parsed #'parsed* #:id #'honu-parse-expression)
       (debug "Parsed ~a. Unparsed ~a\n" #'parsed #'unparsed)
       ;; we need to smuggle our parsed syntaxes through the macro transformer
       #'(#%datum parsed* unparsed))]))

(define-primitive-splicing-syntax-class (honu-expression/phase+1)
  #:attributes (result)
  #:description "expression at phase + 1"
  (lambda (stx fail)
    (debug "honu expression phase + 1: ~a\n" (pretty-format (syntax->datum stx)))
    (define transformed
      (with-syntax ([stx stx])
        (local-transformer-expand #'(parse-expression #'stx)
                                  'expression '())))
    (debug "Transformed ~a\n" (pretty-format (syntax->datum transformed)))
    (define-values (parsed unparsed)
                   (syntax-parse transformed
                     [(ignore-quote (parsed unparsed)) (values #'parsed
                                                               #'unparsed)]))
    (debug "Parsed ~a unparsed ~a\n" parsed unparsed)
    (list (parsed-things stx unparsed)
          (with-syntax ([parsed parsed])
            #'(%racket parsed)))))

) ;; begin-for-syntax

(provide define-make-honu-operator)
(define-honu-syntax define-make-honu-operator 
  (lambda (code context)
    (syntax-parse code
      [(_ name:id level:number association:honu-expression function:honu-expression/phase+1 . rest)
       (debug "Operator function ~a\n" (syntax->datum #'function.result))
       (define out #'(%racket (define-honu-operator/syntax name level association.result function.result)))
       (values out #'rest #t)])))

(provide honu-dot)
(define-honu-fixture honu-dot
  (lambda (left rest)

    ;; v.x = 5
    (define-syntax-class assign #:literal-sets (cruft)
                                #:literals (honu-assignment)
      [pattern (_ name:identifier honu-assignment argument:honu-expression . more)
               #:with result (with-syntax ([left left])
                               #'(%racket
                                   (let ([left* left])
                                     (cond
                                       [(honu-struct? left*)
                                        (honu-struct-set! left* 'name argument.result)]
                                       [(object? left*) (error 'set "implement set for objects")]))))
               #:with rest #'more])

    ;; v.x
    (define-syntax-class plain #:literal-sets (cruft)
                               #:literals (honu-assignment)
      [pattern (_ name:identifier . more)
       #:with result (with-syntax ([left left])
                       #'(%racket
                           (let ([left* left])
                             (cond
                               [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                                       (use left* 'name))]
                               [(object? left*) (get-field name left*)]
                               ;; possibly handle other types of data
                               [else (error 'dot "don't know how to deal with ~a (~a)" 'left left*)]))))
       #:with rest #'more])

    (syntax-parse rest
      [stuff:assign (values #'stuff.result #'stuff.rest)]
      [stuff:plain (values #'stuff.result #'stuff.rest)])))
#;
(define-honu-operator/syntax honu-dot 10000 'left
  (lambda (left right)
    (debug "dot left ~a right ~a\n" left right)
    (with-syntax ([left left]
                  [right right])
      #'(%racket
          (let ([left* left])
            (cond
              [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                      (use left* 'right))]
              [(object? left*) (get-field right left*)]
              ;; possibly handle other types of data
              [else (error 'dot "don't know how to deal with ~a (~a)" 'left left*)]))))))

(provide honu-flow)
(define-honu-operator/syntax honu-flow 0.001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(%racket (right left)))))

(provide honu-assignment)
(define-honu-operator/syntax honu-assignment 0.0001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(%racket (set! left right)))))

(define-binary-operator honu-+ 1 'left +)
(define-binary-operator honu-- 1 'left -)
(define-binary-operator honu-* 2 'left *)
(define-binary-operator honu-/ 2 'left /)
(define-binary-operator honu-^ 2 'right expt)
(define-binary-operator honu-< 0.9 'left <)
(define-binary-operator honu-<= 0.9 'left <=)
(define-binary-operator honu-> 0.9 'left >)
(define-binary-operator honu->= 0.9 'left >=)
;; (define-binary-operator honu-= 0.9 'left =)
(define-binary-operator honu-and 0.5 'left and)
(define-binary-operator honu-or 0.5 'left or)
(define-binary-operator honu-cons 0.1 'right cons)
(define-binary-operator honu-map 0.09 'left map)
(define-binary-operator honu-string=? 1 'left string=?)
(define-binary-operator honu-modulo 2 'left modulo)

(define-binary-operator honu-to 0.001 'left
                        (lambda (left right)
                          (for/list ([i (in-range left right)]) i)))

(define-unary-operator honu-not 0.7 'left not)

(define-binary-operator honu-equal 1 'left equal?)

(begin-for-syntax
  (define (fix-module-name name)
    (format-id name "~a" (regexp-replace* #rx"_" (symbol->string (syntax->datum name)) "-")))
  (define-splicing-syntax-class require-form
                                #:literals (honu-prefix)
                                #:literal-sets (cruft)
    [pattern (~seq honu-prefix prefix module)
             #:with result (with-syntax ([module (fix-module-name #'module)])
                             #'(prefix-in prefix module))]
    [pattern x:str #:with result #'x]
    [pattern x:id
             #:with result (with-syntax ([name (fix-module-name #'x)]) #'name)
             #:when (not ((literal-set->predicate cruft) #'x))]))

(define-for-syntax (racket-names->honu name)
  (regexp-replace* #rx"-" "_"))

(provide honu-require)
(define-honu-syntax honu-require
  (lambda (code context)
    (syntax-parse code
      [(_ form:require-form ... . rest)
       (values
         #'(%racket (require (filtered-in (lambda (name)
                                   (regexp-replace* #rx"-"
                                                    (regexp-replace* #rx"->" name "_to_")
                                                    "_"))
                                 (combine-in form.result ...))))

         #'rest
         #f)])))

(provide honu-provide)
(define-honu-syntax honu-provide
  (lambda (code context)
    (syntax-parse code
      [(_ name:id ...)
       (values #'(%racket (provide name ...))
               #'()
               #f)])))

(provide honu-with-input-from-file)
(define-honu-syntax honu-with-input-from-file
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens name:id) something:honu-expression . rest)
       (define with #'(%racket (with-input-from-file name (lambda () something.result))))
       (values
         with
         #'rest
         #f)])))

(provide honu-while)
(define-honu-syntax honu-while
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ condition:honu-expression body:honu-body . rest)
       (values
         #'(%racket (let loop ()
                      body.result
                      (when condition.result (loop))))
         #'rest
         #t)])))

(provide honu-with honu-match)
(define-literal honu-with)
(define-honu-syntax honu-match
  (lambda (code context)
    (define-splicing-syntax-class match-clause
                                  #:literal-sets (cruft)
                                  #:literals (else)
      [pattern (~seq else body:honu-body)
               #:with final #'else
               #:with code #'body.result]
      [pattern (~seq (#%parens pattern ...) body:honu-body)
               #:with final #'(pattern ...)
               #:with code #'body.result])

    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-with)
      [(_ thing:honu-expression honu-with clause:match-clause ... . rest)
       (values
         #'(%racket (match thing.result
                      [clause.final clause.code]
                      ...))
         #'rest
         #t)])))

(provide honu-->)
(define-honu-fixture honu-->
  (lambda (left rest)
    (syntax-parse rest #:literal-sets (cruft)
      [(_ name:identifier (#%parens argument:honu-expression/comma) . more)
       (with-syntax ([left left])
         (values #'(send/apply left name (list argument.result ...))
                 #'more))])))

(begin-for-syntax
  (define-splicing-syntax-class (id-must-be what)
    [pattern (~reflect x (what))])
  (define-syntax-class (id-except ignore1 ignore2)
    [pattern (~and x:id (~not (~or (~reflect x1 (ignore1))
                                   (~reflect x2 (ignore2)))))])

  (provide separate-ids)
  (define-splicing-syntax-class (separate-ids separator end)
    [pattern (~seq (~var first (id-except separator end))
                   (~seq (~var between (id-must-be separator))
                         (~var next (id-except separator end))) ...)
             #:with (id ...) #'(first.x next.x ...)]
    [pattern (~seq) #:with (id ...) '()]))

(begin-for-syntax
  (provide honu-declaration)

  (define-literal-set declaration-literals (honu-comma honu-equal))
  (define-splicing-syntax-class var-id
    [pattern (~var x (id-except (literal-syntax-class honu-comma)
                                (literal-syntax-class honu-equal)))])

  ;; parses a declaration
  ;; var x = 9
  ;; var a, b, c = values(1 + 2, 5, 9)
  (define-splicing-syntax-class honu-declaration
                              #:literal-sets (cruft)
                              #:literals (honu-equal honu-var)
     [pattern (~seq honu-var (~var variables (separate-ids (literal-syntax-class honu-comma)
                                                           (literal-syntax-class honu-equal)))
                    honu-equal one:honu-expression)
              #:with (name ...) #'(variables.id ...)
              #:with expression #'one.result]))

(provide honu-var)
(define-honu-syntax honu-var
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-equal)
      [(var:honu-declaration . rest)
       (define result #'(%racket (define-values (var.name ...) var.expression)))
       (values result #'rest #t)])))

(provide (rename-out [honu-with-syntax withSyntax]))
(define-honu-syntax honu-with-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-->)
      [(_ (~seq name:id honu--> data:honu-expression (~optional honu-comma)) ...
          (#%braces code ...) . rest)
       (define out #'(%racket (with-syntax ([name data.result] ...)
                                (parse-body code ...))))
       (values out #'rest #t)])))

(provide true false)
(define true #t)
(define false #f)

(provide honu-for)
(define-honu-syntax honu-for
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-equal honu-in)
                       #;
      [(_ iterator:id honu-equal start:honu-expression honu-to end:honu-expression
          honu-do body:honu-expression . rest)
       (values
           #'(%racket (for/list ([iterator (in-range start.result
                                                     end.result)])
                        body.result))
         #'rest
         #t)]
      [(_ iterator:id honu-in stuff:honu-expression
          honu-do body:honu-expression . rest)
       (values #'(%racket (for/list ([iterator stuff.result])
                            body.result))
               #'rest
               #t)])))
