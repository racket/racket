#lang racket/base

(require "syntax.rkt"
         "operator.rkt"
         "struct.rkt"
         "honu-typed-scheme.rkt"
         racket/match
         racket/class
         racket/require
         (only-in "literals.rkt"
                  honu-then
                  honu-in
                  honu-in-lines
                  honu-prefix
                  postfix
                  semicolon
                  honu-comma
                  define-literal)
         (for-syntax syntax/parse
                     syntax/parse/experimental/reflect
                     syntax/parse/experimental/splicing
                     macro-debugger/emit
                     racket/syntax
                     racket/pretty
                     racket/string
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
     (parse-all #'(stuff ...))]))

(provide honu-function)
(define-honu-syntax honu-function
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name:identifier (#%parens (~seq arg:identifier (~optional honu-comma)) ...)
          (#%braces code ...) . rest)
       (values
         (racket-syntax (define (name arg ...) (parse-body code ...)))
         #'rest
         #f)]
      [(_ (#%parens (~seq arg:identifier (~optional honu-comma)) ...)
          (#%braces code ...)
          . rest)
       (values
         (racket-syntax (lambda (arg ...)
                      (parse-body code ...)))
         #'rest
         #f)])))

(provide honu-if)
(define-honu-syntax honu-if
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (else honu-then)
      [(_ (#%parens condition:honu-expression) true:honu-expression
          (~optional else) false:honu-expression . rest)
       (values
         (racket-syntax (if condition.result true.result false.result))
         #'rest
         #f)])))

(provide honu-val)
(define-honu-syntax honu-val
  (lambda (code)
    (syntax-parse code
      [(_ rest ...)
       (define-values (parsed unparsed)
                      (parse #'(rest ...)))
       (values parsed unparsed #t)])))

(provide honu-quote)
(define-honu-syntax honu-quote
  (lambda (code)
    (syntax-parse code
      [(_ expression rest ...)
       (values (racket-syntax (quote expression)) #'(rest ...) #f)])))

(provide honu-quasiquote)
(define-honu-syntax honu-quasiquote
  (lambda (code)
    (syntax-parse code
      [(_ expression rest ...)
       (values (racket-syntax (quasiquote expression))
               #'(rest ...)
               #f)])))

(begin-for-syntax

(define-syntax (parse-expression stx)
  (syntax-parse stx
    [(_ (syntax-ignore (stuff ...)))
     (debug "Parse expression ~a\n" (pretty-format (syntax->datum #'(stuff ...))))
     (define-values (parsed unparsed)
                    (parse #'(stuff ...)))
     (with-syntax ([parsed* parsed]
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
            (racket-syntax parsed)))))

) ;; begin-for-syntax

(provide define-make-honu-operator)
(define-honu-syntax define-make-honu-operator 
  (lambda (code)
    (syntax-parse code
      [(_ name:id level:number association:honu-expression function:honu-expression/phase+1 . rest)
       (debug "Operator function ~a\n" (syntax->datum #'function.result))
       (define out (racket-syntax (define-honu-operator/syntax name level association.result function.result)))
       (values out #'rest #t)])))

(provide define-make-honu-binary-operator)
(define-honu-syntax define-make-honu-binary-operator
  (lambda (code)
    (syntax-parse code
      [(_ name:id level:honu-expression association:honu-expression function:honu-expression . rest)
       (define out (racket-syntax
                     (define-binary-operator name level.result association.result function.result)))
       (values out #'rest #t)])))

(provide define-make-honu-unary-operator)
(define-honu-syntax define-make-honu-unary-operator
  (lambda (code)
    (syntax-parse code #:literals (postfix)
      [(_ name:id level:honu-expression
          (~optional (~and postfix postfix?)
                     #:defaults ([postfix? #f]))
          function:honu-expression . rest)
       (define out
         (with-syntax ([postfix? (if (attribute postfix?) #t #f)])
           (racket-syntax
             (define-unary-operator name level.result postfix? function.result))))
       (values out #'rest #t)])))

;; equals can have a compile time property that allows it to do something like set!
;; v.x could return a syntax object with a property that can be invoked by an equals
;; thing so that it can be rewritten to do the set! thing
;; if the property is not used then it will just be a field lookup
;;   v.x => (syntax-property #'(foo-x v)
;;                           'setter (lambda (e) (set-foo-x! v e)))
;; where `e' is the right hand side of the = expression

;; default dot interpretation
(define-syntax (dot stx)
  (syntax-parse stx
    [(_ object field:identifier)
     (racket-syntax
       (let ([left* object])
         (cond
           [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                   (use left* 'field))]
           [(object? left*) (get-field field left*)]
           ;; possibly handle other types of data
           [else (error 'dot "don't know how to deal with ~a (~a)" 'object left*)])))]))

(define-syntax (dot-assign stx)
  (syntax-parse stx
    [(_ left field:identifier expression)
     (racket-syntax
       (let ([left* left])
         (cond
           [(honu-struct? left*)
            (honu-struct-set! left* 'field expression)]
           [(object? left*) (error 'assign "implement set for objects")]
           [else (error 'assign "don't know how to do set for ~a" left*)])))]))

(provide honu-dot)
(define-honu-operator/syntax honu-dot 10000 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right (syntax-parse right
                           [field:identifier #'field])])
      (syntax-property (racket-syntax (dot left right))
                       'assign
                       (lambda (expression)
                         (with-syntax ([expression expression])
                           (racket-syntax (dot-assign left right expression))))))))
#;
(define-honu-fixture honu-dot
  (lambda (left rest)

    ;; v.x = 5
    (define-syntax-class assign #:literal-sets (cruft)
                                #:literals (honu-equal)
      [pattern (_ name:identifier honu-equal argument:honu-expression . more)
               #:with result (with-syntax ([left left])
                               (racket-syntax 
                                   (let ([left* left])
                                     (cond
                                       [(honu-struct? left*)
                                        (honu-struct-set! left* 'name argument.result)]
                                       [(object? left*) (error 'set "implement set for objects")]))))
               #:with rest #'more])

    ;; v.x
    (define-syntax-class plain #:literal-sets (cruft)
                               #:literals (honu-equal)
      [pattern (_ name:identifier . more)
       #:with result (with-syntax ([left left])
                       (racket-syntax 
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
      (racket-syntax 
          (let ([left* left])
            (cond
              [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                      (use left* 'right))]
              [(object? left*) (get-field right left*)]
              ;; possibly handle other types of data
              [else (error 'dot "don't know how to deal with ~a (~a)" 'left left*)]))))))

(provide honu-for-syntax)
(define-literal honu-for-syntax) 

(begin-for-syntax
  (define (fix-module-name name)
    (format-id name "~a" (regexp-replace* #rx"_" (symbol->string (syntax->datum name)) "-")))
  (define (combine-paths paths name)
    (define all (for/list ([path (if paths
                                   (append paths (list name))
                                   (list name))])
                  (cond
                    [(identifier? path) (symbol->string (syntax->datum path))]
                    [(string? path) path]
                    [else (error 'combine-paths "what is ~a" path)])))
    (format-id name (string-join all "/")))
  (define-splicing-syntax-class not-comma
    [pattern x #:when (not ((literal-set->predicate cruft) #'x))])
  (define-splicing-syntax-class require-form
                                #:literals (honu-prefix honu-for-syntax)
                                #:literal-sets (cruft)
    [pattern (~seq honu-prefix prefix module:require-form)
             #:with result #'(prefix-in prefix module.result)]
    [pattern (~seq honu-for-syntax ~! (#%parens module:require-form))
             #:with result #'(for-syntax module.result)]
    [pattern x:str #:with result #'x]
    [pattern (~seq x:not-comma ...)
             #:with result (with-syntax ([name
                                           ;; use the lexical context of the original syntax
                                           (datum->syntax (car (syntax->list #'(x.x ...)))
                                           (string->symbol
                                             (apply string-append
                                                    (map (compose symbol->string syntax->datum)
                                                         (syntax->list #'(x.x ...)))))
                                           #'(x.x ...))
                                           ])
                             (fix-module-name #'name))]))
                                                                    
(provide honu-require)
(define-honu-syntax honu-require
  (lambda (code)
    (syntax-parse code
      [(_ form1:require-form (~seq honu-comma form:require-form) ... . rest)
       (values
         (racket-syntax (require (filtered-in (lambda (name)
                                                (regexp-replace* #rx"-"
                                                                 (regexp-replace* #rx"->" name "_to_")
                                                                 "_"))
                                              (combine-in form1.result form.result ...))))

         #'rest
         #f)])))

(provide honu-provide)
(define-honu-syntax honu-provide
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name:honu-identifier ... (~optional semicolon) . rest)
       (debug "Provide matched names ~a\n" (syntax->datum #'(name.result ...)))
       (define out (racket-syntax (provide name.result ...)))
       (debug "Provide properties ~a\n" (syntax-property-symbol-keys out))
       (debug "Rest ~a\n" #'rest)
       (values out #'rest #f)])))

(provide honu-with-input-from-file)
(define-honu-syntax honu-with-input-from-file
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ file:honu-expression something:honu-expression . rest)
       (define with (racket-syntax (with-input-from-file file.result
                                                     (lambda () something.result))))
       (values
         with
         #'rest
         #f)])))

(provide honu-while)
(define-honu-syntax honu-while
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ condition:honu-expression body:honu-body . rest)
       (values
         (racket-syntax (let loop ()
                          (when condition.result
                            body.result
                            (loop))))
         #'rest
         #t)])))

(provide honu-with honu-match)
(define-literal honu-with)
(define-honu-syntax honu-match
  (lambda (code)
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
         (racket-syntax (match thing.result
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
         (values (racket-syntax (send/apply left name (list argument.result ...)))
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
                              #:literals (honu-var)
     [pattern (~seq honu-var (~var variables (separate-ids (literal-syntax-class honu-comma)
                                                           (literal-syntax-class honu-equal)))
                    honu-equal one:honu-expression)
              #:with (name ...) #'(variables.id ...)
              #:with expression #'one.result]))

(begin-for-syntax
  (provide honu-declaration/phase+1)

  ;; parses a declaration
  ;; var x = 9
  ;; var a, b, c = values(1 + 2, 5, 9)
  (define-splicing-syntax-class honu-declaration/phase+1
                              #:literal-sets (cruft)
                              #:literals (honu-syntax-var)
     [pattern (~seq honu-syntax-var (~var variables (separate-ids (literal-syntax-class honu-comma)
                                                           (literal-syntax-class honu-equal)))
                    honu-equal one:honu-expression/phase+1)
              #:with (name ...) #'(variables.id ...)
              #:with expression #'one.result]))

;; like define
(provide honu-var)
(define-honu-syntax honu-var
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(var:honu-declaration . rest)
       (define result 
         ;; wrap the expression in a let so that we can insert new `define-syntax'es
         (racket-syntax (define-values (var.name ...) (let () var.expression))))
       (values result #'rest #t)])))

;; like define-syntax
(provide honu-syntax-var)
(define-honu-syntax honu-syntax-var
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(var:honu-declaration/phase+1 . rest)
       (define result 
         ;; wrap the expression in a let so that we can insert new `define-syntax'es
         (racket-syntax (define-syntaxes (var.name ...) (let () var.expression))))
       (values result #'rest #t)])))

(provide (rename-out [honu-with-syntax withSyntax]))
(define-honu-syntax honu-with-syntax
  (lambda (code)
    (define-splicing-syntax-class clause
                                  #:literal-sets (cruft)
                                  #:literals [(ellipses ...) honu-equal]
      [pattern (~seq name:id honu-equal data:honu-expression)
               #:with out #'(name data.result)]
      [pattern (~seq (#%parens name:id ellipses) honu-equal data:honu-expression)
               #:with out #'((name (... ...)) data.result)]
      [pattern (~seq (#%parens (#%parens name:id ellipses) ellipses) honu-equal data:honu-expression)
               #:with out #'(((name (... ...)) (... ...)) data.result)])
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-equal)
      [(_ (~seq all:clause (~optional honu-comma)) ...
          (#%braces code ...) . rest)
       (define out (racket-syntax
                     (with-syntax (all.out ...)
                       (parse-body code ...))))
       (values out #'rest #t)])))

(provide true false)
(define true #t)
(define false #f)

(provide honu-for)
(define-honu-syntax honu-for
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-in)
      [(_ (~seq iterator:id honu-in stuff:honu-expression)
          (~seq honu-comma iterator*:id honu-in stuff*:honu-expression) ...
          honu-do body:honu-expression . rest)
       (values (racket-syntax (for ([iterator stuff.result]
                                    [iterator* stuff*.result] ...)
                                body.result))
               #'rest
               #t)])))

(provide honu-fold)
(define-honu-syntax honu-fold
  (lambda (code)
    (define-splicing-syntax-class sequence-expression
                                  #:literals (honu-in honu-in-lines)
       [pattern (~seq iterator:id honu-in stuff:honu-expression)
                 #:with variable #'iterator
                 #:with expression #'stuff.result]
        [pattern (~seq iterator:id honu-in-lines)
                 #:with variable #'iterator
                 #:with expression #'(in-lines)])
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-equal)
      [(_ (~seq init:id honu-equal init-expression:honu-expression (~optional honu-comma)) ...
          (~seq sequence:sequence-expression (~optional honu-comma)) ...
          honu-do body:honu-expression . rest)
       (values (racket-syntax (for/fold ([init init-expression.result] ...)
                                    ([sequence.variable sequence.expression] ...)
                            body.result))
               #'rest
               #t)])))

(provide honu-primitive-macro)
(define-honu-syntax honu-primitive-macro
  (lambda (code)
    (syntax-parse code
      [(_ name:id transformer:honu-expression/phase+1 . rest)
       (values
         (racket-syntax (define-honu-syntax name transformer.result))
         #'rest
         #t)])))
