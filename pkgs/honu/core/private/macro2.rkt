#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/parse
                     syntax/stx
                     racket/set
                     racket/syntax
                     macro-debugger/emit
                     "template.rkt"
                     "literals.rkt"
                     "syntax.rkt"
                     (prefix-in phase1: "parse2.rkt")
                     "debug.rkt"
                     (prefix-in phase1: "compile.rkt")
                     "util.rkt"
                     racket/base)
         (for-meta 2 syntax/parse
                     racket/base
                     macro-debugger/emit
                     racket/syntax
                     racket/set
                     racket/pretty
                     "literals.rkt"
                     "debug.rkt"
                     (prefix-in phase2: "parse2.rkt")
                     (prefix-in phase2: "compile.rkt"))
         (prefix-in phase0: "compile.rkt")
         racket/splicing
         "literals.rkt"
         "syntax.rkt"
         "debug.rkt"

         (for-meta 0 "template.rkt" syntax/stx)

         (for-meta -1 "literals.rkt" "compile.rkt" "parse2.rkt" "parse-helper.rkt")
         #;
         (for-syntax "honu-typed-scheme.rkt")
         syntax/parse)

(module analysis racket/base
  (require syntax/parse
           "literals.rkt"
           "debug.rkt"
           "util.rkt"
           (prefix-in syntax: syntax/parse/private/residual-ct)
           racket/syntax
           racket/set
           racket/match
           (for-syntax syntax/parse
                       racket/base
                       racket/syntax)
           (for-template racket/base
                         syntax/parse))

    (provide (all-defined-out))
    (struct pattern-variable [name original depth class] #:transparent)

    ;; given the name of an object and some fields this macro defines
    ;; name.field for each of the fields
    (define-syntax (define-struct-fields stx)
      (syntax-parse stx
        [(_ name type (field ...))
         (with-syntax ([(field* ...)
                        (for/list ([field (syntax->list #'(field ...))])
                          (format-id field "~a.~a" (syntax-e #'name) (syntax-e field)))])
           #'(match-define (struct type (field* ...)) name))]))

    ;; makes a syntax object with the right number of nested ellipses patterns
    (define (pattern-variable->syntax variable)
      (debug 2 "Convert pattern variable to syntax ~a location ~a\n" variable (pattern-variable-original variable))
      (define location (pattern-variable-original variable))
      (for/fold ([out (pattern-variable-name variable)])
                ([depth (pattern-variable-depth variable)])
                (with-syntax ([out out]
                              [ellipses (syntax/loc location (... ...))])
                  (syntax/loc location (out ellipses)))))

  (define (convert-pattern original-pattern new-names)
    (define-splicing-syntax-class pattern-type
        #:literal-sets (cruft)
        [pattern (~seq name colon class)
                 #:with (result ...)
                 (with-syntax ([new-name (pattern-variable-name (hash-ref new-names (syntax-e #'name)))])
                   #'((~var new-name class #:attr-name-separator "_")))]
        [pattern (x:pattern-type ...) #:with (result ...) #'((x.result ... ...))]
        [pattern x #:with (result ...) #'(x)])
    (syntax-parse original-pattern
      [(thing:pattern-type ...)
       #'(thing.result ... ...)]))

  ;; reverse the syntax so that ellipses appear in front of the s-expr
  ;; then search for ellipses nodes and s-exprs of the form class:id %colon name:id
  ;; the output should match the ellipses depth, so for example
  ;;   ... foo %colon bar
  ;; output would be
  ;;   ((bar ...) (foo_result ...)
  (define (find-pattern-variables original-pattern)
    (define (reverse-syntax input)
      (syntax-parse input
        [(x ...) (datum->syntax input
                                (reverse (for/list ([x (syntax->list #'(x ...))])
                                           (reverse-syntax x)))
                                input input)]
        [x #'x]))
    (define (merge set1 set2)
      (debug 2 "Merge ~a with ~a\n" set1 set2)
      (set-union set1 set2))
    (define (wrap-ellipses stuff)
      (for/set ([variable stuff])
               (pattern-variable (pattern-variable-name variable)
                                 (pattern-variable-original variable)
                                 (add1 (pattern-variable-depth variable))
                                 (pattern-variable-class variable))))
    #;
    (define (wrap-ellipses stuff)
      (for/set ([name+result stuff])
               (syntax-case name+result ()
                 [(name result)
                  #'((name (... ...))
                     (result (... ...)))])))
    (define (find the-pattern)
      (debug 2 "Find in ~a\n" (syntax->datum the-pattern))
      (define-splicing-syntax-class maybe
          #:literal-sets (cruft)
          #:literals ([ellipses ...])
          [pattern (~seq class:id colon name:id)
                   #:attr result
                   (with-syntax ([name.result (format-id #'name "~a_result" #'name)])
                     (set (pattern-variable #'name #'class 0 #'class)))]
          [pattern (~seq ellipses thing:maybe rest ...)
                   #:attr result
                   (merge (wrap-ellipses (attribute thing.result))
                          (find #'(rest ...)))]
          [pattern (x:maybe ...) #:attr result (apply set-union (attribute x.result))]
          [pattern x #:attr result (set)])
      (syntax-parse the-pattern
        #:literals ([ellipses ...])
        [(ellipses thing:maybe rest ...)
         (debug 2 "Ellipses case\n")
         (merge (wrap-ellipses (attribute thing.result))
                (find #'(rest ...)))]
        [(thing:maybe rest ...)
         (debug 2 "Normal list case ~a\n" (attribute thing.result))
         (merge (attribute thing.result) (find #'(rest ...)))]
        [thing (set)]))
    (define variables (find (reverse-syntax original-pattern)))
    (debug 2 "Found variables ~a\n" variables)
    (for/list ([x variables]) x))

  ;; variable is the original pattern variable, like 'foo'
  ;; and new-name is the new generated name, 'temp1'
  ;; we want to bind all the attributes from temp1 to foo, so if temp1 has
  ;;   temp1_a
  ;;   temp1_b ...
  ;;
  ;; we want to bind
  ;;   foo_a temp_a
  ;;   (foo_b ...) (temp_b ...)
  (define (bind-attributes variable new-name)
    (debug 2 "Syntax class of ~a is ~a at ~a\n"
           (pattern-variable-class variable)
           (syntax-local-value (pattern-variable-class variable) (lambda () #f))
           (syntax-local-phase-level))
    (define attributes
      (let ([syntax-class (syntax-local-value (pattern-variable-class variable))])
        (for/list ([attribute (syntax:stxclass-attrs syntax-class)])
          (pattern-variable (syntax:attr-name attribute)
                            (pattern-variable-original variable)
                            (+ (pattern-variable-depth variable)
                               (syntax:attr-depth attribute))
                            #f))))

    (define (mirror-attribute attribute)
      (debug 2 "Mirror attribute ~a\n" attribute)
      (define-struct-fields attribute pattern-variable
                            (name original depth class))
      ;; create a new pattern variable with a syntax object that uses
      ;; the given lexical context and whose name is prefix_suffix
      (define (create lexical prefix suffix)
        (pattern-variable->syntax
          (pattern-variable (format-id lexical "~a_~a" prefix suffix)
                            attribute.original attribute.depth attribute.class)))
      (define-struct-fields variable pattern-variable
                            (name original depth class))
      (debug 2 "Bind attributes ~a ~a\n" variable.name attribute.name)
      (with-syntax ([bind-attribute
                      #;
                      (create name (syntax-e name) name)
                      (pattern-variable->syntax
                        (pattern-variable (format-id variable.name "~a_~a"
                                                     (syntax-e variable.name)
                                                     attribute.name)
                                          attribute.original
                                          attribute.depth
                                          attribute.class))]
                    [new-attribute 
                      #;
                      (create new-name new-name name)
                      (pattern-variable->syntax
                        (pattern-variable
                          (format-id new-name "~a_~a"
                                     new-name
                                     attribute.name)
                          attribute.original attribute.depth #f))])
        (debug 2 "Bind ~a to ~a\n" #'bind-attribute #'new-attribute)
        #'(#:with bind-attribute #'new-attribute)))

    (for/set ([attribute attributes])
             (mirror-attribute attribute)))

  ;; returns a set of #:with clauses for syntax-parse that
  ;; bind all the old variables and their attributes to some new names
  ;; taking care of ellipses depth
  (define (pattern-variables+attributes variables use)
    (for/union ([old variables]
                [new use])
               (define-struct-fields old pattern-variable (name original depth class))
               (with-syntax ([old-syntax (pattern-variable->syntax old)]
                             [new.result (pattern-variable->syntax
                                           (pattern-variable (format-id new "~a_result" new)
                                                             old.original
                                                             old.depth
                                                             old.class))])
                 (set-union (set #'(#:with old-syntax #'new.result))
                            (bind-attributes old new)))))
  )

(require (for-meta 2 (submod "." analysis)))

(begin-for-syntax
  (define-syntax (parse-stuff stx)
    (syntax-parse stx
      [(_ stuff ...)
       (emit-remark "Parse stuff ~a\n" #'(stuff ...))
       (phase2:parse-all #'(stuff ...))
       #;
       (honu->racket (parse-all #'(stuff ...)))])))

(begin-for-syntax
  (define-syntax (create-honu-macro stx)
    (syntax-parse stx
      [(_ name (literal ...) (pattern ...) (action ...))
       (debug 2 "Name is ~a\n" #'name)
       (define pattern-variables (find-pattern-variables #'(pattern ...)))

       ;; only need a 1-to-1 mapping here
       (define mapping (make-hash))
       (for ([variable pattern-variables])
         (debug 2 "Update mapping for ~a\n" (pattern-variable-name variable))
         (hash-set! mapping (syntax-e (pattern-variable-name variable))
                    variable)) 
       (debug 2 "Create pattern\n")
       (with-syntax ([(syntax-parse-pattern ...)
                      (convert-pattern #'(pattern ...) mapping)]
                     [((pattern-variable.name pattern-variable.result) ...)
                      (for/list ([name pattern-variables])
                        (debug 2 "Create new pattern variable from ~a\n" name)
                        (with-syntax ([name (pattern-variable->syntax name)]
                                      [name.result (pattern-variable->syntax
                                                     (pattern-variable (format-id (pattern-variable-name name)
                                                                                  "~a_result"
                                                                                  (pattern-variable-name name))
                                                                       (pattern-variable-original name)
                                                                       (pattern-variable-depth name)
                                                                       (pattern-variable-class name)
                                                                       ))])
                          #'(name name.result)))])

         (define output 
           (syntax (quote-syntax 
                     (lambda (stx)
                       (emit-remark "Invoke macro" (symbol->string 'name) "on" stx)
                       (define-literal-set local-literals (literal ...))
                       (syntax-parse stx
                         #:literal-sets ([cruft #:at name]
                                         [local-literals #:at name])
                         [(_ syntax-parse-pattern ... . more)
                          (values
                            ;; if the pattern is x:expression then x_result will
                            ;; hold the parsed version of x, so we rebind x to
                            ;; x_result so you can use just x in the template
                            ;; instead of x_result. x_result is still there, too
                            (with-syntax ([pattern-variable.name #'pattern-variable.result]
                                          ...)
                              (debug "~a = ~a\n" 'pattern-variable.name (syntax->datum #'pattern-variable.name)) ...
                              (parse-stuff action ...))
                            #'more #t)]
                         [else (raise-syntax-error #f "Could not match macro" stx)])))))
             (debug "Create macro: ~a\n" (pretty-format (syntax->datum output)))
             output)]))
  )

(provide honu-macro)
(define-honu-syntax honu-macro
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name
          (#%parens literal ...)
          (#%braces pattern ...)
          (#%braces action ...) . rest)
       (values (phase1:racket-syntax
                 ;; trampoline to phase 1
                 (splicing-let-syntax ([make (lambda (stx)
                                               (syntax-parse stx
                                                 [(_ new-name)
                                                  (define output 
                                                    (create-honu-macro name
                                                                       (literal ...)
                                                                       (pattern ...)
                                                                       (action ...)))
                                                  (debug "Output from create macro ~a\n" output)
                                                  (with-syntax ([output output])
                                                    (debug "Output is ~a\n" #'output)
                                                    #'(define-honu-syntax new-name output))]))])
                   (make name)))
               #'rest
               #t)])))

#|
;; FIXME: we shouldn't need this definition here
(define-syntax (as-honu-syntax stx)
  (syntax-parse stx
    [(_ form)
     (define compressed (compress-dollars #'form))
     (with-syntax ([stuff* (datum->syntax #'form (syntax->list compressed)
                                          #'form #'form)])
       (syntax #'stuff*))]))

(begin-for-syntax
  (define-syntax (as-honu-syntax stx)
    (syntax-parse stx
      [(_ form)
       (define compressed (phase1:compress-dollars #'form))
       (with-syntax ([stuff* (datum->syntax #'form (syntax->list compressed)
                                            #'form #'form)])
         (syntax #'stuff*))])))
|#


;; combine syntax objects
;; #'(a b) + #'(c d) = #'(a b c d)
(provide mergeSyntax)
(define (mergeSyntax syntax1 syntax2)
  (debug "Merge syntax ~a with ~a\n" (syntax->datum syntax1) (syntax->datum syntax2))
  (with-syntax ([(syntax1* ...) syntax1]
                [(syntax2* ...) syntax2])
          #'(syntax1* ... syntax2* ...))
  #;
  (syntax-parse syntax1
    [(r1 (unexpand something1))
     (syntax-parse syntax2
       [(r2 (unexpand2 something2))
        (with-syntax ([(syntax1* ...) #'something1]
                      [(syntax2* ...) #'something2])
          #'(%racket (unexpand (syntax1* ... syntax2* ...))))])]))

(begin-for-syntax
  (define-for-syntax (pretty-syntax stx) (pretty-format (syntax->datum stx))))

;; honu-pattern should expand to
;; (define-syntax honu-pattern ...)

(require (for-syntax (submod "." analysis)))
(require (for-meta 2 "util.rkt"
                   racket/match
                   (prefix-in syntax: syntax/parse/private/residual-ct))
         (for-meta 3 racket/base syntax/parse racket/syntax)
         )
;; creates a new syntax/parse pattern
(provide honu-pattern)

(begin-for-syntax
  (define-syntax (generate-pattern stx)
    (syntax-parse stx
      [(_ name literals (pattern-stx out-stx) ...)

      (define (make-syntax-class-pattern honu-pattern maybe-out)
        (define variables (find-pattern-variables honu-pattern))
        (define use (generate-temporaries variables))
        (define mapping (make-hash))
        (for ([old variables]
              [new use])
          (debug 2 "Update mapping ~a to ~a\n" (syntax-e (pattern-variable-name old)) new)
          (hash-set! mapping
                     (syntax-e (pattern-variable-name old))
                     (pattern-variable new
                                       (pattern-variable-original old)
                                       (pattern-variable-depth old)
                                       (pattern-variable-class old))))

        (define withs (pattern-variables+attributes variables use))

        (with-syntax ([(new-pattern ...) (convert-pattern honu-pattern mapping)]
                      [((withs ...) ...) (set->list withs)]
                      [(result-with ...) (if (syntax-e maybe-out)
                                           (with-syntax ([(out ...)
                                                          (syntax-parse maybe-out
                                                            #:literal-sets (cruft)
                                                            [(#%braces what ...)
                                                             #'(what ...)])
                                                            ])
                                             ;; #'(#:with result (parse-stuff honu-syntax (#%parens out ...)))
                                             #'(#:with result (parse-stuff out ...))
                                             )
                                           #'(#:with result #'()))])
          (syntax/loc honu-pattern
                      [pattern (~seq new-pattern ...)
                               withs ... ...
                               result-with ...
                               #:with (inner (... ...)) #'result
                               ])))

      (define pattern-stuff
        (for/list ([pattern (syntax->list #'(pattern-stx ...))]
                   [out (syntax->list #'(out-stx ...))])
          (make-syntax-class-pattern pattern out)))

      #;
      (debug "With bindings ~a\n" withs)
      (with-syntax ([(literal ...) #'literals]
                    [(new-pattern ...) pattern-stuff])
        #;
        (debug "Result with ~a\n" (syntax->datum #'(quote-syntax (result-with ...))))
        (define output
          #'(quote-syntax
              (begin
                ;; define at phase1 so we can use it in a macro
                (begin-for-syntax
                  (define-literal-set local-literals (literal ...))
                  (define-splicing-syntax-class name
                      #:literal-sets ([cruft #:at name]
                                      [local-literals #:at name])
                      new-pattern ...

                      #;
                      [pattern x #:when (begin
                                          (debug "All patterns failed for ~a\n" 'name)
                                          #f)]

                      )))))
        (debug "Output is ~a\n" (pretty-syntax output))
        output)])))

;; generates a phase 1 binding for the pattern. analyzes its pattern so it
;; must execute in phase 2
(define-honu-syntax honu-pattern
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens literal ...)
          (~seq (#%braces original-pattern ...)
                (~optional (~seq honu-comma maybe-out)
                           #:defaults ([maybe-out #'#f])))
          ...
          . rest)
       (values
         (phase1:racket-syntax
           (splicing-let-syntax
             ([make (lambda (stx)
                      (syntax-parse stx
                        [(_ new-name)
                         (syntax-local-introduce
                           (generate-pattern name
                                             (literal ...)
                                             ((original-pattern ...) maybe-out)
                                             ...))]))])
             (make name)))
               #'rest
               #f)])))

;; like begin-for-syntax
(provide honu-phase)
(define-honu-syntax honu-phase
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%braces body ...) . rest)
       (define out
         (phase1:racket-syntax (begin-for-syntax (parse-stuff body ...))))
       (values out #'rest #t)])))

;; not sure this is useful but it lets you write racket syntax expressions
;; from inside honu. the main issue is all the bindings available
;; are honu bindings so things like (+ 1 x) wont work.
(provide honu-racket)
(define-honu-syntax honu-racket
  (lambda (code)
    (define (remove-cruft stx)
      (syntax-parse stx #:literal-sets (cruft)
        [(#%parens inside ...)
         (remove-cruft #'(inside ...))]
        [(#%braces inside ...)
         (remove-cruft #'(inside ...))]
        [(#%brackets inside ...)
         (remove-cruft #'(inside ...))]
        [(head rest ...)
         (with-syntax ([head* (remove-cruft #'head)]
                       [(rest* ...) (remove-cruft #'(rest ...))])
           #'(head* rest* ...))]
        [x #'x]))

    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stx ...) . rest)
       (define out
         (with-syntax ([(stx* ...) (remove-cruft #'(stx ...))])
           (phase1:racket-syntax (phase0:racket-syntax (stx* ...)))))
       (values out #'rest #t)])))
