#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/parse
                     syntax/stx
                     racket/set
                     racket/syntax
                     "literals.rkt"
                     (prefix-in phase1: "parse2.rkt")
                     "debug.rkt"
                     (prefix-in phase1: "compile.rkt")
                     "util.rkt"
                     (prefix-in syntax: syntax/parse/private/residual-ct)
                     racket/base)
         (for-meta 2 syntax/parse
                     racket/base
                     macro-debugger/emit
                     (prefix-in phase2: "parse2.rkt")
                     (prefix-in phase2: "compile.rkt"))
         (prefix-in phase0: "compile.rkt")
         "literals.rkt"
         "syntax.rkt"
         "debug.rkt"
         (for-meta -1 "literals.rkt" "compile.rkt" "parse2.rkt" "parse-helper.rkt")
         #;
         (for-syntax "honu-typed-scheme.rkt")
         syntax/parse)

(define-for-syntax (convert-pattern original-pattern new-names)
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

(begin-for-syntax
  (struct pattern-variable [name depth class] #:transparent)

  ;; makes a syntax object with the right number of nested ellipses patterns
  (define (pattern-variable->syntax variable)
    (for/fold ([out (pattern-variable-name variable)])
              ([depth (pattern-variable-depth variable)])
      (with-syntax ([out out])
        #'(out (... ...))))))

;; reverse the syntax so that ellipses appear in front of the s-expr
;; then search for ellipses nodes and s-exprs of the form class:id %colon name:id
;; the output should match the ellipses depth, so for example
;;   ... foo %colon bar
;; output would be
;;   ((bar ...) (foo_result ...)
(define-for-syntax (find-pattern-variables original-pattern)
  (define (reverse-syntax input)
    (syntax-parse input
      [(x ...) (datum->syntax #'(x ...)
                              (reverse (for/list ([x (syntax->list #'(x ...))])
                                         (reverse-syntax x)))
                              #'(x ...) #'(x ...))]
      [x #'x]))
  (define (merge set1 set2)
    (debug "Merge ~a with ~a\n" set1 set2)
    (set-union set1 set2))
  (define (wrap-ellipses stuff)
    (for/set ([variable stuff])
      (pattern-variable (pattern-variable-name variable)
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
    (debug "Find in ~a\n" (syntax->datum the-pattern))
    (define-splicing-syntax-class maybe
                                  #:literal-sets (cruft)
                                  #:literals ([ellipses ...])
      [pattern (~seq class:id colon name:id)
               #:attr result
               (with-syntax ([name.result (format-id #'name "~a_result" #'name)])
                             (set (pattern-variable #'name 0 #'class)))]
      [pattern (~seq ellipses thing:maybe rest ...)
               #:attr result
               (merge (wrap-ellipses (attribute thing.result))
                      (find #'(rest ...)))]
      [pattern (x:maybe ...) #:attr result (apply set-union (attribute x.result))]
      [pattern x #:attr result (set)])
    (syntax-parse the-pattern
      #:literals ([ellipses ...])
      [(ellipses thing:maybe rest ...)
       (debug "Ellipses case\n")
       (merge (wrap-ellipses (attribute thing.result))
              (find #'(rest ...)))]
      [(thing:maybe rest ...)
       (debug "Normal list case ~a\n" (attribute thing.result))
       (merge (attribute thing.result) (find #'(rest ...)))]
      [thing (set)]))
  (define variables (find (reverse-syntax original-pattern)))
  (debug "Found variables ~a\n" variables)
  (for/list ([x variables]) x))

#;
(define-for-syntax (find-pattern-variables original-pattern)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             ;; we know the output of syntactic classes will end with _result
             #:with (result ...)
             ;; FIXME: dont need name.result anymore here
             (with-syntax ([name.result (format-id #'name "~a_result" #'name)])
               #'(name))]
    [pattern (x:pattern-type ...) #:with (result ...) #'(x.result ... ...)]
    [pattern x #:with (result ...) #'()])
  (syntax-parse original-pattern
    [(thing:pattern-type ...)
     (filter (lambda (x) (syntax-e x)) (syntax->list #'(thing.result ... ...)))]))

(begin-for-syntax
(define-syntax (parse-stuff stx)
  (syntax-parse stx
    [(_ stuff ...)
     (emit-remark "Parse stuff ~a\n" #'(stuff ...))
     (phase2:parse-all #'(stuff ...))
     #;
     (honu->racket (parse-all #'(stuff ...)))])))

(provide honu-macro)
(define-honu-syntax honu-macro 
  (lambda (code context)
    (debug "Macroize ~a\n" code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens literal ...) (#%braces pattern ...) (#%braces action ...) . rest)
       (debug "Pattern is ~a\n" #'(pattern ...))
       (debug 2 "Pattern variables ~a\n" (find-pattern-variables #'(pattern ...)))
       (values
         (let ()
           (define pattern-variables (find-pattern-variables #'(pattern ...)))

           ;; only need a 1-to-1 mapping here
           (define mapping (make-hash))
           (for ([variable pattern-variables])
             (debug "Update mapping for ~a\n" (pattern-variable-name variable))
             (hash-set! mapping (syntax-e (pattern-variable-name variable))
                        variable)) 
           (debug "Create pattern\n")
           (with-syntax ([(syntax-parse-pattern ...)
                          (convert-pattern #'(pattern ...) mapping)]
                         [((pattern-variable.name pattern-variable.result) ...)
                          (for/list ([name pattern-variables])
                            (debug "Create new pattern variable from ~a\n" name)
                            (with-syntax ([name (pattern-variable->syntax name)]
                                          [name.result (pattern-variable->syntax
                                                         (pattern-variable (format-id (pattern-variable-name name)
                                                                                      "~a_result"
                                                                                      (pattern-variable-name name))
                                                                           (pattern-variable-depth name)
                                                                           (pattern-variable-class name)
                                                                           ))])
                              #'(name name.result)))])
             (debug "Done with syntax\n")
             (phase1:racket-syntax 
               (define-honu-syntax name
                          (lambda (stx context-name)
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
                                   (debug "~a = ~a\n" 'pattern-variable.name #'pattern-variable.name) ...
                                   (parse-stuff action ...))
                                 #'more #t)]
                              [else (raise-syntax-error #f "Could not match macro" stx)]
                              ))))))
         #'rest
         #t)])))

;; FIXME: we shouldn't need this definition here
(define-syntax (as-honu-syntax stx)
  (syntax-parse stx
    [(_ form)
     (define compressed (phase0:compress-dollars #'form))
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

(provide honu-syntax)
;; Do any honu-specific expansion here
(define-honu-syntax honu-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stuff ...) . rest)
       (define context (stx-car #'(stuff ...)))
       (define compressed (phase0:compress-dollars #'(stuff ...)))
       (values
         (with-syntax ([stuff* (datum->syntax context
                                              (syntax->list compressed)
                                              context context)])
           ;; (debug "Stuff is ~a\n" (syntax->datum #'stuff*))
           ;; (debug "Stuff syntaxed is ~a\n" (syntax->datum #'#'stuff*))
           (with-syntax ([(out ...) #'stuff*])
             (phase1:racket-syntax #'stuff*)))
         #; #'(%racket-expression (parse-stuff stuff ...))
         #'rest
         #f)])))

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

;; creates a new syntax/parse pattern
(provide honu-pattern)
(define-honu-syntax honu-pattern
  (lambda (code context)
    (define (generate-pattern name literals original-pattern maybe-out)
      (define variables (find-pattern-variables original-pattern))
      (define use (generate-temporaries variables))
      (define mapping (make-hash))
      (for ([old variables]
            [new use])
        (debug "Update mapping ~a to ~a\n" (syntax-e (pattern-variable-name old)) new)
        (hash-set! mapping
                   (syntax-e (pattern-variable-name old))
                   (pattern-variable new
                                     (pattern-variable-depth old)
                                     (pattern-variable-class old))))

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
        (debug "Syntax class of ~a is ~a at ~a\n" (pattern-variable-class variable)
               (syntax-local-value (pattern-variable-class variable) (lambda () #f))
               (syntax-local-phase-level))
        (define attributes
          (let ([syntax-class (syntax-local-value (pattern-variable-class variable))])
            (for/list ([attribute (syntax:stxclass-attrs syntax-class)])
              (pattern-variable (syntax:attr-name attribute)
                                (+ (pattern-variable-depth variable)
                                   (syntax:attr-depth attribute))
                                #f))))
        (for/set ([attribute attributes])
          (with-syntax ([bind-attribute
                          (let ([name (pattern-variable-name attribute)])
                            (pattern-variable->syntax
                              (pattern-variable (format-id (pattern-variable-name variable) "~a_~a"
                                                           (syntax-e (pattern-variable-name variable))
                                                           name)
                                                (pattern-variable-depth attribute)
                                                (pattern-variable-class attribute))))]
                        [new-attribute (pattern-variable->syntax
                                         (pattern-variable
                                           (format-id new-name "~a_~a" new-name (pattern-variable-name attribute))
                                           (pattern-variable-depth attribute)
                                           #f))])
            #'(#:with bind-attribute #'new-attribute))))

      (define withs
        (for/union ([old variables]
                   [new use])
          (with-syntax ([old-syntax (pattern-variable->syntax old)]
                        [new.result (pattern-variable->syntax
                                      (pattern-variable (format-id new "~a_result" new)
                                                        (pattern-variable-depth old)
                                                        (pattern-variable-class old)))])
            (set-union (set #'(#:with old-syntax #'new.result))
                       (bind-attributes old new)))))
      (debug "With bindings ~a\n" withs)
      (with-syntax ([name name]
                    [(literal ...) literals]
                    [(new-pattern ...) (convert-pattern original-pattern mapping)]
                    [((withs ...) ...) (set->list withs)]
                    [(result-with ...) (if maybe-out
                                         (with-syntax ([(out ...) maybe-out])
                                           #'(#:with result (as-honu-syntax out ...)))
                                         #'(#:with result #'()))])
        (phase1:racket-syntax (begin
                     ;; define at phase1 so we can use it
                     (begin-for-syntax
                       (define-literal-set local-literals (literal ...))
                       (define-splicing-syntax-class name
                                                     #:literal-sets ([cruft #:at name]
                                                                     [local-literals #:at name])
                                                     [pattern (~seq new-pattern ...)
                                                              withs ... ...
                                                              result-with ...
                                                              ]))
                     ;; and define at phase 0 so we can inspect it
                     (define-literal-set local-literals (literal ...))
                     (define-splicing-syntax-class name
                                                     #:literal-sets ([cruft #:at name]
                                                                     [local-literals #:at name])
                                                     [pattern (~seq new-pattern ...)
                                                              withs ... ...
                                                              result-with ...
                                                              ])))))
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens literal ...)
          (#%braces pattern ...)
          (~optional (#%braces out ...))
          . rest)
       (values (generate-pattern #'name #'(literal ...)
                                 #'(pattern ...)
                                 (attribute out))
               #'rest
               #f)])))
