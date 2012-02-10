#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/parse
                     syntax/stx
                     racket/set
                     racket/syntax
                     "literals.rkt"
                     "parse2.rkt"
                     "debug.rkt"
                     racket/base)
         (for-meta 2 syntax/parse
                     racket/base
                     "parse2.rkt"
                     "compile.rkt")
         "literals.rkt"
         "syntax.rkt"
         #;
         (for-syntax "honu-typed-scheme.rkt")
         syntax/parse)

(define-for-syntax (convert-pattern original-pattern new-names)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             #:with (result ...)
             (with-syntax ([new-name (hash-ref new-names (syntax-e #'name))])
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
#;
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
                             (set #'(name name.result)))]
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
             (debug "Update mapping for ~a\n" (syntax-e variable))
             (hash-set! mapping (syntax-e variable) variable)) 
           (with-syntax ([(syntax-parse-pattern ...)
                          (convert-pattern #'(pattern ...) mapping)]
                         [((pattern-variable.name pattern-variable.result) ...)
                          (for/list ([name pattern-variables])
                            (with-syntax ([name name]
                                          [name.result (format-id name "~a_result" name)])
                              #'(name name.result)))])
             #'(%racket (define-honu-syntax name
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
                                   (parse-stuff action ...))
                                 #'more #t)]
                              [else (raise-syntax-error #f "Could not match macro" stx)]
                              ))))))
         #'rest
         #t)])))

(provide honu-syntax)
;; Do any honu-specific expansion here
(define-honu-syntax honu-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stuff ...) . rest)
       (define context (stx-car #'(stuff ...)))
       (values
         (with-syntax ([stuff* (datum->syntax context
                                              (syntax->list #'(stuff ...))
                                              context context)])
           #'(%racket #'stuff*))
         #; #'(%racket-expression (parse-stuff stuff ...))
         #'rest
         #f)])))

;; combine syntax objects
;; #'(a b) + #'(c d) = #'(a b c d)
(provide mergeSyntax)
(define (mergeSyntax syntax1 syntax2)
  (with-syntax ([(syntax1* ...) syntax1]
                [(syntax2* ...) syntax2])
    #'(syntax1* ... syntax2* ...)))

;; creates a new syntax/parse pattern
(provide honu-pattern)
(define-honu-syntax honu-pattern
  (lambda (code context)
    (define (bind-to-results pattern)
      (with-syntax ([((pattern-variable.name pattern-variable.result) ...)
                    (find-pattern-variables pattern)])
        (with-syntax ([(each ...)
                       (for/list ([name (syntax->list #'(pattern-variable.name ...))]
                                  [result (syntax->list #'(pattern-variable.result ...))])
                         (with-syntax ([name name]
                                       [result result])
                           #'(#:with result result)))])
          #'(each ...))))
    (define (generate-pattern name literals original-pattern)
      (define variables (find-pattern-variables original-pattern))
      (define use (generate-temporaries variables))
      (define mapping (make-hash))
      (for ([old variables]
            [new use])
        (hash-set! mapping (syntax-e old) new))
      (define withs
        (for/list ([old variables]
                   [new use])
          (with-syntax ([old old]
                        [new.result (format-id new "~a_result" new)])
            #'(#:with old #'new.result))))
      (with-syntax ([name name]
                    [(literal ...) literals]
                    [(new-pattern ...) (convert-pattern original-pattern mapping)]
                    [((withs ...) ...) withs]
                    #;
                    [((bindings ...) ...) (bind-to-results original-pattern)])
        #'(%racket (begin-for-syntax
                     (define-literal-set local-literals (literal ...))
                     (define-splicing-syntax-class name
                       #:literal-sets ([cruft #:at name]
                                       [local-literals #:at name])
                       [pattern (~seq new-pattern ...)
                                withs ... ...
                                ; bindings ... ...
                                ])))))
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens literal ...)
          (#%braces pattern ...)
          . rest)
       (values (generate-pattern #'name #'(literal ...) #'(pattern ...))
               #'rest
               #f)])))
