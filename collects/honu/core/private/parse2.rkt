#lang racket/base

(define-syntax-rule (require-syntax stuff ...)
                    (require (for-syntax stuff ...)))

;; phase 0
(require ;; "macro2.rkt"
         "literals.rkt"
         "debug.rkt"
         (prefix-in transformer: "transformer.rkt")
         syntax/stx
         syntax/parse/experimental/splicing
         syntax/parse)
;; phase 1
(require-syntax racket/base)

;; phase -1
(require (for-template racket/base
                       racket/splicing
                       "extra.rkt"))

(provide parse parse-all)

#;
(define-literal-set literals
                    [honu-macro])

(define (get-value what)
  (syntax-local-value what (lambda () #f)))

#;
(define (get-value what)
  (debug "what is ~a\n" what)
  (with-syntax ([what what])
    (let-syntax ([v (lambda (stx)
                      (debug "get ~a\n" #'what)
                      (with-syntax ([x (syntax-local-value #'what (lambda () #f))])
                        #'x))])
      (v))))

#;
(define (get-value check)
  (eval-syntax
    (with-syntax ([check check])
      #'(syntax-local-value check #'check (lambda () #f))))) 

(define (bound-to-operator? check)
  (let ([value (get-value check)])
    (debug "operator? ~a ~a\n" check value)
    (transformer:honu-operator? value)))

(define (bound-to-macro? check)
  (let ([value (get-value check)])
    (debug "macro? ~a ~a\n" check value)
    (transformer:honu-transformer? value))
  #;
  (let ([value (syntax-local-value check (lambda () #f))])
    (transformer:honu-transformer? value)))

(define (honu-macro? something)
  (and (identifier? something)
       (bound-to-macro? something)))

(define (honu-operator? something)
  (and (identifier? something)
       (bound-to-operator? something)))

(define (semicolon? what)
  (define-literal-set check (semicolon))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug "Semicolon? ~a ~a\n" what is)
  is)

(define (comma? what)
  (define-literal-set check (honu-comma))
  (define is (and (identifier? what)
                    ((literal-set->predicate check) what)))
  (debug "Comma? ~a ~a\n" what is)
  is)

(define-literal-set argument-stuff [honu-comma])

(define (parse-arguments arguments)
  (define-syntax-class val
    [pattern x:identifier #:when (equal? 'val (syntax-e #'x))])
  (let loop ([out '()]
             [arguments arguments])
    (syntax-parse arguments #:literal-sets (argument-stuff)
      [(x:val name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(name:identifier honu-comma more ...)
       (loop (cons #'name out) #'(more ...))]
      [(x:val name:identifier)
       (loop (cons #'name out) #'())]
      [(name:identifier)
       (loop (cons #'name out) #'())]
      [() (reverse out)])))

;; removes syntax that causes expression parsing to stop
(define (strip-stops code)
  (define-syntax-class stopper #:literal-sets (cruft)
    #;
    [pattern semicolon]
    [pattern honu-comma]
    [pattern colon])
  (syntax-parse code
    [(x:stopper rest ...) (strip-stops #'(rest ...))]
    [else code]))

(define (parse-comma-expression arguments)
  (if (null? (syntax->list arguments))
    '()
    (let loop ([used '()]
               [rest arguments])
      (if (empty-syntax? rest)
        (reverse used)
        (let-values ([(parsed unparsed)
                      ;; FIXME: don't strip all stops, just comma
                      (parse (strip-stops rest))])
          (loop (cons parsed used)
                unparsed))))))

(define parsed-property (gensym 'honu-parsed))
(define (parsed-syntax syntax)
  (if syntax
    (syntax-property syntax parsed-property #t)
    syntax))

(define (parsed-syntax? syntax)
  (syntax-property syntax parsed-property))

(define (stopper? what)
  (define-literal-set check (honu-comma semicolon colon))
  (define is (and (identifier? what)
                  ((literal-set->predicate check) what)))
  (debug "Comma? ~a ~a\n" what is)
  is)

(provide honu-function)
(define-splicing-syntax-class honu-function #:literal-sets (cruft)
  [pattern (~seq function:identifier (#%parens args ...) (#%braces code ...))
           #:with result
           (with-syntax ([(parsed-arguments ...)
                          (parse-arguments #'(args ...))])
             #'(define (function parsed-arguments ...)
                 (let-syntax ([parse-more (lambda (stx)
                                            (parse-all #'(code ...)))])
                   (parse-more))))])

;; 1 + 1
;; ^
;;  left: identity
;;  current: 1
;; 1 + 1
;;   ^
;;  left: (lambda (x) (+ 1 x))
;;  current: #f
;; 1 + 1
;;     ^
;;  left: (lambda (x) (+ 1 x))
;;  current: 1
;;
;; 1 + 1 * 2
;;       ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: #f
;;
;; 1 + 1 * 2
;;         ^
;;  left: (lambda (x) (left (* 1 x)))
;;  current: 2

;; parse one form
;; return the parsed stuff and the unparsed stuff
(define (parse input)
  (define (do-macro head rest precedence left current stream)
    (if current
      (values (left current) stream)
      (begin
        (debug "Honu macro ~a\n" head)
        (let-values ([(parsed unparsed terminate?)
                      ((syntax-local-value head)
                       (with-syntax ([head head]
                                     [(rest ...) rest])
                         #'(head rest ...))
                       #f)])
          (with-syntax ([(parsed ...) parsed]
                        [(rest ...) unparsed])
            (debug "Output from macro ~a\n" #'(parsed ...))
            (do-parse #'(parsed ... rest ...)
                      precedence left current)
            #;
            (if terminate?
              (values (left #'parsed)
                      #'rest)
              (do-parse #'rest precedence
                        left #'parsed)))))))
  (define (do-parse stream precedence left current)
    (define-syntax-class atom
      [pattern x:identifier]
      [pattern x:str]
      [pattern x:number])

    (debug "parse ~a precedence ~a left ~a current ~a\n" (syntax->datum stream) precedence left current)
    (define final (if current current #f))
    (syntax-parse stream #:literal-sets (cruft)
      [()
       (values (left final) #'())]
      ;; dont reparse pure racket code
      [(%racket racket rest ...)
       (if current
         (values (left current) stream)
         (values (left #'racket) #'(rest ...)))]
      ;; for expressions that can keep parsing
      [(%racket-expression racket rest ...)
       (if current
         (values (left current) stream)
         (do-parse #'(rest ...)
                   precedence left
                   #'racket))]
      [(head rest ...)
       (cond
         [(honu-macro? #'head)
          (do-macro #'head #'(rest ...) precedence left current stream)]
         [(parsed-syntax? #'head)
          (do-parse #'(rest ...) precedence left #'head)]
         [(honu-operator? #'head)
          (define new-precedence (transformer:honu-operator-ref (syntax-local-value #'head) 0))
          (define association (transformer:honu-operator-ref (syntax-local-value #'head) 1))
          (define binary-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 2))
          (define unary-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 3))
          (define higher
            (case association
              [(left) >]
              [(right) >=]))
          (debug "precedence old ~a new ~a higher? ~a\n" precedence new-precedence (higher new-precedence precedence))
          (if (higher new-precedence precedence)
            (do-parse #'(rest ...) new-precedence
                      (lambda (stuff)
                        (if current
                          (if binary-transformer
                            (left (binary-transformer current stuff))
                            (error '#'head "cannot be used as a binary operator"))
                          (if unary-transformer
                            (left (unary-transformer stuff))
                            (error '#'head "cannot be used as a unary operator"))))
                      #f)
            (do-parse #'(head rest ...)
                      0
                      (lambda (x) x)
                      (left final)))]
         
         [(stopper? #'head)
          (values (left final)
                  stream)]
         [else
           (syntax-parse #'(head rest ...) #:literal-sets (cruft)
             [((semicolon more ...) . rest)
              #;
              (define-values (parsed unparsed)
                             (do-parse #'(more ...)
                                       0
                                       (lambda (x) x)
                                       #f))
              #;
              (when (not (stx-null? unparsed))
                (raise-syntax-error 'parse "found unparsed input" unparsed))
              (values (parse-all #'(more ...)) #'rest)]
             [(function:honu-function . rest)
              (values #'function.result #'rest)]
             #;
             [(function:identifier (#%parens args ...) (#%braces code ...) . rest)
              (values (with-syntax ([(parsed-arguments ...)
                                     (parse-arguments #'(args ...))])
                        #'(define (function parsed-arguments ...)
                            (let-syntax ([parse-more (lambda (stx)
                                                       (parse-all #'(code ...)))])
                              (parse-more))))
                      #'rest)]
             [else (syntax-parse #'head
                     #:literal-sets (cruft)
                     [x:atom
                       (debug "atom ~a current ~a\n" #'x current)
                       (if current
                         (values (left current) stream)
                         (do-parse #'(rest ...) precedence left #'x))]
                     ;; [1, 2, 3] -> (list 1 2 3)
                     [(#%brackets stuff ...)
                      (syntax-parse #'(stuff ...) #:literal-sets (cruft)
                        [(work:honu-expression colon (~seq variable:id honu-<- list:honu-expression (~optional honu-comma)) ...)
                         (define comprehension #'(for/list ([variable list.result]
                                                            ...)
                                                   work.result))
                         (if current
                           (error 'parse "a list comprehension cannot follow an expression")
                           (do-parse #'(rest ...) precedence left comprehension))]
                        [else
                          (define value (with-syntax ([(data ...)
                                                       (parse-comma-expression #'(stuff ...))])
                                          #'(list data ...)))
                          (define lookup (with-syntax ([(data ...)
                                                        (parse-comma-expression #'(stuff ...))]
                                                       [current current])
                                           #'(do-lookup current data ...)))
                          (if current
                            ;; (values (left current) stream)
                            (do-parse #'(rest ...) precedence left lookup)
                            (do-parse #'(rest ...) precedence left value))])]
                     ;; block of code
                     [(#%braces stuff ...)
                      (if current
                        (values (left current) stream)
                        (let ()
                          (define body (parse-all #'(stuff ...)))
                          (with-syntax ([body body])
                            (do-parse #'(rest ...) precedence left #'(let () body)))))]
                     ;; expression or function application
                     [(#%parens args ...)
                      (if current
                        (if (> precedence 9000)
                          (let ()
                            (define call (with-syntax ([current (left current)]
                                                       [(parsed-args ...)
                                                        (parse-comma-expression #'(args ...)) ])
                                           #'(current parsed-args ...)))
                            (do-parse #'(rest ...) 9000 (lambda (x) x) call))
                          (let ()
                            (debug "function call ~a\n" left)
                            (define call (with-syntax ([current current]
                                                       [(parsed-args ...)
                                                        (parse-comma-expression #'(args ...)) ])
                                           #'(current parsed-args ...)))
                            (do-parse #'(rest ...) precedence left call)))
                        (let ()
                          (debug "inner expression ~a\n" #'(args ...))
                          (define-values (inner-expression unparsed) (parse #'(args ...)))
                          (when (not (empty-syntax? unparsed))
                            (error 'parse "expression had unparsed elements ~a" unparsed))
                          (do-parse #'(rest ...) precedence left inner-expression)))

                      #;
                      (do-parse #'(rest ...)
                                0
                                (lambda (x) x)
                                (left (with-syntax ([current current]
                                                    [(parsed-args ...)
                                                     (if (null? (syntax->list #'(args ...)))
                                                       '()
                                                       (list (parse #'(args ...))))])
                                        #'(current parsed-args ...))))
                      #;
                      (error 'parse "function call")]
                     [else (error 'what "dont know how to parse ~a" #'head)])])])]))

  (define-values (parsed unparsed)
                 (do-parse input 0 (lambda (x) x) #f))
  (values (parsed-syntax parsed)
          unparsed))

(define (empty-syntax? what)
  (syntax-parse what
    [() #t]
    [else #f]))

(provide parse-one)
(define (parse-one code)
  (parse (strip-stops code)))

(define (parse-all code)
  (let loop ([all '()]
             [code code])
    (define-values (parsed unparsed)
                   (parse (strip-stops code)))
    (debug "Parsed ~a unparsed ~a\n" (if parsed (syntax->datum parsed) parsed)
           (if unparsed (syntax->datum unparsed) unparsed))
    (if (empty-syntax? unparsed)
      (with-syntax ([(use ...) (reverse (if parsed
                                          (cons parsed all)
                                          all))])
        #'(begin use ...))
      (loop (cons parsed all)
            unparsed))))

;; rest will be some subset of full
(define (parsed-things full rest)
  (define full-datum (syntax->datum full))
  (define rest-datum (syntax->datum rest))
  (- (length full-datum) (length rest-datum)))

(provide honu-expression)
(define-primitive-splicing-syntax-class (honu-expression)
  #:attributes (result)
  #:description "expression"
  (lambda (stx fail)
    (debug "honu expression syntax class\n")
    (define-values (parsed unparsed)
                   (parse stx))
    (debug "parsed ~a\n" (if parsed (syntax->datum parsed) parsed))
    (list (parsed-things stx unparsed) parsed)))

(provide identifier-comma-list)
(define-splicing-syntax-class identifier-comma-list
                              #:literal-sets (cruft)
  [pattern (~seq (~seq name:id (~optional honu-comma)) ...)]) 
