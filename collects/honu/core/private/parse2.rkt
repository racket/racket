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

(define (parse-comma-expression arguments)
  (if (null? (syntax->list arguments))
    '()
    (let loop ([used '()]
               [rest arguments])
      (if (empty-syntax? rest)
        (reverse used)
        (let-values ([(parsed unparsed)
                      (parse rest)])
          (loop (cons parsed used)
                unparsed))))))

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
  (define (do-parse stream precedence left current)
    (define-syntax-class atom
      [pattern x:identifier]
      [pattern x:str]
      [pattern x:number])

    (debug "parse ~a precedence ~a left ~a current ~a\n" stream precedence left current)
    (define final (if current current #'(void)))
    (syntax-parse stream #:literal-sets (cruft)
      [()
       (values (left final) #'())]
      [(head rest ...)
       (cond
         [(honu-macro? #'head)
          (begin
            (debug "Honu macro ~a\n" #'head)
            (let-values ([(parsed unparsed terminate?)
                          ((syntax-local-value #'head) #'(head rest ...) #f)])
              (with-syntax ([parsed parsed]
                            [rest unparsed])
                (if terminate?
                  (values (left #'parsed)
                          #'rest)
                  (do-parse #'rest precedence
                            (lambda (x) x)
                            #;
                            (lambda (x)
                              (with-syntax ([x x])
                                #'(begin parsed x)))
                            (left #'parsed)))
                #;
                #'(splicing-let-syntax ([more-parsing (lambda (stx)
                                                        (do-parse (stx-cdr stx)
                                                                  precedence left
                                                                  current))])
                                       parsed
                                       (more-parsing . rest)))))]
         [(honu-operator? #'head)
          (define new-precedence (transformer:honu-operator-ref (syntax-local-value #'head) 0))
          (define association (transformer:honu-operator-ref (syntax-local-value #'head) 1))
          (define operator-transformer (transformer:honu-operator-ref (syntax-local-value #'head) 2))
          (define higher
            (case association
              [(left) >]
              [(right) >=]))
          (debug "new precedence ~a\n" new-precedence)
          (if (higher new-precedence precedence)
            (do-parse #'(rest ...) new-precedence
                      (lambda (stuff)
                        (left (operator-transformer current stuff)))
                      #f)
            (do-parse #'(head rest ...)
                      0
                      (lambda (x) x)
                      (left final)))]
         [(comma? #'head)
          (values (left final)
                  #'(rest ...))]
         [(semicolon? #'head)
          (values (left final)
                  #'(rest ...))
          #;
          (do-parse #'(rest ...) 0
                    (lambda (stuff)
                      (with-syntax ([stuff stuff]
                                    [current (left current)])
                        #'(begin current stuff)))
                    #'(void))
          #;
          (with-syntax ([so-far (left current)])
            #'(splicing-let-syntax ([more (lambda (stx)
                                            (parse #'(rest ...)))])
                                   so-far (more)))]
         [else
           (syntax-parse #'(head rest ...) #:literal-sets (cruft)
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
                         (do-parse #'(rest ...) precedence left value))]
                     ;; block of code
                     [(#%braces stuff ...)
                      (if current
                        (values (left current) stream)
                        (let ()
                          (define body (parse-all #'(stuff ...)))
                          (do-parse #'(rest ...) precedence left body)))]
                     ;; expression or function application
                     [(#%parens args ...)
                      (if current
                        (let ()
                          (debug "function call ~a\n" left)
                          (values (left (with-syntax ([current current]
                                                      [(parsed-args ...)
                                                       (parse-comma-expression #'(args ...)) ])
                                          #'(current parsed-args ...)))
                                  #'(rest ...)))
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

  (do-parse input 0 (lambda (x) x) #f))

(define (empty-syntax? what)
  (syntax-parse what
    [() #t]
    [else #f]))

(define (parse-all code)
  (let loop ([all '()]
             [code code])
    (define-values (parsed unparsed)
                   (parse code))
    (debug "Parsed ~a unparsed ~a\n" (syntax->datum parsed)
           (syntax->datum unparsed))
    (if (empty-syntax? unparsed)
      (with-syntax ([(use ...) (reverse (cons parsed all))])
        #'(begin use ...))
      (loop (cons parsed all)
            unparsed))))

(define (parse2 forms)
  (debug "parse forms ~a\n" forms)
  (when (stx-pair? forms)
    (define head (stx-car forms))
    (if (honu-macro? head)
      (begin
        (debug "honu macro ~a\n" head)
        (let-values ([(parsed rest)
                      ((syntax-local-value head) forms #f)])
          (with-syntax ([parsed parsed]
                        [rest rest])
          #'(splicing-let-syntax ([more-parsing (lambda (stx)
                                         (debug "more parsing!!\n")
                                         (parse stx))])
              parsed
              (more-parsing . rest)))))
      #'(debug "regular parsing\n"))))

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
    (debug "parsed ~a\n" parsed)
    (list (parsed-things stx unparsed) parsed)))
