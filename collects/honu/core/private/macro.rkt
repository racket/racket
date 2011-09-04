#lang racket/base

(require "honu-typed-racket.rkt"
         "literals.rkt"
         "parse.ss"
         "syntax.ss"
         (prefix-in honu: "honu.rkt")
         syntax/parse
         (for-syntax macro-debugger/emit)
         (for-meta 2 macro-debugger/emit
                   racket/base)
         (for-meta -3
           (only-in "literals.rkt" (#%parens literal-parens)))
         (for-syntax "debug.rkt"
                     "contexts.rkt"
                     "parse.rkt"
                     "syntax.rkt"
                     "literals.rkt"
                     "honu-typed-racket.rkt"
                     racket/base
                     syntax/parse
                     syntax/stx
                     scheme/pretty
                     scheme/trace))

#;
(provide (all-defined-out))

(define-syntax (ensure-defined stx)
  (syntax-case stx ()
    [(_ id ...)
     (begin
       (for-each (lambda (id)
                   (syntax-local-value id (lambda () (raise-syntax-error 'syntax-id "not defined" id))))
                 (syntax->list #'(id ...)))
       #'(void))]))

(ensure-defined #%parens #%braces)

(define-for-syntax (extract-conventions pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       ;; todo: export honu attributes for syntax/parse
       (loop (cons #'(any expr) out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (loop out #'(rest1 rest ...))]
      [(foo) out])))

(define-for-syntax (extract-patterns pattern)
  (let loop ([out '()]
             [in pattern])
    (syntax-case in (:)
      [(any : attribute rest ...)
       (loop (cons #'any out)
             #'(rest ...))]
      [(foo rest1 rest ...)
       (let ([f (if (eq? (syntax->datum #'foo) 'crackers)
                  #'(... ...)
                  #'foo)])
         (loop (cons f out)
               #'(rest1 rest ...)))]
      [(foo) (reverse (cons #'foo out))])))

(define-for-syntax (fix-template stx)
  (define (fix-classes stx)
    (syntax-parse stx #:literals (honu-:)
      [(variable:identifier honu-: class:identifier rest ...)
       (with-syntax ([(rest* ...) (fix-template #'(rest ...))])
         (datum->syntax stx (cons #'(~var variable class #:attr-name-separator "_")
                                  #'(rest* ...))
                        stx))]
      [(one rest ...)
       (with-syntax ([one* (fix-template #'one)]
                     [(rest* ...) (fix-template #'(rest ...))])
         (datum->syntax stx (cons #'one*
                                  #'(rest* ...))
                        stx))]
      [else stx]))
  ;; removes commas from a pattern
  (define (fix-commas stx)
    (syntax-parse stx
      #:literals (honu-comma
                   [ellipses ...])
      [(a honu-comma ellipses rest ...)
       (with-syntax ([a* (fix-commas #'a)]
                     [(rest* ...) (fix-commas #'(rest ...))])
         (datum->syntax stx
                        `((~seq ,#'a* (~optional |,|)) ... ,@#'(rest* ...))
                        stx stx))]
      [(z rest ...)
       (with-syntax ([z* (fix-commas #'z)]
                     [(rest* ...) (fix-commas #'(rest ...))])
         (datum->syntax stx
                        (cons #'z* #'(rest* ...))
                        stx stx))]
      [else stx]))
  (define all-fixes (compose fix-commas fix-classes))
  (all-fixes stx))


(define-for-syntax (delimiter? x)
  (or (free-identifier=? x #'\;)))

(define wrapped #f)
(define unwrap #f)

(define-for-syntax (pull stx)
  (define (reverse-syntax stx)
    (with-syntax ([(x ...) (reverse (syntax->list stx))])
      #'(x ...)))
  (define-syntax-class stop-class
    (pattern x:id #:when (or (free-identifier=? #'x #'(... ...))
                             (free-identifier=? #'x #'\;))))
  (define (do-ellipses stx)
    (let loop ([ellipses '()]
               [body '()]
               [stx stx])
      (cond
        [(null? stx) (values (with-syntax ([(ellipses ...) ellipses]
                                           [(body ...) body])
                               #'(ellipses ... body ...))
                             stx)]
        [(and (identifier? (car stx))
              (free-identifier=? (car stx) #'(... ...)))
         (loop (cons #'(... ...) ellipses) body (cdr stx))]
        [(and (identifier? (car stx))
              (free-identifier=? (car stx) #'\;))
         (with-syntax ([all (cdr stx)])
           (syntax-parse #'all
                         [((~and x (~not _:stop-class)) ... stop:stop-class y ...)
                          (with-syntax ([(ellipses ...) ellipses]
                                        [(x* ...) (reverse-syntax #'(x ...))])
                            (values #'(ellipses ... (wrapped x* ... \;) unwrap)
                                    #'(stop y ...)))]
                         [else (with-syntax ([(f ...) (reverse-syntax #'all)]
                                             [(ellipses ...) ellipses])
                                 (values #'(ellipses ... (wrapped f ... \;) unwrap)
                                         #'()))]))])))
  (let loop ([all '()]
             [stx (reverse (syntax->list stx))])
    (if (null? stx)
        (with-syntax ([x all])
          #'x)
        (let ([head (car stx)]
              [tail (cdr stx)])
          (cond
            [(and (identifier? head)
                  (free-identifier=? head #'(... ...)))
             (let-values ([(wrapped rest) (do-ellipses (cons head tail))])
               (loop (cons (reverse-syntax wrapped) all) (syntax->list rest)))]
            [else (loop (cons head all) tail)])))))


(define-for-syntax (unpull stx)
  (define-syntax-class ellipses-class
    (pattern x:id #:when (free-identifier=? #'x #'(... ...))))
  (define-syntax-class delimiter-class
    (pattern x:id #:when (delimiter? #'x)))
  (syntax-parse stx
                #:literals (wrapped unwrap)
                [((~and z (~not (unwrap _ ...))) ... (unwrap (wrapped x ... delimiter:delimiter-class) ...) rest ...)
                 (with-syntax ([(x1 ...) (apply append (map syntax->list (syntax->list #'((x ... delimiter) ...))))]
                               [(rest* ...) (unpull #'(rest ...))])
                 #'(z ... x1 ... rest* ...))]
                [(unwrap (wrapped x ... delimiter:delimiter-class) ...)
                 (with-syntax ([(x1 ...) (apply append (map syntax->list (syntax->list #'((x ... delimiter) ...))))])
                 #'(x1 ...))]
                [(unwrap (wrapped x ... y) ...)
                 (with-syntax ([(x1 ...) (car (syntax->list #'((x ...) ...)))])
                   (with-syntax ([(x1* ...) (map unpull (syntax->list #'(x1 ...)))]
                                 [(y* ...) (map unpull (syntax->list #'(y ...)))])
                     #'(x1* ... y* ...)))]
                [(unwrap . x) (raise-syntax-error 'unpull "unhandled unwrap ~a" stx)]
                [(x ...) (with-syntax ([(x* ...) (map unpull (syntax->list #'(x ...)))])
                           #'(x* ...))]
                [else stx]))

(provide (for-syntax unpull))

(honu:define-honu-syntax honu-pattern
  (lambda (stx ctx)
    (syntax-parse stx #:literal-sets ([cruft #:at stx])
      #:literals (honu-literal)
      ;; #%parens #%brackets semicolon)
      [(_ name
          (~optional (~seq honu-literal (#%parens literals ...)))
          (#%parens all-attributes:identifier ...)
          (#%brackets xpattern ...)
          semicolon . rest)
       (define my-parens (datum->syntax #'name '#%parens #'name #'name))
       (define (create-pattern stuff)
         (with-syntax ([(fixed ...) (fix-template stuff)])
           (syntax/loc stuff (pattern (~seq fixed ...)))))
       (values
         (lambda ()
           (if (attribute literals)
             (with-syntax ([final-pattern (create-pattern #'(xpattern ...))])
                           (syntax/loc stx
                                       (define-splicing-syntax-class name
                                                                     #:literal-sets ([cruft #:at name])
                                                                     #:literals (literals ...)
                                                                     #:attributes (all-attributes ...)
                                                                     final-pattern)))
             (with-syntax ([final-pattern (create-pattern #'(xpattern ...))])
                           (syntax/loc stx
                                       (define-splicing-syntax-class name
                                                                     #:literal-sets ([cruft #:at name])
                                                                     #:attributes (all-attributes ...)
                                                                     final-pattern)))))
         #'rest)])))

(honu:define-honu-syntax honu-infix-macro
  (lambda (stx ctx)
    (debug "Infix macro!\n")
    (define-splicing-syntax-class patterns
                         #:literal-sets ([cruft #:phase (syntax-local-phase-level)])
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) (fix-template #'(template ...))])
    (define-syntax-class honu-macro3
                         ;; #:literals (#%parens #%braces)
                         #:literal-sets ([cruft ;;#:at stx
                                           #:phase (syntax-local-phase-level)
                                           ])
      [pattern (_ name (#%parens literals ...)
                  pattern:patterns ...
                  . rest)
               #:with result
               (list
                 (with-syntax ()
                   (apply-scheme-syntax
                   (syntax/loc stx
                               (honu:define-honu-infix-syntax name
                                 (lambda (stx ctx)
                                   (debug "~a pattern is ~a\n" 'name '(pattern.fixed ... ...))
                                   (syntax-parse stx
                                     #:literal-sets ([cruft #:at name])
                                     #:literals (literals ...)
                                     [(pattern.fixed ... rrest (... ...))
                                      (values
                                        (begin
                                          (emit-remark "Do macro transformer" (quote-syntax (pattern.code ...)))
                                          (let ([result (let ()
                                                          (honu-unparsed-begin pattern.code ...))])
                                            (lambda ()
                                              (emit-remark "Excuting macro " (symbol->string 'name))
                                              result)))
                                        #'(rrest (... ...)))]
                                     ...
                                     [else (raise-syntax-error 'name "bad syntax")]
                                     )))))) 
                 #'rest)])
    (debug "Executing honu infix macro\n")
    (syntax-parse stx
      [out:honu-macro3 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]

      [(_ (m x ...)
          (z y ...)
          . rest)
       (begin
         (debug "Got literals ~a\n" #'(x ...))
         (debug "M is ~a, = to #%parens is ~a\n" #'m (free-identifier=? #'#%parens #'m))
         (debug "Z is ~a, = to #%braces is ~a\n" #'z (free-identifier=? #'#%braces #'z))
         (debug "Rest is ~a\n" (syntax->datum #'rest))
         (raise-syntax-error 'honu-macro "f1" stx))]
      [else (raise-syntax-error 'honu-macro "fail" stx)]
      )))

(honu:define-honu-syntax honu-macro
  (lambda (stx ctx)
    (define-splicing-syntax-class patterns
                         #:literal-sets ([cruft #:phase (syntax-local-phase-level)])
      [pattern (~seq (#%braces template ...)
                     (#%braces code ...))
               #:with (fixed ...) (fix-template #'(template ...))])
    (define-syntax-class honu-macro3
                         ;; #:literals (#%parens #%braces)
                         #:literal-sets ([cruft ;;#:at stx
                                           #:phase (syntax-local-phase-level)
                                           ])
      [pattern (_ name (#%parens literals ...)
                  pattern:patterns ...
                  . rest)
               #:with result
               (list
                 (with-syntax ()
                   (apply-scheme-syntax
                   (syntax/loc stx
                               (define-honu-syntax name
                                 (lambda (stx ctx)
                                   (debug "~a pattern is ~a\n" 'name '(pattern.fixed ... ...))
                                   (syntax-parse stx
                                     #:literal-sets ([cruft #:at name])
                                     #:literals (literals ...)
                                     [(pattern.fixed ... rrest (... ...))
                                      (values
                                        (begin
                                          (emit-remark "Do macro transformer" (quote-syntax (pattern.code ...)))
                                          (let ([result (let ()
                                                          (honu-unparsed-begin pattern.code ...))])
                                            (lambda ()
                                              (emit-remark "Excuting macro " (symbol->string 'name))
                                              result)))
                                        #'(rrest (... ...)))]
                                     ...
                                     [else (raise-syntax-error 'name "bad syntax")]
                                     ))))))
                 #'rest)])
    (define-syntax-class honu-macro2
                         #:literals (#%parens #%braces)
      [pattern (_ name (#%braces code ...)
                  . rest)
               #:with result
               (list
                 (syntax/loc stx
                             (define-honu-syntax name
                               (lambda (stx ctx)
                                 (values
                                   (honu-unparsed-begin code ...)
                                   (begin
                                     (debug "inside ~a stx is ~a\n" 'name stx)
                                     (syntax-parse stx #:literals (semicolon)
                                       [(_ semicolon rrest (... ...))
                                        #'(rrest (... ...))]))))))
                 #'rest)])

    (define-syntax-class honu-macro1
                     #:literals (#%parens #%braces)
                     [pattern (_ (#%parens honu-literal ...)
                                 (#%braces (#%braces name pattern ...))
                                 (#%braces (#%braces template ...))
                                 . rest)
                              #:with result
                              (with-syntax ([pulled (pull #'(template ...))]
                                            [(pattern* ...) (map (lambda (stx)
                                                                   (if (and (identifier? stx)
                                                                            (not (ormap (lambda (f)
                                                                                          (free-identifier=? stx f))
                                                                                        (syntax->list #'(honu-literal ...))))
                                                                            (not (free-identifier=? stx #'(... ...))))
                                                                     (with-syntax ([x stx])
                                                                       #'(~and x (~not (~or honu-literal ...))))
                                                                     stx))
                                                                 (syntax->list #'(pattern ...)))]
                                            )
                                (list
                                  (syntax/loc stx
                                              (define-honu-syntax name
                                                (lambda (stx ctx)
                                                  ;; (define-literal-set literals (honu-literal ...))
                                                  (syntax-parse stx
                                                    ;; #:literal-sets (literals)
                                                    #:literals (honu-literal ...)
                                                    [(name pattern* ... . rrest)
                                                     (with-syntax ([(out (... ...)) (unpull #'pulled)])
                                                       (define (X) (raise-syntax-error (syntax->datum #'name) "implement for this context"))
                                                       (values
                                                         (syntax/loc stx (honu-unparsed-expr (honu-syntax (#%parens out (... ...)))))
                                                         #'rrest)
                                                       )]))))
                                  #'rest))])
    (debug "Executing honu macro\n")
    (syntax-parse stx
      [out:honu-macro1 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]
      [out:honu-macro3 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]
      [out:honu-macro2 (apply (lambda (a b) (values (lambda () a) b)) (syntax->list (attribute out.result)))]

      [(_ (m x ...)
          (z y ...)
          . rest)
       (begin
         (debug "Got literals ~a\n" #'(x ...))
         (debug "M is ~a, = to #%parens is ~a\n" #'m (free-identifier=? #'#%parens #'m))
         (debug "Z is ~a, = to #%braces is ~a\n" #'z (free-identifier=? #'#%braces #'z))
         (debug "Rest is ~a\n" (syntax->datum #'rest))
         (raise-syntax-error 'honu-macro "f1" stx))]
      [else (raise-syntax-error 'honu-macro "fail" stx)]
      )))
