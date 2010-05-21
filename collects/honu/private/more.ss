#lang scheme

(require "honu-typed-scheme.ss"
         "literals.ss"
         syntax/parse
         mzlib/trace
         "syntax.ss"
         (for-syntax syntax/parse
                     syntax/stx
                     racket/list
                     "syntax.ss"
                     (only-in racket (... scheme-ellipses))
                     "literals.ss")
         (for-template "honu-typed-scheme.ss"
                       "literals.ss"
                       "syntax.ss"
                       (only-in racket ...)
                       ))

(provide (all-defined-out))

(define (combine-syntax lexical . all)
  (define consed (for/fold ([item all])
                           ([out '()])
                           (cons item out)))
  (datum->syntax lexical consed lexical))

(define (replace-commas stuff)
  (printf "Replace commas with: ~a\n" (syntax->datum stuff))
  (syntax-parse stuff #:literals (ellipses-comma)
    #;
    [((ellipses-comma (z ...)) thing blah ...)
     (define (maybe-apply-raw stx)
       (syntax-parse stuff #:literals (ellipses-comma)
                     [((ellipses-comma x) . rest)
                      (if (raw-scheme? #'x)
                        (apply-scheme-syntax stx)
                        stx)]))
     (printf "Thing ~a and blah ~a replaced ~a\n" #'thing #'(blah ...) (replace-commas #'(thing blah ...)))
     (with-syntax ([(rest ...) (replace-commas #'(thing blah ...))])
       (datum->syntax stuff (cons (maybe-apply-raw #'(z ...)) (cons #'honu-comma #'(rest ...)))
                      stuff
                      stuff)
       #;
       #'(z honu-comma rest ...))]
    [((ellipses-comma z) thing blah ...)
     #;
     (printf "Thing ~a and blah ~a replaced ~a\n" #'thing #'(blah ...) (replace-commas #'(thing blah ...)))
     (with-syntax ([(rest ...) (replace-commas #'(thing blah ...))])
       #;
       (combine-syntax stuff #'z #'honu-comma #'(rest ...))
       (datum->syntax stuff (cons #'z (cons #'honu-comma #'(rest ...)))
                      stuff
                      stuff)
       #;
       #'(z honu-comma rest ...))]
    [(front (ellipses-comma z) thing more ...)
     (define (maybe-apply-raw stx)
       (syntax-parse stuff #:literals (ellipses-comma)
                     [(front (ellipses-comma x) . rest)
                      (if (raw-scheme? #'x)
                        (apply-scheme-syntax stx)
                        stx)]))
     (with-syntax ([front* (replace-commas #'front)]
                   [(rest* ...) (replace-commas #'(thing more ...))])
       (datum->syntax stuff (cons #'front* (cons #'z (cons #'honu-comma #'(rest* ...))))
                      stuff
                      stuff))]
    #;
    [(front (ellipses-comma (z ...)) thing more ...)
     (define (maybe-apply-raw stx)
       (syntax-parse stuff #:literals (ellipses-comma)
                     [(front (ellipses-comma x) . rest)
                      (if (raw-scheme? #'x)
                        (apply-scheme-syntax stx)
                        stx)]))
     (with-syntax ([front* (replace-commas #'front)]
                   [(rest* ...) (replace-commas #'(thing more ...))])
       (datum->syntax stuff (cons #'front*
                                  (cons (maybe-apply-raw (datum->syntax stuff #'(z ...) stuff))
                                        (cons #'honu-comma #'(rest* ...))))
                      stuff
                      stuff))]
    #;
    [((ellipses-comma (z ...))) (datum->syntax stuff #'(z ...) stuff stuff)]
    [((ellipses-comma z)) (datum->syntax stuff #'(z) stuff stuff)]
    [(z rest ...)
     (with-syntax ([z* (replace-commas #'z)]
                   [(rest* ...) (replace-commas #'(rest ...))])
       #;
       (combine-syntax stuff #'z #'(rest* ...))
       (datum->syntax stuff
                      (cons #'z* #'(rest* ...))
                      stuff stuff)
       #;
       #'(z* rest* ...))]
    [else stuff]))

;; (trace replace-commas)

(define-syntax (fix-template stuff)
  (define (fix stuff)
    (printf "Macro fix template for ~a\n" (syntax->datum stuff))
    (syntax-parse stuff #:literals (ellipses-comma)
      [(any ellipses-comma rest ...)
       (define (addit item)
         (with-syntax ([i item])
           #'(i honu-comma)))
       (define (remove-last list)
         (take list (sub1 (length list))))
       (define (add-commas stuff)
         (remove-last (apply append (map syntax->list (map addit (syntax->list stuff))))))
       (with-syntax ([(any* ...) (add-commas #'any)]
                     [(rest* ...) (fix #'(rest ...))])
         #'(any* ... rest* ...))]
      [(one rest ...)
       (with-syntax ([one* (fix #'one)]
                     [(rest* ...) (fix #'(rest ...))])
         (datum->syntax stuff (cons #'one*
                                    #'(rest* ...))
                        stuff stuff)
         #;
         #'(one* rest* ...))]
      [else stuff]))
  (define (replace stuff)
    (syntax-parse stuff #:literals (ellipses-comma)
      [(a ellipses-comma rest ...)
       (with-syntax ([a* (replace #'a)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons
                          (cons #'ellipses-comma (cons #'a* '())
                                #;
                                (if (stx-pair? #'a*)
                                  #'a*
                                  (cons #'a* '())))
                          (cons
                            #'(... ...)
                            #'(rest* ...)))
                        stuff stuff)
         #;
         #'((ellipses-comma a*) (... ...) rest* ...))]
      [(z rest ...)
       (with-syntax ([z* (replace #'z)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons #'z* #'(rest* ...))
                        stuff stuff)
         #;
         #'(z* rest* ...))]
      [else stuff]))
  
  (printf "Do fix template for ~a\n" (syntax->datum stuff))
  (syntax-parse stuff
    [(_ blah)
     (let ([replaced (replace #'blah)])
       (printf "Replaced ~a\n" (syntax->datum replaced))
       (with-syntax ([out2 replaced])
         (let ([x #'(apply-scheme-syntax (replace-commas #'out2))])
           (printf "Final syntax ~a\n" (syntax->datum x))
           x)))]
    #;
    [(_ blah ...) (fix #'(blah ...))]))

(define-honu-syntax honu-syntax
  (lambda (stx ctx)
    (syntax-parse stx #:literals (semicolon #%parens)
      [(_ (#%parens expr ...) semicolon . rest)
       (values
         (lambda ()
           (define (show-pattern-variables what)
             (cond
               [(syntax-pattern-variable? what) (printf "~a is a pattern variable\n") what]
               [(stx-pair? what) (for-each show-pattern-variables (syntax->list what))]
               [else (printf "~a is *not* a pattern variable\n" what)]))

           #;
           (printf "Original code is ~a\n" (syntax->datum #'(expr ...)))
           #;
           (printf "Expanded is ~a\n" (syntax->datum (expand-syntax-once #'(expr ...))))
           #;
           (for-each show-pattern-variables (syntax->list #'(expr ...)))
           ;; outer is relative phase 1, inner is relative phase 0
           #|
           #'#'(honu-unparsed-begin expr ...)
           |#

           #;
           (syntax (fix-template (syntax (honu-unparsed-begin expr ...))))

           #;
           (with-syntax ([a #'(fix-template #'(honu-unparsed-begin expr ...))])
             #'a)

           #'(fix-template (honu-unparsed-begin expr ...))

           #;
           #'(fix-template (expr ...))

           #;
           (apply-scheme-syntax #'(fix-template (expr ...)))

           #;
           (let ([x #'(fix-template (honu-unparsed-begin expr ...))])
             (printf "Final syntax ~a\n" (syntax->datum x))
             x)

           #;
           #'(fix-template 1 2 3)

           #;
           (with-syntax ([(out ...) (local-expand #'(expr ...) 'expression '())])
             #'(honu-unparsed-begin out ...)))
         #'rest)])))
