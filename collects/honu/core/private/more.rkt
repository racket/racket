#lang racket/base

#|

(require "honu-typed-scheme.rkt"
         "literals.rkt"
         syntax/parse
         mzlib/trace
         "syntax.rkt"
         (for-syntax syntax/parse
                     syntax/stx
                     racket/list
                     racket/base
                     "debug.rkt"
                     "contexts.rkt"
                     "syntax.rkt"
                     (only-in racket (... scheme-ellipses))
                     "literals.rkt")
         (for-template "honu-typed-scheme.rkt"
                       "literals.rkt"
                       "syntax.rkt"
                       (only-in racket ...)
                       ))

(provide (all-defined-out))

(define (combine-syntax lexical . all)
  (define consed (for/fold ([item all])
                           ([out '()])
                           (cons item out)))
  (datum->syntax lexical consed lexical))

(define (replace-commas stuff)
  (syntax-parse stuff #:literals (ellipses-comma ellipses-comma*)
    [((ellipses-comma* z ...) thing blah ...)
     (with-syntax ([(rest ...) (replace-commas #'(thing blah ...))])
       (datum->syntax stuff
                      #'(z ... honu-comma rest ...)
                      ;; `(1 ,#'(z ...) ,#'honu-comma ,#'(rest ...))
                      stuff
                      stuff))]
    [((ellipses-comma z) thing blah ...)
     (with-syntax ([(rest ...) (replace-commas #'(thing blah ...))])
       (datum->syntax stuff (cons #'z (cons #'honu-comma #'(rest ...)))
                      stuff
                      stuff))]
    [(front (ellipses-comma* z ...) thing more ...)
     (with-syntax ([front* (replace-commas #'front)]
                   [(rest* ...) (replace-commas #'(thing more ...))])
       (datum->syntax stuff #'(front z ... honu-comma rest* ...) stuff stuff))]
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
    [((ellipses-comma* z ...)) (datum->syntax stuff #'(z ...) stuff stuff)]
    [((ellipses-comma z)) (datum->syntax stuff #'(z) stuff stuff)]
    [(z rest ...)
     (with-syntax ([z* (replace-commas #'z)]
                   [(rest* ...) (replace-commas #'(rest ...))])
       (datum->syntax stuff
                      (cons #'z* #'(rest* ...))
                      stuff stuff))]
    [else stuff]))

;; (trace replace-commas)

(define-syntax (fix-template stuff)
  (define (fix stuff)
    (debug "Macro fix template for ~a\n" (syntax->datum stuff))
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
                        stuff stuff))]
      [else stuff]))
  (define (replace2 stuff)
    (syntax-parse stuff #:literals (ellipses-comma ellipses-repeat #%parens)
      [(ellipses-repeat (#%parens ellipses-comma things-to-repeat ...) rest ...)
       (with-syntax ([(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons
                          (cons #'ellipses-comma* #'(things-to-repeat ...))
                          (cons
                            #'(... ...)
                            #'(rest* ...)))
                        stuff stuff))]
      [(a ellipses-comma rest ...)
       (with-syntax ([a* (replace #'a)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons
                          (cons #'ellipses-comma (cons #'a* '()))
                          (cons
                            #'(... ...)
                            #'(rest* ...)))
                        stuff stuff))]
      [(z rest ...)
       (with-syntax ([z* (replace #'z)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons #'z* #'(rest* ...))
                        stuff stuff))]
      [else stuff]))

  (define (replace stuff)
    (syntax-parse stuff
      #:literals (;; honu-comma
                  ;; FIXME! Use a literal-set and #:at instead of this
                  [honu-comma honu-comma #:phase (sub1 (syntax-local-phase-level))]
                  [ellipses ...] ellipses-comma ellipses-repeat #%parens)
      [(a honu-comma ellipses rest ...)
       (with-syntax ([a* (replace #'a)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons
                          (cons #'ellipses-comma (cons #'a* '()))
                          (cons
                            #'(... ...)
                            #'(rest* ...)))
                        stuff stuff))]
      [(z rest ...)
       (with-syntax ([z* (replace #'z)]
                     [(rest* ...) (replace #'(rest ...))])
         (datum->syntax stuff
                        (cons #'z* #'(rest* ...))
                        stuff stuff))]
      [else stuff]
      ))
  
  (debug "Do fix template for ~a\n" (syntax->datum stuff))
  (syntax-parse stuff
    [(_ blah)
     (let ([replaced (replace #'blah)])
       (debug "Replaced ~a\n" (syntax->datum replaced))
       (with-syntax ([out2 replaced])
         (let ([x #'(apply-scheme-syntax (replace-commas #'out2))])
           (debug "Final syntax ~a\n" (syntax->datum x))
           x)))]))

(define-syntax-rule (honu-syntax-maker maker unparsed)
  (define-honu-syntax maker
    (lambda (stx ctx)
      (syntax-parse stx #:literal-sets ([cruft #:at unparsed])
        [(_ (#%parens expr (... ...)) semicolon . rest)
         (values
           (lambda ()
             (define (show-pattern-variables what)
               (cond
                 [(syntax-pattern-variable? what) (debug "~a is a pattern variable\n") what]
                 [(stx-pair? what) (for-each show-pattern-variables (syntax->list what))]
                 [else (debug "~a is *not* a pattern variable\n" what)]))

             (debug "Making unparsed syntax with `~a'\n" (syntax->datum #'(expr (... ...))))

             #'(fix-template (unparsed expr (... ...))))
           #'rest)]
        [else (raise-syntax-error 'maker "you have used this incorrectly")]
        ))))

(honu-syntax-maker honu-syntax honu-unparsed-begin)
(honu-syntax-maker honu-expression-syntax honu-unparsed-expr)

|#
