#lang racket/base
(require "../common/contract.rkt"
         "syntax.rkt"
         "scope.rkt"
         "taint.rkt"
         (only-in '#%kernel regexp-replace*))

(provide (struct-out exn:fail:syntax)
         make-exn:fail:syntax
         (struct-out exn:fail:syntax:unbound)
         make-exn:fail:syntax:unbound

         do-raise-syntax-error
         
         raise-syntax-error
         raise-unbound-syntax-error

         set-current-previously-unbound!
         install-error-syntax->string-handler!)

(struct exn:fail:syntax exn:fail (exprs)
  #:extra-constructor-name make-exn:fail:syntax
  #:transparent
  #:property prop:exn:srclocs (lambda (e) (filter values (map syntax-srcloc (exn:fail:syntax-exprs e))))
  #:guard (lambda (str cm exprs info)
            (unless (and (list? exprs)
                         (andmap syntax? exprs))
              (raise-argument-error 'exn:fail:syntax "(listof syntax?)" exprs))
            (values str cm exprs)))
(struct exn:fail:syntax:unbound exn:fail:syntax ()
  #:extra-constructor-name make-exn:fail:syntax:unbound
  #:transparent)

(define/who (raise-syntax-error given-name message
                                [expr #f] [sub-expr #f]
                                [extra-sources null]
                                [message-suffix ""])
  (do-raise-syntax-error who exn:fail:syntax given-name message
                         expr sub-expr
                         extra-sources
                         message-suffix))

(define/who (raise-unbound-syntax-error given-name message
                                        [expr #f] [sub-expr #f]
                                        [extra-sources null]
                                        [message-suffix ""])
  (do-raise-syntax-error who exn:fail:syntax:unbound given-name message
                         expr sub-expr
                         extra-sources
                         message-suffix))

(define (do-raise-syntax-error who exn:fail:syntax given-name message
                               expr sub-expr
                               extra-sources
                               message-suffix)
  (check who symbol? #:or-false given-name)
  (check who string? message)
  (unless (and (list? extra-sources)
               (andmap syntax? extra-sources))
    (raise-argument-error who "(listof syntax?)" extra-sources))
  (check who string? message-suffix)
  (define name
    (format "~a" (or given-name
                     (extract-form-name expr)
                     '?)))
  (define unbound-message
    ;; If we have unbound identifiers to check, then it's possible that the
    ;; true error is that the identifier should have been bound to a syntactic
    ;; form. So, list those identifiers. The list will be non-empty only for
    ;; phase >= 1.
    (let ([ids (current-previously-unbound)])
      (or (and (pair? ids)
               (format "\n  after encountering unbound identifier~a (which is possibly the real problem):~a"
                       (if (null? (cdr ids)) "" "s")
                       (apply string-append
                              (for/list ([id (in-list ids)])
                                (format "\n   ~s" (syntax-e id))))))
          "")))
  (define at-message
    (or (and sub-expr
             (error-print-source-location)
             (string-append "\n  at:" (syntax->string sub-expr)))
        ""))
  (define in-message
    (or (and expr
             (error-print-source-location)
             (string-append "\n  in:" (syntax->string expr)))
        ""))
  (define src-loc-str
    (or (and (error-print-source-location)
             (or (extract-source-location sub-expr)
                 (extract-source-location expr)))
        ""))
  (define e
    (exn:fail:syntax
     (string-append src-loc-str
                    name ": "
                    message
                    unbound-message
                    at-message
                    in-message
                    message-suffix)
     (current-continuation-marks)
     (map syntax-taint
          (if (or sub-expr expr)
              ;; accomodate `datum->syntax` failure similar to `->datum`:
              (with-handlers ([exn:fail:contract? (lambda (exn) extra-sources)])
                (cons (datum->syntax #f (or sub-expr expr))
                      extra-sources))
              extra-sources))))
  (unless (exn:fail:syntax? e)
    (raise-result-error who "exn:fail:syntax?" e))
  (raise e))

(define (extract-form-name s)
  (cond
   [(syntax? s)
    (define e (syntax-e s))
    (cond
     [(symbol? e) e]
     [(and (pair? e)
           (identifier? (car e)))
      (syntax-e (car e))]
     [else #f])]
   [else #f]))

(define (extract-source-location s)
  (and (syntax? s)
       (syntax-srcloc s)
       (let ([str (srcloc->string (syntax-srcloc s))])
         (and str
              (string-append str ": ")))))

;; `raise-syntax-error` is meant to accept either syntax objects or
;; S-expressions, and it has traditionally supported hybird values by
;; coercing to a syntax object and them back; in case the expression
;; cannot be represented as a syntax object due to cycles, though,
;; fall back to showing the value in raw form (instead of constraining
;; `raise-syntax-error`)
(define (->datum expr)
  (with-handlers ([exn:fail:contract? (lambda (exn) expr)])
    (syntax->datum (datum->syntax #f expr))))

;; Hook for the expander:
(define current-previously-unbound (lambda () #f))
(define (set-current-previously-unbound! proc)
  (set! current-previously-unbound proc))

;; if `v` prints on multiple lines, add indentation,
;; otherwise just add a space
(define (syntax->string v)
  (define str ((error-syntax->string-handler) v (error-print-width)))
  (if (regexp-match? #rx"\n" str)
      (string-append "\n   "
                     (regexp-replace* #rx"\n" str "\n   "))
      (string-append " " str)))

(define (install-error-syntax->string-handler!)
  (error-syntax->string-handler
   (let ([default-error-syntax->string-handler
           (lambda (v len)
             (unless (or (not len) (exact-nonnegative-integer? len))
               (raise-argument-error 'default-error-value->string-handler
                                     "(or/c exact-nonnegative-integer? #f)"
                                     len))
             (if len
                 (parameterize ([error-print-width (max 3 len)])
                   (format "~.s" (->datum v)))
                 (format "~s" (->datum v))))])
     default-error-syntax->string-handler)))
