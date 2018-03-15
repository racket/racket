#lang racket/base
(require "correlate-syntax.rkt"
         "../syntax/datum-map.rkt"
         "../common/make-match.rkt")

;; A "correlated" is the host's notion of syntax objects for
;; `compile-linklet`, which is an S-expression with source locations
;; and properties (but no scopes).

;; For historical reasons, the names here can be a bit confusing.  The
;; host layer provides functions named `syntax?`, `datum->syntax`,
;; etc., but these are wrapped here by functions with the names
;; `correlated?`, `datum->correlated`, etc. Additionally,
;; `racket/linklet` obtains the names `syntax?` etc directly from the
;; `#%kernel` primitive table, and provides them under the name
;; `correlated?` etc. This expander defines other functions with the
;; names `syntax?` etc (see "../syntax/syntax.rkt") which are the
;; syntax objects used by this expander.

;; When the expander is run as a regular Racket program, the host
;; notion of syntax is a full Racket syntax object, but the expander
;; ignores all but the contained datum, the properties, and the source
;; location.

;; When the expander is used as the expander for Racket on the older,
;; C-based runtime, it uses a C-level implementation of syntax
;; objects, Scheme_Stx, which contains only the features needed
;; here. In that implementation, the names implemented are `syntax?`,
;; etc.

;; When the expander is run as the Racket expander on the Chez
;; Scheme-based runtime, it uses a record named `correlated` which
;; provides only the features needed here. There, the implemented
;; operations are named `correlated?`, etc, but are provided to this
;; expander as `syntax?`, etc.

(provide correlate
         correlated?
         datum->correlated
         correlated-e
         correlated-cadr
         correlated-length
         correlated->list
         correlated->datum
         correlated-property
         correlated-property-symbol-keys
         define-correlated-match

         correlated-source
         correlated-line
         correlated-column
         correlated-position
         correlated-span)

(define (correlate src-e s-exp)
  (define e (datum->correlated s-exp src-e))
  (define maybe-n (syntax-property src-e 'inferred-name))
  (if maybe-n
      (syntax-property e 'inferred-name maybe-n)
      e))

(define (correlated? e)
  (syntax? e))

(define (datum->correlated d [srcloc #f])
  (datum->syntax #f d srcloc))

(define (correlated-e e)
  (if (syntax? e)
      (syntax-e e)
      e))

(define (correlated-cadr e)
  (car (correlated-e (cdr (correlated-e e)))))

(define (correlated-length e)
  (define l (correlated-e e))
  (and (list? l)
       (length l)))

(define (correlated->list e)
  (let loop ([e e])
    (cond
     [(list? e) e]
     [(pair? e) (cons (car e) (loop (cdr e)))]
     [(null? e) null]
     [(syntax? e) (loop (syntax-e e))]
     [else (error 'correlated->list "not a list")])))

(define (correlated->datum e)
  (datum-map e (lambda (tail? d) d) (lambda (tail? d)
                                      (if (syntax? d)
                                          (syntax->datum d)
                                          d))))

(define (correlated-property-symbol-keys e)
  (syntax-property-symbol-keys e))

(define correlated-property
  (case-lambda
    [(e k) (syntax-property e k)]
    [(e k v) (syntax-property e k v)]))

(define-define-match define-correlated-match
  syntax? syntax-e (lambda (false str e) (error str)))

(define (correlated-source s) (syntax-source s))
(define (correlated-line s) (syntax-line s))
(define (correlated-column s) (syntax-column s))
(define (correlated-position s) (syntax-position s))
(define (correlated-span s) (syntax-span s))
