#lang racket/base
(require (for-syntax racket/base))

(define-syntax-rule (provide-except-unsafe (ulib ...) u! id ...)
  (begin
    (require ulib ...)
    (provide (except-out (all-from-out ulib ...) id ...))
    (define-syntax (u! stx)
      (syntax-case stx ()
        [(_) (with-syntax ([lib+ids (datum->syntax stx `((,#'combine-in ulib ...) id ...))])
               #'(require (only-in . lib+ids)))]))))

(provide-except-unsafe (ffi/unsafe ffi/unsafe/cvector ffi/vector) unsafe!
                       
 free end-stubborn-change
 ptr-ref ptr-set! cast
 make-sized-byte-string
 memcpy memmove memset
 malloc-immobile-cell free-immobile-cell
 malloc
 ffi-lib
 ffi-obj-ref
 get-ffi-obj
 set-ffi-obj!
 make-c-parameter
 define-c
 define-fun-syntax
 make-cvector*
 cpointer-tag set-cpointer-tag!
 cpointer-has-tag? cpointer-push-tag!
 cblock->list
 cblock->vector)

(provide provide* define-unsafer
         unsafe!)

;; This module is full of unsafe bindings that are not provided to requiring
;; modules.  Instead, an `unsafe!' binding is provided that makes these unsafe
;; bindings available.  The following two syntaxes do that: `provide*' is like
;; `provide', but using `(unsafe id)' registers an unsafe binding.  Then,
;; `define-unsafer' should be used with a binding that will expose the unsafe
;; bindings.  This might move elsewhere at some point if it turns out to be
;; useful in other contexts.
(provide provide* define-unsafer)
(define-syntaxes (provide* define-unsafer)
  (let ((unsafe-bindings '()))
    (values
     (lambda (stx)
       (syntax-case stx ()
         [(_ p ...)
          (let loop ([provides '()]
                     [unsafes  '()]
                     [ps (syntax->list #'(p ...))])
            (if (null? ps)
              (begin (set! unsafe-bindings
                           (append unsafe-bindings (reverse unsafes)))
                     (with-syntax ([(p ...) provides]) #'(provide p ...)))
              (syntax-case (car ps) (unsafe)
                [(unsafe u)
                 (syntax-case #'u (rename-out)
                   [(rename-out [from to])
                    (loop provides (cons (cons #'from #'to) unsafes) (cdr ps))]
                   [id (identifier? #'id)
                    (loop provides (cons (cons #'id #'id) unsafes) (cdr ps))]
                   [_
                    (raise-syntax-error 'provide* "bad unsafe usage"
                                        (car ps) stx)])]
                [_ (loop (cons (car ps) provides) unsafes (cdr ps))])))]))
     (lambda (stx)
       (syntax-case stx ()
         [(_ unsafe)
          (with-syntax ([(from ...)  (map car unsafe-bindings)]
                        [(to   ...)  (map cdr unsafe-bindings)]
                        [(id   ...) (generate-temporaries unsafe-bindings)])
            (set! unsafe-bindings '())
            #'(begin
                (provide (protect-out unsafe))
                (define-syntax (unsafe stx)
                  (syntax-case stx ()
                    [(_) (with-syntax ([(id ...) (list (datum->syntax
                                                        stx 'to stx)
                                                       ...)])
                           #'(begin (define-syntax id
                                      (make-rename-transformer (syntax-property
                                                                (syntax-property
                                                                 #'from
                                                                 'not-provide-all-defined
                                                                 #t)
                                                                'nominal-id
                                                                'to)))
                                    ...))]))))])))))
