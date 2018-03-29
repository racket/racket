#lang racket/base

(require (for-syntax syntax/parse
                     racket/base)
         ffi/unsafe)

(provide (protect-out define-ffi-definer)
         provide-protected
         make-not-available)

(define (make-not-available id)
  (lambda ()
    (lambda args
      (error id "implementation not found; ~a"
             (if (null? args)
                 "no arguments provided"
                 (apply
                  string-append
                  "arguments:"
                  (let loop ([args args])
                    (if (null? args)
                        null
                        (cons (format " ~e"
                                      (car args))
                              (loop (cdr args)))))))))))

(define-syntax-rule (provide-protected p ...)
  (provide (protect-out p ...)))

(begin-for-syntax
  (define (make-ffi-definer-transformer the-ffi-lib  ;; Identifier
                                        provide-form ;; Identifier/#'#f
                                        define-form  ;; Identifier
                                        default-make-fail ;; Identifier
                                        make-c-id)  ;; Identifier/#'#f
    ;; do-make-c-id : Identifier -> Identifier
    (define (do-make-c-id id)
      (cond [(identifier? make-c-id)
             (define result ((syntax-local-value make-c-id) id))
             (unless (identifier? result)
               (raise-syntax-error #f "invalid make-c-id result" make-c-id))
             result]
            [else id]))
    (with-syntax ([the-ffi-lib  the-ffi-lib]
                  [provide      provide-form]
                  [define-form  define-form]
                  [default-make-fail default-make-fail])
      (lambda (stx)
        (syntax-parse stx
          [(_ s-id:id type:expr
              (~seq (~or (~optional (~seq #:c-id c-id:id)
                                    #:defaults ([c-id (do-make-c-id #'s-id)])
                                    #:name "#:c-id keyword")
                         (~optional (~seq #:wrap wrapper:expr)
                                    #:defaults ([wrapper #'values])
                                    #:name "#:wrap keyword")
                         (~optional (~or (~seq #:make-fail make-fail:expr)
                                         (~seq #:fail fail:expr))
                                    #:defaults ([make-fail #'default-make-fail])))
                    ...))
           (with-syntax ([fail (if (attribute fail)
                                   #'fail
                                   #'(make-fail 's-id))])
             (with-syntax ([def (syntax/loc stx
                                  (define-form s-id
                                    (wrapper (get-ffi-obj 'c-id the-ffi-lib type fail))))])
               (if (syntax-e #'provide)
                   (syntax/loc stx
                     (begin
                       (provide s-id)
                       def))
                   #'def)))])))))

(define-syntax (define-ffi-definer stx)
  (syntax-parse stx
    [(_ define-:id ffi-lib:expr
        (~seq (~or (~optional (~seq #:provide provide-form:id)
                              #:defaults ([provide-form #'#f])
                              #:name "#:provide keyword")
                   (~optional (~seq #:define define-form:id)
                              #:defaults ([define-form #'define])
                              #:name "#:define keyword")
                   (~optional (~seq #:default-make-fail default-make-fail:expr)
                              #:defaults ([default-make-fail #'(lambda (id) #f)])
                              #:name "#:default-make-fail keyword")
                   (~optional (~seq #:make-c-id make-c-id:id)
                              #:defaults ([make-c-id #'#f])
                              #:name "#:make-c-id"))
              ...))
     #`(begin
         (define the-ffi-lib
           (let ([v ffi-lib])
             (if (or (not v) (ffi-lib? v))
                 v
                 (raise-type-error 'define-ffi-definer
                                   "ffi-lib or #f"
                                   v))))
         (define (the-default-make-fail sym) (default-make-fail sym))
         (define-syntax define-
           (make-ffi-definer-transformer
            (quote-syntax the-ffi-lib)
            (quote-syntax provide-form)
            (quote-syntax define-form)
            (quote-syntax the-default-make-fail)
            (quote-syntax make-c-id))))]))
