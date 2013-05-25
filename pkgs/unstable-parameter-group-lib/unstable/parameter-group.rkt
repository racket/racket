#lang racket/base

;; Provides a way to treat a group of parameters as a parameter itself

(require racket/match racket/list
         (for-syntax racket/base racket/syntax syntax/parse racket/match)
         ;; Can't make parameter lists first-class values without these:
         (only-in '#%paramz parameterization-key extend-parameterization))

(provide parameter-group? define-parameter-group parameterize/group parameterize*/group)

;; A wrapper for a group of parameters that acts like a parameter-procedure
(struct parameter-group (proc extract-extension)
  #:property prop:procedure (struct-field-index proc))

;; Given the left and right side of a 'parameterize' binding, returns a list of alternating
;; parameters and parameter values
(define (extract-extension p v)
  (cond [(parameter? p)        (list p v)]
        [(parameter-group? p)  ((parameter-group-extract-extension p) v)]
        [else  (raise-type-error 'parameterize "parameter or parameter-group" p)]))

(define-syntax (define-parameter-group stx)
  (syntax-parse stx
    [(_ name:id (param ...) #:struct struct-name:id)
     (with-syntax ([(param-name ...)  (generate-temporaries (syntax->list #'(param ...)))]
                   [(temp-name ...)   (generate-temporaries (syntax->list #'(param ...)))]
                   [proc              (format-id #'name "~a-proc" #'name)]
                   [extract           (format-id #'name "~a-extract" #'name)])
       (syntax-protect
        (syntax/loc stx
          (define name
            (let ([param-name  param] ...)
              (unless (or (parameter? param-name) (parameter-group? param-name))
                (raise-type-error 'define-parameter-group "parameter or parameter-group" param-name))
              ...
              (define proc
                (case-lambda
                  [()   (struct-name (param-name) ...)]
                  [(v)  (match v
                          [(struct-name temp-name ...)  (param-name temp-name) ... (void)]
                          [_  (raise-type-error 'name (symbol->string 'struct-name) v)])]))
              (define (extract v)
                (match v
                  [(struct-name temp-name ...)  (append (extract-extension param-name temp-name) ...)]
                  [_  (raise-type-error 'name (symbol->string 'struct-name) v)]))
              (parameter-group proc extract))))))]
    [(_ name:id (param-name:id ...))
     (with-syntax ([struct-name  (format-id #'name "~a-value" #'name)])
       (syntax-protect
        (syntax/loc stx
          (begin (struct struct-name (param-name ...) #:transparent)
                 (define-parameter-group name (param-name ...) #:struct struct-name)))))]))

;; Corresponds to parameterize
(define-syntax (parameterize/group stx)
  (syntax-case stx ()
    [(_ () expr1 expr ...)
     (syntax-protect (syntax/loc stx (let () expr1 expr ...)))]
    [(_ ([p v] ...) expr1 expr ...)
     (with-syntax* ([(p-name ...)  (generate-temporaries (syntax->list #'(p ...)))]
                    [(p/v ...)  (apply append (map list 
                                                   (syntax->list #'(p-name ...))
                                                   (syntax->list #'(v ...))))])
       (syntax-protect
        (syntax/loc stx
          (with-continuation-mark parameterization-key
            (let ([p-name p] ...)
              (if (and (parameter? p-name) ...)
                  (extend-parameterization
                   (continuation-mark-set-first #f parameterization-key)
                   p/v ...)
                  (apply extend-parameterization
                         (continuation-mark-set-first #f parameterization-key)
                         (append (extract-extension p-name v) ...))))
            (let () expr1 expr ...)))))]))

;; Corresponds to parameterize*
(define-syntax parameterize*/group
  (syntax-rules ()
      [(_ () body1 body ...)
       (let () body1 body ...)]
      [(_ ([lhs1 rhs1] [lhs rhs] ...) body1 body ...)
       (parameterize/group
        ([lhs1 rhs1])
        (parameterize*/group
         ([lhs rhs] ...)
         body1 body ...))]))
