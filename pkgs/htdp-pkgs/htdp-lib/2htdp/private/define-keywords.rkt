#lang racket/base

;; ---------------------------------------------------------------------------------------------------
;; provide a mechanism for defining the shape of big-bang and universe clauses 
;; to specify individual clauses see syn-aux.rkt

(provide define-keywords DEFAULT)

(require racket/class
         (for-syntax racket/base syntax/parse))

(define-syntax (DEFAULT stx)
  (raise-syntax-error 'DEFAULT "used out of context" stx))

(define-syntax (define-keywords stx)
  (syntax-parse stx #:literals (DEFAULT) 
    [(_ the-list super-list define-create
        (kw:identifier 
         (~optional kw-alt:identifier #:defaults ((kw-alt #'kw)))
         (~optional (~seq DEFAULT default:expr))
         coerce:expr) ...)
     (let* ([defs (attribute default)])
       #`(begin
           ;; define and create list of keywords and associated values 
           (define-for-syntax the-list
             (list* (list #'kw #'kw-alt (coerce ''kw) default) ... super-list))
           ;; define and provide keywords
           (provide (rename-out (kw  kw-alt) ...))
           (provide kw ...) 
           (define-syntaxes (kw ...)
             (values (lambda (x) (raise-syntax-error 'kw "used out of context" x)) ...))
           
           ;; a macro for creating functions that instantiate the proper object
           ;; (define-create para ...) :: additional parameters for the new func
           (define-syntax (define-create stx)
             (syntax-case stx ()
               [(_ para (... ...))
                (let*-values
                    ([(kwds defs)
                      (values (map car the-list) '())]
                     ;; the defaults list defs is no longer needed
                     [(args) (lambda (para*) (append para* (foldr cons '() kwds)))]
                     [(body) (lambda (para*) (map (lambda (x) `(,x ,x)) (append para* kwds)))])
                  (let ([para* (syntax->list #'(para (... ...)))])
                    #`(lambda (%)
                        (lambda #,(args para*)
                          (lambda ()
                            (new % #,@(body para*)))))))]))))]))
