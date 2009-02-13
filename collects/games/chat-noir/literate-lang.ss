#lang scheme

(provide (except-out (all-from-out scheme)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out scribble/basic
                       scribble/manual)
         chunk)

(require (for-syntax scheme/base syntax/boundmap scheme/list)
         scribble/manual
         scribble/struct
         scribble/basic
         scribble/decode)

(begin-for-syntax
  (define main-id #f)
  (define code-blocks (make-free-identifier-mapping))
  (define (get-id-exprs id)
    (free-identifier-mapping-get code-blocks id (lambda () '())))
  (define (get-block id)
    (map syntax-local-introduce (get-id-exprs id)))
  (define (add-to-block! id exprs)
    (unless main-id (set! main-id id))
    (free-identifier-mapping-put!
     code-blocks id
     `(,@(get-id-exprs id) ,@(map syntax-local-introduce exprs)))))

(define :make-splice make-splice)

(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ name expr ...)
     (begin
       (unless (identifier? #'name)
         (raise-syntax-error #f "expected a chunk name" stx #'name))
       (unless (regexp-match #rx"^<.*>$" (symbol->string (syntax-e #'name)))
         (raise-syntax-error #f "chunk names must begin and end with angle brackets, <...>"
                             stx 
                             #'name))
       (add-to-block! #'name (syntax->list #'(expr ...)))
       #`(:make-splice
          (list
           (italic #,(format "~a = " (syntax-e #'name)))
           (schemeblock expr ...))))]))

(define-syntax (tangle stx)
  #`(begin
      #,@(let loop ([block (get-block main-id)])
           (append-map (lambda (expr)
                         (if (identifier? expr)
                             (let ([subs (get-block expr)])
                               (if (pair? subs) (loop subs) (list expr)))
                             (let ([subs (syntax->list expr)])
                               (if subs
                                   (list (loop subs))
                                   (list expr)))))
                       block))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin expr ...)
     (with-syntax ([doc (datum->syntax stx 'doc stx)]
                   ;; this forces expansion so `chunk' can appear anywhere, if
                   ;; it's allowed only at the toplevel, then there's no need
                   ;; for it
                   [(expr ...)
                    (map (lambda (expr) (local-expand expr 'module '()))
                         (syntax->list #'(expr ...)))])
       ;; define doc as the binding that has all the scribbled documentation
       #'(#%module-begin 
          (define doc '())
          (provide doc)
          (set! doc (cons expr doc)) ...
          (tangle)
          (set! doc (decode (reverse doc)))))]))
