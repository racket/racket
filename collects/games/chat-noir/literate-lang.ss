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
  (define (mapping-get mapping id)
    (free-identifier-mapping-get mapping id (lambda () '())))
  ;; maps a block identifier to its collected expressions
  (define code-blocks (make-free-identifier-mapping))
  ;; maps a block identifier to all identifiers that are used to define it
  (define block-groups (make-free-identifier-mapping))
  (define (get-block id)
    (map syntax-local-introduce (mapping-get code-blocks id)))
  (define (add-to-block! id exprs)
    (unless main-id (set! main-id id))
    (free-identifier-mapping-put!
     block-groups id
     (cons (syntax-local-introduce id) (mapping-get block-groups id)))
    (free-identifier-mapping-put!
     code-blocks id
     `(,@(mapping-get code-blocks id) ,@(map syntax-local-introduce exprs)))))

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
  (define block-mentions '())
  (define body
    (let loop ([block (get-block main-id)])
      (append-map
       (lambda (expr)
         (if (identifier? expr)
             (let ([subs (get-block expr)])
               (if (pair? subs)
                   (begin (set! block-mentions (cons expr block-mentions))
                          (loop subs))
                   (list expr)))
             (let ([subs (syntax->list expr)])
               (if subs
                   (list (loop subs))
                   (list expr)))))
       block)))
  (with-syntax ([(body ...) body]
                ;; construct arrows manually
                [((b-use b-id) ...)
                 (append-map (lambda (m)
                               (map (lambda (u)
                                      (list m (syntax-local-introduce u)))
                                    (mapping-get block-groups m)))
                             block-mentions)])
    #`(begin body ... (let ([b-id (void)]) b-use) ...)))

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
