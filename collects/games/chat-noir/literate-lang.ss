#lang scheme

(provide (except-out (all-from-out scheme)
                     #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out scribble/basic
                       scribble/manual)
         chunk)

(require (for-syntax scheme/base
                     syntax/boundmap
                     scheme/list
                     syntax/kerncase)
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

(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ name expr ...)
     (cond [(not (identifier? #'name))
            (raise-syntax-error #f "expected a chunk name" stx #'name)]
           [(not (regexp-match? #rx"^<.*>$" (symbol->string (syntax-e #'name))))
            (raise-syntax-error
             #f "chunk names must begin and end with angle brackets, <...>"
             stx #'name)]
           [else (add-to-block! #'name (syntax->list #'(expr ...)))
                 #`(void)])]))

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
     (let ([body-code
            (let loop ([exprs (syntax->list #'(expr ...))])
              (cond
                [(null? exprs) null]
                [else
                 (let ([expanded 
                        (local-expand (car exprs)
                                      'module 
                                      (append (kernel-form-identifier-list)
                                              (syntax->list #'(provide
                                                               require
                                                               #%provide
                                                               #%require))))])
                   (syntax-case expanded (begin)
                     [(begin rest ...)
                      (append (loop (syntax->list #'(rest ...)))
                              (loop (cdr exprs)))]
                     [(id . rest)
                      (ormap (lambda (kw) (free-identifier=? #'id kw))
                             (syntax->list #'(require
                                              provide
                                              chunk
                                              #%require
                                              #%provide)))
                      (cons expanded (loop (cdr exprs)))]
                     [else (loop (cdr exprs))]))]))])
       
       (with-syntax ([(body-code ...) body-code])
         #'(#%module-begin 
            body-code ...
            (tangle))))]))
