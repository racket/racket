#lang scheme/base

(provide (except-out (all-from-out scheme/base) #%module-begin)
         (rename-out [module-begin #%module-begin])
         chunk)

(require (for-syntax scheme/base syntax/boundmap scheme/list syntax/kerncase))

(begin-for-syntax
  (define main-id #f)
  (define (mapping-get mapping id)
    (free-identifier-mapping-get mapping id (lambda () '())))
  ;; maps a chunk identifier to its collected expressions
  (define chunks (make-free-identifier-mapping))
  ;; maps a chunk identifier to all identifiers that are used to define it
  (define chunk-groups (make-free-identifier-mapping))
  (define (get-chunk id)
    (map syntax-local-introduce (mapping-get chunks id)))
  (define (add-to-chunk! id exprs)
    (unless main-id (set! main-id id))
    (free-identifier-mapping-put!
     chunk-groups id
     (cons (syntax-local-introduce id) (mapping-get chunk-groups id)))
    (free-identifier-mapping-put!
     chunks id
     `(,@(mapping-get chunks id) ,@(map syntax-local-introduce exprs)))))

;; This is the code-view implementation of `chunk', see
;; "literate-doc-wrapper.ss" for the doc-view implementation.  Defines
;; `chunk' as a macro that collects the code to be later reassembled
;; by `tangle'.
(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ name expr ...)
     (cond [(not (identifier? #'name))
            (raise-syntax-error #f "expected a chunk name" stx #'name)]
           [(not (regexp-match? #rx"^<.*>$" (symbol->string (syntax-e #'name))))
            (raise-syntax-error
             #f "chunk names must begin and end with angle brackets, <...>"
             stx #'name)]
           [else (add-to-chunk! #'name (syntax->list #'(expr ...)))
                 #'(void)])]))

(define-syntax (tangle stx)
  (define chunk-mentions '())
  (define body
    (let loop ([block (get-chunk main-id)])
      (append-map
       (lambda (expr)
         (if (identifier? expr)
           (let ([subs (get-chunk expr)])
             (if (pair? subs)
               (begin (set! chunk-mentions (cons expr chunk-mentions))
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
                                    (mapping-get chunk-groups m)))
                             chunk-mentions)])
    #`(begin body ... (let ([b-id (void)]) b-use) ...)))

(define-syntax (literate-begin stx)
  (syntax-case stx ()
    [(_ . exprs)
     (let loop ([exprs #'exprs])
       (syntax-case exprs ()
         [() #'(tangle)]
         [(expr . exprs)
          (let ([expanded
                 (local-expand #'expr
                               'module
                               (append (kernel-form-identifier-list)
                                       (syntax->list #'(provide
                                                        require
                                                        chunk
                                                        #%provide
                                                        #%require))))])
            (syntax-case expanded (begin chunk require/chunk)
              [(begin rest ...)
               (loop (datum->syntax
                      expanded
                      (append
                       (syntax->list #'(rest ...))
                       #'exprs)))]
              [(id . _)
               (ormap (lambda (kw) (free-identifier=? #'id kw))
                      (syntax->list #'(require
                                       provide
                                       chunk
                                       #%require
                                       #%provide)))
               #`(begin #,expanded (literate-begin . exprs))]
              [else (loop #'exprs)]))]))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ id exprs . body)
     #'(#%module-begin
        (literate-begin id exprs . body))]))
