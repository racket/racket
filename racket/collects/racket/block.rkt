(module block '#%kernel
  (#%require "private/define.rkt"
             (for-syntax '#%kernel
                         "private/stx.rkt"
                         "private/small-scheme.rkt"
                         "private/stxcase-scheme.rkt"
                         "private/qqstx.rkt"
                         syntax/intdef))

(#%provide block)

(define-values-for-syntax (make-context)
  (let-values ([(struct: mk ? ref set)
                (make-struct-type 'in-liberal-define-context #f 0 0 #f
                                  (list (cons prop:liberal-define-context #t)))])
    mk))

(define-syntax (block stx)
  ;; Body can have mixed exprs and defns. Wrap expressions with
  ;; `(define-values () ... (values))' as needed, and add a (void)
  ;; at the end if needed.
  (let* ([def-ctx (syntax-local-make-definition-context)]
         [ctx (list (make-context))]
         ;; [kernel-forms (kernel-form-identifier-list)]
         [stoplist (list #'begin #'define-syntaxes #'define-values)]
         [init-exprs (let ([v (syntax->list stx)])
                       (unless v (raise-syntax-error #f "bad syntax" stx))
                       (cdr v))]
         [exprs
          (let loop ([todo init-exprs] [r '()])
            (if (null? todo)
              (reverse r)
              (let ([expr (local-expand (car todo) ctx stoplist def-ctx)]
                    [todo (cdr todo)])
                (syntax-case expr (begin define-syntaxes define-values)
                  [(begin . rest)
                   (loop (append (syntax->list #'rest) todo) r)]
                  [(define-syntaxes (id ...) rhs)
                   (andmap identifier? (syntax->list #'(id ...)))
                   (with-syntax ([rhs (local-transformer-expand
                                       #'rhs 'expression null)])
                     (syntax-local-bind-syntaxes
                      (syntax->list #'(id ...))
                      #'rhs def-ctx)
                     (with-syntax ([(id ...) (map syntax-local-identifier-as-binding
                                                  (syntax->list #'(id ...)))])
                       (loop todo (cons #'(define-syntaxes (id ...) rhs) r))))]
                  [(define-values (id ...) rhs)
                   (andmap identifier? (syntax->list #'(id ...)))
                   (let ([ids (syntax->list #'(id ...))])
                     (syntax-local-bind-syntaxes ids #f def-ctx)
                     (with-syntax ([(id ...) (map syntax-local-identifier-as-binding
                                                    (syntax->list #'(id ...)))])
                       (loop todo (cons #'(define-values (id ...) rhs) r))))]
                  [else (loop todo (cons expr r))]))))])
    (internal-definition-context-seal def-ctx)
    (let loop ([exprs exprs]
               [prev-stx-defns null]
               [prev-defns null]
               [prev-exprs null])
      (cond
        [(null? exprs)
         (internal-definition-context-track
          def-ctx
          #`(letrec-syntaxes+values
             #,(map stx-cdr (reverse prev-stx-defns))
             #,(map stx-cdr (reverse prev-defns))
             #,@(if (null? prev-exprs)
                    (list #'(void))
                    (reverse prev-exprs))))]
        [(and (stx-pair? (car exprs))
              (identifier? (stx-car (car exprs)))
              (free-identifier=? #'define-syntaxes (stx-car (car exprs))))
         (loop (cdr exprs)
               (cons (car exprs) prev-stx-defns)
               prev-defns
               prev-exprs)]
        [(and (stx-pair? (car exprs))
              (identifier? (stx-car (car exprs)))
              (free-identifier=? #'define-values (stx-car (car exprs))))
         (loop (cdr exprs)
               prev-stx-defns
               (cons (car exprs)
                     (append
                      (map (lambda (expr)
                             #`(define-values () (begin #,expr (values))))
                           prev-exprs)
                      prev-defns))
               null)]
        [else (loop (cdr exprs)
                    prev-stx-defns
                    prev-defns
                    (cons (car exprs) prev-exprs))]))))

)
