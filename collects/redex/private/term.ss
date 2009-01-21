(module term mzscheme
  (require-for-syntax "term-fn.ss")
  (require "matcher.ss")
  
  (provide term term-let term-let-fn term-define-fn)
  
  (define (with-syntax* stx)
    (syntax-case stx ()
      [(_ () e) (syntax e)]
      [(_ (a b ...) e) (syntax (with-syntax (a) (with-syntax* (b ...) e)))]))
  
  (define-syntax (term stx)
    (syntax-case stx ()
      [(_ arg)
       #`(term-let-fn ((#,(datum->syntax-object stx 'in-hole)
                        (λ (x) 
                          (unless (and (list? x)
                                       (= 2 (length x)))
                            (error 'in-hole "expected two arguments, got ~s" x))
                          (apply plug x))))
                      (term/private arg))]))
  
  (define-syntax (term/private orig-stx)
    (define outer-bindings '())
    
    (define (rewrite stx)
      (let-values ([(rewritten has-term-let-bound-id?)
                    (rewrite/has-term-let-bound-id? stx)])
        rewritten))
    
    (define (rewrite/has-term-let-bound-id? stx)
      (let loop ([stx stx]
                 [depth 0])
        (syntax-case stx (unquote unquote-splicing in-hole in-named-hole hole hole-here)
          [(f arg ...)
           (and (identifier? (syntax f))
                (term-fn? (syntax-local-value (syntax f) (λ () #f))))
           (let-values ([(rewritten has-term-let-bound-id?) (loop (syntax (arg ...)) depth)])
             (cond
               [(or #t has-term-let-bound-id?)
                (let ([term-fn (syntax-local-value (syntax f) (λ () #f))])
                  (with-syntax ([f (term-fn-get-id term-fn)]
                                [(f-results) (generate-temporaries '(f-results))])
                    (let d-loop ([arg-dots rewritten]
                                 [fres (syntax f-results)]
                                 [func (syntax (lambda (x) (f (syntax-object->datum x))))]
                                 [depth depth])
                      (cond
                        [(zero? depth) 
                         (let ([res 
                                (with-syntax ([fres fres]
                                              [func func]
                                              [arg-dots arg-dots])
                                  (set! outer-bindings (cons (syntax [fres (func (quasisyntax arg-dots))])
                                                             outer-bindings))
                                  (syntax f-results))])
                           (values res #t))]
                        [else 
                         (with-syntax ([dots (quote-syntax ...)]
                                       [arg-dots arg-dots]
                                       [fres fres])
                           (d-loop (syntax (arg-dots dots))
                                   (syntax (fres dots))
                                   (with-syntax ([f func])
                                     (syntax (lambda (l) (map f (syntax->list l)))))
                                   (- depth 1)))]))))]
               [else
                (with-syntax ([(args ...) rewritten])
                  (syntax/loc stx (f args ...)))]))]
          [f
           (and (identifier? (syntax f))
                (term-fn? (syntax-local-value (syntax f) (λ () #f))))
           (raise-syntax-error 'term "metafunction must be in an application" orig-stx stx)]
          [(unquote x)
           (values (syntax (unsyntax x)) #f)]
          [(unquote . x)
           (raise-syntax-error 'term "malformed unquote" orig-stx stx)]
          [(unquote-splicing x)
           (values (syntax (unsyntax-splicing x)) #f)]
          [(unquote-splicing . x)
           (raise-syntax-error 'term "malformed unquote splicing" orig-stx stx)]
          [(in-hole id body)
           (values (syntax (unsyntax (plug (term id) (term body)))) #f)]
          [(in-hole . x)
           (raise-syntax-error 'term "malformed in-hole" orig-stx stx)]
          [hole (values (syntax (unsyntax the-hole)) #f)]
          [(x ...)
           (let-values ([(x-rewrite has-term-let-bound-id?)
                         (let i-loop ([xs (syntax->list (syntax (x ...)))])
                           (cond
                             [(null? xs) (values null #f)]
                             [else
                              (let ([new-depth (if (and (not (null? (cdr xs)))
                                                        (identifier? (cadr xs))
                                                        (free-identifier=? (quote-syntax ...)
                                                                           (cadr xs)))
                                                   (+ depth 1)
                                                   depth)])
                                (let-values ([(fst fst-has-term-let-bound-id?)
                                              (loop (car xs) new-depth)]
                                             [(rst rst-has-term-let-bound-id?)
                                              (i-loop (cdr xs))])
                                  (values (cons fst rst)
                                          (or fst-has-term-let-bound-id?
                                              rst-has-term-let-bound-id?))))]))])
             
             (with-syntax ([(x-rewrite ...) x-rewrite])
               (values (syntax/loc stx (x-rewrite ...))
                       has-term-let-bound-id?)))]
          [_ (values stx #f)])))
    
    (syntax-case orig-stx ()
      [(_ arg)
       (with-syntax ([rewritten (rewrite (syntax arg))])
         (let loop ([bs (reverse outer-bindings)])
           (cond
             [(null? bs) (syntax (syntax-object->datum (quasisyntax rewritten)))]
             [else (with-syntax ([rec (loop (cdr bs))]
                                 [fst (car bs)])
                     (syntax (with-syntax (fst)
                               rec)))])))]))
  
  (define-syntax (term-let-fn stx)
    (syntax-case stx ()
      [(_ ([f rhs] ...) body1 body2 ...)
       (with-syntax ([(g ...) (generate-temporaries (syntax (f ...)))])
         (syntax 
          (let ([g rhs] ...)
            (let-syntax ([f (make-term-fn #'g)] ...)
              body1
              body2 ...))))]))
  
  (define-syntax (term-define-fn stx)
    (syntax-case stx ()
      [(_ id exp)
       (with-syntax ([(id2) (generate-temporaries (syntax (id)))])
         (syntax
          (begin
            (define id2 exp)
            (define-syntax id
              (make-term-fn ((syntax-local-certifier) #'id2))))))]))
  
  (define-syntax (term-let stx)
    (syntax-case stx ()
      [(_ ([x rhs] ...) body1 body2 ...)
       (syntax
        (with-syntax ([x rhs] ...)
          (begin body1 body2 ...)))]
      [(_ x)
       (raise-syntax-error 'term-let "expected at least one body" stx)])))
