#lang scheme/base

(require (for-syntax scheme/base 
                     "term-fn.ss"
                     syntax/private/util/misc)
         "matcher.ss")

(provide term term-let term-let/error-name term-let-fn term-define-fn)

(define (with-syntax* stx)
  (syntax-case stx ()
    [(_ () e) (syntax e)]
    [(_ (a b ...) e) (syntax (with-syntax (a) (with-syntax* (b ...) e)))]))

(define-syntax (term stx)
  (syntax-case stx ()
    [(_ arg)
     #`(term-let-fn ((#,(datum->syntax stx 'in-hole)
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
      (syntax-case stx (unquote unquote-splicing in-hole hole)
        [(metafunc-name arg ...)
         (and (identifier? (syntax metafunc-name))
              (term-fn? (syntax-local-value (syntax metafunc-name) (λ () #f))))
         (let-values ([(rewritten has-term-let-bound-id?) (loop (syntax (arg ...)) depth)])
           (let ([term-fn (syntax-local-value/catch (syntax metafunc-name) (λ (x) #t))])
             (with-syntax ([f (term-fn-get-id term-fn)])
               (cond
                 [has-term-let-bound-id?
                  (with-syntax ([(f-results) (generate-temporaries '(f-results))])
                    (let d-loop ([arg-dots rewritten]
                                 [fres (syntax f-results)]
                                 [func (syntax (lambda (x) (f (syntax->datum x))))]
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
                                   (- depth 1)))])))]
                 [else
                  (with-syntax ([rewritten rewritten])
                    (values (syntax/loc stx (unsyntax (f (syntax->datum (quasisyntax rewritten)))))
                            #f))]))))]
        [f
         (and (identifier? (syntax f))
              (term-fn? (syntax-local-value (syntax f) (λ () #f))))
         (raise-syntax-error 'term "metafunction must be in an application" orig-stx stx)]
        [x
         (and (identifier? (syntax x))
              (term-id? (syntax-local-value (syntax x) (λ () #f))))
         (values (term-id-id (syntax-local-value/catch (syntax x) (λ (x) #t))) #t)]
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
        
        
        [() (values stx #f)]
        [(x ... . y)
         (not (null? (syntax->list #'(x ...))))
         (let-values ([(x-rewrite has-term-let-bound-id?)
                       (let i-loop ([xs (syntax->list (syntax (x ...)))])
                         (cond
                           [(null? xs) (loop #'y depth)]
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
           
           (with-syntax ([x-rewrite x-rewrite])
             (values (syntax/loc stx x-rewrite)
                     has-term-let-bound-id?)))]
        
        [_ (values stx #f)])))
  
  (syntax-case orig-stx ()
    [(_ arg)
     (with-disappeared-uses
      (with-syntax ([rewritten (rewrite (syntax arg))])
        (let loop ([bs (reverse outer-bindings)])
          (cond
            [(null? bs) (syntax (syntax->datum (quasisyntax rewritten)))]
            [else (with-syntax ([rec (loop (cdr bs))]
                                [fst (car bs)])
                    (syntax (with-syntax (fst)
                              rec)))]))))]))

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

(define-syntax (term-let/error-name stx)
  (syntax-case stx ()
    [(_ error-name ([x1 rhs1] [x rhs] ...) body1 body2 ...)
     (let-values ([(orig-names new-names new-x1)
                   (let loop ([stx #'x1])
                     (syntax-case stx ()
                       [x 
                        (and (identifier? #'x)
                             (not (free-identifier=? (quote-syntax ...) #'x)))
                        (let ([new-name (car (generate-temporaries (list #'x)))])
                          (values (list #'x)
                                  (list new-name)
                                  new-name))]
                       [(x ...)
                        (let l-loop ([lst (syntax->list #'(x ...))])
                          (cond
                            [(null? lst)
                             (values '() '() '())]
                            [else
                             (let-values ([(hd-orig-names hd-new-names hd-new-pattern)
                                           (loop (car lst))]
                                          [(tl-orig-names tl-new-names tl-new-pattern)
                                           (l-loop (cdr lst))])
                               (values (append hd-orig-names tl-orig-names)
                                       (append hd-new-names tl-new-names)
                                       (cons hd-new-pattern tl-new-pattern)))]))]
                       [_
                        (values '() '() stx)]))])
       (with-syntax ([(orig-names ...) orig-names]
                     [(new-names ...) new-names]
                     [new-x1 new-x1])
         (syntax
          (syntax-case rhs1 ()
            [new-x1 
             (let-syntax ([orig-names (make-term-id #'new-names)] ...)
               (term-let/error-name error-name ((x rhs) ...) body1 body2 ...))]
            [_ (error 'error-name "term ~s does not match pattern ~s" rhs1 'x1)]))))]
    [(_ error-name () body1 body2 ...)
     (syntax
      (begin body1 body2 ...))]
    [(_ x)
     (raise-syntax-error 'term-let "expected at least one body" stx)]))

(define-syntax (term-let stx)
  (syntax-case stx ()
    [(_ () body1)
     #'body1]
    [(_ ([x rhs] ...) body1 body2 ...)
     (syntax
      (term-let/error-name term-let ((x rhs) ...) body1 body2 ...))]
    [(_ x)
     (raise-syntax-error 'term-let "expected at least one body" stx)]))
