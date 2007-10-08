(module freevars mzscheme
  (require-for-template mzscheme)
  (require (lib "kerncase.ss" "syntax")
           (lib "list.ss")
           (lib "toplevel.ss" "syntax")
           (lib "plt-match.ss")
           (lib "stx.ss" "syntax")
           "util.ss")
  (provide free-vars)  
  
  ;; free-vars: syntax -> (listof identifier)
  ;; Find the free variables in an expression
  (define (free-vars stx)  
    (kernel-syntax-case 
        stx (transformer?)
      [(begin be ...)
       (free-vars* (syntax->list #'(be ...)))]
      [(begin0 be ...)
       (free-vars* (syntax->list #'(be ...)))]
      [(define-values (v ...) ve)
       (set-diff (free-vars #'ve)
                 (syntax->list #'(v ...)))]
      [(define-syntaxes (v ...) ve)
       (parameterize ([transformer? #t])
         (set-diff (free-vars #'ve)
                   (syntax->list #'(v ...))))]
      [(define-values-for-syntax (v ...) ve)
       (parameterize ([transformer? #t])
         (set-diff (free-vars #'ve)
                   (syntax->list #'(v ...))))]
      [(set! v ve)
       (union (free-vars #'v)
              (free-vars #'ve))]
      [(let-values ([(v ...) ve] ...) be ...)
       (union (free-vars* (syntax->list #'(ve ...)))
              (set-diff (free-vars* (syntax->list #'(be ...)))
                        (apply append (map syntax->list (syntax->list #'((v ...) ...))))))]
      [(letrec-values ([(v ...) ve] ...) be ...)
       (set-diff (union (free-vars* (syntax->list #'(ve ...)))
                        (free-vars* (syntax->list #'(be ...))))
                 (apply append (map syntax->list (syntax->list #'((v ...) ...)))))]
      [(lambda formals be ...)
       (set-diff (free-vars* (syntax->list #'(be ...)))
                 (formals-list #'formals))]
      [(case-lambda [formals be ...] ...)
       (apply union*
              (map (lambda (fs bes)
                     (set-diff (free-vars* (syntax->list bes))
                               (formals-list fs)))
                   (syntax->list #'(formals ...))
                   (syntax->list #'((be ...) ...))))]
      [(if te ce ae)
       (free-vars* (syntax->list #'(te ce ae)))]
      [(if te ce)
       (free-vars #`(if te ce (#%app void)))]
      [(quote datum)
       empty]
      [(quote-syntax datum)
       empty]
      [(letrec-syntaxes+values ([(sv ...) se] ...)
         ([(vv ...) ve] ...)
         be ...)
       (set-diff (union* (free-vars* (syntax->list #'(se ...)))
                         (free-vars* (syntax->list #'(ve ...)))
                         (free-vars* (syntax->list #'(be ...))))
                 (append (apply append (map syntax->list (syntax->list #'((sv ...) ...))))
                         (apply append (map syntax->list (syntax->list #'((vv ...) ...))))))]
      [(with-continuation-mark ke me be)
       (free-vars* (syntax->list #'(ke me be)))]
      [(#%expression d)
       (free-vars #'d)]
      [(#%app e ...)
       (free-vars* (syntax->list #'(e ...)))]
      [(#%top . v)
       #;(printf "Not including top ~S in freevars~n" (syntax-object->datum #'v))
       empty]
      [(#%datum . d)
       empty]
      [(#%variable-reference . id)
       (let ([i-bdg (identifier-binding #'id)])
         (cond
           [(eqv? 'lexical (identifier-binding #'id))
            (list #'id)]
           [else 
            #;(printf "Not including var-reference ~S with binding ~S in freevars~n" (syntax-object->datum #'id) i-bdg)
            empty]))]
      [id (identifier? #'id)
          (let ([i-bdg (identifier-binding #'id)])
            (cond
              [(eqv? 'lexical i-bdg)
               (list #'id)]
              [(not i-bdg)
               (list #'id)]
              [else 
               #;(printf "Not including id ~S with binding ~S in freevars~n" (syntax-object->datum #'id) i-bdg)
               empty]))]
      [_
       (raise-syntax-error 'freevars "Dropped through:" stx)]))  
  
  ;; free-vars*: (listof expr) -> (listof identifier)
  ;; union the free variables that occur in several expressions
  (define (free-vars* exprs)
    (foldl
     (lambda (expr acc) (union (free-vars expr) acc))
     empty exprs))
  
  ;; union: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic union of two lists
  (define (union l1 l2)
    (cond
      [(null? l1) l2]
      [else (insert (car l1) (union (cdr l1) l2))]))
  
  (define (union* . ll)
    (foldl union
           empty
           ll))
  
  ;; insert: symbol (listof identifier) -> (listof symbol)
  ;; insert a symbol into a list without creating a duplicate
  (define (insert sym into)
    (unless (identifier? sym)
      (raise-syntax-error 'insert "Not identifier" sym))
    (cond
      [(null? into) (list sym)]
      [(bound-identifier=? sym (car into)) into]
      [else (cons (car into) (insert sym (cdr into)))]))
  
  ;; set-diff: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic difference of two lists
  (define (set-diff s1 s2)
    (cond
      [(null? s2) s1]
      [else (set-diff (sans s1 (car s2)) (cdr s2))]))
  
  ;; sans: (listof identifier) symbol -> (listof identifier)
  ;; produce the list sans the symbol
  (define (sans s elt)
    (unless (identifier? elt)
      (raise-syntax-error 'sans "Not identifier" elt))
    (cond
      [(null? s) empty]
      [(bound-identifier=? (car s) elt)
       (cdr s)] ;; if we maintain the no-dupe invariant then we don't need to recur
      [else (cons (car s)
                  (sans (cdr s) elt))])))