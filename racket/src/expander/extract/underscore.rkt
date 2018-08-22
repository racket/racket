#lang racket/base
(require racket/match)

(provide simplify-underscore-numbers
         collapse-underscore-numbers)

;; Small changes to the input code can trigger lots of renumberings
;; for local variables, where the expander adds "_<num>" suffixes to
;; generate local-variable names, and the "<num>"s count up across all
;; symbols. Renumber with symbol-specific counting to reduce
;; unneccessary changes to generated code. A relatively simple
;; strategy works because no primitive or exported name has a "_<num>"
;; suffix.

(define (select-new-name s base-counts)
  (define str (symbol->string s))
  (define m (regexp-match-positions #rx"_[0-9]+$" str))
  (cond
    [(not m) s]
    [else
     (define base (substring str 0 (caar m)))
     (define base-s (string->symbol base))
     (define n (base-counts base-s))
     (base-counts base-s (add1 n))
     (string->symbol (format "~a_~a" base n))]))

;; ----------------------------------------

;; First variant: preserve the property that every binder is
;; repersented by a unique symbol.

(define (simplify-underscore-numbers linklet-expr)
  (define replacements (make-hasheq))
  (define base-counts (make-hasheq))
  (let loop ([s linklet-expr])
    (cond
     [(symbol? s)
      (cond
       [(hash-ref replacements s #f)
        => (lambda (r) r)]
       [else
        (define str (symbol->string s))
        (define m (regexp-match-positions #rx"_[0-9]+$" str))
        (cond
         [(not m)
          (hash-set! replacements s s)
          s]
         [else
          (define r (select-new-name s (case-lambda
                                         [(base-s) (hash-ref base-counts base-s 0)]
                                         [(base-s n) (hash-set! base-counts base-s n)])))
          (hash-set! replacements s r)
          r])])]
     [(pair? s)
      (if (eq? (car s) 'quote)
          s
          (cons (loop (car s)) (loop (cdr s))))]
     [else s])))

;; ----------------------------------------

;; Second variant: preserve the property that every binder is
;; represented by a unique symbol. A relatively simple strategy works
;; because no primitive or exported name has a "_<num>" suffix.

(define (collapse-underscore-numbers linklet-expr)

  (define (collapse e replacements base-counts)
    (match e
      [`(define-values ,ids ,rhs)
       `(define-values ,ids ,(collapse rhs replacements base-counts))]
      [`(lambda ,formals ,body ...)
       (define-values (new-formals new-replacements new-base-counts)
         (rename-vars formals replacements base-counts))
       `(lambda ,new-formals . ,(collapse-in-body body new-replacements new-base-counts))]
      [`(case-lambda [,formalss ,bodys ...] ...)
       `(case-lambda
          . ,(for/list ([formals (in-list formalss)] [body (in-list bodys)])
               (define-values (new-formals new-replacements new-base-counts)
                 (rename-vars formals replacements base-counts))
               `[,new-formals . ,(collapse-in-body body new-replacements new-base-counts)]))]
      [`(quote . ,_) e]
      [`(let-values . ,_) (collapse-in-let e #f replacements base-counts)]
      [`(letrec-values . ,_) (collapse-in-let e #t replacements base-counts)]
      [`(,pseudo-es ...) ; catch-all for remaining syntactic forms
       (collapse-in-body pseudo-es replacements base-counts)]
      [_ (if (symbol? e)
             (hash-ref replacements e e)
             e)]))
  
  (define (collapse-in-body es replacements base-counts)
    (for/list ([e (in-list es)])
      (collapse e replacements base-counts)))

  (define (collapse-in-let e rec? replacements base-counts)
    (match e
      [`(,let-form ([,idss ,rhss] ...) ,body ...)
       (define-values (new-idss body-replacements body-base-counts)
         (rename-vars idss replacements base-counts))
       (define-values (rhs-replacements rhs-base-counts)
         (if rec?
             (values body-replacements body-base-counts)
             (values replacements base-counts)))
       `(,let-form ,(for/list ([ids (in-list new-idss)]
                               [rhs (in-list rhss)])
                      `[,ids ,(collapse rhs rhs-replacements rhs-base-counts)])
                   . ,(collapse-in-body body body-replacements body-base-counts))]))

  (define (rename-vars p replacements base-counts)
    (define new-p
      (let loop ([p p])
        (cond
          [(null? p) null]
          [(symbol? p)
           (define r (select-new-name p (case-lambda
                                          [(s) (hash-ref base-counts s 0)]
                                          [(s n) (set! base-counts (hash-set base-counts s n))])))
           (set! replacements (hash-set replacements p r))
           r]
          [(pair? p) (cons (loop (car p)) (loop (cdr p)))])))
    (values new-p replacements base-counts))
  
  (match linklet-expr
    [`(linklet ,imports ,exports ,forms ...)
     `(linklet ,imports ,exports ,@(for/list ([form (in-list forms)])
                                     (collapse form #hasheq() #hasheq())))]))
