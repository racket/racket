#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap
         syntax/stx
         scheme/struct-info
         "patterns.ss"
         "compiler.ss"         
         "parse-helper.ss"
         "parse-quasi.ss"
         (only-in srfi/1 delete-duplicates))

(provide parse/legacy/cert)

(define (parse/legacy/cert stx cert)
  (define (parse stx) (parse/legacy/cert stx cert))
  (syntax-case* stx (not $ ? and or = quasiquote quote)
    (lambda (x y) (eq? (syntax-e x) (syntax-e y)))
    
    [(expander args ...)
       (and (identifier? #'expander)
            ;; for debugging
            (syntax-transforming?)
            (match-expander? (syntax-local-value (cert #'expander) (lambda () #f))))
       (let* ([expander (syntax-local-value (cert #'expander))]
              [transformer (match-expander-legacy-xform expander)])
         (unless transformer
           (raise-syntax-error #f "This expander only works with the standard match syntax" #'expander))
         (let* ([introducer (make-syntax-introducer)]
                [certifier (match-expander-certifier expander)]
                [mstx (introducer (syntax-local-introduce stx))]
                [mresult (transformer mstx)]
                [result (syntax-local-introduce (introducer mresult))]
                [cert* (lambda (id) (certifier (cert id) #f introducer))])
           (parse/legacy/cert result cert*)))]
    [(and p ...)
     (make-And (map parse (syntax->list #'(p ...))))]
    [(or p ...)
     (let ([ps (map parse (syntax->list #'(p ...)))])
       (all-vars ps stx)
       (make-Or ps))]
    [(not p ...)
     ;; nots are conjunctions of negations
     (let ([ps (map (compose make-Not parse) (syntax->list #'(p ...)))])
       (make-And ps))]
    [bx 
     (box? (syntax-e #'bx))
     (make-Box (parse (unbox (syntax-e #'bx))))]
    [#(es ...)
     (ormap ddk? (syntax->list #'(es ...)))
     (make-And (list (make-Pred #'vector?) (make-App #'vector->list (parse (syntax/loc stx (es ...))))))]
    [#(es ...)
     (make-Vector (map parse (syntax->list #'(es ...))))]    
      
    [($ s . pats)
     (let* ([fail (lambda () 
                    (raise-syntax-error 'match (format "~a does not refer to a structure definition" (syntax->datum #'s)) stx #'s))]
            [v (syntax-local-value (cert #'s) fail)])
       (unless (struct-info? v)
         (fail))
       (let-values ([(id _1 pred acc _2 super) (apply values (extract-struct-info v))])
         ;; this produces a list of all the super-types of this struct
         ;; ending when it reaches the top of the hierarchy, or a struct that we can't access
         (define (get-lineage struct-name)
           (let ([super (list-ref 
                         (extract-struct-info (syntax-local-value struct-name))
                         5)])
             (cond [(equal? super #t) '()] ;; no super type exists
                   [(equal? super #f) '()] ;; super type is unknown
                   [else (cons super (get-lineage super))])))
         (let* (;; the accessors come in reverse order
                [acc (reverse acc)]
                ;; remove the first element, if it's #f
                [acc (cond [(null? acc) acc] [(not (car acc)) (cdr acc)] [else acc])])
           (make-Struct id pred (get-lineage #'s) acc 
                        (if (eq? '_ (syntax-e #'pats))
                            (map make-Dummy acc)
                            (let* ([ps (syntax->list #'pats)])
                              (unless (= (length ps) (length acc))
                                (raise-syntax-error 'match (format "wrong number for fields for structure ~a: expected ~a but got ~a"
                                                                   (syntax->datum #'s) (length acc) (length ps))
                                                    stx #'pats))
                              (map parse ps)))))))]
    [(? p q1 qs ...)
     (make-And (cons (make-Pred (cert #'p)) (map parse (syntax->list #'(q1 qs ...)))))]
    [(? p)
     (make-Pred (cert #'p))]
    [(= f p)
     (make-App #'f (parse (cert #'p)))]
    [(quasiquote p)
     (parse-quasi #'p cert parse/legacy/cert)]
    [(quote ())
     (make-Null (make-Dummy stx))]
    [(quote (a . b))
     (make-Pair (parse (syntax/loc stx (quote a)))
                (parse (syntax/loc stx (quote b))))]
    [(quote vec)
     (vector? (syntax-e #'vec))
     (make-Vector (for/list ([e (vector->list (syntax-e #'vec))])
                            (parse (quasisyntax/loc stx (quote #,e)))))]
    [(quote bx)
     (vector? (syntax-e #'bx))
     (make-Box (parse (quasisyntax/loc stx (quote #,(syntax-e #'bx)))))]
    [(quote v)
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "non-literal in quote pattern" stx #'v))]
    [() (make-Null (make-Dummy #f))]
    [(..)
     (ddk? #'..)
     (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'..)]
    [(p .. . rest)
     (ddk? #'..)
     (let* ([count (ddk? #'..)]
            [min (if (number? count) count #f)]
            [max (if (number? count) count #f)])
       (make-GSeq 
        (parameterize ([match-...-nesting (add1 (match-...-nesting))])
          (list (list (parse #'p))))
        (list min)
        ;; no upper bound
        (list #f)
        ;; patterns in p get bound to lists
        (list #f)
        (parse (syntax/loc stx rest))))]
    [(e . es)
     (make-Pair (parse #'e) (parse (syntax/loc stx es)))]  
    [x
     (identifier? #'x)
     (cond [(eq? '_ (syntax-e #'x))
            (make-Dummy #'x)]
           [(ddk? #'x) (raise-syntax-error 'match "incorrect use of ... in pattern" stx #'x)]
           [else (make-Var #'x)])]
    [v
     (or (parse-literal (syntax-e #'v))
         (raise-syntax-error 'match "syntax error in pattern" stx))]))
