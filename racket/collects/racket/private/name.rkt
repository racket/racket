
(module name '#%kernel
  (#%require "define.rkt" "small-scheme.rkt")
  (#%provide syntax-local-infer-name)

  (define syntax-local-infer-name
    (case-lambda
     [(stx use-local?)
      (let-values ([(prop) (simplify-inferred-name (syntax-property stx 'inferred-name))])
        (or (and prop
                 (not (void? prop))
                 prop)
            (let ([n (and use-local?
                          (not (void? prop))
                          (syntax-local-name))])
              (or n
                  (let ([s (syntax-source stx)])
                    (and s
                         (let ([s (let ([s (format
                                            "~a"
                                            (cond
                                             [(path? s)
                                              ;; Make the result consistent across platforms by
                                              ;; converting backslashes to forward slashes:
                                              (regexp-replace* #rx"\\\\" (path->string s) "/")]
                                             [else s]))])
                                    (if ((string-length s) . > . 20)
                                        (string-append "..." (substring s (- (string-length s) 20)))
                                        s))]
                               [l (syntax-line stx)]
                               [c (syntax-column stx)])
                           (if l
                               (string->symbol (format "~a:~a:~a" s l c))
                               (let ([p (syntax-position stx)])
                                 (string->symbol (format "~a::~a" s p)))))))))))]
     [(stx) (syntax-local-infer-name stx #t)]))
  
  (define (simplify-inferred-name name)
    (if (pair? name)
        (let ([name-car (simplify-inferred-name (car name))]
              [name-cdr (simplify-inferred-name (cdr name))])
          (if (eq? name-car name-cdr)
              name-car
              name))
        name)))
