(#rx"incompatible ellipsis match counts"
 ([body (((x y) ...) ...)])
 ([xlhs (x ...)] [ylhs ((y ...) ...)])
 (term-let ([xlhs '(a b c)]
            [ylhs '((1 2) (4 5 6) (7 8 9))])
           (term body)))

(#rx"incompatible ellipsis match counts"
 ([body ((((f x) y) ...) ...)])
 ([fn f] [xlhs (x ...)] [ylhs ((y ...) ...)])
 (term-let-fn ([fn car])
              (term-let ([xlhs '(a b c)]
                         [ylhs '((1 2) (4 5 6) (7 8 9))])
                        (term body))))

(#rx"incompatible ellipsis match counts"
 ([body (f ((x y) ...))])
 ([fn f] [xlhs (x ...)] [ylhs (y ...)])
 (term-let-fn ([fn car])
              (term-let ([xlhs '(a b)]
                         [ylhs '(c d e)])
                        (term body))))

(#rx"incompatible ellipsis match counts"
 ([app (f (x y))])
 ([fn f] [xlhs (x ...)] [ylhs (y ...)] [ellipsis ...])
 (term-let-fn ([fn car])
              (term-let ([xlhs '(a b)]
                         [ylhs '(c d e)])
                        (term (app ellipsis)))))

(#rx"incompatible ellipsis match counts"
 ([plug (in-hole hole (x y))])
 ([xlhs (x ...)] [ylhs (y ...)] [ellipsis ...])
 (term-let-fn ([fn car])
              (term-let ([xlhs '(a b)]
                         [ylhs '(c d e)])
                        (term (plug ellipsis)))))

(#rx"term .* does not match pattern"
 ([rhs 'a]) ([ellipsis ...])
 (term-let ([(x ellipsis) rhs]) 3))

("reference to term x before its definition"
 ([use x]) ([def x])
 (let ()
   (define t (term (use y)))
   (define-term def z)
   t))
