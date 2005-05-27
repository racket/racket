;; This library is used by match.ss

;;!(function setter
;;          (form (setter e ident let-bound) -> syntax)
;;          (contract (syntax syntax list) -> syntax)
;;          (example (setter (syntax (car x)) (syntax here) '())
;;                   ->
;;          (syntax (lambda (y) (set-car! x y)))))
;; This function takes an expression and returns syntax which
;; represents a function that is able to set the value that the
;; expression points to.
(define setter (lambda (e ident let-bound)
                 (let ((mk-setter (lambda (s)
                                    (symbol-append 'set- s '!))))
                   (syntax-case e (vector-ref unbox car cdr)
                     (p
                      (not (stx-pair? (syntax p)))
                      (match:syntax-err
                       ident
                       "set! pattern should be nested inside of a list, vector or box"))
                     ((vector-ref vector index)
                      (quasisyntax/loc
                       ident
                       (let ((x #,(subst-bindings (syntax vector)
                                                  let-bound)))
                         (lambda (y)
                           (vector-set!
                            x
                            index
                            y)))))
                     ((unbox boxed)
                      (quasisyntax/loc
                       ident (let ((x #,(subst-bindings (syntax boxed)
                                                        let-bound)))
                               (lambda (y)
                                 (set-box! x  y)))))
                     ((car exp)
                      (quasisyntax/loc
                       ident
                       (let ((x #,(subst-bindings (syntax exp)
                                                  let-bound)))
                         (lambda (y)
                           (set-car! x y)))))
                     ((cdr exp)
                      (quasisyntax/loc
                       ident
                       (let ((x #,(subst-bindings (syntax exp)
                                                  let-bound)))
                         (lambda (y)
                           (set-cdr! x y)))))
                     ((acc exp)
                      (let ((a (assq (syntax-object->datum (syntax acc))
                                     get-c---rs)))
                        (if a
                            (quasisyntax/loc
                             ident
                             (let ((x (#,(cadr a) 
                                       #,(subst-bindings (syntax exp)
                                                         let-bound))))
                               (lambda (y)
                                 (#,(mk-setter (cddr a)) x y))))
                            (quasisyntax/loc
                             ident
                             (let ((x #,(subst-bindings (syntax exp)
                                                        let-bound)))
                               (lambda (y)
                                 (#,(mk-setter 
                                     (syntax-object->datum (syntax acc)))
                                  x y)))))))))))

;;!(function getter
;;          (form (getter e ident let-bound) -> syntax)
;;          (contract (syntax syntax list) -> syntax)
;;          (example (getter (syntax (car x)) (syntax here) '())
;;                   ->
;;          (syntax (lambda () (car x)))))
;; This function takes an expression and returns syntax which
;; represents a function that is able to get the value that the
;; expression points to.
(define getter (lambda (e ident let-bound)
                 (syntax-case e (vector-ref unbox car cdr)
                   (p
                    (not (stx-pair? (syntax p)))
                    (match:syntax-err 
                     ident
                     "get! pattern should be nested inside of a list, vector or box"))
                   ((vector-ref vector index)
                    (quasisyntax/loc
                     ident
                     (let ((x #,(subst-bindings (syntax vector)
                                                let-bound)))
                       (lambda ()
                         (vector-ref
                          x
                          index)))))
                   ((unbox boxed)
                    (quasisyntax/loc
                     ident
                     (let ((x #,(subst-bindings (syntax boxed)
                                                let-bound)))
                       (lambda () (unbox x)))))
                   ((car exp)
                    (quasisyntax/loc
                     ident
                     (let ((x #,(subst-bindings (syntax exp)
                                                let-bound)))
                       (lambda () (car x)))))
                   ((cdr exp)
                    (quasisyntax/loc
                     ident
                     (let ((x #,(subst-bindings (syntax exp)
                                                let-bound)))
                       (lambda () (cdr x)))))
                   ((acc exp)
                    (let ((a (assq (syntax-object->datum (syntax acc))
                                   get-c---rs)))
                      (if a
                          (quasisyntax/loc
                           ident
                           (let ((x (#,(cadr a)
                                     #,(subst-bindings (syntax exp)
                                                       let-bound))))
                             (lambda () (#,(cddr a) x))))
                          (quasisyntax/loc
                           ident
                           (let ((x #,(subst-bindings (syntax exp)
                                                      let-bound)))
                             (lambda ()
                               (acc x))))))))))
