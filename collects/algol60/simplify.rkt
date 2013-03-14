#cs(module simplify mzscheme
     (require "parse.rkt"
              "prims.rkt"
              mzlib/match)

     (provide simplify)

     ;; flatten/label-block : list-of-decl list-of-stmt -> block-stmt
     ;; Desugars `for', converts `if' so that it's always of the form
     ;; `if <test> then goto <label> else goto <label>', flattens
     ;; compound statements into the enclosing block, and gives every
     ;; statement exactly one label. The result usually has lots of
     ;; "dummy" statements that could easily be eliminated by merging
     ;; labels.
     (define (flatten/label-block decls statements ->stx)
       (define extra-decls null)
       (define new-statements
         (let loop ([l statements])
           (if (null? l)
               null
               (match (car l)
                 [($ a60:block decls statements)
                  (cons (cons (gensym 'block) (flatten/label-block decls statements ->stx))
                        (loop (cdr l)))]
                 [($ a60:compound statements)
                  (loop (append statements (cdr l)))]
                 [($ a60:branch test then else)
                  (if (and (a60:goto? then) (a60:goto? else))
                      (cons (cons (gensym 'branch) (car l))
                            (loop (cdr l)))
                      (let ([then-label (gensym 'then)]
                            [else-label (gensym 'else)]
                            [cont-label (gensym 'if-cont)])
                        (loop
                         (list*
                          (make-a60:branch test (make-a60:goto then-label) (make-a60:goto else-label))
                          (make-a60:label then-label then)
                          (make-a60:goto cont-label)
                          (make-a60:label else-label else)
                          (make-a60:label cont-label (make-a60:dummy))
                          (cdr l)))))]
                 [($ a60:for variable val-exprs body)
                  (let ([body-label (gensym 'for-body)]
                        [cont-label (gensym 'for-cont)])
                    (letrec ([make-init+test+increment+loop
                              (lambda (value)
                                (match value
                                  [($ a60:for-number value)
                                   (values (make-a60:assign (list variable) (make-a60:binary 'num 'num
											     (->stx '+) 
											     (->stx '0)
											     value)) ; +0 => number
                                           (->stx #t)
                                           (make-a60:dummy)
                                           #f)]
                                  [($ a60:for-step start step end)
                                   (values (make-a60:assign (list variable) start)
                                           (make-a60:binary 'bool 'num
							    (->stx '<=)
                                                            (make-a60:binary 'num 'num
									     (->stx '*)
                                                                             (make-a60:binary 'num 'num (->stx '-) variable end)
                                                                             (make-a60:app (->stx 'sign) (list step)))
                                                            (->stx '0))
                                           (make-a60:assign (list variable) (make-a60:binary 'num 'num (->stx '+) variable step))
                                           #t)]
                                  [($ a60:for-while value test)
                                   (values (make-a60:assign (list variable) value)
                                           test
                                           (make-a60:assign (list variable) value)
                                           #t)]))])
                      (if (= 1 (length val-exprs))
                          (let-values ([(init test inc loop?) (make-init+test+increment+loop (car val-exprs))])
                            (loop (list*
                                   init
                                   (make-a60:label body-label (make-a60:dummy))
                                   (make-a60:branch test 
                                                    (make-a60:compound
                                                     (list
                                                      body
                                                      inc
                                                      (if loop?
                                                          (make-a60:goto body-label)
                                                          (make-a60:dummy))))
                                                    (make-a60:dummy))
                                   (cdr l))))
                          (let* ([stage-name (datum->syntax-object #f (gensym 'stage-number))]
                                 [switch-name (datum->syntax-object #f (gensym 'stage-switch))]
                                 [end-switch-name (datum->syntax-object #f (gensym 'stage-switch))]
                                 [stage-var (make-a60:variable stage-name null)]
                                 [start-labels (map (lambda (x) (gensym 'stage)) (append val-exprs (list 'extra)))]
                                 [end-labels (map (lambda (x) (gensym 'stage)) val-exprs)])
                            (set! extra-decls (list* stage-name 
                                                     (cons switch-name start-labels)
                                                     (cons end-switch-name end-labels)
                                                     extra-decls))
                            (loop
                             (append
                              (list (make-a60:assign (list stage-var) (->stx '0)))
                              (let loop ([start-labels start-labels][end-labels end-labels][val-exprs val-exprs])
                                (if (null? val-exprs)
                                    (list (make-a60:label (car start-labels) (make-a60:dummy)))
                                    (let-values ([(init test inc loop?) (make-init+test+increment+loop (car val-exprs))])
                                      (list*
                                       (make-a60:label (car start-labels) (make-a60:dummy))
                                       init
                                       (make-a60:branch test 
                                                        (make-a60:goto body-label)
                                                        (make-a60:compound                                                   
                                                         (list
                                                          (make-a60:assign (list stage-var) (make-a60:binary 'num 'num
													     (->stx '+)
													     (->stx '1)
													     stage-var))
                                                          (make-a60:goto (make-a60:subscript switch-name stage-var)))))
                                       (make-a60:label (car end-labels) (make-a60:dummy))
                                       inc
                                       (if loop?
                                           (make-a60:goto (car start-labels))
                                           (make-a60:goto (cadr start-labels)))
                                       (loop (cdr start-labels)
                                             (cdr end-labels)
                                             (cdr val-exprs))))))
                              (list 
                               (make-a60:goto cont-label)
                               (make-a60:label body-label (make-a60:dummy))
                               body
                               (make-a60:goto (make-a60:subscript end-switch-name stage-var))
                               (make-a60:label cont-label (make-a60:dummy)))
                              (cdr l)))))))]
                 [($ a60:label name statement)
                  (cons (cons name (make-a60:dummy))
                        (loop (cons statement (cdr l))))]
                 [else
                  (cons (cons (gensym 'other) (car l))
                        (loop (cdr l)))]))))
       (make-a60:block
        (append
         (map (lambda (decl)
                (match decl
                  [($ a60:proc-decl result-type var arg-vars by-value-vars arg-specs body)
                   (make-a60:proc-decl result-type var arg-vars by-value-vars arg-specs
                                       (simplify-statement body ->stx))]
                  [else decl]))
              decls)
         (map (lambda (extra)
                (if (identifier? extra)
                    (make-a60:type-decl (->stx 'integer) (list extra))
                    (make-a60:switch-decl
                     (car extra)
                     (map (lambda (x)
                            (make-a60:variable (datum->syntax-object #f x)
                                               null))
                          (cdr extra)))))
              extra-decls))
        (if (null? new-statements)
            (list (cons (gensym 'other) (make-a60:dummy)))
            new-statements)))

     (define (simplify stmt ctx)
       (simplify-statement stmt (lambda (x) (datum->syntax-object ctx x))))

     (define (simplify-statement stmt ->stx)
       (match stmt
         [($ a60:block decls statements)
          (flatten/label-block decls statements ->stx)]
         [($ a60:compound statements)
          (flatten/label-block null statements ->stx)]
         [else stmt])))
