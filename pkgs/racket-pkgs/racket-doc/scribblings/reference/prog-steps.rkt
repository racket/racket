
(module prog-steps mzscheme
  (require scribble/struct
           scribble/decode
           scribble/manual
           scribble/scheme
           mzlib/kw
           mzlib/class
           mzlib/for)

  (provide prog-steps
           prog-steps/cont
           prog-steps/no-obj)

  (define-syntax prog-steps/no-obj
    (syntax-rules ()
      [(_ [{def ...} prog] ...)
       (*prog-steps
        #f
        #f
        (list (racketblock0 def ...) ...)
        (list (racketblock0 prog) ...))]))

  (define-syntax prog-steps
    (syntax-rules ()
      [(_ [{obj ...} {def ...} prog] ...)
       (*prog-steps
        #f
        (list (racketblock0 obj ...) ...)
        (list (racketblock0 def ...) ...)
        (list (racketblock0 prog) ...))]))

  (define-syntax prog-steps/cont
    (syntax-rules ()
      [(_ [{obj ...} {def ...} prog] ...)
       (*prog-steps
        #t
        (list (racketblock0 obj ...) ...)
        (list (racketblock0 def ...) ...)
        (list (racketblock0 prog) ...))]))

  (define (to-flow e) (make-flow (list (make-paragraph (list e)))))

  (define (*prog-steps cont? objs defs progs)
    (make-table
     '((valignment baseline baseline baseline baseline baseline baseline))
     (apply
      append
      (for/list ([obj (or objs (in-naturals))]
                 [def defs]
                 [prog progs]
                 [i (in-naturals)])
        (let ([l
               (list
                (list (to-flow " ")
                      (to-flow (if (and (or (positive? i)
                                            cont?)
                                        (not objs))
                                   'rarr
                                   " "))
                      (to-flow " ")
                      (to-flow "defined:")
                      (to-flow " ")
                      (make-flow (list def)))
                (list (to-flow " ")
                      (to-flow " ")
                      (to-flow " ")
                      (to-flow "evaluate:")
                      (to-flow " ")
                      (make-flow (list prog))))])
          (if objs
              (cons (list
                     (to-flow " ")
                     (to-flow (if (or (positive? i)
                                      cont?)
                                  'rarr
                                  " "))
                      (to-flow " ")
                      (to-flow "objects:")
                      (to-flow " ")
                      (make-flow (list obj)))
                    l)
              l)))))))
