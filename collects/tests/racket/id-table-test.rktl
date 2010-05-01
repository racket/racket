(load-relative "loadtest.rktl")

(require syntax/id-table
         scheme/dict)
  
(Section 'id-table)

(test #t bound-id-table? (make-bound-id-table))
(test #t bound-id-table? (make-immutable-bound-id-table))

(test #t mutable-bound-id-table? (make-bound-id-table))
(test #t immutable-bound-id-table? (make-immutable-bound-id-table))

(let ()
  ;; contains-same? : (listof x) (listof x) -> boolean
  (define (contains-same? l1 l2)
    (and (andmap (lambda (x) (member x l2)) l1)
         (andmap (lambda (x) (member x l1)) l2)
         #t))

  (let-values ([(x1 x2 x3 x4)
                (syntax-case (expand #'((lambda (x) x) (lambda (x) x))) ()
                  [(x (a (x1) x2) (c (x3) x4))
                   (values (syntax x1)
                           (syntax x2)
                           (syntax x3)
                           (syntax x4))])])

    (let ([check (lambda (=?)
                   (test #t =? x1 x2)
                   (test #t =? x3 x4)
                   (when (=? x1 x3)
                     ((current-print) "huh!?"))
                   (test #f =? x1 x3)
                   (test #f =? x1 x4)
                   (test #f =? x2 x3)
                   (test #f =? x2 x4))])
      (check bound-identifier=?)
      (check free-identifier=?))

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table x1 #f)
      (test #f bound-id-table-ref table x1)

      (bound-id-table-set! table x1 1)
      (bound-id-table-set! table x2 2)
      (bound-id-table-set! table x3 3)
      (bound-id-table-set! table x4 4)
      (test 2 bound-id-table-ref table x1)
      (test 2 bound-id-table-ref table x2)
      (test 4 bound-id-table-ref table x3)
      (test 4 bound-id-table-ref table x4)
      (test #t
            contains-same?
            (list 2 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))

      (test 2 bound-id-table-count table)

      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))


    (let ([table (make-free-id-table)])
      (free-id-table-set! table x1 1)
      (free-id-table-set! table x2 2)
      (free-id-table-set! table x3 3)
      (free-id-table-set! table x4 4)
      (test 2 free-id-table-ref table x1)
      (test 2 free-id-table-ref table x2)
      (test 4 free-id-table-ref table x3)
      (test 4 free-id-table-ref table x4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))))

  (let-values ([(y1 y2 y3 y4)
                (syntax-case (expand #'(module m mzscheme (require (prefix x: mzscheme)) + x:+ - x:-)) ()
                  [(a b c (d e f y1 y2 y3 y4))
                   (values (syntax y1)
                           (syntax y2)
                           (syntax y3)
                           (syntax y4))])])

    (let ([table (make-bound-id-table)])
      (bound-id-table-set! table y1 1)
      (bound-id-table-set! table y2 2)
      (bound-id-table-set! table y3 3)
      (bound-id-table-set! table y4 4)
      (test 1 bound-id-table-ref table y1)
      (test 2 bound-id-table-ref table y2)
      (test 3 bound-id-table-ref table y3)
      (test 4 bound-id-table-ref table y4)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (bound-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (dict-map table (lambda (x y) y)))
      (test 4 bound-id-table-count table)

      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (bound-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l)))

    (let ([table (make-free-id-table)])
      (free-id-table-set! table y1 #f)
      (test #f free-id-table-ref table y1)

      (free-id-table-set! table y1 1)
      (free-id-table-set! table y2 2)
      (free-id-table-set! table y3 3)
      (free-id-table-set! table y4 4)
      (test 2 free-id-table-ref table y1)
      (test 2 free-id-table-ref table y2)
      (test 4 free-id-table-ref table y3)
      (test 4 free-id-table-ref table y4)

      (test #t
            contains-same?
            (list 2 4)
            (free-id-table-map table (lambda (x y) y)))
      (test #t
            contains-same?
            (list 2 4)
            (dict-map table (lambda (x y) y)))
      (test 2 free-id-table-count table)

      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (free-id-table-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (dict-for-each
               table
               (lambda (x y)
                 (set! l (cons y l))))
              l))
      )))

(report-errs)
