(load-relative "loadtest.ss")

(require (lib "boundmap.ss" "syntax"))
  
(section 'BOUNDMAP)

(test #t bound-identifier-mapping? (make-bound-identifier-mapping))

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
    
    
    (let ([mapping (make-bound-identifier-mapping)])
      (bound-identifier-mapping-put! mapping x1 1)
      (bound-identifier-mapping-put! mapping x2 2)
      (bound-identifier-mapping-put! mapping x3 3)
      (bound-identifier-mapping-put! mapping x4 4)
      (test 2 bound-identifier-mapping-get mapping x1)
      (test 2 bound-identifier-mapping-get mapping x2)
      (test 4 bound-identifier-mapping-get mapping x3)
      (test 4 bound-identifier-mapping-get mapping x4)
      (test #t
            contains-same?
            (list 2 4)
            (bound-identifier-mapping-map mapping (lambda (x y) y)))
      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (bound-identifier-mapping-for-each
               mapping
               (lambda (x y)
                 (set! l (cons y l))))
              l)))
    
    (let ([mapping (make-module-identifier-mapping)])
      (module-identifier-mapping-put! mapping x1 1)
      (module-identifier-mapping-put! mapping x2 2)
      (module-identifier-mapping-put! mapping x3 3)
      (module-identifier-mapping-put! mapping x4 4)
      (test 2 module-identifier-mapping-get mapping x1)
      (test 2 module-identifier-mapping-get mapping x2)
      (test 4 module-identifier-mapping-get mapping x3)
      (test 4 module-identifier-mapping-get mapping x4)
      (test #t
            contains-same?
            (list 2 4)
            (module-identifier-mapping-map mapping (lambda (x y) y)))
      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (module-identifier-mapping-for-each
               mapping
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
    
    (let ([mapping (make-bound-identifier-mapping)])
      (bound-identifier-mapping-put! mapping y1 1)
      (bound-identifier-mapping-put! mapping y2 2)
      (bound-identifier-mapping-put! mapping y3 3)
      (bound-identifier-mapping-put! mapping y4 4)
      (test 1 bound-identifier-mapping-get mapping y1)
      (test 2 bound-identifier-mapping-get mapping y2)
      (test 3 bound-identifier-mapping-get mapping y3)
      (test 4 bound-identifier-mapping-get mapping y4)
      (test #t
            contains-same?
            (list 1 2 3 4)
            (bound-identifier-mapping-map mapping (lambda (x y) y)))
      
      (test #t
            contains-same?
            (list 1 2 3 4)
            (let ([l '()])
              (bound-identifier-mapping-for-each
               mapping
               (lambda (x y)
                 (set! l (cons y l))))
              l)))
    
    (let ([mapping (make-module-identifier-mapping)])
      (module-identifier-mapping-put! mapping y1 1)
      (module-identifier-mapping-put! mapping y2 2)
      (module-identifier-mapping-put! mapping y3 3)
      (module-identifier-mapping-put! mapping y4 4)
      (test 2 module-identifier-mapping-get mapping y1)
      (test 2 module-identifier-mapping-get mapping y2)
      (test 4 module-identifier-mapping-get mapping y3)
      (test 4 module-identifier-mapping-get mapping y4)
      (test #t
            contains-same?
            (list 2 4)
            (module-identifier-mapping-map mapping (lambda (x y) y)))
      
      (test #t
            contains-same?
            (list 2 4)
            (let ([l '()])
              (module-identifier-mapping-for-each
               mapping
               (lambda (x y)
                 (set! l (cons y l))))
              l)))))

(report-errs)