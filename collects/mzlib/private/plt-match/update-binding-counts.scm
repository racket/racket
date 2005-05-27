;; This library is used by match.ss

(define-values (update-binding-counts update-binding-count)
  (letrec 
      (
       ;;!(function update-binding-count
       ;;          (form (update-binding-count render-list) -> list)
       ;;          (contract list -> list))
       ;; This function is normally executed for its side effect of
       ;; setting the count for the number of times an expression used in
       ;; a test if found in the rest of the list of tests.  This does
       ;; not only count occurrances of the exp in other tests but
       ;; whether the expression is also a sub expression in the other tests.
       ;; Arg:
       ;; render-list - a list of test structs
       (update-binding-count 
        (lambda (render-list)
          (define (inc-bind-count test)
            (set-test-bind-count! test
                                  (add1 (test-bind-count test))))
          (if (null? render-list)
              '()
              (let ((cur-test (car render-list)))
                (update-binding-count
                 (let loop ((l (cdr render-list)))
                   (cond ((null? l) '())
                         ((>= (test-bind-count cur-test) 2) l)
                         ((and (valid-for-let-binding (test-bind-exp cur-test))
                               (equal? (test-bind-exp cur-test)
                                       (test-bind-exp (car l))))
                          (begin
                            (inc-bind-count cur-test)
                            (loop (cdr l))))
                         ((sub-exp-contains (test-bind-exp cur-test)
                                            (test-bind-exp (car l)))
                          (begin
                            (inc-bind-count cur-test)
                            (cons (car l) (loop (cdr l)))))
                         (else (cons (car l) (loop (cdr l)))))))))))

       ;;!(function valid-for-let-binding
       ;;          (form (valid-for-let-binding exp) -> bool)
       ;;          (contract s-exp -> bool)
       ;;          (example (valid-for-let-binding 'x) -> #f))
       ;; This function is a predicate that determins if an expression
       ;; should be considered for let binding.
       (valid-for-let-binding 
        (lambda (exp)
          ;; it must be a pair
          ;; the index must be an integer
          '(match exp
                  (('vector-ref _ n) (number? n))
                  ((? pair?) #t)
                  (_ #f))
          ;; the following is expanded fromt the above match expression
          (let ((x exp))
            (if (pair? x)
                (if (and (equal? (car x) 'vector-ref)
                         (pair? (cdr x))
                         (pair? (cdr (cdr x)))
                         (null? (cdr (cdr (cdr x)))))
                    ((lambda (n) (number? n)) (car (cdr (cdr x))))
                    ((lambda () #t)))
                ((lambda () #f))))))

       ;;!(function sub-exp-contains
       ;;          (form (sub-exp-contains exp1 exp2) -> bool)
       ;;          (contract (s-exp s-exp) -> bool)
       ;;          (example (sub-exp-contains '(cdr x) '(car (cdr x))) -> #t))
       ;; This function returns true if exp2 contains a sub-expression
       ;; that is equal? to exp1.  For this function to work the subexp
       ;; must always be in the second position in a exp.  This is a
       ;; convention that is followed throughout the match program.
       (sub-exp-contains 
        (lambda (exp1 exp2)
          '(match exp2
                  (() #f)
                  ((_ sub-exp _ ...)
                   (if (and (valid-for-let-binding sub-exp)
                            (equal? sub-exp exp1))
                       #t
                       (sub-exp-contains exp1 sub-exp)))
                  (_ #f))
          ;; The following was expanded from the above match expression
          (let ((x exp2))
            (if (null? x)
                ((lambda () #f))
                (if (and (pair? x) (pair? (cdr x)) (list? (cdr (cdr x))))
                    ((lambda (sub-exp)
                       (if (and (pair? sub-exp)
                                (equal? sub-exp exp1))
                           #t
                           (sub-exp-contains exp1 sub-exp)))
                     (car (cdr x)))
                    ((lambda () #f)))))))

       ;;!(function update-binding-counts
       ;;          (form (update-binding-counts render-lists) -> list)
       ;;          (contract list -> list))
       ;; This function calls update-binding-count for each render list
       ;; in the list of render lists.  This is used mainly for its side
       ;; affects.  The result is of no consequence.
       (update-binding-counts 
        (lambda (render-lists)
          (map update-binding-count (map car render-lists))))
       )
    (values update-binding-counts update-binding-count)))
