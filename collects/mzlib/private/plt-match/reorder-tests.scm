;; This library is used by match.ss
;; This requires the test data structure.

(define-values (reorder-all-lists)
  (letrec 
      (
       ;;!(function insertion-sort
       ;;          (form (insertion-sort ls less-than?) -> list)
       ;;          (contract (list (any any -> bool) -> list)))
       ;; This is the classic stable sort.  Any stable sort will do.
       (insertion-sort 
        (lambda (ls less-than?)
          (define (insert el ls)
            (define (ins ls)
              (cond ((null? ls) (list el))
                    ((less-than? el (car ls))
                     (cons el ls))
                    (else (cons (car ls) (ins (cdr ls))))))
            (ins ls))
          (letrec ((IS (lambda (ls)
                         (if (null? ls)
                             '()
                             (insert (car ls)
                                     (IS (cdr ls)))))))
            (IS ls))))

        ;;!(function make-test-order-func
        ;;          (form (make-test-order-func whole-list) -> less-than?)
        ;;          (contract list -> (any any -> bool)))
        ;; This function creates a test function which has access to the
        ;;whole list of test structures capured in the closure.  This
        ;;function places tests that are used more ahead of those used
        ;;less.  When tests are used an equal number of times the test whos
        ;;membership set has the greatest presence is placed ahead.
        (make-test-order-func
         (lambda (whole-list)
           (lambda (t1 t2)
             (let ((t1-tu (test-times-used t1))
                   (t2-tu (test-times-used t2)))
               (cond ((> t1-tu t2-tu) #t)
                     ;; these two new rules allow negate
                     ;; tests to be placed properly
                     ((and (= t1-tu t2-tu)
                           (shape-test? t1)
                           (not (shape-test? t2))
                           (negate-test? t2))
                      #t)
                     ((and (= t1-tu t2-tu)
                           (not (shape-test? t1))
                           (negate-test? t1)
                           (shape-test? t2))
                      #f)
                     ((and (= t1-tu t2-tu)
                           (or (equal? (test-used-set t1) (test-used-set t2))
                               (>= (number-of-similar (test-used-set t1) 
                                                      whole-list)
                                   (number-of-similar (test-used-set t2) 
                                                      whole-list))))
                      #t)
                     (else #f))))))

        ;;!(function number-of-similar
        ;;          (form (number-of-similar set ls) -> integer)
        ;;          (contract (list list) -> integer))
        ;; This function returns the number of tests that have a
        ;; membership set similar to set.  A membership set is the set of
        ;; test-lists that have a similar tests as the test itself.
        (number-of-similar 
         (lambda (set ls)
           (apply + (map (lambda (set2) (if (equal? set set2) 1 0))
                         (map test-used-set ls)))))

        ;;!(function reorder-tests
        ;;          (form (reorder-tests2 test-list) -> test-list)
        ;;          (contract list -> list))
        ;; This function reorders one list of test structs.
        (reorder-tests 
         (lambda (test-list)
           ;;(pretty-print test-list)(newline)
           (insertion-sort test-list (make-test-order-func test-list))))

        ;;!(function reorder-all-lists
        ;;          (form (reorder-all-lists2 rendered-list) -> list)
        ;;          (contract list -> list))
        ;; This function reorders all of the rendered-lists that have
        ;; success-functions attached to them.
        (reorder-all-lists 
         (lambda (rendered-list)
           (if (null? rendered-list)
               '()
               (let ((success-func (cdr (car rendered-list)))
                     (rot (reorder-tests (caar rendered-list))))
                                        ;(pretty-print rot)(newline)
                 (cons (cons rot success-func)
                       (reorder-all-lists (cdr rendered-list)))))))
        )
    (values reorder-all-lists)))


