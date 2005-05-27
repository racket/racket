;; This library is used by match.ss
;; This requires the test data structure.

(define-values (update-counts)
  (letrec 
      (
       ;;!(function test-filter
       ;;          (form (test-filter test-list) -> test-list)
       ;;          (contract list -> list))
       ;; This function filters out tests that do not need to be to have
       ;; their counts updated for reordering purposes.  These are the
       ;; more complex patterns such as or-patterns or ddk patterns.
       (test-filter
        (lambda (tlist)
          (if (null? tlist)
              '()
              (if (= -1 (test-times-used (car tlist)))
                  (test-filter (cdr tlist))
                  (cons (car tlist)
                        (test-filter (cdr tlist)))))))


       ;; !(function inverse-in
       ;;            (form (inverse-in test test-list) -> bool)
       ;;            (contract (s-exp list) -> bool))
       ;; This function checks to see if any of the members of the test-list
       ;; would be eliminated by the function if the test was in the test so far 
       ;; list.  This is the opposite of what the in function does.
       (inverse-in
        (lambda (test test-list)
          (or (pos-inverse-in test test-list)
              (neg-inverse-in test test-list))))

       (pos-inverse-in
        (lambda (test test-list)
          (let ((test-with-implied (cons test (implied test))))
            (ormap (lambda (t) (in t test-with-implied))
                   test-list)
            )))

       (neg-inverse-in
        (lambda (test test-list)
          (let ((test-with-implied (cons test (implied test))))
            (ormap (lambda (t) (in `(not ,t) test-with-implied))
                   test-list)
            )))

       (logical-member
        (lambda (item lst)
          (ormap (lambda (cur)
                   (logical-equal? item cur))
                 lst)))
         
       (logical-equal?  
        (lambda x
          (if (pair? x)
              (let ((exp8163 (cdr x)))
                (if (and (pair? exp8163) (null? (cdr exp8163)))
                    (if (equal? (car exp8163) (car x))
                        ((lambda (a) #t) (car x))
                        (let ((exp8164 (car x)))
                          (if (and (pair? exp8164) (equal? (car exp8164) 'list?))
                              (let ((exp8165 (cdr exp8164)))
                                (if (and (pair? exp8165) (null? (cdr exp8165)))
                                    (let ((exp8166 (car exp8163)))
                                      (if (and (pair? exp8166) (equal? (car exp8166) 'null?))
                                          (let ((exp8167 (cdr exp8166)))
                                            (if (and (pair? exp8167)
                                                     (null? (cdr exp8167))
                                                     (equal? (car exp8167) (car exp8165)))
                                                ((lambda (x) #t) (car exp8165))
                                                ((lambda (else) #f) x)))
                                          ((lambda (else) #f) x)))
                                    ((lambda (else) #f) x)))
                              ((lambda (else) #f) x))))
                    ((lambda (else) #f) x)))
              ((lambda (else) #f) x))))

       (truncate 
        (lambda (pos used-set-neg)
          (cond ((null? used-set-neg)
                '())
                ((>= pos (car used-set-neg))
                 (list pos))
                (else
                 (cons (car used-set-neg)
                       (truncate pos (cdr used-set-neg)))))))

       (truncate-neg 
        (lambda (pos used-set-neg)
          (cond ((null? used-set-neg)
                '())
                ((>= pos (car used-set-neg))
                 '())
                (else
                 (cons (car used-set-neg)
                       (truncate-neg pos (cdr used-set-neg)))))))
          


       ;;!(function update-count
       ;;          (form (update-count test tests-rest pos) -> void)
       ;;          (contract (test-struct list integer) -> void))
       ;; This function updates the test-times-used and test-used-set
       ;; fields of the test structs.  These fields are essential to
       ;; determining the order of the tests.
       (update-count 
        (lambda (test tests-rest pos mem-table)
          (let loop ((l tests-rest)
                     (p (add1 pos)))
            (if (null? l)
                (begin
                  ;; memoize
                  (hash-table-get mem-table (test-tst test) 
                                  (lambda () 
                                    (hash-table-put! 
                                     mem-table 
                                     (test-tst test) (list (test-used-set test)
                                                           (test-used-set-neg test)))))
                  )
                (let ((entry-pair 
                       (hash-table-get mem-table (test-tst test)
                                      (lambda ()
                                         (when (
                                                ;member
                                                logical-member
                                                ;inverse-in
                                                (test-tst test) (car l))
                                           (set-test-times-used! test (add1 (test-times-used test)))
                                           (set-test-used-set! test (cons p (test-used-set test)))
                                           (set-test-equal-set! test (cons p (test-equal-set test)))
                                           )
                                         (when (neg-inverse-in (test-tst test) (car l))
                                           (set-test-used-set-neg! test (cons p (test-used-set-neg test))))
                                         (loop (cdr l) (add1 p))
                                       ))))
                  (when (and (list? entry-pair) (not (null? entry-pair)))
                    (let ((trun-used (truncate pos (car entry-pair))))
                      (set-test-used-set! test trun-used)
                      (set-test-equal-set! test trun-used)
                      (set-test-times-used! test (length trun-used))
                      (set-test-used-set-neg! test (truncate-neg pos (cadr entry-pair)))))
                )))))

       ;;!(function update-counts
       ;;          (form (update-counts render-list) -> void)
       ;;          (contract list -> void))
       ;; This function essentially calls update-count on every test in
       ;; all of the test lists.
       (update-counts 
        (lambda (render-list)
          (let* ((mem-table (make-hash-table 'equal))
                 (test-master-list (map test-filter 
                                        (map car render-list)))
                 (test-so-far-lists ;; horrible name
                  (map
                    (lambda (tl)
                     (let ((f (map test-tst (test-filter tl))))
                       f))
                   test-master-list)))
            (let loop ((tml test-master-list)
                       (tsf test-so-far-lists)
                       (pos 1))
              (if (null? tml)
                  '()
                  (begin
                    (map (lambda (t)
                           (set-test-times-used! t 1)
                           (set-test-used-set! 
                            t 
                            (cons pos (test-used-set t)))
                           (set-test-equal-set! 
                            t 
                            (cons pos (test-equal-set t)))
                           (update-count t (cdr tsf) pos mem-table))
                         (car tml))
                    (loop (cdr tml) (cdr tsf) (add1 pos))))))))
       )
    (values update-counts)))


