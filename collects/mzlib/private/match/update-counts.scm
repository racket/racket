;; This library is used by match.ss
;; This requires the test data structure.

(module update-counts mzscheme
  (provide update-counts)
  
  (require "test-structure.scm"
	   "match-helper.ss"
           (lib "etc.ss")
           (lib "list.ss"))
  
  ;;!(function test-filter
  ;;          (form (test-filter test-list) -> test-list)
  ;;          (contract list -> list))
  ;; This function filters out tests that do not need to be to have
  ;; their counts updated for reordering purposes.  These are the
  ;; more complex patterns such as or-patterns or ddk patterns.
  
  (define (test-filter tlist)
    (filter (lambda (t) (not (= -1 (test-times-used t)))) tlist))
  
  
  ;; !(function inverse-in
  ;;            (form (inverse-in test test-list) -> bool)
  ;;            (contract (s-exp list) -> bool))
  ;; This function checks to see if any of the members of the test-list
  ;; would be eliminated by the function if the test was in the test so far 
  ;; list.  This is the opposite of what the in function does.
  (define (inverse-in test test-list)
    (or (pos-inverse-in test test-list)
        (neg-inverse-in test test-list)))
  
  (define (pos-inverse-in test test-list)
    (let ([test-with-implied (cons test (implied test))])
      (ormap (lambda (t) (in t test-with-implied))
             test-list)))
        
  
  (define (neg-inverse-in test test-list)
    (let ([test-with-implied (cons test (implied test))])
      (ormap (lambda (t) (in `(not ,t) test-with-implied))
             test-list)))
      
  
  (define (logical-member item lst)
    (ormap (lambda (cur)
             (logical-equal? item cur))
           lst))
  
  (define (logical-equal? a b)
    (or (equal? a b)
        (and 
         ;; error checking
         (list? a) 
         (list? b)
         (list? (cdr a))
         (list? (cdr b))
         (null? (cddr a))
         (null? (cddr b))
         ;; end error checking
         (eq? (car a) 'list?)
         (eq? (car b) 'null?)
         (equal? (cadr a) (cadr b)))))
  
  ;; truncate-list : int listof[int] -> listof[int] 
  ;; truncate-list-neg : int listof[int] -> listof[int] 
  ;; truncate-list removes all elements of a list after the element at least as large as p
  ;; truncate-list-neg removes the found element as well
  (define-values (truncate-list truncate-list-neg)
    (let ([mk (lambda (pos-f) 
                (define (f p l)
                   (cond [(null? l)
                         '()]
                        [(>= p (car l))
                         (pos-f p)]
                        [else
                         (cons (car l)
                               (f p (cdr l)))]))
                f)])
      (values (mk list) (mk (lambda (x) '())))))
  
  
  
  ;; update-count : test listof[test] int -> void
  ;; This function updates the test-times-used and test-used-set
  ;; fields of the test structs.  These fields are essential to
  ;; determining the order of the tests.
  (define (update-count test tests-rest pos mem-table)
      (let loop ([l tests-rest]
                 [p (add1 pos)])
        (if (null? l)
            (hash-table-get mem-table (test-tst test) 
                            (lambda () 
                              (hash-table-put! 
                               mem-table 
                               (test-tst test) 
                               (list (test-used-set test)
                                     (test-used-set-neg test)))))            
            (let ([entry-pair 
                   (hash-table-get mem-table (test-tst test)
                                   (lambda ()
                                     (when (logical-member (test-tst test) (car l))
                                       (set-test-times-used! test (add1 (test-times-used test)))
                                       (set-test-used-set! test (cons p (test-used-set test)))
                                       (set-test-equal-set! test (cons p (test-equal-set test))))                                       
                                     (when (neg-inverse-in (test-tst test) (car l))
                                       (set-test-used-set-neg! test (cons p (test-used-set-neg test))))
                                     (loop (cdr l) (add1 p))))])                                  
              (when (and (list? entry-pair) (not (null? entry-pair)))
                (let ([trun-used (truncate-list pos (car entry-pair))])
                  (set-test-used-set! test trun-used)
                  (set-test-equal-set! test trun-used)
                  (set-test-times-used! test (length trun-used))
                  (set-test-used-set-neg! test (truncate-list-neg pos (cadr entry-pair)))))))))
              
  
  ;; update-counts : listof[(cons test any)] -> void
  ;; This function essentially calls update-count on every test in
  ;; all of the test lists.
  (define (update-counts render-list)
      (let* ([mem-table (make-hash-table 'equal)]
             [test-master-list (map (compose test-filter car) render-list)]
             [test-so-far-lists ;; horrible name
              (map
               (lambda (tl) (map test-tst (test-filter tl)))
               test-master-list)])
        (let loop ([tml test-master-list]
                   [tsf test-so-far-lists]
                   [pos 1])
          (if (null? tml)
              (void)
              (begin
                (for-each 
                 (lambda (t)
                   (set-test-times-used! t 1)
                   (set-test-used-set! 
                    t 
                    (cons pos (test-used-set t)))
                   (set-test-equal-set! 
                    t 
                    (cons pos (test-equal-set t)))
                   (update-count t (cdr tsf) pos mem-table))
                 (car tml))
                (loop (cdr tml) (cdr tsf) (add1 pos)))))))
  )



