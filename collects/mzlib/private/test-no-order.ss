(module test-no-order mzscheme
  (require (lib "list.ss"))

  (provide match:test-no-order)
  
  ;;!(function match:test-no-order 
  ;;          (form (match:test-no-order tests l last-test ddk-num)
  ;;                -> 
  ;;                bool)
  ;;          (contract (list list test integer) -> bool))
  ;; This is a recursive depth first search for a sequence of
  ;; items in list l which will satisfy all of the tests in list
  ;; tests.  This is used for list-no-order and hash-table patterns.
  ;; This function also handles ddk patterns by passing it the last
  ;; test before the ddk and the value of k.
  (define (match:test-no-order tests l last-test ddk-num)
    (define (handle-last-test test l)
      (and (>= (length l) ddk-num) 
           (andmap test l)))
    (define (dep-first-test head rest tests)
      (cond [(null? tests) 
             (if last-test 
                 (handle-last-test last-test (cons head rest)) 
                 #f)]
            [(null? rest)
             (if last-test
                 (and (= 0 ddk-num)
                      (= 1 (length tests))
                      ((car tests) head))
                 (and (= 1 (length tests))
                      ((car tests) head)))]
            [else (and (pair? tests)
                       ((car tests) head)
                       (match:test-no-order (cdr tests) 
                                            rest 
                                            last-test 
                                            ddk-num))]))
    (ormap (lambda (elem) (dep-first-test elem (remove elem l) tests)) l)))