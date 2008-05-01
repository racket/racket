#!r6rs

(library (tests r6rs enums)
  (export run-enums-tests)
  (import (rnrs)
          (tests r6rs test))

  ;; ----------------------------------------

  (define-enumeration color
    (black white purple maroon)
    color-set)
  
  ;; ----------------------------------------

  (define (run-enums-tests)
    
    (test (let* ((e (make-enumeration '(red green blue)))
                 (i (enum-set-indexer e)))
            (list (i 'red) (i 'green) (i 'blue) (i 'yellow)))
          '(0 1 2 #f))

    (let* ((e (make-enumeration '(red green blue)))
           (r ((enum-set-constructor e) '(red))))
      (test (enum-set->list (enum-set-universe e))
            '(red green blue))
      (test (enum-set->list (enum-set-universe r))
            '(red green blue))
      (test ((enum-set-indexer
              ((enum-set-constructor e) '(red)))
             'green)
            1)
      (test (enum-set-member? 'red e) #t)
      (test (enum-set-member? 'black e) #f)
      (test (enum-set-subset? e e) #t)
      (test (enum-set-subset? r e) #t)
      (test (enum-set-subset? e r) #f)
      (test (enum-set-subset? e (make-enumeration '(blue green red))) #t)
      (test (enum-set-subset? e (make-enumeration '(blue green red black))) #t)
      (test (enum-set-subset? (make-enumeration '(blue green red black)) e) #f)
      (test (enum-set-subset? ((enum-set-constructor
                                (make-enumeration '(blue green red black)))
                               '(red))
                              e) #f)
      (test (enum-set-subset? ((enum-set-constructor
                                (make-enumeration '(green red)))
                               '(red))
                              e) #t)
      (test (enum-set=? e e) #t)
      (test (enum-set=? r e) #f)
      (test (enum-set=? e r) #f)
      (test (enum-set=? e (make-enumeration '(blue green red))) #t))

    (test (let* ((e (make-enumeration '(red green blue)))
                 (c (enum-set-constructor e)))
            (list
             (enum-set-member? 'blue (c '(red blue)))
             (enum-set-member? 'green (c '(red blue)))
             (enum-set-subset? (c '(red blue)) e)
             (enum-set-subset? (c '(red blue)) (c '(blue red)))
             (enum-set-subset? (c '(red blue)) (c '(red)))
             (enum-set=? (c '(red blue)) (c '(blue red)))))
          (list #t #f #t #t #f #t))

    (test (let* ((e (make-enumeration '(red green blue)))
                 (c (enum-set-constructor e)))
            (enum-set->list (c '(blue red)))) 
          '(red blue))

    (test (let* ((e (make-enumeration '(red green blue)))
                 (c (enum-set-constructor e)))
            (list (enum-set->list
                   (enum-set-union (c '(blue)) (c '(red))))
                  (enum-set->list
                   (enum-set-intersection (c '(red green))
                                          (c '(red blue))))
                  (enum-set->list
                   (enum-set-difference (c '(red green))
                                        (c '(red blue))))))
          '((red blue) (red) (green)))

    (test (let* ((e (make-enumeration '(red green blue)))
                 (c (enum-set-constructor e)))
            (enum-set->list
             (enum-set-complement (c '(red)))))
          '(green blue))

    (test (let ((e1 (make-enumeration
                     '(red green blue black)))
                (e2 (make-enumeration
                     '(red black white))))
            (enum-set->list
             (enum-set-projection e1 e2)))
          '(red black))

    (test (color black)                      'black)
    ; (test/exn (color purpel) &syntax) ; not a runtime exception
    (test (enum-set->list (color-set))       '())
    (test (enum-set->list
           (color-set maroon white)) 
          '(white maroon))

    ;;
    ))

