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

