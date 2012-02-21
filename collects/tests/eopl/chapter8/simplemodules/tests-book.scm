(module tests-book mzscheme
  
  (provide tests-for-run tests-for-check tests-for-parse)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define the-test-suite
 
    '(

      (modules-dans-simplest "
         module m1
          interface 
           [a : int
            b : int]
          body
           [a = 33
            c = -(a,1)
            b = -(c,a)]

         let a = 10
         in -(-(from m1 take a, from m1 take b), 
              a)"
        int 24)


      (example-8.2 "
         module m1 
          interface 
           [u : bool]
          body 
           [u = 33]

         44"
        error 44)

      (example-8.3 "
         module m1 
          interface 
           [u : int 
            v : int]
          body 
           [u = 33]

         44"
        error)

      (example-8.4 "
         module m1 
          interface 
           [u : int 
            v : int] 
          body 
           [v = 33 
            u = 44]

         from m1 take u" 
        error)

      (example-8.5a "
         module m1 
          interface 
           [u : int] 
          body 
           [u = 44]

         module m2 
          interface
           [v : int] 
          body 
           [v = -(from m1 take u,11)]

         -(from m1 take u, from m2 take v)"
        int)

      (example-8.5b "
         module m2 
          interface [v : int] 
          body 
           [v = -(from m1 take u,11)]

         module m1 
          interface [u : int] 
          body [u = 44]

         -(from m1 take u, from m2 take v)"
        error)

      ))

  (define tests-for-run
    (let loop ((lst the-test-suite))
      (cond
        ((null? lst) '())
        ((= (length (car lst)) 4)
         ;; (printf "creating item: ~s~%" (caar lst))
         (cons
           (list
             (list-ref (car lst) 0)
             (list-ref (car lst) 1)
             (list-ref (car lst) 3))
           (loop (cdr lst))))
        (else (loop (cdr lst))))))

  ;; ok to have extra members in a test-item.
  (define tests-for-check the-test-suite)

  (define tests-for-parse the-test-suite)

  )

