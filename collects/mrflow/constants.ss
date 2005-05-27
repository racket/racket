
(module constants mzscheme
  (provide
   dummy
   dummy-thunk
   undefined
   thunk-empty
   thunk-false
   test-true
   test-false
   id
   (rename void-cst void)
   select-right
   select-left
   ;car!
   ;cdr!
   )
  
  (define dummy (void))
  (define dummy-thunk (lambda () dummy))
  (define undefined (letrec ([x x]) x))
  (define thunk-empty (lambda () '()))
  (define thunk-false (lambda () #f))
  (define test-true (lambda (x) #t))
  (define test-false (lambda (x) #f))
  (define id (lambda (x) x))
  (define void-cst (void))
  (define select-right (lambda (x y) y))
  (define select-left (lambda (x y) x))
  ;(define car! (case-lambda
  ;               [(pair) (car pair)]
  ;               [(pair val) (set-car! pair val)]))
  ;(define cdr! (case-lambda
  ;               [(pair) (cdr pair)]
  ;               [(pair val) (set-cdr! pair val)]))
  )
