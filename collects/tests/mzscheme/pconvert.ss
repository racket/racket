
(load-relative "loadtest.ss")

(SECTION 'pconvert)

(require (lib "unit.ss")
         (lib "file.ss")
         (lib "class.ss")
         (lib "pconvert.ss"))

(constructor-style-printing #t)
(quasi-read-style-printing #f)

(define (xl) 1)
(define (xu) (unit (import) (export)))
(define (xc) (class object% () (sequence (super-init))))

(let ()
  (define-struct pctest (value constructor-sexp
                               whole/frac-constructor-sexp
                               shared-constructor-sexp
                               quasi-sexp
                               whole/frac-quasi-sexp
                               shared-quasi-sexp
                               cons-as-list))
  
  (define-struct no-cons-test (value constructor-sexp shared-constructor-sexp
                                     quasi-sexp shared-quasi-sexp))
  (define-struct same-test (value sexp))
  (define get-value
    (lambda (test-case)
      (cond
        [(pctest? test-case)
         (pctest-value test-case)]
        [(no-cons-test? test-case)
         (no-cons-test-value test-case)]
        [(same-test? test-case)
         (same-test-value test-case)])))
  (define run-test
    (lambda (test-case)
      (let* ([before (get-value test-case)]
             [cmp
              (lambda (selector constructor-style?
                                quasi-read?
                                sharing?
                                cons-as-list?
                                whole/fractional-numbers?)
                (unless (parameterize ([constructor-style-printing constructor-style?]
                                       [show-sharing sharing?]
                                       [quasi-read-style-printing quasi-read?]
                                       [abbreviate-cons-as-list cons-as-list?]
                                       [whole/fractional-exact-numbers whole/fractional-numbers?])
                          (test (selector test-case) print-convert before))
                  (printf
                   ">> (constructor-style-printing ~a) (quasi-read-style-printing ~a) (show-sharing ~a) (abbreviate-cons-as-list ~a) (whole/fractional-exact-numbers ~a)~n"
                   constructor-style? quasi-read? 
                   sharing? cons-as-list?
                   whole/fractional-numbers?)))])
        ;(printf "testing: ~s~n" before)
        ;(printf ".") (flush-output (current-output-port))
        (cond
          [(pctest? test-case)
           (cmp pctest-constructor-sexp #t #f #f #t #f)
           (cmp pctest-whole/frac-constructor-sexp #t #f #f #t #t)
           (cmp pctest-shared-constructor-sexp #t #f #t #t #f)
           (cmp pctest-quasi-sexp #f #f #f #t #f)
           (cmp pctest-whole/frac-quasi-sexp #f #f #f #t #t)
           (cmp pctest-shared-quasi-sexp #f #f #t #t #f)
           (cmp pctest-cons-as-list #t #f #f #f #f)]
          [(no-cons-test? test-case)
           (cmp no-cons-test-shared-constructor-sexp #t #f #t #t #t)
           (cmp no-cons-test-constructor-sexp #t #f #f #t #t)
           (cmp no-cons-test-shared-quasi-sexp #f #f #t #t #t)
           (cmp no-cons-test-quasi-sexp #f #f #f #t #t)]
          [(same-test? test-case)
           (cmp same-test-sexp #t #t #t #t #t)
           (cmp same-test-sexp #t #t #t #t #f)
           (cmp same-test-sexp #t #t #t #f #t)
           (cmp same-test-sexp #t #t #t #f #f)
           (cmp same-test-sexp #t #t #f #t #t)
           (cmp same-test-sexp #t #t #f #t #f)
           (cmp same-test-sexp #t #t #f #f #t)
           (cmp same-test-sexp #t #t #f #f #f)
           
           (cmp same-test-sexp #t #f #t #t #t)
           (cmp same-test-sexp #t #f #t #t #f)
           (cmp same-test-sexp #t #f #t #f #t)
           (cmp same-test-sexp #t #f #t #f #f)
           (cmp same-test-sexp #t #f #f #t #t)
           (cmp same-test-sexp #t #f #f #t #f)
           (cmp same-test-sexp #t #f #f #f #t)
           (cmp same-test-sexp #t #f #f #f #f)
           
           (cmp same-test-sexp #f #t #t #t #t)
           (cmp same-test-sexp #f #t #t #t #f)
           (cmp same-test-sexp #f #t #t #f #t)
           (cmp same-test-sexp #f #t #t #f #f)
           (cmp same-test-sexp #f #t #f #t #t)
           (cmp same-test-sexp #f #t #f #t #f)
           (cmp same-test-sexp #f #t #f #f #t)
           (cmp same-test-sexp #f #t #f #f #f)
           
           (cmp same-test-sexp #f #f #t #t #t)
           (cmp same-test-sexp #f #f #t #t #f)
           (cmp same-test-sexp #f #f #t #f #t)
           (cmp same-test-sexp #f #f #t #f #f)
           (cmp same-test-sexp #f #f #f #t #t)
           (cmp same-test-sexp #f #f #f #t #f)
           (cmp same-test-sexp #f #f #f #f #t)
           (cmp same-test-sexp #f #f #f #f #f)]))))
  
  (define 
    tests
    (list
     (make-same-test "abc" "abc")
     (make-same-test 'a ''a)
     
     (make-same-test 8 8)
     (make-same-test 1/2 1/2)
     (make-same-test 1.1 1.1)
     
     (make-pctest -10/3 -10/3 '(+ -3 -1/3) -10/3 -10/3 '(+ -3 -1/3) -10/3 -10/3)
     (make-pctest 3/2 3/2 '(+ 1 1/2) 3/2 3/2 '(+ 1 1/2) 3/2 3/2)
     
     (make-pctest (list 1) '(list 1) '(list 1) '(list 1) '`(1) '`(1) '`(1) '(cons 1 empty))
     (make-pctest (list 1/2) '(list 1/2) '(list 1/2) '(list 1/2)
                  '`(1/2) '`(1/2)  '`(1/2)
                  '(cons 1/2 empty))
     (make-pctest (list 3/2) '(list 3/2) '(list (+ 1 1/2)) '(list 3/2)
                  '`(3/2) '`(,(+ 1 1/2)) '`(3/2)
                  '(cons 3/2 empty))
     (make-pctest (list 1/2+1/2i) 
                  '(list 1/2+1/2i) 
                  '(list (+ 1/2 (* 0+1i 1/2))) 
                  '(list 1/2+1/2i)
                  '`(1/2+1/2i) 
                  '`(,(+ 1/2 (* 0+1i 1/2)))  
                  '`(1/2+1/2i)
                  '(cons 1/2+1/2i empty))
     (make-pctest (list 3/2+1/2i) 
                  '(list 3/2+1/2i) 
                  '(list (+ (+ 1 1/2) (* 0+1i 1/2))) 
                  '(list 3/2+1/2i)
                  '`(3/2+1/2i) 
                  '`(,(+ (+ 1 1/2) (* 0+1i 1/2)))  
                  '`(3/2+1/2i)
                  '(cons 3/2+1/2i empty))
     (make-pctest (list 1/2+3/2i) 
                  '(list 1/2+3/2i) 
                  '(list (+ 1/2 (* 0+1i (+ 1 1/2)))) 
                  '(list 1/2+3/2i)
                  '`(1/2+3/2i) 
                  '`(,(+ 1/2 (* 0+1i (+ 1 1/2))))  
                  '`(1/2+3/2i)
                  '(cons 1/2+3/2i empty))
     (make-pctest (list 3/2+3/2i) 
                  '(list 3/2+3/2i) 
                  '(list (+ (+ 1 1/2) (* 0+1i (+ 1 1/2)))) 
                  '(list 3/2+3/2i)
                  '`(3/2+3/2i) 
                  '`(,(+ (+ 1 1/2) (* 0+1i (+ 1 1/2))))  
                  '`(3/2+3/2i)
                  '(cons 3/2+3/2i empty))
     
     (make-same-test (vector 0 0 0 0 0 0 0 0 0 0) '(vector 0 0 0 0 0 0 0 0 0 0))
     (make-same-test #t 'true)
     (make-same-test #f 'false)
     
     (make-same-test (interface () a b c) '(interface ...))
     
     (make-same-test (delay 1) '(delay ...))
     (make-same-test (let () (define-struct a (a) (make-inspector)) (make-a 3)) '(make-a 3))
     (make-same-test (box 3) '(box 3))
     (make-pctest null 'empty 'empty 'empty '`() '`() '`() 'empty)
     (make-same-test add1 'add1)
     (make-same-test (void) '(void))
     (make-same-test (unit (import) (export)) '(unit ...))
     (make-same-test (make-weak-box 12) '(make-weak-box 12))
     (make-same-test (regexp "1") '(regexp "1"))
     (make-same-test (module-path-index-join #f #f) '(module-path-index-join false false))
     (make-same-test (lambda () 0) '(lambda () ...))

     (make-same-test xl 'xl)
     (make-same-test (letrec ([xl (lambda () 1)]) xl) '(lambda () ...))
     (make-same-test (letrec ([xl-ID-BETTER-NOT-BE-DEFINED (lambda () 1)]) 
                       xl-ID-BETTER-NOT-BE-DEFINED)
                     '(lambda () ...))
     (make-same-test xc 'xc)
     (make-same-test (letrec ([xc (class object% ())]) xc) '(class ...))
     (make-same-test (letrec ([xc-ID-BETTER-NOT-BE-DEFINED (class object% ())]) 
                       xc-ID-BETTER-NOT-BE-DEFINED)
                     '(class ...))
     (make-same-test xu 'xu)
     (make-same-test (letrec ([xu (unit (import) (export))]) xu)
                     '(unit ...))
     (make-same-test (letrec ([xu-ID-BETTER-NOT-BE-DEFINED (unit (import) (export))]) 
                       xu-ID-BETTER-NOT-BE-DEFINED)
                     '(unit ...))
     (make-same-test (lambda (x) x) '(lambda (a1) ...))
     (make-same-test (lambda x x) '(lambda args ...))
     (make-same-test (lambda (a b . c) a) '(lambda (a1 a2 . args) ...))
     (make-same-test (case-lambda) '(case-lambda))
     (make-same-test (case-lambda [() 'a] [(x) 'a]) '(case-lambda [() ...] [(a1) ...]))
     (make-same-test (case-lambda [() 'a] [(x y) 'a])
                     '(case-lambda [() ...] [(a1 a2) ...]))
     (make-same-test (case-lambda [() 'a] [(x . y) 'a])
                     '(case-lambda [() ...] [(a1 . args) ...]))
     (make-same-test (case-lambda [() 'a] [x 'a])
                     '(case-lambda [() ...] [args ...]))
     (make-same-test (case-lambda [() 'a] [(x y z) 'a] [x 'a])
                     '(case-lambda [() ...] [(a1 a2 a3) ...] [args ...]))
     (make-same-test (make-hash-table)
                     '(hash-table))
     (make-same-test (make-hash-table 'weak)
                     '(hash-table 'weak))
     (make-same-test (make-hash-table 'equal)
                     '(hash-table 'equal))
     (make-same-test (make-hash-table 'equal 'weak)
                     '(hash-table 'equal 'weak))
     (make-same-test (let ([ht (make-hash-table)])
                       (hash-table-put! ht 'x 1)
                       ht)
                     '(hash-table ('x 1)))
     (make-pctest (list 'a (box (list ())) (cons 1 '()))
                  '(list (quote a) (box (list empty)) (list 1))
                  '(list (quote a) (box (list empty)) (list 1))
                  '(list (quote a) (box (list empty)) (list 1))
                  '`(a ,(box `(())) (1))
                  '`(a ,(box `(())) (1))
                  '`(a ,(box `(())) (1))
                  '(cons 'a 
                         (cons (box (cons empty empty))
                               (cons (cons 1 empty)
                                     empty))))
     (make-pctest (list "" "" (vector) (vector))
                  '(list "" "" (vector) (vector))
                  '(list "" "" (vector) (vector))
                  '(list "" "" (vector) (vector))
                  '`("" "" ,(vector) ,(vector))
                  '`("" "" ,(vector) ,(vector))
                  '`("" "" ,(vector) ,(vector))
                  '(cons "" (cons "" (cons (vector) (cons (vector) empty)))))
     (make-pctest (let ([x (list 1)]) (set-car! x x) x)
                  '(shared ([-0- (list -0-)]) -0-)
                  '(shared ([-0- (list -0-)]) -0-)
                  '(shared ([-0- (list -0-)]) -0-)
                  '(shared ([-0- `(,-0-)]) -0-)
                  '(shared ([-0- `(,-0-)]) -0-)
                  '(shared ([-0- `(,-0-)]) -0-)
                  '(shared ([-0- (cons -0- empty)]) -0-))
     (make-pctest (let ([x (list 1)]) (set-cdr! x x) x)
                  '(shared ([-0- (cons 1 -0-)]) -0-)
                  '(shared ([-0- (cons 1 -0-)]) -0-)
                  '(shared ([-0- (cons 1 -0-)]) -0-)
                  '(shared ([-0- `(1 . ,-0-)]) -0-)
                  '(shared ([-0- `(1 . ,-0-)]) -0-)
                  '(shared ([-0- `(1 . ,-0-)]) -0-)
                  '(shared ([-0- (cons 1 -0-)]) -0-))
     (make-pctest (let* ([a (list 1 2 3)]
                         [b (list 1 a (cdr a))])
                    (set-car! b b)
                    (append b (list (list 2 3))))
                  '(shared ([-1- (list -1- (list 1 2 3) (list 2 3))])
                     (list -1- (list 1 2 3) (list 2 3) (list 2 3)))
                  '(shared ([-1- (list -1- (list 1 2 3) (list 2 3))])
                     (list -1- (list 1 2 3) (list 2 3) (list 2 3)))
                  '(shared ([-1- (list -1- -3- -4-)]
                            [-3- (cons 1 -4-)]
                            [-4- (list 2 3)])
                     (list -1- -3- -4- (list 2 3)))
                  '(shared ([-1- `(,-1- (1 2 3) (2 3))])
                     `(,-1- (1 2 3) (2 3) (2 3)))
                  '(shared ([-1- `(,-1- (1 2 3) (2 3))])
                     `(,-1- (1 2 3) (2 3) (2 3)))
                  '(shared ([-1- `(,-1- ,-3- ,-4-)]
                            [-3- `(1 . ,-4-)]
                            [-4- `(2 3)])
                     `(,-1- ,-3- ,-4- (2 3)))
                  '(shared ([-1- (cons -1- 
                                       (cons (cons 1 (cons 2 (cons 3 empty)))
                                             (cons (cons 2 (cons 3 empty))
                                                   empty)))])
                     (cons -1- 
                           (cons (cons 1 (cons 2 (cons 3 empty)))
                                 (cons (cons 2 (cons 3 empty)) 
                                       (cons (cons 2 (cons 3 empty))
                                             empty))))))
     (make-no-cons-test (let* ([a (list 1 2 3)]
                               [b (list 1 a (cdr a))])
                          (set-car! b b)
                          (let* ([share-list (append b (list (list 2 3)))]
                                 [v (vector 1 share-list (cdr share-list))])
                            (vector-set! v 0 v)
                            v))
                        '(shared
                             ((-0- (vector -0-
                                           (list -2-
                                                 (list 1 2 3)
                                                 (list 2 3)
                                                 (list 2 3))
                                           (list (list 1 2 3)
                                                 (list 2 3)
                                                 (list 2 3))))
                              (-2- (list -2- (list 1 2 3) (list 2 3))))
                           -0-)
                        '(shared
                             ((-0- (vector -0- (cons -2- -8-) -8-))
                              (-2- (list -2- -4- -5-))
                              (-4- (cons 1 -5-))
                              (-5- (list 2 3))
                              (-8- (list -4- -5- (list 2 3))))
                           -0-)
                        '(shared
                             ((-0- (vector -0-
                                           `(,-2-
                                             (1 2 3)
                                             (2 3)
                                             (2 3))
                                           `((1 2 3)
                                             (2 3)
                                             (2 3))))
                              (-2- `(,-2- (1 2 3) (2 3))))
                           -0-)
                        '(shared
                             ((-0- (vector -0- `(,-2- . ,-8-) -8-))
                              (-2- `(,-2- ,-4- ,-5-))
                              (-4- `(1 . ,-5-))
                              (-5- `(2 3))
                              (-8- `(,-4- ,-5- (2 3))))
                           -0-))))
  (for-each run-test tests))

(let ()
  (define make-pctest-shared
    (lambda (shared?)
      (lambda (object output)
        (parameterize ([constructor-style-printing #t]
                       [show-sharing #t]
                       [quasi-read-style-printing #f]
                       [abbreviate-cons-as-list #t])
          (test (if shared?
                    `(shared ((-1- ,output))
                       (list -1- -1-))
                    `(list ,output ,output))
                print-convert
                (list object object))))))
  (define test-shared (make-pctest-shared #t))
  (define test-not-shared (make-pctest-shared #f))
  
  (test-not-shared #t 'true)
  (test-not-shared #f 'false)
  (test-not-shared 1 1)
  (test-not-shared 3276832768327683276832768327683276832768
                   3276832768327683276832768327683276832768)
  (test-shared (regexp "") '(regexp ""))
  (let ([in (open-input-string "")]) (test-shared in in))
  (let ([out (open-output-string)]) (test-shared out out))
  (test-not-shared #\a #\a)
  (test-not-shared 'x ''x)
  (test-shared (lambda (x) x) '(lambda (a1) ...))
  (test-shared (delay 1) '(delay ...))
  (test-shared (class object% ()) '(class ...))
  (test-shared (unit (import) (export)) '(unit ...))
  (test-shared (new (class object% (super-new))) '(instantiate (class ...) ...))
  
  (test-shared "abc" "abc")
  (test-shared (list 1 2 3) '(list 1 2 3))
  (test-shared (vector 1 2 3) '(vector 1 2 3))
  (let () (define-struct a () (make-inspector)) (test-shared (make-a) '(make-a)))
  (test-shared (box 1) '(box 1))
  (test-shared (make-hash-table) '(hash-table)))

(arity-test print-convert 1 2)
(arity-test build-share 1 1)
(arity-test get-shared 1 2)
(arity-test print-convert-expr 3 3)

(test 'empty print-convert '())

(let ([fn (make-temporary-file "pconvert.ss-test~a")])
  (let ([in (open-input-file fn)])
    (test `(open-input-file ,fn) print-convert in)
    (close-input-port in))
  (delete-file fn))

(let ()
  (define-struct hidden (a))
  (define-struct visible (b) (make-inspector))
  (test '(make-hidden ...) print-convert (make-hidden 1))
  (test '(make-visible 2) print-convert (make-visible 2)))

(let ([pc
       (lambda (pv)
         (lambda (x)
           (parameterize ([booleans-as-true/false pv])
             (print-convert x))))])
  (test 'false (pc #t) #f)
  (test 'true (pc #t) #t)
  (test #f (pc #f) #f)
  (test #t (pc #f) #t))

(let ([pc
       (lambda (pv)
         (lambda (x)
           (parameterize ([named/undefined-handler (lambda (x) 'whee)]
                          [use-named/undefined-handler
                           (lambda (x) pv)])
             (print-convert x))))])
  (test '(lambda (a1) ...) (pc #f) (let ([f (lambda (x) x)]) f))
  (test 'whee (pc #t) (let ([f (lambda (x) x)]) f))
  (test '(list whee whee) 
        (pc #t) 
        (let ([g (lambda (y) (let ([f (lambda (x) y)]) f))]) (list (g 1) (g 2)))))

(report-errs)
