(load-relative "loadtest.rktl")

(require (for-syntax syntax/parse racket/syntax)
         syntax/id-set)
  
(Section 'id-set)

;; ----------------------------------------------------------------------------
;; Universal Tests

(define-syntax (mk-test-suite stx)
  (syntax-parse stx
    [(_ #:constructor mk-free-id-set)
     #:with free-id-set-pred? (format-id #'mk-free-id-set "~a?" #'mk-free-id-set)
     #'(begin
         (define EMPTY (mk-free-id-set))
         (define ABC (mk-free-id-set (list #'a #'b #'c)))
         (define ABCD (mk-free-id-set (list #'a #'b #'c #'d)))

         (test #t free-id-set? EMPTY)
         (test #t free-id-set? ABC)
         (test #t free-id-set? ABCD)
         (test #t free-id-set-pred? EMPTY)
         (test #t free-id-set-pred? ABC)
         (test #t free-id-set-pred? ABCD)

         (test #t free-id-set-empty? EMPTY)
         (test #f free-id-set-empty? ABC)
         (test #f free-id-set-empty? ABCD)

         (test 0 free-id-set-count EMPTY)
         (test 3 free-id-set-count ABC)
         (test 4 free-id-set-count ABCD)

         (test #t free-id-set-member? ABC #'a)
         (test #t free-id-set-member? ABC #'b)
         (test #t free-id-set-member? ABC #'c)
         (test #f free-id-set-member? ABC #'d)
         (test #t free-id-set-member? ABCD #'a)
         (test #t free-id-set-member? ABCD #'b)
         (test #t free-id-set-member? ABCD #'c)
         (test #t free-id-set-member? ABCD #'d)
         (test #t free-id-set-member? (mk-free-id-set (list #'x)) #'x)
         (test #f free-id-set-member? (let ([x 1]) (mk-free-id-set (list #'x))) #'x)
         (test #f free-id-set-member? (let ([x 1]) (mk-free-id-set (list #'x))) 
               (let ([x 1]) #'x))
         
         ;(test #t free-id-set=? (free-id-set->list ABC) (for/list ([v ABC]) v))
         (test #t free-id-set=? (mk-free-id-set (free-id-set->list ABC))
                                (mk-free-id-set (for/list ([v (in-free-id-set ABC)]) v)))
         (test #t free-id-set=? (mk-free-id-set (free-id-set->list ABCD))
                                (let ([seq (in-free-id-set ABCD)])
                                  (mk-free-id-set (for/list ([v seq]) v))))
         (test #t sequence? (in-free-id-set ABCD))
         (test #f stream? (in-free-id-set ABCD))
         (test #t stream? (free-id-set->stream ABCD))
         (test #t sequence? (free-id-set->stream ABCD)) 
         (test #t free-id-set=? (mk-free-id-set (free-id-set->list ABCD))
                                (let ([seq (free-id-set->stream ABCD)])
                                  (mk-free-id-set (for/list ([v seq]) v))))
         (test #t values (let ([noset #t])
                           (for ([v (in-free-id-set (mk-free-id-set))]) (set! noset #f))
                           noset))

         (test #t free-id-set=? ABCD (free-id-set-copy ABCD))
         (test #t eq? ABCD ABCD)
         (test #f eq? ABCD (free-id-set-copy ABCD))
         (test #t free-id-set-empty? (free-id-set-copy-clear ABCD))
         (test #f eq? EMPTY (free-id-set-copy-clear ABCD))
         (test #t free-id-set-pred? (free-id-set-copy ABCD))
         (test #t free-id-set-pred? (free-id-set-copy-clear ABCD))
         
         ;; set union
         (let ()
           (define EMPTY/MUTABLE (mutable-free-id-set null))
           (define EMPTY/IMMUTABLE (immutable-free-id-set null))
           (define ABC-LIST (list #'a #'b #'c))
           (define ABC/MUTABLE (mutable-free-id-set ABC-LIST))
           (define ABC/IMMUTABLE (immutable-free-id-set ABC-LIST))
           (test 3 free-id-set-count (free-id-set-union ABC/IMMUTABLE))
           (test #t free-id-set-empty? (free-id-set-union EMPTY/IMMUTABLE))
           (test 4 free-id-set-count (free-id-set-union EMPTY/IMMUTABLE ABCD))
           (test 4 free-id-set-count (free-id-set-union ABC/IMMUTABLE ABCD))
           (test 3 free-id-set-count (free-id-set-union ABC/IMMUTABLE EMPTY))
           (define IMMUTABLE/UNION/3 
             (free-id-set-union ABC/IMMUTABLE
                                ABCD
                                (immutable-free-id-set (list #'d #'e #'f))))
           (test 6 free-id-set-count IMMUTABLE/UNION/3)
           (test #t free-id-set-member? IMMUTABLE/UNION/3 #'d)
           (test #t free-id-set-member? IMMUTABLE/UNION/3 #'e)
           (test #t free-id-set-member? IMMUTABLE/UNION/3 #'f)
           
           (free-id-set-union! ABC/MUTABLE)
           (test 3 free-id-set-count ABC/MUTABLE)
           (free-id-set-union! EMPTY/MUTABLE)
           (test #t free-id-set-empty? EMPTY/MUTABLE)
           (free-id-set-union! EMPTY/MUTABLE ABCD)
           (test 4 free-id-set-count EMPTY/MUTABLE)
           (free-id-set-union! ABC/MUTABLE ABCD)
           (test 4 free-id-set-count ABC/MUTABLE)
           (free-id-set-union! ABC/MUTABLE EMPTY)
           (test 4 free-id-set-count ABC/MUTABLE)
           (define MUTABLE/UNION/3 (mutable-free-id-set (list #'a #'b #'c)))
           (free-id-set-union! MUTABLE/UNION/3
                               ABCD
                               (mutable-free-id-set (list #'d #'e #'f)))
           (test 6 free-id-set-count MUTABLE/UNION/3)
           (test #t free-id-set-member? MUTABLE/UNION/3 #'d)
           (test #t free-id-set-member? MUTABLE/UNION/3 #'e)
           (test #t free-id-set-member? MUTABLE/UNION/3 #'f)
           (void))
         
         ;; set intersect
         (let ()
           (define EMPTY/MUTABLE (mutable-free-id-set null))
           (define EMPTY/IMMUTABLE (immutable-free-id-set null))
           (define ABC-LIST (list #'a #'b #'c))
           (define ABC/MUTABLE (mutable-free-id-set ABC-LIST))
           (define ABC/IMMUTABLE (immutable-free-id-set ABC-LIST))
           (test 3 free-id-set-count (free-id-set-intersect ABC/IMMUTABLE))
           (test #t free-id-set-empty? (free-id-set-intersect EMPTY/IMMUTABLE))
           (test 0 free-id-set-count (free-id-set-intersect EMPTY/IMMUTABLE ABCD))
           (test 3 free-id-set-count (free-id-set-intersect ABC/IMMUTABLE ABCD))
           (test 0 free-id-set-count (free-id-set-intersect ABC/IMMUTABLE EMPTY))
           (define IMMUTABLE/INTERSECT/3 
             (free-id-set-intersect ABC/IMMUTABLE
                                    ABCD
                                    (immutable-free-id-set (list #'b #'c))))
           (test 2 free-id-set-count IMMUTABLE/INTERSECT/3)
           (test #f free-id-set-member? IMMUTABLE/INTERSECT/3 #'a)
           (test #t free-id-set-member? IMMUTABLE/INTERSECT/3 #'b)
           (test #t free-id-set-member? IMMUTABLE/INTERSECT/3 #'c)
           
           (free-id-set-intersect! ABC/MUTABLE)
           (test 3 free-id-set-count ABC/MUTABLE)
           (free-id-set-intersect! EMPTY/MUTABLE)
           (test #t free-id-set-empty? EMPTY/MUTABLE)
           (free-id-set-intersect! EMPTY/MUTABLE ABCD)
           (test 0 free-id-set-count EMPTY/MUTABLE)
           (free-id-set-intersect! ABC/MUTABLE ABCD)
           (test 3 free-id-set-count ABC/MUTABLE)
           (test #t free-id-set-member? ABC/MUTABLE #'a)
           (test #t free-id-set-member? ABC/MUTABLE #'b)
           (test #t free-id-set-member? ABC/MUTABLE #'c)
           (test #f free-id-set-member? ABC/MUTABLE #'d)
           (test #t mutable-free-id-set? ABC/MUTABLE)
           (test #t free-id-set-empty? EMPTY)
           (free-id-set-intersect! ABC/MUTABLE EMPTY)
           (test 0 free-id-set-count ABC/MUTABLE)
           (define MUTABLE/INTERSECT/3 (mutable-free-id-set (list #'a #'b #'c)))
           (free-id-set-intersect! MUTABLE/INTERSECT/3
                                   ABCD
                                   (mutable-free-id-set (list #'a #'b)))
           (test 2 free-id-set-count MUTABLE/INTERSECT/3)
           (test #t free-id-set-member? MUTABLE/INTERSECT/3 #'a)
           (test #t free-id-set-member? MUTABLE/INTERSECT/3 #'b)
           (test #f free-id-set-member? MUTABLE/INTERSECT/3 #'c)
         
           (void))
         
         ;; set subtract
         (let ()
           (define EMPTY/MUTABLE (mutable-free-id-set null))
           (define EMPTY/IMMUTABLE (immutable-free-id-set null))
           (define ABCDE-LIST (list #'a #'b #'c #'d #'e))
           (define ABCDE/MUTABLE (mutable-free-id-set ABCDE-LIST))
           (define ABCDE/IMMUTABLE (immutable-free-id-set ABCDE-LIST))
           (test 5 free-id-set-count (free-id-set-subtract ABCDE/IMMUTABLE))
           (test #t free-id-set-empty? (free-id-set-subtract EMPTY/IMMUTABLE))
           (test 0 free-id-set-count (free-id-set-subtract EMPTY/IMMUTABLE ABCD))
           (test 1 free-id-set-count (free-id-set-subtract ABCDE/IMMUTABLE ABCD))
           (test 5 free-id-set-count (free-id-set-subtract ABCDE/IMMUTABLE EMPTY))
           (define IMMUTABLE/SUBTRACT/3
             (free-id-set-subtract ABCDE/IMMUTABLE
                                   ABC
                                   (immutable-free-id-set (list #'a #'b #'e))))
           (test 1 free-id-set-count IMMUTABLE/SUBTRACT/3)
           (test #f free-id-set-member? IMMUTABLE/SUBTRACT/3 #'a)
           (test #f free-id-set-member? IMMUTABLE/SUBTRACT/3 #'b)
           (test #t free-id-set-member? IMMUTABLE/SUBTRACT/3 #'d)
           (test #f free-id-set-member? IMMUTABLE/SUBTRACT/3 #'e)
           
           (free-id-set-subtract! ABCDE/MUTABLE)
           (test 5 free-id-set-count ABCDE/MUTABLE)
           (free-id-set-subtract! EMPTY/MUTABLE)
           (test #t free-id-set-empty? EMPTY/MUTABLE)
           (free-id-set-subtract! EMPTY/MUTABLE ABCD)
           (test 0 free-id-set-count EMPTY/MUTABLE)
           (free-id-set-subtract! ABCDE/MUTABLE ABC)
           (test 2 free-id-set-count ABCDE/MUTABLE)
           (test #f free-id-set-member? ABCDE/MUTABLE #'a)
           (test #f free-id-set-member? ABCDE/MUTABLE #'b)
           (test #f free-id-set-member? ABCDE/MUTABLE #'c)
           (test #t free-id-set-member? ABCDE/MUTABLE #'d)
           (test #t free-id-set-member? ABCDE/MUTABLE #'e)
           (test #t mutable-free-id-set? ABCDE/MUTABLE)
           (test #t free-id-set-empty? EMPTY)
           (set! ABCDE/MUTABLE (mutable-free-id-set ABCDE-LIST))
           (free-id-set-subtract! ABCDE/MUTABLE EMPTY)
           (test 5 free-id-set-count ABCDE/MUTABLE)
           (define MUTABLE/SUBTRACT/3 (mutable-free-id-set (list #'a #'b #'c #'d #'e)))
           (free-id-set-subtract! MUTABLE/SUBTRACT/3
                                  ABC
                                  (mutable-free-id-set (list #'a #'b #'e)))
           (test 1 free-id-set-count MUTABLE/SUBTRACT/3)
           (test #f free-id-set-member? MUTABLE/SUBTRACT/3 #'a)
           (test #f free-id-set-member? MUTABLE/SUBTRACT/3 #'b)
           (test #t free-id-set-member? MUTABLE/SUBTRACT/3 #'d)
           (test #f free-id-set-member? MUTABLE/SUBTRACT/3 #'e)
         
           (void))
         
         )]))


;; ----------------------------------------------------------------------------
;; run test suite instances
(mk-test-suite #:constructor mutable-free-id-set)
(mk-test-suite #:constructor immutable-free-id-set)

;; ----------------------------------------------------------------------------
;; immutable/mutable set tests
(define EMPTY/MUTABLE/FREE (mutable-free-id-set))
(define NONEMPTY/MUTABLE/FREE (mutable-free-id-set (list #'a #'b #'c)))
(define EMPTY/IMMUTABLE/FREE (immutable-free-id-set))
(define NONEMPTY/IMMUTABLE/FREE (immutable-free-id-set (list #'a #'b #'c)))

(test #t mutable-free-id-set? EMPTY/MUTABLE/FREE)
(test #f mutable-free-id-set? EMPTY/IMMUTABLE/FREE)
(test #t mutable-free-id-set? NONEMPTY/MUTABLE/FREE)
(test #f mutable-free-id-set? NONEMPTY/IMMUTABLE/FREE)
(test #f immutable-free-id-set? EMPTY/MUTABLE/FREE)
(test #t immutable-free-id-set? EMPTY/IMMUTABLE/FREE)
(test #f immutable-free-id-set? NONEMPTY/MUTABLE/FREE)
(test #t immutable-free-id-set? NONEMPTY/IMMUTABLE/FREE)

(test #t free-id-set=? EMPTY/MUTABLE/FREE EMPTY/IMMUTABLE/FREE)
(test #t free-id-set=? NONEMPTY/MUTABLE/FREE NONEMPTY/IMMUTABLE/FREE)
(test #f free-id-set=? EMPTY/MUTABLE/FREE NONEMPTY/MUTABLE/FREE)
(test #f free-id-set=? EMPTY/IMMUTABLE/FREE NONEMPTY/IMMUTABLE/FREE)
(test #f free-id-set=? EMPTY/MUTABLE/FREE NONEMPTY/IMMUTABLE/FREE)

;; ----------------------------------------------------------------------------
;; immutable set tests

(let ([s (immutable-free-id-set (list #'a #'b #'c))])
  ;; fns not implemented for immutable id sets
  (err/rt-test (free-id-set-add! s #'z) exn:fail?)
  (err/rt-test (free-id-set-remove! s #'z) exn:fail?)
  (err/rt-test (free-id-set-clear! s) exn:fail?)
  (err/rt-test (free-id-set-union! s) exn:fail?)
  (err/rt-test (free-id-set-intersection! s) exn:fail?)
  (err/rt-test (free-id-set-subtract! s) exn:fail?)

  (test #t free-id-set=? 
        s 
        (free-id-set-add 
         (free-id-set-add 
          (free-id-set-add (immutable-free-id-set) #'b) #'a) #'c))
  (test #f free-id-set=?
        s
        (let ([a 1])
          (free-id-set-add 
           (free-id-set-add 
            (free-id-set-add (immutable-free-id-set) #'b) #'a) #'c)))

  (test #t free-id-set-member? (free-id-set-add s #'d) #'c)
  (test #t free-id-set-member? (free-id-set-add s #'d) #'d)
  (test #f free-id-set-member? (free-id-set-add s #'d) #'e)
  (test #f free-id-set-member? (free-id-set-add s (let ([d 1]) #'d)) #'d)
  (test #f free-id-set-member? (free-id-set-add s #'d) (let ([d 1]) #'d))
  
  (test #t free-id-set-member? (free-id-set-remove s #'a) #'b)
  (test #f free-id-set-member? (free-id-set-remove s #'b) #'b)
  (test #t free-id-set-member? (free-id-set-remove s (let ([c 1]) #'c)) #'c)
  
  (test #t free-identifier=? (free-id-set-first s) (free-id-set-first s))
  (test #t free-id-set=? (free-id-set-remove s (free-id-set-first s)) 
                         (free-id-set-rest s))
  
  (void))

;; ----------------------------------------------------------------------------
;; mutable set tests

(let ([ms1 (mutable-free-id-set (list #'a #'b #'c))]
      [ms2 (mutable-free-id-set)])
  ;; fns not implemented for mutable id sets
  (err/rt-test (free-id-set-add ms1 #'z) exn:fail?)
  (err/rt-test (free-id-set-remove ms1 #'z) exn:fail?)
  (err/rt-test (free-id-set-rest ms1) exn:fail?)
  (err/rt-test (free-id-set-clear ms1) exn:fail?)
  (err/rt-test (free-id-set-union ms1) exn:fail?)
  (err/rt-test (free-id-set-intersection ms1) exn:fail?)
  (err/rt-test (free-id-set-subtract ms1) exn:fail?)

  (free-id-set-add! ms2 #'b)
  (free-id-set-add! ms2 #'a)
  (free-id-set-add! ms2 #'c)
  (test #t free-id-set=? ms1 ms2)
  (define ms3 (mutable-free-id-set))
  (let ([a 1])
    (free-id-set-add! ms3 #'b)
    (free-id-set-add! ms3 #'a)
    (free-id-set-add! ms3 #'c))
  (test #f free-id-set=? ms1 ms3)

  (define ms4 (mutable-free-id-set (list #'a #'b #'c)))
  (test #t free-id-set-member? ms4 #'c)
  (free-id-set-add! ms4 #'d)
  (test #t free-id-set-member? ms4 #'c)
  (test #t free-id-set-member? ms4 #'d)
  (free-id-set-add! ms4 #'d)
  (test #t free-id-set-member? ms4 #'d)
  (test #f free-id-set-member? ms4 #'e)
  (free-id-set-add! ms4 (let ([e 1]) #'e))
  (test #f free-id-set-member? ms4 #'e)
  (test #f free-id-set-member? ms4 (let ([d 1]) #'d))
  
  (free-id-set-remove! ms4 #'a)
  (test #t free-id-set-member? ms4 #'b)
  (free-id-set-remove! ms4 #'b)
  (test #f free-id-set-member? ms4 #'b)
  (free-id-set-remove! ms4 (let ([c 1]) #'c))
  (test #t free-id-set-member? ms4 #'c)
  
  (test #t free-identifier=? (free-id-set-first ms1) (free-id-set-first ms1))
  (free-id-set-remove! ms1 #'a)
  (test #t free-id-set=? ms1 (mutable-free-id-set (list #'b #'c)))
  
  (free-id-set-clear! ms1)
  (test #t free-id-set-empty? ms1)
  
  (void))

;(test #t generic-set? (set))
;(test #t set-empty? (set))
;(test #t generic-set? (set 1 2 3))
;(test #f set-empty? (set 1 2 3))
;(test #t generic-set? (seteq))
;(test #t set-empty? (seteq))
;(test #t generic-set? (seteq 1 2 3))
;(test #f set-empty? (seteq 1 2 3))
;(test #t generic-set? (seteqv))
;(test #t set-empty? (seteqv))
;(test #t generic-set? (seteqv 1 2 3))
;(test #f set-empty? (seteqv 1 2 3))
;(test #t generic-set? (mutable-set))
;(test #t set-empty? (mutable-set))
;(test #t generic-set? (mutable-set 1 2 3))
;(test #f set-empty? (mutable-set 1 2 3))
;(test #t generic-set? (mutable-seteq))
;(test #t set-empty? (mutable-seteq))
;(test #t generic-set? (mutable-seteq 1 2 3))
;(test #f set-empty? (mutable-seteq 1 2 3))
;(test #t generic-set? (mutable-seteqv))
;(test #t set-empty? (mutable-seteqv))
;(test #t generic-set? (mutable-seteqv 1 2 3))
;(test #f set-empty? (mutable-seteqv 1 2 3))
;(test #t generic-set? (list))
;(test #t set-empty? (list))
;(test #t generic-set? (list 1 2 3))
;(test #f set-empty? (list 1 2 3))
;
;(test #f set-eq? (set 1 2 3))
;(test #f set-eqv? (set 1 2 3))
;(test #t set-equal? (set 1 2 3))
;(test #t set-eq? (seteq 1 2 3))
;(test #f set-eqv? (seteq 1 2 3))
;(test #f set-equal? (seteq 1 2 3))
;(test #f set-eq? (seteqv 1 2 3))
;(test #t set-eqv? (seteqv 1 2 3))
;(test #f set-equal? (seteqv 1 2 3))
;(test #f set-eq? (mutable-set 1 2 3))
;(test #f set-eqv? (mutable-set 1 2 3))
;(test #t set-equal? (mutable-set 1 2 3))
;(test #t set-eq? (mutable-seteq 1 2 3))
;(test #f set-eqv? (mutable-seteq 1 2 3))
;(test #f set-equal? (mutable-seteq 1 2 3))
;(test #f set-eq? (mutable-seteqv 1 2 3))
;(test #t set-eqv? (mutable-seteqv 1 2 3));  (test s set-intersect s)
;  (test (set 3) set-intersect s (set 5 4 3 6))
;  (test (set 3) set-intersect (set 5 4 3 6) s)
;  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
;  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
;  (test (set 3 2) set-intersect s (set 5 2 3))
;  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
;  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
;  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))
;(test #f set-equal? (mutable-seteqv 1 2 3))
;(test #f set-eq? (list 1 2 3))
;(test #f set-eqv? (list 1 2 3))
;(test #f set-equal? (list 1 2 3))
;
;(test 3 set-count (set (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (seteqv (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (seteq (string #\a) "b" "c" (string #\a)))
;(test 3 set-count (mutable-set (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (mutable-seteqv (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (mutable-seteq (string #\a) "b" "c" (string #\a)))
;(test 4 set-count (list (string #\a) "b" "c" (string #\a)))
;
;(test #t set-member? (set 1 2 3) 1)
;(test #t set-member? (set 1 2 3) 2);  (test 3 set-count (set-union s))
;  (test 6 set-count (set-union s (set 3 4 5 6)))
;  (test 6 set-count (set-union (set 3 4 5 6) s))
;  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))
;
;  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
;  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))

;(test #t set-member? (set 1 2 3) 3)
;(test #f set-member? (set 1 2 3) 4)
;
;(test #t set-member? (seteq 1 2 3) 1)
;(test #t set-member? (seteq 1 2 3) 2)
;(test #t set-member? (seteq 1 2 3) 3)
;(test #f set-member? (seteq 1 2 3) 4)
;
;(test #t set-member? (seteqv 1 2 3) 1)
;(test #t set-member? (seteqv 1 2 3) 2)
;(test #t set-member? (seteqv 1 2 3) 3)
;(test #f set-member? (seteqv 1 2 3) 4)
;
;(test #t set-member? (mutable-set 1 2 3) 1)
;(test #t set-member? (mutable-set 1 2 3) 2)
;(test #t set-member? (mutable-set 1 2 3) 3)
;(test #f set-member? (mutable-set 1 2 3) 4)
;
;(test #t set-member? (mutable-seteq 1 2 3) 1)
;(test #t set-member? (mutable-seteq 1 2 3) 2)
;(test #t set-member? (mutable-seteq 1 2 3) 3)
;(test #f set-member? (mutable-seteq 1 2 3) 4)
;
;(test #t set-member? (mutable-seteqv 1 2 3) 1)
;(test #t set-member? (mutable-seteqv 1 2 3) 2)
;(test #t set-member? (mutable-seteqv 1 2 3) 3)
;(test #f set-member? (mutable-seteqv 1 2 3) 4)
;
;(test #t set-member? (list 1 2 3) 1)
;(test #t set-member? (list 1 2 3) 2)
;(test #t set-member? (list 1 2 3) 3)
;(test #f set-member? (list 1 2 3) 4)
;
;(test #t stream? (set 1 2 3))
;(test (set-first (set 1 2 3)) set-first (set 1 2 3))
;(test (set-remove (set 1 2 3) (set-first (set 1 2 3))) set-rest (set 1 2 3))
;
;(test #t stream? (seteq 1 2 3))
;(test (set-first (seteq 1 2 3)) set-first (seteq 1 2 3))
;(test (set-remove (seteq 1 2 3) (set-first (seteq 1 2 3))) set-rest (seteq 1 2 3))
;
;(test #t stream? (seteqv 1 2 3))
;(test (set-first (seteqv 1 2 3)) set-first (seteqv 1 2 3))
;(test (set-remove (seteqv 1 2 3) (set-first (seteqv 1 2 3))) set-rest (seteqv 1 2 3))
;
;(test #f stream? (mutable-set 1 2 3))
;(test (set-first (mutable-set 1 2 3)) set-first (mutable-set 1 2 3))
;
;(test #f stream? (mutable-seteq 1 2 3))
;(test (set-first (mutable-seteq 1 2 3)) set-first (mutable-seteq 1 2 3))
;
;(test #f stream? (mutable-seteqv 1 2 3))
;(test (set-first (mutable-seteqv 1 2 3)) set-first (mutable-seteqv 1 2 3))
;
;(test (set-first (list 1 2 3)) set-first (list 1 2 3))
;(test (set-remove (list 1 2 3) (set-first (list 1 2 3))) set-rest (list 1 2 3))
;
;(test (sort (set-union '(1 2) '(2 3)) <)
;      'set-union/list
;      '(1 2 3))
;
;(test (sort (set-intersect '(1 2) '(2 3)) <)
;      'set-intersect/list
;      '(2))
;
;(test (sort (set-subtract '(1 2) '(2 3)) <)
;      'set-subtract/list
;      '(1))
;
;(test (sort (set-symmetric-difference '(1 2) '(2 3)) <)
;      'set-symmetric-difference/list
;      '(1 3))
;
;(let ([s (set 1 2 3)])
;  (test #t equal? s (set-add (set-add (set-add (set) 1) 2) 3))
;  (test #t equal? (seteq 1 2 3) (seteq 1 2 3))
;  (test #t equal? (seteq 1 2 3) (seteq 3 2 1))
;  (test #t equal? (seteqv 1 2 3) (seteqv 1 2 3))
;  (test #f equal? s (seteq 1 2 3))
;  (test #f equal? s (seteqv 1 2 3))
;  (test #f equal? (seteq 1 2 3) (seteqv 1 2 3))
;
;  (test #t set-member? (set-add s 5) 3)
;  (test #t set-member? (set-add s 5) 5)
;  (test #f set-member? (set-add s 5) 4)
;
;  (test #t set-member? (set-remove s 5) 3)
;  (test #f set-member? (set-remove s 3) 3)
;
;  (test #t subset? (set 1 3) s)
;  (test #t subset? (set 1 2 3) s)
;  (test #f subset? (set 1 4) s)
;  (test #t subset? (set) s)
;
;  (test 3 set-count (set-union s))
;  (test 6 set-count (set-union s (set 3 4 5 6)))
;  (test 6 set-count (set-union (set 3 4 5 6) s))
;  (test 8 set-count (set-union (set 3 4 5 6) s (set 1 10 100)))
;
;  (test (seteq 1 2 3) set-union (seteq 1 2) (seteq 3))
;  (test (seteqv 1 2 3) set-union (seteqv 1 2) (seteqv 3))
;
;  (test s set-intersect s)
;  (test (set 3) set-intersect s (set 5 4 3 6))
;  (test (set 3) set-intersect (set 5 4 3 6) s)
;  (test (seteq 3) set-intersect (seteq 5 4 3 6) (seteq 1 2 3))
;  (test (seteqv 3) set-intersect (seteqv 5 4 3 6) (seteqv 1 2 3))
;  (test (set 3 2) set-intersect s (set 5 2 3))
;  (test (seteq 3 2) set-intersect (seteq 1 2 3) (seteq 5 2 3))
;  (test (set 2) set-intersect s (set 5 2 3) (set 2 20 200))
;  (test (seteq 2) set-intersect (seteq 1 2 3) (seteq 5 2 3) (seteq 2 20 200))
;
;  (test s set-subtract s)
;  (test (set) set-subtract s s)
;  (test s set-subtract s (set 100))
;  (test (set 1 3) set-subtract s (set 2 100))
;  (test (seteq 100) set-subtract (seteq 2 100) (seteq 1 2 3))
;  (test (seteq 9 100) set-subtract (seteq 2 100 1000 9) (seteq 1 2 3) (seteq 1000 5))
;
;  (let ([try-mismatch (lambda (set-op)
;                        (err/rt-test (set-op (seteqv 1 2) (set 3)))
;                        (err/rt-test (set-op (seteqv 1 2) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (set 4) (seteq 3)))
;                        (err/rt-test (set-op (set 1 2) (seteq 3) (set 4)))
;                        (err/rt-test (set-op (seteq 3) (set 1 2) (set 4))))])
;    (try-mismatch set-union)
;    (try-mismatch set-intersect)
;    (try-mismatch set-subtract))
;
;  (test #t andmap negative? (set-map s -))
;  (test 3 length (set-map s +))
;
;  (let ([v 0])
;    (set-for-each s (lambda (n) (set! v (+ v n))))
;    (test 6 values v))
;
;  (test '(1 2 3) sort (for/list ([v s]) v) <)
;  (test '(1 2 3) sort (for/list ([v (in-set s)]) v) <)
;  (test '(1 2 3) sort (let ([seq (in-set s)]) (for/list ([v seq]) v)) <)
;  ;; Optimized
;  (test '(1) sort (for/list ([v (in-set (set 1))]) v) <)
;  (test #t values (let ([noset #t])
;                    (for ([v (in-set (set))]) (set! noset #f))
;                    noset))
;        
;
;  (void))
;
;(let ()
;
;  (define (str=? x y rec) (string=? x y))
;  (define (str-hc1 x rec) (string-length x))
;  (define (str-hc2 x rec) (rec (string-ref x 0)))
;
;  (define-custom-set-types string-set #:elem? string? str=? str-hc1 str-hc2)
;
;  (define (strset . strs) (make-immutable-string-set strs))
;  (define (mutable-strset . strs) (make-mutable-string-set strs))
;  (define (weak-strset . strs) (make-weak-string-set strs))
;
;  ;; Tests for the different set types:
;
;  (define (t mset-A mset-B mset-C set-A set-B set-C)
;
;    (define (t1 ms s subs0 just-elems just-supers <? f)
;
;      ;; Construct sets for comparison:
;
;      (define subs (sort subs0 <?))
;      (define elems (sort (append subs just-elems) <?))
;      (define supers (sort (append elems just-supers) <?))
;      (define not-subs (sort (append just-elems just-supers) <?))
;      (define msA (apply mset-A elems))
;      (define msB (apply mset-B elems))
;      (define msC (apply mset-C elems))
;      (define sA (apply set-A elems))
;      (define sB (apply set-B elems))
;      (define sC (apply set-C elems))
;      (define ms-sub (apply mset-A subs))
;      (define ms-super (apply mset-A supers))
;      (define ms-not-sub (apply mset-A not-subs))
;      (define s-sub (apply set-A subs))
;      (define s-super (apply set-A supers))
;      (define s-not-sub (apply set-A not-subs))
;
;      ;; For weak hash tables, to make the results more predictable:
;      (collect-garbage)
;
;      ;; Test contents:
;
;      (define mcontents (sort (set->list ms) <?))
;      (test #true equal? mcontents elems)
;      (test (null? just-elems) equal? mcontents subs)
;      (test (null? just-supers) equal? mcontents supers)
;      (test (and (null? subs) (null? just-supers)) equal? mcontents not-subs)
;
;      (define contents (sort (set->list s) <?))
;      (test #true equal? contents elems)
;      (test (null? just-elems) equal? contents subs)
;      (test (null? just-supers) equal? contents supers)
;      (test (and (null? subs) (null? just-supers)) equal? contents not-subs)
;
;      ;; Test equality:
;
;      (test #true equal? ms msA)
;      (test #false equal? ms msB)
;      (test #false equal? ms msC)
;      (test #false equal? ms sA)
;      (test #false equal? ms sB)
;      (test #false equal? ms sC)
;      (test #true equal? ms ms)
;      (test (null? just-elems) equal? ms ms-sub)
;      (test (null? just-supers) equal? ms ms-super)
;      (test (and (null? subs) (null? just-supers)) equal? ms ms-not-sub)
;
;      (test #false equal? s msA)
;      (test #false equal? s msB)
;      (test #false equal? s msC)
;      (test #true equal? s sA)
;      (test #false equal? s sB)
;      (test #false equal? s sC)
;      (test #true equal? s s)
;      (test (null? just-elems) equal? s s-sub)
;      (test (null? just-supers) equal? s s-super)
;      (test (and (null? subs) (null? just-supers)) equal? s s-not-sub)
;
;      ;; Test membership:
;
;      (for ([elem (in-list elems)])
;        (test #true set-member? ms elem)
;        (test #true set-member? s elem)
;        (test #false set-member? (set-clear s) elem)
;        (test #false set-member? (set-copy-clear ms) elem)
;        (test #false set-member? (set-copy-clear s) elem))
;
;      (for ([elem (in-list just-supers)])
;        (test #false set-member? ms elem)
;        (test #false set-member? s elem))
;
;      ;; Test set equality:
;
;      (test #true set=? ms ms)
;
;      (test #true set=? ms msA)
;      (test (null? just-elems) set=? ms ms-sub)
;      (test (null? just-supers) set=? ms ms-super)
;      (test (and (null? subs) (null? just-supers)) set=? ms ms-not-sub)
;
;      (test #true set=? ms sA)
;      (test (null? just-elems) set=? ms s-sub)
;      (test (null? just-supers) set=? ms s-super)
;      (test (and (null? subs) (null? just-supers)) set=? ms s-not-sub)
;
;      (err/rt-test (set=? ms msB))
;      (err/rt-test (set=? ms msC))
;      (err/rt-test (set=? ms sB))
;      (err/rt-test (set=? ms sC))
;
;      (test #true set=? s s)
;
;      (test #true set=? s msA)
;      (test (null? just-elems) set=? s ms-sub)
;      (test (null? just-supers) set=? s ms-super)
;      (test (and (null? subs) (null? just-supers)) set=? s ms-not-sub)
;
;      (test #true set=? s sA)
;      (test (null? just-elems) set=? s s-sub)
;      (test (null? just-supers) set=? s s-super)
;      (test (and (null? subs) (null? just-supers)) set=? s s-not-sub)
;
;      (err/rt-test (set=? s msB))
;      (err/rt-test (set=? s msC))
;      (err/rt-test (set=? s sB))
;      (err/rt-test (set=? s sC))
;
;      ;; Test subset:
;
;      (test #true subset? ms ms)
;
;      (test #true subset? ms msA)
;      (test (null? just-elems) subset? ms ms-sub)
;      (test #true subset? ms ms-super)
;      (test (null? subs) subset? ms ms-not-sub)
;
;      (test #true subset? ms sA)
;      (test (null? just-elems) subset? ms s-sub)
;      (test #true subset? ms s-super)
;      (test (null? subs) subset? ms s-not-sub)
;
;      (err/rt-test (subset? ms msB))
;      (err/rt-test (subset? ms msC))
;      (err/rt-test (subset? ms sB))
;      (err/rt-test (subset? ms sC))
;
;      (test #true subset? s s)
;
;      (test #true subset? s msA)
;      (test (null? just-elems) subset? s ms-sub)
;      (test #true subset? s ms-super)
;      (test (null? subs) subset? s ms-not-sub)
;
;      (test #true subset? s sA)
;      (test (null? just-elems) subset? s s-sub)
;      (test #true subset? s s-super)
;      (test (null? subs) subset? s s-not-sub)
;
;      (err/rt-test (subset? s msB))
;      (err/rt-test (subset? s msC))
;      (err/rt-test (subset? s sB))
;      (err/rt-test (subset? s sC))
;
;      ;; Test proper subset:
;
;      (test #false proper-subset? ms ms)
;
;      (test #false proper-subset? ms msA)
;      (test #false proper-subset? ms ms-sub)
;      (test #true proper-subset? ms ms-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? ms ms-not-sub)
;
;      (test #false proper-subset? ms sA)
;      (test #false proper-subset? ms s-sub)
;      (test #true proper-subset? ms s-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? ms s-not-sub)
;
;      (err/rt-test (proper-subset? ms msB))
;      (err/rt-test (proper-subset? ms msC))
;      (err/rt-test (proper-subset? ms sB))
;      (err/rt-test (proper-subset? ms sC))
;
;      (test #false proper-subset? s s)
;
;      (test #false proper-subset? s msA)
;      (test #false proper-subset? s ms-sub)
;      (test #true proper-subset? s ms-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? s ms-not-sub)
;
;      (test #false proper-subset? s sA)
;      (test #false proper-subset? s s-sub)
;      (test #true proper-subset? s s-super)
;      (test (and (null? subs) (pair? just-supers)) proper-subset? s s-not-sub)
;
;      (err/rt-test (proper-subset? s msB))
;      (err/rt-test (proper-subset? s msC))
;      (err/rt-test (proper-subset? s sB))
;      (err/rt-test (proper-subset? s sC))
;
;      ;; Test iteration:
;
;      (define sorted (sort elems <?))
;
;      (test (map f sorted) 'set-map/mutable (sort (set-map ms f) <?))
;      (test (map f sorted) 'set-map/immutable (sort (set-map s f) <?))
;
;      (test sorted
;            'set-for-each/mutable
;            (sort
;             (let ([xs '()])
;               (set-for-each ms (lambda (x) (set! xs (cons x xs))))
;               xs)
;             <?))
;      (test sorted
;            'set-for-each/immutable
;            (sort
;             (let ([xs '()])
;               (set-for-each s (lambda (x) (set! xs (cons x xs))))
;               xs)
;             <?))
;
;      (test sorted 'in-set/mutable (sort (for/list ([x (in-set ms)]) x) <?))
;      (test sorted 'in-set/immutable (sort (for/list ([x (in-set s)]) x) <?))
;
;      (test sorted 'in-set/proc/mutable (sort (sequence->list (in-set ms)) <?))
;      (test sorted 'in-set/proc/immutable (sort (sequence->list (in-set s)) <?))
;
;      (test sorted 'set->list/mutable (sort (set->list ms) <?))
;      (test sorted 'set->list/immutable (sort (set->list s) <?))
;
;      (void))
;
;    ;; Test instances:
;
;    ;; Using string constants stored in variables:
;    ;; - allows us to hash them via equal, eqv, and eq
;    ;; - allows us to keep them around in weak tables
;    (define x1 (string-copy "one"))
;    (define x2 (string-copy "two"))
;    (define x3 (string-copy "three"))
;    (define x4 (string-copy "four"))
;
;    (define ms (mset-A x1 x2 x3))
;    (define s0 (set-A x1 x2 x3))
;    (t1 ms s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (define msc (set-copy ms))
;    (t1 msc s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (set-remove! ms x3)
;    (define s1 (set-remove s0 x3))
;    (t1 ms s1 (list x1) (list x2) (list x3 x4) string<? string-upcase)
;
;    ;; Ensure the copy hasn't changed.
;    (t1 msc s0 (list x1 x2) (list x3) (list x4) string<? string-upcase)
;
;    (set-add! ms x4)
;    (define s2 (set-add s1 x4))
;    (t1 ms s2 (list x1) (list x2 x4) (list x3) string<? string-upcase)
;
;    (set-clear! ms)
;    (define s3 (set-clear s2))
;    (t1 ms s3 (list) (list) (list x1 x2 x3 x4) string<? string-upcase)
;
;    (set-union! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s4 (set-union s3 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s4 (list x2) (list x1 x3) (list x4) string<? string-upcase)
;
;    (set-intersect! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s5 (set-intersect s4 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s5 (list x2) (list) (list x1 x3 x4) string<? string-upcase)
;    (t1 ms s5 (list) (list x2) (list x1 x3 x4) string<? string-upcase)
;
;    (set-symmetric-difference! ms (mset-A x1 x2) (mset-A x2 x3))
;    (define s6 (set-symmetric-difference s5 (set-A x1 x2) (set-A x2 x3)))
;    (t1 ms s6 (list x1 x3) (list x2) (list x4) string<? string-upcase)
;
;    (set-subtract! ms (mset-A x1 x4) (mset-A x2 x4))
;    (define s7 (set-subtract s6 (set-A x1 x4) (set-A x2 x4)))
;    (t1 ms s7 (list x3) (list) (list x1 x2 x4) string<? string-upcase)
;    (t1 ms s7 (list) (list x3) (list x1 x2 x4) string<? string-upcase)
;
;    ;; need to do something to keep these from being garbage collected
;    (test "one" string-copy x1)
;    (test "two" string-copy x2)
;    (test "three" string-copy x3)
;    (test "four" string-copy x4))
;
;  (t mutable-set mutable-seteqv mutable-seteq set seteqv seteq)
;  (t mutable-seteqv mutable-seteq mutable-set seteqv seteq set)
;  (t mutable-seteq mutable-set mutable-seteqv seteq set seteqv)
;  (t weak-set weak-seteqv weak-seteq set seteqv seteq)
;  (t weak-seteqv weak-seteq weak-set seteqv seteq set)
;  (t weak-seteq weak-set weak-seteqv seteq set seteqv)
;  (t weak-set mutable-seteqv mutable-seteq set seteqv seteq)
;  (t mutable-set weak-seteqv weak-seteq set seteqv seteq)
;  (t mutable-strset mutable-set weak-set strset set seteqv)
;  (t weak-strset mutable-seteqv weak-seteq strset seteqv seteq))
;
;(test "#<set: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (set 1) sp)
;        (get-output-string sp)))
;
;(test "#<seteq: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (seteq 1) sp)
;        (get-output-string sp)))
;
;(test "#<seteqv: 1>" 
;      'print-set1
;      (let ([sp (open-output-string)])
;        (write (seteqv 1) sp)
;        (get-output-string sp)))
;
;;; ----------------------------------------
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2)]) (add1 i)))
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
;                                    #:break (= i 3)
;                                    (add1 i)))
;
;(test (set 1 2 3) 'for/set (for/set ([i '(0 1 2 3 4)]) 
;                                    #:final (= i 2)
;                                    (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2)]) (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2 3 4)]) 
;                       #:break (= i 3)
;                       (add1 i)))
;
;(test (mutable-set 1 2 3)
;      'for/mutable-set
;      (for/mutable-set ([i '(0 1 2 3 4)]) 
;                       #:final (= i 2)
;                       (add1 i)))
;
;;; ----------------------------------------
;
;(err/rt-test (set/c '(not a contract)))
;(err/rt-test (set/c any/c #:cmp 'not-a-comparison))
;(err/rt-test (set/c any/c #:kind 'not-a-kind-of-set))
;(err/rt-test (set/c (-> integer? string?) #:cmp 'eq))
;(err/rt-test (set/c (-> integer? string?) #:cmp 'eqv))

(report-errs)
