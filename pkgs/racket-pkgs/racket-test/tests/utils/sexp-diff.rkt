(module sexp-diff mzscheme
  (require rackunit)
  
  ; sexp-diff and sexp-diff/expound show the difference between two specified s-expressions.
  ; in each case, the part of the s-expression that is the same is preserved in the result. When
  ; traversal reveals a difference, the point that is different is replaced with either the symbol
  ; different!-g##### (in the sexp-diff function) or a list of the form (different!-g#### <thing-from-a> <thing-from-b>)
  ; in the sexp-diff/expound function.
  
  (provide sexp-diff sexp-diff/expound)
  (provide list-diff list-diff/expound)

  
  (define (sexp-diff/core expound? collect?)    
    (letrec ([construct-diff-result (if expound? 
                                        (lambda (a b) (list diff-id a b))
                                        (lambda (a b) diff-id))]
             [diff-result-test (if expound? 
                                   (lambda (a) (and (pair? a) (eq? (car a) diff-id)))
                                   (lambda (a) (eq? a diff-id)))]
             [combine-diff-results (if expound? 
                                       (lambda (a b) 
                                         (if (diff-result-test a)
                                             (list diff-id (cons (cadr a) (cadr b)) (cons (caddr a) (caddr b)))
                                             (let-values ([(a-side b-side) (separate-sides a)])
                                               (list diff-id (cons a-side (cadr b)) (cons b-side (caddr b))))))
                                       (lambda (a b) diff-id))]
             ; if a previously diff'-ed result is re-separated (because it occurs in a list which is being recombined),
             ; the diff'ed result should be split again.
             [separate-sides 
              (lambda (a)
                (cond [(diff-result-test a)
                       (values (cadr a) (caddr a))]
                      ; otherwise, recur structurally
                      [(pair? a)
                       (let-values ([(car-a car-b) (separate-sides (car a))]
                                    [(cdr-a cdr-b) (separate-sides (cdr a))])
                         (values (cons car-a cdr-a)
                                 (cons car-b cdr-b)))]
                      [else
                       (values a a)]))]
             [sexp-diff 
              (lambda (a b)
                (cond [(and (null? a) (null? b)) 
                       null]
                      [(and (pair? a) (pair? b))
                       (let ([car-result (sexp-diff (car a) (car b))]
                             [cdr-result (sexp-diff (cdr a) (cdr b))])
                         (if collect?
                             (cond [(diff-result-test cdr-result)
                                    (combine-diff-results car-result cdr-result)]
                                   [else (cons car-result cdr-result)])
                             (cons car-result cdr-result)))]
                      [(equal? a b) a]
                      [else (construct-diff-result a b)]))])
      sexp-diff))
  
  (define sexp-diff (sexp-diff/core #f #f))
  (define sexp-diff/expound (sexp-diff/core #t #f))
  
  (define diff-id (gensym "different!-"))
  
  ; list-diff does a diff at a list level, rather than at a pair list.  The difference may be 
  ; observed in the result of applying sexp-diff/expound and list-diff/expound to `(3 4 5) and `(5 6).
  ; the prior gives `((different! 3 5) (different! 4 6) . (different! (5) ()), whereas the latter
  ; gives `(different! (3 4 5) (5 6)).  The difference from an implementation standpoint is that if the
  ; cdr's of two pair elements is different, the results are recombined to make the whole pair labeled
  ; different.
  
  (define list-diff (sexp-diff/core #f #t))
  (define list-diff/expound (sexp-diff/core #t #t))
  
  (check-equal? (sexp-diff null null) null)
  (define a `(1 2 (3 4 (5 6) (6 7) () 8) 9))
  (check-equal? (sexp-diff a a) a)
  (define b `(1 0 (3 0 (5 7) 0 0 8 3 4) 9))
  (check-equal? (sexp-diff a b)
                `(1 ,diff-id (3 ,diff-id (5 ,diff-id) ,diff-id ,diff-id 8 . ,diff-id) 9))
  
  (check-equal? (sexp-diff/expound null null) null)
  (check-equal? (sexp-diff/expound a a) a)
  (check-equal? (sexp-diff/expound a b)
                `(1 (,diff-id 2 0) (3 (,diff-id 4 0) (5 (,diff-id 6 7)) (,diff-id (6 7) 0) (,diff-id () 0) 8 . (,diff-id () (3 4))) 9))
  
  (check-equal? (list-diff null null) null)
  (check-equal? (list-diff/expound null null) null)
  (check-equal? (list-diff a a) a)
  (check-equal? (list-diff/expound a a) a)
  (check-equal? (list-diff a b) `(1 ,diff-id ,diff-id 9))
  (check-equal? (list-diff/expound a b) 
                `(1 (,diff-id 2 0) (,diff-id (3 4 (5 6) (6 7) () 8) (3 0 (5 7) 0 0 8 3 4)) 9)))

