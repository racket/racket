;;; generic-bag.scm  --  5th November 2005  --  Jens Axel Soegaard

;;; Bags implemented on top of sets.

;; This module exports (eventually) a macro, that implements a bag using
;; on top a given set representaiton.
  
  (require
   (lib "67.ss" "srfi")
   (lib "42.ss" "srfi")
   (only (lib "list.ss") foldl))
  
  ;;; REPRESENTATION
  ;;;     A bag is represented a set of element/number-of pairs.
  
  (define-struct bag (compare set-of-element/number-pairs))
  
  (define elm car)
  (define no cdr)
  (define elms bag-set-of-element/number-pairs)
  
  (define empty
    (case-lambda 
      [()    (empty (current-compare))]
      [(cmp) (make-bag cmp
                       (set:empty (lambda (p1 p2)
                                    (cmp (elm p1) (elm p2)))))]))
  
  (define (empty? b)
    (set:empty? (elms b)))
  
  (define (-bag . xs)
    (insert* xs (empty)))
  
  (define (insert x b)
    (let ([x1 (set:get (cons x 'dummy) (elms b))])
      (if x1
          (make-bag (bag-compare b)
                    (set:insert (cons x (add1 (no x1)))
                                (set:delete x1 (elms b))))
          (make-bag (bag-compare b)
                    (set:insert (cons x 1)
                                (elms b))))))

  (define (insert* xs b)
    (foldl insert b xs))
  
  ;(define (bag cmp . xs)
  ;  (insert* xs (empty cmp)))
  
  (define (fold f init b)
    ; f : elm alpha -> alpha  
    (set:fold (lambda (x acc)
                ; TODO: Eliminate temporary list
                (foldl f acc (list-ec (: i (no x)) (elm x))))
              init (elms b)))
  
  (define (fold/no f base b)
    ; f : elm no alpha -> alpha
    (set:fold (lambda (x akk)
                (f (elm x) (no x) akk))
              base (elms b)))
  
  (define (elements b)
    (fold cons '() b))
  
  (define (get x b)
    (let ([y (set:get (cons x 'ignore) (elms b))])
      (if y
          (elm y)
          #f)))
  
  (define (count x b)
    (let ([y (set:get (cons x 'ignore) (elms b))])
      (if y
          (no y)
          0)))
  
  (define list->bag
    (case-lambda 
      [(xs)     (insert* xs (empty))]
      [(cmp xs) (insert* xs (empty cmp))]))
  
  (define (member? x b)
    (if (get x b)
        #t
        #f))
  
  ; remove a single occurence of x
  (define (delete x b)
    (let ([p (set:get (cons x 'ignore) (elms b))])
      (cond
        [(not p)       b]
        [(> (no p) 1)  (make-bag (bag-compare b)
                                 (set:insert (cons (elm p) (sub1 (no p)))
                                             (set:delete p (elms b))))]
        [else          (make-bag (bag-compare b)
                                 (set:delete p (elms b)))])))
  
  ; remove all elements in the list xs
  (define (delete* xs b)
    (foldl delete b xs))

  ; remove all occurences of x in b
  (define (delete-all x b)
    (make-bag (bag-compare b)
              (set:delete (cons x 'ignore) (elms b))))
  
  (define (union b1 b2)
    (make-bag (bag-compare b1)
              (set:union (set:union 
                          (set:difference (elms b1) (elms b2))
                          (set:difference (elms b2) (elms b1)))
                         (set:fold (lambda (x s)
                                     (set:insert (cons (elm x) (+ (no (set:get x (elms b1)))
                                                                  (no (set:get x (elms b2)))))
                                                 (set:delete x s)))
                                   (elms b1)
                                   (set:intersection (elms b1) (elms b2))))))
  
  (define (difference b1 b2)
    (make-bag (bag-compare b1)
              (set:union (set:difference (elms b1) (elms b2))
                         (set:fold (lambda (x s)
                                     (let ([new-no (- (no (set:get x (elms b1)))
                                                      (no (set:get x (elms b2))))])
                                       (if (> new-no 0)
                                           (set:insert (cons (elm x) new-no)
                                                       (set:delete x s))
                                           (set:delete x s))))
                                   (elms b1)
                                   (set:intersection (elms b1) (elms b2))))))
  
  (define (occurrences x b)
    (let ([n (set:get (cons x 'ignore) (elms b))])
      (if n
          (no n)
          0)))
  
  (define (intersection b1 b2)
    (unless (eq? (bag-compare b1) (bag-compare b2))
      (error 'difference "the compare functions of the two bags were different"))
    (make-bag (bag-compare b1)
              (set:fold (lambda (x s)
                          (set:insert (cons (elm x) (min (no (set:get x (elms b1)))
                                                         (no (set:get x (elms b2)))))
                                      s))
                        (set:empty (let ([cmp (bag-compare b1)])
                                     (lambda (p1 p2)
                                       (cmp (elm p1) (elm p2)))))
                        (set:intersection (elms b1) (elms b2)))))
  
  (define singleton
    (case-lambda
      [(x)     (insert x (empty))]
      [(cmp x) (insert x (empty cmp))]))
  
  (define (size-distinct b)
    (set:size (elms b)))
  
  (define (size b)
    (fold/no (lambda (x n sum)
               (+ sum n))
             0 b))
  
  (define (select b)
    (when (empty? b)
      (error 'select "can't select element from empty bag"))
    (elm (set:select (elms b))))
  
  (define (equal=? b1 b2)
    (set:equal=? (elms b1) (elms b2)))
  
  (define (subbag? b1 b2)
    (let/ec return 
      (fold/no (lambda (x no p?)
                 (if p?
                     (<= no
                         (occurrences x b2))
                     (return #f)))
               #t b1)))
  
  ;; support for srfi-42
  
  (define-syntax bag-ec
    (syntax-rules ()
      [(_ cmp etc1 etc ...)
       (fold-ec (empty cmp) etc1 etc ... insert)]))
  
  (define-syntax :bag
    (syntax-rules (index)
      ((:bag cc var (index i) arg)
       (:parallel cc (:stack var arg) (:integers i)) )
      ((:bag cc var arg)
       (:do cc
            (let ())
            ((t (elements arg)))
            (not (null? t))
            (let ((var (car t))))
            #t
            ((cdr t)) ))))
  
  (define (:bag-dispatch args)
    (cond
      [(null? args)
       'bag]
      [(and (= (length args) 1)
            (bag? (car args)))
       (:generator-proc (:bag (car args)))]
      [else
       #f]))
  
  (:-dispatch-set! 
   (dispatch-union (:-dispatch-ref) :bag-dispatch))

  
  (require "signatures/bag-signature.scm")
  (provide-bag)
  