;;; list-set.scm  --  Jens Axel Søgaard 

(module list-set mzscheme
  ;(print-struct #t)
  (require (lib "67.ss" "srfi") ; compare
           (all-except (lib "list.ss") 
                       empty empty? remove remove*)
           (lib "26.ss" "srfi")
           (lib "42.ss" "srfi"))
  
  ;; an LSET consists of a compare functions
  ;; and a sorted (w.r.t to the compare function)
  ;; list of elements
  (define-struct lset (compare elements) (make-inspector))

  ; set? : object -> boolean
  (define (set? o)
    (lset? o))
  
  ; empty : [compare] -> set
  (define empty
    (case-lambda 
      [()    (empty (current-compare))]
      [(cmp) (make-lset cmp '())]))
  
  ; empty? : set -> boolean
  (define (empty? s)
    (null? (lset-elements s)))
  
  ; find-min : set -> element
  (define (find-min s)
    (when (empty? s)
      (error 'find-min "an empty set does not have a minimum element"))
    (car (lset-elements s)))
  
  (define (delete-min s)
    (delete (find-min s) s))
  
  ; singleton : [compare] element -> set
  (define singleton
    (case-lambda 
      [(x)     (singleton (current-compare) x)]
      [(cmp x) (make-lset cmp (list x))]))
  
  ; elements : set -> list
  (define (elements s)
    (lset-elements s))
  
  ; select : set -> element
  (define (select s)
    (if (empty? s)
        (error 'select "can't select an element from an empty set")
        (car (lset-elements s))))
  
  ; size : set -> integer
  (define (size s)
    (length (elements s)))
  
  ; list-sorted? : compare list -> boolean
  (define (list-sorted? cmp xs)
    (or (null? xs)
        (null? (cdr xs))
        (and (if<=? cmp (car xs) (cadr xs))
             (list-sorted? cmp (cdr xs)))))
  
  ; list-merge : compare list list -> list
  ;   merge two cmp-sorted lists
  (define (list-merge cmp xs ys)
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [else       (let ([x (car xs)] [y (car ys)])
                    (if<=? (cmp x y)
                           (cons x (list-merge cmp (cdr xs) ys))
                           (cons y (list-merge cmp xs       (cdr ys)))))]))
  
  ; list-split : list -> list list
  ;   splits a list xs into two lists ys and zs,
  ;   s.t. ys and zs are half as long as xs
  ;   and (append ys zs) is a permutation of xs
  (define (list-split xs)
    (cond
      [(null? xs)       (values '() '())]
      [(null? (cdr xs)) (values xs '())]
      [else             (let-values ([(ys zs) (list-split (cddr xs))])
                          (values (cons (car xs) ys)
                                  (cons (cadr xs) zs)))]))
  
  ; list-sort : compare list -> list
  (define (list-sort cmp xs)
    (cond
      [(null? xs)       '()]
      [(null? (cdr xs)) xs]
      [else             (let-values ([(ys zs) (list-split xs)])
                          (list-merge cmp 
                                      (list-sort cmp ys)
                                      (list-sort cmp zs)))]))
  
  
  (define (union s1 s2)
    (let ([cmp (lset-compare s1)])
      (make-lset cmp (list-union cmp (elements s1) (elements s2)))))

  (define (union/combiner s1 s2 combine)
    (let ([cmp (lset-compare s1)])
      (make-lset cmp (list-union/combiner cmp (elements s1) (elements s2) combine))))
  
  (define (intersection s1 s2)
    (let ([cmp (lset-compare s1)])
      (make-lset cmp (list-intersection cmp (elements s1) (elements s2)))))
  
  (define (intersection/combiner s1 s2 combine)
    (let ([cmp (lset-compare s1)])
      (make-lset cmp (list-intersection/combiner cmp (elements s1) (elements s2) combine))))
  
  (define (difference s1 s2)
    (let ([cmp (lset-compare s1)])
      (make-lset cmp (list-difference cmp (elements s1) (elements s2)))))
  
  (define (subset? s1 s2)
    (let ([cmp (lset-compare s1)])
      (list-subset? cmp (elements s1) (elements s2))))
  
  (define (equal=? s1 s2)
    (let ([cmp (lset-compare s1)])
      (list-equal=? cmp (elements s1) (elements s2))))

  
  
  
  ; list-union : compare list list -> list
  (define (list-union cmp xs ys)
    (list-union/combiner cmp xs ys (lambda (x y) x)))

  ; list-union/combiner : compare list list (element element -> element) -> list
  (define (list-union/combiner cmp xs ys combine)
    ; xs and ys are cmp-sorted
    (cond
      [(null? xs) ys]
      [(null? ys) xs]
      [else  (let ([x (car xs)] [y (car ys)])
               (if3 (cmp x y)
                    (cons x (list-union/combiner cmp (cdr xs) ys combine))       
                    (cons (combine x y) (list-union/combiner cmp (cdr xs) (cdr ys) combine))
                    (cons y (list-union/combiner cmp xs (cdr ys) combine))))]))
  
  ; list-intersection : compare list list -> list
  (define (list-intersection cmp xs ys)
    (list-intersection/combiner cmp xs ys (lambda (x y) x)))
  
  ; list-intersection/combiner : compare list list (element element -> element) -> list
  (define (list-intersection/combiner cmp xs ys combine)
    ; xs and ys are cmp-sorted
    (cond
      [(null? xs) '()]
      [(null? ys) '()]
      [else       (let ([x (car xs)] [y (car ys)])
                    (if3 (cmp x y)
                         (list-intersection/combiner cmp (cdr xs) ys combine)
                         (cons (combine x y) (list-intersection/combiner cmp (cdr xs) (cdr ys) combine))
                         (list-intersection/combiner cmp xs (cdr ys) combine)))]))
  
  ; list-difference : compare list list -> list
  (define (list-difference cmp xs ys)
    ; xs and ys are cmp-sorted
    (cond 
      [(null? ys) xs]
      [(null? xs) '()]
      [else       (let ([x (car xs)] [y (car ys)])
                    (if3 (cmp x y)
                         (cons x (list-difference cmp (cdr xs) ys))
                         (list-difference cmp (cdr xs) (cdr ys))
                         (list-difference cmp xs (cdr ys))))]))
  
  
  
  ; list-insert : compare element list -> list
  ;   given x and the cmp-sorted list xs, 
  ;   returns (sort cmp (cons x xs))
  (define (list-insert cmp x xs)
    ; insert x into xs, which is sorted w.r.t. cmp
    (cond
      [(null? xs) (list x)]
      [else       (let ([y (car xs)])
                    (if3 (cmp x y)
                         (cons x xs)
                         (cons x (cdr xs))  ; keep the last representative
                         (cons y (list-insert cmp x (cdr xs)))))]))

  ; list-insert/combiner : compare element list (element element -> element) -> list
  ;   given x and the cmp-sorted list xs, 
  ;   returns (sort cmp (cons x xs))
  (define (list-insert/combiner cmp x xs combine)
    ; insert x into xs, which is sorted w.r.t. cmp
    (cond
      [(null? xs) (list x)]
      [else       (let ([y (car xs)])
                    (if3 (cmp x y)
                         (cons x xs)
                         (cons (combine x y) (cdr xs))
                         (cons y (list-insert/combiner cmp x (cdr xs) combine))))]))

  
  (define (list-subset? cmp xs ys)
    (cond
      [(null? xs) #t]
      [(null? ys) #f]
      [else       (let ([x (car xs)] [y (car ys)])
                    (if3 (cmp x y)
                         #f
                         (list-subset? cmp (cdr xs) (cdr ys))
                         (list-subset? cmp xs (cdr ys))))]))
  
  (define (list-equal=? cmp xs ys)
    (cond
      [(and (null? xs) (null? ys))  #t]
      [(null? xs)                   #f]
      [(null? ys)                   #f]
      [(cmp (car xs) (car ys))      (list-equal=? cmp (cdr xs) (cdr ys))]
      [else                         #f]))
  
  (define member? 
    (case-lambda
      [(x s)     (list-member? (lset-compare s) x (lset-elements s))]
      [(cmp x s) (if (eq? cmp (lset-compare s))
                     (member? x s)
                     (member? x (list-sort cmp s)))]))
  (define get 
    (case-lambda
      [(x s)     (list-get (lset-compare s) x (lset-elements s))]
      [(cmp x s) (if (eq? cmp (lset-compare s))
                     (get x s)
                     (get x (list-sort cmp s)))]))
  
  (define (insert x s)
    (let ([cmp (lset-compare s)])
      (make-lset cmp
                 (list-insert cmp x (lset-elements s)))))
  
  (define (insert/combiner x s combine)
    (let ([cmp (lset-compare s)])
      (make-lset cmp
                 (list-insert/combiner cmp x (lset-elements s) combine))))
  
  (define (insert* xs s)
    (foldl insert s xs))
  
  (define (insert*/combiner xs s combine)
    (foldl (lambda (x s) (insert/combiner x s combine)) s xs))
  
  (define (list-member? cmp x xs)
    (and (not (null? xs))
         (or (=? cmp x (car xs))
             (and (not (<? cmp x (car xs)))
                  (list-member? cmp x (cdr xs))))))
  
  (define (list-get cmp x xs)
    (cond 
      [(null? xs)    #f]
      [else          (if3 (cmp x (car xs))
                          #f
                          (car xs)
                          (list-get cmp x (cdr xs)))]))
  
  
  (define (delete x s)
    (difference s (singleton (lset-compare s) x)))
  
  (define (delete-all x s)
    (delete x s))
  
  (define (delete* xs s)
    (difference s (list->set (lset-compare s) xs)))
  
  (define (fold f init s)
    (foldl f init (lset-elements s)))
  
  (define (list-remove-duplicates cmp xs)
    ; removes removes duplicates from an sorted list
    (cond
      [(null? xs)                  xs]
      [(null? (cdr xs))            xs]
      [(=? cmp (car xs) (cadr xs)) (list-remove-duplicates cmp (cdr xs))]
      [else                        (cons (car xs) (list-remove-duplicates cmp (cdr xs)))]))
  
  (define list->set
    (case-lambda
      [(xs)     (list->set (current-compare) xs)]
      [(cmp xs) (foldl insert (empty cmp) xs)]))
  
  (define list->set/combiner
    (case-lambda
      [(xs combine)     (list->set (current-compare) xs combine)]
      [(cmp xs combine) (foldl (lambda (x s) (insert/combiner x s combine)) (empty cmp) xs)]))
  
  (define (set . xs)
    (list->set xs))
  
  
  ;; support for srfi-42
  
  (define-syntax set-ec
    (syntax-rules ()
      [(_ cmp etc1 etc ...)
       (fold-ec (empty cmp) etc1 etc ... insert)]))
  
  (define-syntax :set
    (syntax-rules (index)
      ((:set cc var (index i) arg)
       (:parallel cc (:stack var arg) (:integers i)) )
      ((:set cc var arg)
       (:do cc
            (let ())
            ((t (lset-elements arg)))
            (not (null? t))
            (let ((var (car t))))
            #t
            ((cdr t)) ))))
  
  (define (:set-dispatch args)
    (cond
      [(null? args)
       'set]
      [(and (= (length args) 1)
            (set? (car args)))
       (:generator-proc (:set (car args)))]
      [else
       #f]))
  
  (:-dispatch-set! 
   (dispatch-union (:-dispatch-ref) :set-dispatch))
  
  
  ;;; PROVIDE SET FUNCTIONS
  (require "signatures/set-signature.scm")
  (provide-set)
  )
