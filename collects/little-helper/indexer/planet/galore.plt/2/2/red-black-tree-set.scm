;;; red-black-tree-set.ss   --  Jens Axel Søgaard  --  3rd nov 2003

;;; SETS IMPLEMENTED AS RED-BLACK TREES.

(module red-black-tree-set mzscheme
  
  (require (all-except (lib "list.ss") empty empty? remove remove* filter)
           (lib "match.ss")
           (lib "67.ss" "srfi")
           (lib "42.ss" "srfi"))
  
  (require (prefix raw- "private/raw-red-black-tree-set.scm"))
  
  (define-struct Set (compare raw))
  (define (raw s) (Set-raw s))
  (define (compare s) (Set-compare s))

  (define (set? o)
    (Set? o))
  
  (define (same-compare? s1 s2)
    (eq? (Set-compare s1) (Set-compare s2)))

  ; empty : [compare] -> set
  (define empty
    (case-lambda 
      [()    (empty (current-compare))]
      [(cmp) (make-Set cmp raw-empty)]))

  ; empty? : set -> boolean
  (define (empty? s)
    (raw-empty? (raw s)))
  
  ; singleton : [compare] element -> set
  (define singleton
    (case-lambda 
      [(x)     (singleton (current-compare) x)]
      [(cmp x) (make-Set cmp (raw-singleton x))]))
  
  ; member? : element set -> boolean
  (define (member? x s)
    (raw-member? (compare s) x (raw s)))
  
  ; get : element set -> element
  (define (get x s)
    (raw-get (compare s) x (raw s)))
  
  
  (define (insert x s)
    (make-Set (compare s)
              (raw-insert (compare s) x (raw s))))
  
  (define (insert/combiner x s combine)
    (make-Set (compare s)
              (raw-insert/combiner (compare s) x (raw s) combine)))
  
  (define (insert* xs s)
    (make-Set (compare s)
              (raw-insert* (compare s) xs (raw s))))
  
  (define (insert*/combiner xs s combine)
    (make-Set (compare s)
              (raw-insert*/combiner (compare s) xs (raw s) combine)))
  
  (define (delete x s)
    (make-Set (compare s)
              (raw-remove (compare s) x (raw s))))
  
  (define (delete-all x s)
    (delete x s))
  
  (define (delete-min s)
    (delete (find-min s) s))
  
  (define (delete* xs s)
    (make-Set (compare s)
              (raw-remove* (compare s) xs (raw s))))

  (define (find-min s)
    (when (empty? s)
      (error 'find-min "an empty set does not have a minimum element"))
    (raw-find-min (raw s)))
  
  (define (fold f init s)
    (raw-fold f init (raw s)))
  
  (define (set . xs)
    (list->set (current-compare) xs))
  
  (define list->set
    (case-lambda
      [(xs)     (list->set (current-compare) xs)]
      [(cmp xs) (make-Set cmp (raw-list->set cmp xs))]))
  
  (define list->set/combiner
    (case-lambda
      [(xs combine)     (list->set/combiner (current-compare) xs combine)]
      [(cmp xs combine) (make-Set cmp (raw-list->set/combiner cmp xs combine))]))
  
  (define (elements s)
    (raw-elements (raw s)))
  
  (define (select s)
    (if (empty? s)
        (error 'select "can't select an element from an empty set")
        (raw-select (raw s))))

  (define (size s)
    (raw-size (raw s)))
  
  ; define a binary set operation name, with a signature
  ;   name : [compare] set set -> set
  ; whose implemented in terms of sorted lists are given by list-name.
  ; The return value will the result of applying finish to the
  ; value returned by list-name.
  (define-syntax (define-binary-operation stx)
    (syntax-case stx ()
      [(define-binary-operation name raw-name finish)
       #'(define name
           (case-lambda
             [(s1 s2)     
              (name (Set-compare s1) s1 s2)]
             [(cmp s1 s2) 
              (finish cmp (raw-name cmp (raw s1) (raw s2)))]))]))
  
  (define-binary-operation union        raw-union         make-Set)
  (define-binary-operation intersection raw-intersection  make-Set)
  (define-binary-operation difference   raw-difference    make-Set)
  (define-binary-operation subset?      raw-subset?       (lambda (cmp b) b))
  (define-binary-operation equal=?      raw-equal=?       (lambda (cmp b) b))
  
  (define union/combiner
    (case-lambda
      [(s1 s2 combine)     
       (union/combiner (Set-compare s1) s1 s2 combine)]
      [(cmp s1 s2 combine) 
       (make-Set cmp (raw-union/combiner cmp (raw s1) (raw s2) combine))]))
  
  (define intersection/combiner
    (case-lambda
      [(s1 s2 combine)     
       (intersection/combiner (Set-compare s1) s1 s2 combine)]
      [(cmp s1 s2 combine) 
       (make-Set cmp (raw-intersection/combiner cmp (raw s1) (raw s2) combine))]))
  
  (define (insert/combiner2 x s combine)
    (let ([cmp (Set-compare s)])
      (make-Set cmp (raw-insert/combiner cmp x (raw s) combine))))
  
  (define (insert*/combiner2 xs s combine)
    (let ([cmp (Set-compare s)])
      (make-Set cmp (raw-insert*/combiner cmp xs (raw s) combine))))

  
  ;; support for srfi-42
  
  (define-syntax set-ec
    (syntax-rules ()
      [(_c cmp etc1 etc ...)
       (fold-ec (empty cmp) etc1 etc ... insert)]))
  
  (define-syntax :set
    (syntax-rules (index)
      ((:set cc var (index i) arg)
       (:parallel cc (:stack var arg) (:integers i)) )
      ((:set cc var arg)
       (:do cc
            (let ())
            ((t (raw arg))
             (c (compare arg)))
            (not (null? t))
            (let ((var (raw-select t))))
            #t
            ((raw-remove c var t)
             c) ))))

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

  
  (require "signatures/set-signature.scm")
  (provide-set)
  )
