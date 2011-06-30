#lang scheme/load
(module tlang mzscheme
  (require (prefix tl: typed-scheme))
  (provide (all-from typed-scheme)))


(module even-odd typed-scheme
  (define: (my-odd? [n : Number]) : Boolean
    (if (zero? n) #f
        (my-even? (- n 1))))

  (define: (my-even? [n : Number]) : Boolean
    (if (zero? n) #t
        (my-odd? (- n 1))))

  (display (my-even? 12)))

(module date typed-scheme

  (define-typed-struct my-date ([day : Number] [month : String] [year : Number]))

  (define: (format-date [d : my-date]) : String
    (format "Today is day ~a of ~a in the year ~a" (my-date-day d) (my-date-month d) (my-date-year d)))

  (display (format-date (make-my-date 28 "November" 2006)))

  )

(module tree typed-scheme
  (define-typed-struct leaf ([val : Number]))
  (define-typed-struct node ([left : (Un node leaf)] [right : (Un node leaf)]))

  (define: (tree-height [t : (Un node leaf)]) : Integer
    (cond [(leaf? t) 1]
          [else (max (tree-height (node-left t))
                     (tree-height (node-right t)))]))

  (define: (tree-sum [t : (Un node leaf)]) : Number
    (cond [(leaf? t) (leaf-val t)]
          [else (+ (tree-sum (node-left t))
                   (tree-sum (node-right t)))])))

(module tree typed-scheme
  (define-typed-struct leaf ([val : Number]))
  (define-typed-struct node ([left : (Un node leaf)] [right : (Un node leaf)]))

  (define-type-alias tree (Un node leaf))

  (define: (tree-height [t : tree]) : Integer
    (cond [(leaf? t) 1]
          [else (max (tree-height (node-left t))
                     (tree-height (node-right t)))]))

  (define: (tree-sum [t : tree]) : Number
    (cond [(leaf? t) (leaf-val t)]
          [else (+ (tree-sum (node-left t))
                   (tree-sum (node-right t)))])))

(module add-list typed-scheme
  (define: (sum-list [l : (Listof Number)]) : Number
    (cond [(null? l) 0]
          [else (+ (car l) (sum-list (cdr l)))])))

(module maybe typed-scheme
  (define-typed-struct Nothing ())
  (define-typed-struct (a) Just ([v : a]))

  (define-type-alias (Maybe a) (Un Nothing (Just a)))

  (define: (find [v : Number] [l : (Listof Number)]) : (Maybe Number)
    (cond [(null? l) (make-Nothing)]
          [(= v (car l)) (make-Just v)]
          [else (find v (cdr l))])))
