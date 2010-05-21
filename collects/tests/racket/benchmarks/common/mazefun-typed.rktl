;;; MAZEFUN -- Constructs a maze in a purely functional way,
;;; written by Marc Feeley.

(: iota (Integer -> (Listof Integer)))
(define iota
  (lambda (n)
    (iota-iter n '())))

(: iota-iter (Integer (Listof Integer) -> (Listof Integer)))
(define iota-iter
  (lambda (n lst)
    (if (= n 0)
        lst
        (iota-iter (- n 1) (cons n lst)))))

(: foldr (All (X Y) ((X Y -> Y) Y (Listof X) -> Y)))
(define foldr
  (lambda (f base lst)

    (: foldr-aux ((Listof X) -> Y))
    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst))))))

    (foldr-aux lst)))

(: foldl (All (X Y) ((Y X -> Y) Y (Listof X) -> Y)))
(define foldl
  (lambda (f base lst)

    (: foldl-aux (Y (Listof X) -> Y))
    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
            base
            (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(: for (All (X) (Integer Integer (Integer -> X) -> (Listof X))))
(define for
  (lambda (lo hi f)

    (: for-aux (Integer -> (Listof X)))
    (define for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            '())))

    (for-aux lo)))

(: concat (All (X) ((Listof (Listof X)) -> (Listof X))))
(define concat
  (lambda (lists)
    ((inst foldr (Listof X) (Listof X)) append '() lists)))

(: list-read (All (X) ((Listof X) Integer -> X)))
(define list-read
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read (cdr lst) (- i 1)))))

(: list-write (All (X) ((Listof X) Integer X -> (Listof X))))
(define list-write
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write (cdr lst) (- i 1) val)))))

(: list-remove-pos (All (X) ((Listof X) Integer -> (Listof X))))
(define list-remove-pos
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))

(: duplicates? (All (X) ((Listof X) -> Any)))
(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member (car lst) (cdr lst))
            (duplicates? (cdr lst))))))

;; Manipulation de matrices.

(define-type (Matrix X) (Listof (Listof X)))
(: make-matrix (All (X) (Integer Integer (Integer Integer -> X)
                                 -> (Matrix X))))
(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda: ((i : Integer))
                      (for 0 m (lambda: ((j : Integer))
                                        (init i j)))))))

(: matrix-read (All (X) ((Matrix X) Integer Integer -> X)))
(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(: matrix-write (All (X) ((Matrix X) Integer Integer X -> (Matrix X))))
(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define-type Pos (Pair Integer Integer))
(: matrix-size (All (X) ((Matrix X) -> Pos)))
(define matrix-size
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(: matrix-map (All (X Y) ((X -> Y) (Matrix X) -> (Matrix Y))))
(define matrix-map
  (lambda (f mat)
    (map (lambda: ((lst : (Listof X))) (map f lst)) mat)))

(define initial-random 0)

(: next-random (Integer -> Integer))
(define next-random
  (lambda (current-random)
    (remainder (+ (* current-random 3581) 12751) 131072)))

(: shuffle (All (X) ((Listof X) -> (Listof X))))
(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(: shuffle-aux (All (X) ((Listof X) Integer -> (Listof X))))
(define shuffle-aux
  (lambda (lst current-random)
    (if (null? lst)
        '()
        (let ((new-random (next-random current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux (list-remove-pos lst i)
                               new-random)))))))

(: make-maze (Integer Integer -> (U (Matrix (U '_ '*)) 'error)))
(define make-maze
  (lambda (n m) ; n and m must be odd
    (if (not (and (odd? n) (odd? m)))
        'error
        (let ((cave
               (make-matrix n m (lambda: ((i : Integer) (j : Integer))
                                         (if (and (even? i) (even? j))
                                             (cons i j)
                                             #f))))
              (possible-holes
               (concat
                (for 0 n (lambda: ((i : Integer))
                                  (concat
                                   (for 0 m (lambda: ((j : Integer))
                                                     (if (equal? (even? i) (even? j))
                                                         '()
                                                         (list (cons i j)))))))))))
          (cave-to-maze (pierce-randomly (shuffle possible-holes) cave))))))

(: cave-to-maze (All (X) ((Matrix X) -> (Matrix (U '_ '*)))))
(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))

(: pierce (Pos (Matrix (Option Pos)) -> (Matrix (Option Pos))))
(define pierce
  (lambda (pos cave)
    (let: ((i : Integer (car pos)) (j : Integer (cdr pos)))
          (matrix-write cave i j pos))))

(: pierce-randomly ((Listof Pos) (Matrix (Option Pos))
                    -> (Matrix (Option Pos))))
(define pierce-randomly
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly (cdr possible-holes)
                           (try-to-pierce hole cave))))))

(: try-to-pierce (Pos (Matrix (Option Pos)) -> (Matrix (Option Pos))))
(define try-to-pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities pos cave)))
        (if (duplicates?
             (map (lambda: ((nc : Pos))
                           (matrix-read cave (car nc) (cdr nc)))
                  ncs))
            cave
            (pierce pos
                    (foldl (lambda: ((c : (Matrix (Option Pos))) (nc : Pos))
                                    (change-cavity c nc pos))
                           cave
                           ncs)))))))

(: change-cavity ((Matrix (Option Pos)) Pos Pos -> (Matrix (Option Pos))))
(define change-cavity
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))

(: change-cavity-aux ((Matrix (Option Pos)) Pos Pos (Option Pos)
                      -> (Matrix (Option Pos))))
(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda: ((c : (Matrix (Option Pos))) (nc : Pos))
                            (change-cavity-aux c nc new-cavity-id old-cavity-id))
                   (matrix-write cave i j new-cavity-id)
                   (neighboring-cavities pos cave))
            cave)))))

(: neighboring-cavities (All (X) (Pos (Matrix X) -> (Listof Pos))))
(define neighboring-cavities
  (lambda (pos cave)
    (let ((size (matrix-size cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read cave (- i 1) j))
                      (list (cons (- i 1) j))
                      '())
                  (if (and (< i (- n 1)) (matrix-read cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      '())
                  (if (and (> j 0) (matrix-read cave i (- j 1)))
                      (list (cons i (- j 1)))
                      '())
                  (if (and (< j (- m 1)) (matrix-read cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      '())))))))


(let ((input (with-input-from-file "input.txt" read)))
  (time (let: loop : (U (Matrix (U '_ '*)) 'error)
              ((n : Integer 10000) (v : (U (Matrix (U '_ '*)) 'error) '()))
              (if (zero? n)
                  v
                  (loop (- n 1)
                        (make-maze 11 (if input 11 0)))))))
