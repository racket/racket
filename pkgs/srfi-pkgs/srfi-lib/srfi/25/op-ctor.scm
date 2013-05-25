(begin
  (define array:opt-args '(ctor (4)))
  (define (mlist->list l)
    (if (null? l)
        null
        (cons (mcar l) (mlist->list (mcdr l)))))
  (define (array:optimize f r)
    (case r
      ((0) (let ((n0 (f))) (array:0 n0)))
      ((1) (let ((n0 (f 0))) (array:1 n0 (- (f 1) n0))))
      ((2)
       (let ((n0 (f 0 0)))
         (array:2 n0 (- (f 1 0) n0) (- (f 0 1) n0))))
      ((3)
       (let ((n0 (f 0 0 0)))
         (array:3
           n0
           (- (f 1 0 0) n0)
           (- (f 0 1 0) n0)
           (- (f 0 0 1) n0))))
      (else
       (let ((v
              (do ((k 0 (+ k 1)) (v '() (mcons 0 v)))
                  ((= k r) v))))
         (let ((n0 (apply f (mlist->list v))))
           (apply
            array:n
            n0
            (array:coefficients f n0 v v)))))))
  (define (array:optimize-empty r)
    (let ((x (make-vector (+ r 1) 0)))
      (vector-set! x r -1)
      x))
  (define (array:coefficients f n0 vs vp)
    (case vp
      ((()) '())
      (else
       (set-mcar! vp 1)
       (let ((n (- (apply f (mlist->list vs)) n0)))
         (set-mcar! vp 0)
         (cons n (array:coefficients f n0 vs (mcdr vp)))))))
  (define (array:vector-index x ks)
    (do ((sum 0 (+ sum (* (vector-ref x k) (car ks))))
         (ks ks (cdr ks))
         (k 0 (+ k 1)))
        ((null? ks) (+ sum (vector-ref x k)))))
  (define (array:shape-index) '#(2 1 0))
  (define (array:empty-shape-index) '#(0 0 -1))
  (define (array:shape-vector-index x r k)
    (+
     (* (vector-ref x 0) r)
     (* (vector-ref x 1) k)
     (vector-ref x 2)))
  (define (array:actor-index x k)
    (+ (* (vector-ref x 0) k) (vector-ref x 1)))
  (define (array:0 n0) (vector n0))
  (define (array:1 n0 n1) (vector n1 n0))
  (define (array:2 n0 n1 n2) (vector n1 n2 n0))
  (define (array:3 n0 n1 n2 n3) (vector n1 n2 n3 n0))
  (define (array:n n0 n1 n2 n3 n4 . ns)
    (apply vector n1 n2 n3 n4 (append ns (list n0))))
  (define (array:maker r)
    (case r
      ((0) array:0)
      ((1) array:1)
      ((2) array:2)
      ((3) array:3)
      (else array:n)))
  (define array:indexer/vector
    (let ((em
           (vector
             (lambda (x i) (+ (vector-ref x 0)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (vector-ref x 1)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (vector-ref x 2)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (vector-ref x 3)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (vector-ref x 4)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (vector-ref x 5)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (* (vector-ref x 5) (vector-ref i 5))
                (vector-ref x 6)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (* (vector-ref x 5) (vector-ref i 5))
                (* (vector-ref x 6) (vector-ref i 6))
                (vector-ref x 7)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (* (vector-ref x 5) (vector-ref i 5))
                (* (vector-ref x 6) (vector-ref i 6))
                (* (vector-ref x 7) (vector-ref i 7))
                (vector-ref x 8)))
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (* (vector-ref x 5) (vector-ref i 5))
                (* (vector-ref x 6) (vector-ref i 6))
                (* (vector-ref x 7) (vector-ref i 7))
                (* (vector-ref x 8) (vector-ref i 8))
                (vector-ref x 9)))))
          (it
           (lambda (w)
             (lambda (x i)
               (+
                (* (vector-ref x 0) (vector-ref i 0))
                (* (vector-ref x 1) (vector-ref i 1))
                (* (vector-ref x 2) (vector-ref i 2))
                (* (vector-ref x 3) (vector-ref i 3))
                (* (vector-ref x 4) (vector-ref i 4))
                (* (vector-ref x 5) (vector-ref i 5))
                (* (vector-ref x 6) (vector-ref i 6))
                (* (vector-ref x 7) (vector-ref i 7))
                (* (vector-ref x 8) (vector-ref i 8))
                (* (vector-ref x 9) (vector-ref i 9))
                (do ((xi
                      0
                      (+
                       (* (vector-ref x u) (vector-ref i u))
                       xi))
                     (u (- w 1) (- u 1)))
                    ((< u 10) xi))
                (vector-ref x w))))))
      (lambda (r) (if (< r 10) (vector-ref em r) (it r)))))
  (define array:indexer/array
    (let ((em
           (vector
             (lambda (x v i) (+ (vector-ref x 0)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (vector-ref x 1)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (vector-ref x 2)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (vector-ref x 3)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (vector-ref x 4)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (vector-ref x 5)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (*
                 (vector-ref x 5)
                 (vector-ref v (array:actor-index i 5)))
                (vector-ref x 6)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (*
                 (vector-ref x 5)
                 (vector-ref v (array:actor-index i 5)))
                (*
                 (vector-ref x 6)
                 (vector-ref v (array:actor-index i 6)))
                (vector-ref x 7)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (*
                 (vector-ref x 5)
                 (vector-ref v (array:actor-index i 5)))
                (*
                 (vector-ref x 6)
                 (vector-ref v (array:actor-index i 6)))
                (*
                 (vector-ref x 7)
                 (vector-ref v (array:actor-index i 7)))
                (vector-ref x 8)))
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (*
                 (vector-ref x 5)
                 (vector-ref v (array:actor-index i 5)))
                (*
                 (vector-ref x 6)
                 (vector-ref v (array:actor-index i 6)))
                (*
                 (vector-ref x 7)
                 (vector-ref v (array:actor-index i 7)))
                (*
                 (vector-ref x 8)
                 (vector-ref v (array:actor-index i 8)))
                (vector-ref x 9)))))
          (it
           (lambda (w)
             (lambda (x v i)
               (+
                (*
                 (vector-ref x 0)
                 (vector-ref v (array:actor-index i 0)))
                (*
                 (vector-ref x 1)
                 (vector-ref v (array:actor-index i 1)))
                (*
                 (vector-ref x 2)
                 (vector-ref v (array:actor-index i 2)))
                (*
                 (vector-ref x 3)
                 (vector-ref v (array:actor-index i 3)))
                (*
                 (vector-ref x 4)
                 (vector-ref v (array:actor-index i 4)))
                (*
                 (vector-ref x 5)
                 (vector-ref v (array:actor-index i 5)))
                (*
                 (vector-ref x 6)
                 (vector-ref v (array:actor-index i 6)))
                (*
                 (vector-ref x 7)
                 (vector-ref v (array:actor-index i 7)))
                (*
                 (vector-ref x 8)
                 (vector-ref v (array:actor-index i 8)))
                (*
                 (vector-ref x 9)
                 (vector-ref v (array:actor-index i 9)))
                (do ((xi
                      0
                      (+
                       (*
                        (vector-ref x u)
                        (vector-ref
                          v
                          (array:actor-index i u)))
                       xi))
                     (u (- w 1) (- u 1)))
                    ((< u 10) xi))
                (vector-ref x w))))))
      (lambda (r) (if (< r 10) (vector-ref em r) (it r)))))
  (define array:applier-to-vector
    (let ((em
           (vector
             (lambda (p v) (p))
             (lambda (p v) (p (vector-ref v 0)))
             (lambda (p v)
               (p (vector-ref v 0) (vector-ref v 1)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)
                (vector-ref v 6)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)
                (vector-ref v 6)
                (vector-ref v 7)))
             (lambda (p v)
               (p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)
                (vector-ref v 6)
                (vector-ref v 7)
                (vector-ref v 8)))))
          (it
           (lambda (r)
             (lambda (p v)
               (apply
                p
                (vector-ref v 0)
                (vector-ref v 1)
                (vector-ref v 2)
                (vector-ref v 3)
                (vector-ref v 4)
                (vector-ref v 5)
                (vector-ref v 6)
                (vector-ref v 7)
                (vector-ref v 8)
                (vector-ref v 9)
                (do ((k r (- k 1))
                     (r
                      '()
                      (cons (vector-ref v (- k 1)) r)))
                    ((= k 10) r)))))))
      (lambda (r) (if (< r 10) (vector-ref em r) (it r)))))
  (define array:applier-to-actor
    (let ((em
           (vector
             (lambda (p a) (p))
             (lambda (p a) (p (array-ref a 0)))
             (lambda (p a)
               (p (array-ref a 0) (array-ref a 1)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)
                (array-ref a 5)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)
                (array-ref a 5)
                (array-ref a 6)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)
                (array-ref a 5)
                (array-ref a 6)
                (array-ref a 7)))
             (lambda (p a)
               (p
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)
                (array-ref a 5)
                (array-ref a 6)
                (array-ref a 7)
                (array-ref a 8)))))
          (it
           (lambda (r)
             (lambda (p a)
               (apply
                a
                (array-ref a 0)
                (array-ref a 1)
                (array-ref a 2)
                (array-ref a 3)
                (array-ref a 4)
                (array-ref a 5)
                (array-ref a 6)
                (array-ref a 7)
                (array-ref a 8)
                (array-ref a 9)
                (do ((k r (- k 1))
                     (r '() (cons (array-ref a (- k 1)) r)))
                    ((= k 10) r)))))))
      (lambda (r)
        "These are high level, hiding implementation at call site."
        (if (< r 10) (vector-ref em r) (it r)))))
  (define array:applier-to-backing-vector
    (let ((em
           (vector
             (lambda (p ai av) (p))
             (lambda (p ai av)
               (p (vector-ref av (array:actor-index ai 0))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))
                (vector-ref av (array:actor-index ai 5))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))
                (vector-ref av (array:actor-index ai 5))
                (vector-ref av (array:actor-index ai 6))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))
                (vector-ref av (array:actor-index ai 5))
                (vector-ref av (array:actor-index ai 6))
                (vector-ref av (array:actor-index ai 7))))
             (lambda (p ai av)
               (p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))
                (vector-ref av (array:actor-index ai 5))
                (vector-ref av (array:actor-index ai 6))
                (vector-ref av (array:actor-index ai 7))
                (vector-ref av (array:actor-index ai 8))))))
          (it
           (lambda (r)
             (lambda (p ai av)
               (apply
                p
                (vector-ref av (array:actor-index ai 0))
                (vector-ref av (array:actor-index ai 1))
                (vector-ref av (array:actor-index ai 2))
                (vector-ref av (array:actor-index ai 3))
                (vector-ref av (array:actor-index ai 4))
                (vector-ref av (array:actor-index ai 5))
                (vector-ref av (array:actor-index ai 6))
                (vector-ref av (array:actor-index ai 7))
                (vector-ref av (array:actor-index ai 8))
                (vector-ref av (array:actor-index ai 9))
                (do ((k r (- k 1))
                     (r
                      '()
                      (cons
                       (vector-ref
                         av
                         (array:actor-index ai (- k 1)))
                       r)))
                    ((= k 10) r)))))))
      (lambda (r)
        "These are low level, exposing implementation at call site."
        (if (< r 10) (vector-ref em r) (it r)))))
  (define (array:index/vector r x v)
    ((array:indexer/vector r) x v))
  (define (array:index/array r x av ai)
    ((array:indexer/array r) x av ai))
  (define (array:apply-to-vector r p v)
    ((array:applier-to-vector r) p v))
  (define (array:apply-to-actor r p a)
    ((array:applier-to-actor r) p a)))
