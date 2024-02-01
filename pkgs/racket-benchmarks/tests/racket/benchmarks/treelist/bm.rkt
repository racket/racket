#lang racket/base
(require racket/list
         racket/treelist
         racket/vector
         data/gvector
         racket/mutable-treelist)

(define (measure M N)
  (printf "~s of length ~s\n" M N)
  
  (define-syntax-rule (bm who body)
    (let ()
      (display who)
      (display (make-string (max 0 (- 20 (string-length who))) #\space))
      (collect-garbage)
      (time body)
      (void)))

  (bm "+ 1treelist-append"
      (let ([l (for/treelist ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (treelist-append l l))))

  (bm "+ 1vector-append"
      (let ([l (for/vector ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector-append l l))))

  (bm "+ 1vec->treelist"
      (let ([vec (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector->treelist vec))))

  (bm "+ cons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l null]) ([i (in-range 0 N)])
          (cons i l))))

  (bm "+ 1list-append"
      (let ([l (for/list ([i (in-range 0 (quotient N 2))]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (append l l))))

  (bm "+ for/vector #:len"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/vector #:length N ([i (in-range 0 N)])
                    i)))

  (bm "+ for/vec->tree"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (vector->treelist
         (for/vector #:length N ([i (in-range 0 N)]) i))))

  (bm "+ 1list->treelist"
      (let ([lst (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (list->treelist lst))))

  (bm "+ for/list"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/list ([i (in-range 0 N)])
          i)))

  (bm "+ for/vector"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/vector ([i (in-range 0 N)])
          i)))

  (bm "+ listcons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l null]) ([i (in-range 0 N)])
          (if (list? l)
              (cons i l)
              (error "oops")))))

  (bm "+ for/treelist"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/treelist ([i (in-range 0 N)])
          i)))

  (bm "+ for/mut-treelist"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/mutable-treelist ([i (in-range 0 N)])
          i)))

  (bm "+ treelist-add"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l empty-treelist]) ([i (in-range 0 N)])
          (treelist-add l i))))

  (bm "+ treelist-cons"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([l empty-treelist]) ([i (in-range 0 N)])
          (treelist-cons l i))))

  (bm "+ for/list->tree"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (list->treelist
         (for/list ([i (in-range 0 N)]) i))))

  (bm "+ mut-treelist-add!"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (let ([mtl (make-mutable-treelist 0)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-add! mtl i)))))

  (bm "+ hasheq-set"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/fold ([t #hasheq()]) ([i (in-range 0 N)])
          (hash-set t i i))))

  (bm "+ for/gvector"
      (for/fold ([r #f]) ([i (in-range 0 M)])
        (for/gvector ([i (in-range 0 N)])
          i)))

  (bm "+ gvector-add!"
      (for ([i (in-range 0 M)])
        (let ([gv (make-gvector)])
          (for ([i (in-range 0 N)])
            (gvector-add! gv i)))))

  (bm "- cdr"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (cdr l)))))

  (bm "- rest"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (rest l)))))

  (bm "- treelist-rest"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (treelist-rest l)))))

  (when (N . <= . 1000)
    (bm "- vector-copy N-1"
        (let ([v (for/vector ([i (in-range 0 N)]) i)])
          (for/fold ([r #f]) ([i (in-range 0 M)])
            (for/fold ([v v]) ([i (in-range 0 N)])
              (vector-copy v 1))))))

  (bm "- hasheq-remove"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([t ht]) ([i (in-range 0 N)])
            (hash-remove t i)))))

  (bm "- 1list-tail 1/2"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (list-tail l (quotient N 2)))))

  (bm "- 1treelst-drop 1/2"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (treelist-drop l (quotient N 2)))))

  (bm "- 1vector-copy 1/2"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (vector-copy v (quotient N 2)))))

  (bm "! vector-set!/fx"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (vector-set! v i (+ i j))))))

  (bm "! vector-set!/ptr"
      (let ([v (for/vector ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (vector-set! v i "x")))))

  (bm "! mut-tree-set!/fx"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-set! l i (+ i j))))))

  (bm "! mut-tree-set!/ptr"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for ([j (in-range 0 M)])
          (for ([i (in-range 0 N)])
            (mutable-treelist-set! l i "x")))))

  (bm "! treelist-set"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([l l]) ([j (in-range 0 M)])
          (for/fold ([l l]) ([i (in-range 0 N)])
            (treelist-set l i (+ i j))))))

  (bm "! hasheq-set"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([ht ht]) ([j (in-range 0 M)])
          (for/fold ([ht ht]) ([i (in-range 0 N)])
            (hash-set ht i (+ i j))))))

  (when (N . <= . 10)
    (bm "! list-set"
        (let ([l (for/list ([i (in-range 0 N)]) i)])
          (for/fold ([l l]) ([j (in-range 0 M)])
            (for/fold ([l l]) ([i (in-range 0 N)])
              (list-set l i (+ i j)))))))

  (bm "^ in-vector"
      (let ([l (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-vector l)])
            i))))

  (bm "^ in-list"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-list l)])
            i))))

  (bm "^ in-treelist"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-treelist l)])
            i))))

  (bm "^ in-hash-keys"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-hash-keys ht)])
            i))))

  (bm "^ vector-ref"
      (let ([l (for/vector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (vector-ref l i)))))

  (when (N . <= . 1000)
    (bm "^ list-ref"
        (let ([l (for/list ([i (in-range 0 N)]) i)])
          (for/fold ([r #f]) ([i (in-range 0 M)])
            (for/fold ([v #f]) ([i (in-range 0 N)])
              (list-ref l i))))))

  (bm "^ treelist-ref"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (treelist-ref l i)))))

  (bm "^ mut-tree-ref"
      (let ([l (for/mutable-treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (mutable-treelist-ref l i)))))

  (bm "^ hasheq-ref"
      (let ([ht (for/hasheq ([i (in-range 0 N)]) (values i i))])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (hash-ref ht i)))))

  (bm "^ gvector-ref"
      (let ([l (for/gvector ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (in-range 0 N)])
            (gvector-ref l i)))))

  (bm "^ dyn in-list"
      (let ([l (for/list ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (car (list (in-list l)))])
            i))))

  (bm "^ dyn in-treelist"
      (let ([l (for/treelist ([i (in-range 0 N)]) i)])
        (for/fold ([r #f]) ([i (in-range 0 M)])
          (for/fold ([v #f]) ([i (car (list (in-treelist l)))])
            i))))

  (void))

(module+ main
  (measure 1000000
           10)
  (measure 100000
           100)
  (measure 10000
           1000)
  (measure 1000
           10000)
  (measure 100
           100000)
  (measure 10
           1000000))

(module+ test
  (measure 100000
           100))
