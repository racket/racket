(module persistent-close-tests mzscheme
  (require (lib "file-vector.ss" "web-server" "prototype-web-server" "graveyard")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "serialize.ss")
           (lib "persistent-close.ss" "web-server" "prototype-web-server" "graveyard"))
  
  (provide persistent-close-suite)
  
  (define persistent-close-suite
    (test-suite
     "Tests for persistent-close.ss"
     
     (test-case
      "file-vector references"
      (let ([fv (make-file-vector '/tmp/foo 1 2 3)])
        (check = 1 (file-vector-ref fv 0))
        (check = 2 (file-vector-ref fv 1))
        (check = 3 (file-vector-ref fv 2))
        (file-vector-set! fv 0 -1)
        (file-vector-set! fv 1 -2)
        (file-vector-set! fv 2 -3)
        (check = -1 (file-vector-ref fv 0))
        (check = -2 (file-vector-ref fv 1))
        (check = -3 (file-vector-ref fv 2))))

     (test-case
      "serializing file vectors"
      (let* ([fv (make-file-vector '/tmp/foo -1 -2 -3)]
             [fv2 (deserialize (serialize fv))])
        (check = -1 (file-vector-ref fv2 0))
        (check = -2 (file-vector-ref fv2 1))
        (check = -3 (file-vector-ref fv2 2))))
     
     (test-case
      "close/file test"
      (let ([x 7] [y 8])
        (check = 7 (close/file 'f1 (x y) x))
        (check = 15 (close/file 'f2 (x y) (+ x y))))))))
