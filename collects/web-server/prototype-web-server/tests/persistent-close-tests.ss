(module persistent-close-tests mzscheme
  (require (lib "file-vector.ss" "web-server" "prototype-web-server")
           (planet "test.ss" ("schematics" "schemeunit.plt" 1 1))
           (lib "serialize.ss")
           (lib "persistent-close.ss" "web-server" "prototype-web-server"))
  
  (provide persistent-close-suite)
  
  (define persistent-close-suite
    (make-test-suite
     "tests for persistent-close.ss"
     
     (make-test-case
      "file-vector references"
      (let ([fv (make-file-vector 'foo 1 2 3)])
        (assert = 1 (file-vector-ref fv 0))
        (assert = 2 (file-vector-ref fv 1))
        (assert = 3 (file-vector-ref fv 2))
        (file-vector-set! fv 0 -1)
        (file-vector-set! fv 1 -2)
        (file-vector-set! fv 2 -3)
        (assert = -1 (file-vector-ref fv 0))
        (assert = -2 (file-vector-ref fv 1))
        (assert = -3 (file-vector-ref fv 2))))

     (make-test-case
      "serializing file vectors"
      (let* ([fv (make-file-vector 'foo -1 -2 -3)]
             [fv2 (deserialize (serialize fv))])
        (assert = -1 (file-vector-ref fv2 0))
        (assert = -2 (file-vector-ref fv2 1))
        (assert = -3 (file-vector-ref fv2 2))))
     
     (make-test-case
      "close/file test"
      (let ([x 7] [y 8])
        (assert = 7 (close/file 'f1 (x y) x))
        (assert = 15 (close/file 'f2 (x y) (+ x y))))))))
