(module file-vector-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "serialize.ss")
           (lib "file-vector.ss" "web-server" "graveyard"))
  (provide file-vector-tests)
  
  (define file-vector-tests
    (test-suite
     "File Vector"
     
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
        (check = -3 (file-vector-ref fv2 2)))))))