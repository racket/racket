(module labels-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (planet "util.ss" ("schematics" "schemeunit.plt" 2))
           (lib "etc.ss")
           (lib "file.ss")
           "../lang/labels.ss")
             
  (require/expose (lib "labels.ss" "web-server" "lang") (add1/string))
  
  (define THE-TEST-FILENAME (make-temporary-file))
  
  (provide labels-tests-suite)
  
  (define l1 (make-labeling #"foo" THE-TEST-FILENAME))
  (define l2 (make-labeling #"foo" THE-TEST-FILENAME))
  (define l3 (make-labeling #"bar" THE-TEST-FILENAME))
  (define l4 (make-labeling #"baz" THE-TEST-FILENAME))
  
  (define race-test-file (make-temporary-file))
  
  (define (genbytes)
    (string->bytes/utf-8
     (symbol->string (gensym))))
  
  (define-struct cell (sema ival new-val))
  
  (define (create-cell ival)
    (make-cell (make-semaphore) ival #f))
  
  ;; race?: (listof alpha) (alpha -> beta) ((listof beta) -> boolean)) -> boolean
  ;; compute a list of values in parallel and determine if the result indicates a
  ;; race condition.
  (define (race? initial-vals make-new-val check-new-vals)
    (let ([cells (map create-cell initial-vals)])
      (for-each
       (lambda (cell)
         (thread
          (lambda ()
            (dynamic-wind
             void
             (lambda () (set-cell-new-val! cell (make-new-val (cell-ival cell))))
             (lambda () (semaphore-post (cell-sema cell)))))))
       cells)
      (for-each
       (lambda (cell)
         (semaphore-wait (cell-sema cell)))
       cells)
      (with-handlers ([void
                       (lambda (the-exn) #t)])
        (check-new-vals (map cell-new-val cells)))))
  
  (define (make-labeling-race? n)
    (delete-tag-list! race-test-file)
    (race? (build-list n (lambda (i) (genbytes)))
           (lambda (some-bytes)
             (make-labeling some-bytes race-test-file))
           (lambda (labelings)
             (let loop ([label 0]
                        [labelings labelings])
               (if (null? labelings) 
                   #f
                   (let ([new-label ((car labelings))])
                     (or (eqv? new-label label)
                         (loop new-label (cdr labelings)))))))))
  
  (define (delete-tag-list!-race? n)
    (race? (build-list n (lambda (i) #"foo"))
           (lambda (some-bytes)
             (delete-tag-list! race-test-file)
             (make-labeling some-bytes race-test-file))
           (lambda (labelings)
             (let* ([syms (map (lambda (l) (l)) labelings)]
                    [sym0 (car syms)])
               (not
                (andmap
                 (lambda (sym)
                   (eqv? sym0 sym))
                 syms))))))
  
  (define labels-tests-suite
    (test-suite
     "Tests for labels.ss"
     
     (test-case
      "Test the tag incrementing scheme"
      (check string=? "b" (add1/string ""))
      (check string=? "A" (add1/string "z"))
      (check string=? "B" (add1/string "A"))
      (check string=? "b" (add1/string "a"))
      (check string=? "ab" (add1/string "Z"))
      (check string=? "aab" (add1/string "ZZ"))
      (check string=? "Azz" (add1/string "zzz"))
      (check string=? "aaaab" (add1/string "ZZZZ"))
      (check string=? "baaab" (add1/string "aaaab")))
     
     
     (test-case
      "The same program produces the same labeling"
      (check-eqv? (l1) (l2))
      (check-eqv? (l1) (l2)))
     
     (test-case
      "Different programs produce different labelings"
      (check-false (eqv? (l3) (l4))))
     
     (test-case
      "Check for race condition on make-labeling"
      (check-false (make-labeling-race? 256)))
     
     (test-case
      "Check for race condition on delete-tag-list!"
      (check-false (delete-tag-list!-race? 256))))))