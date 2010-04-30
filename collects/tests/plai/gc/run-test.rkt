#lang scheme
(require tests/eli-tester
         scheme/runtime-path)

(define-runtime-path here ".")

(define (in-directory pth rx)
  (in-list
   (map (curry build-path pth)
        (filter (compose (curry regexp-match rx) path->bytes)
                (directory-list pth)))))

(define (test-mutator m)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (dynamic-require m #f)))

(define run-good? (make-parameter #f))

(command-line #:program "run-test"
              #:once-each ["-g" "Enable running good mutators" (run-good? #t)])

(test
 (if (run-good?)
     (for ([m (in-directory (build-path here "good-mutators") #rx"ss$")])
       (test
        (test-mutator m)))
     (void))
 (for ([m (in-directory (build-path here "bad-mutators") #rx"ss$")])
   (test
    (test-mutator m) =error> #rx"")))