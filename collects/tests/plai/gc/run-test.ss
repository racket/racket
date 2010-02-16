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

(test
 (for ([m (in-directory (build-path here "bad-mutators") #rx"ss$")])
   (test
    (test-mutator m) =error> #rx"")))