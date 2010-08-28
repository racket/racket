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
  (printf "Running ~a\n" m)
  (parameterize ([current-namespace (make-base-empty-namespace)])
    (dynamic-require m #f)))

(define run-good? (make-parameter #f))

(command-line #:program "run-test"
              #:once-each ["-g" "Enable running good mutators" (run-good? #t)])

(test
 (if (run-good?)
     (for ([m (in-directory (build-path here "good-mutators") #rx"rkt$")])
       (test
        (test-mutator m)))
     (void))
 (for ([m (in-directory (build-path here "bad-mutators") #rx"rkt$")])
   (test
    (test-mutator m) =error> #rx""))
 
 (test-mutator (build-path here "other-mutators" "error.rkt"))
 =error>
 #rx"plai/mutator has error"
 
 (test-mutator (build-path here "other-mutators" "top.rkt"))
 =error>
 #rx"unbound identifier in module in: frozzle"
 )