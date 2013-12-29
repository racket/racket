#lang racket
(require tests/eli-tester
         racket/runtime-path
         "../util.rkt")

(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "-g")])
    (dynamic-require (quote-module-path "..") #f)))

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

(define (drop-first-line e)
  (regexp-replace "^[^\n]+\n" e ""))
(define-syntax-rule (capture-output e)
  (drop-first-line (with-both-output-to-string (λ () e))))

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
 #rx"unbound identifier in module\n  in: frozzle"
 
 (capture-output (test-mutator (build-path here "other-mutators" "printing.rkt")))
 =>
 #<<END
(good lst '(1 2 3) '(1 2 3) "at line 6")
(good (length (quote (hello goodbye))) 2 2 "at line 13")
(good (heap-loc head) 62 62 "at line 18")
(bad (heap-loc head) 62 47 "at line 19")

END
 
  (capture-output (test-mutator (build-path here "other-mutators" "begin.rkt")))
 =>
 #<<END
Value at location 2:
#t

END
 
 (test-mutator (build-path here "other-mutators" "quote.rkt"))
 =error> "alloc: out of space"
 )
