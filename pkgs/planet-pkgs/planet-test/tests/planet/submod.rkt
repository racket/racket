#lang racket
(require setup/dirs)

;; do nothing via 'raco test' because run-all.rkt runs this test
;; and that way we can guarantee they run sequentially in drdr
(module test racket/base)


(define raco (build-path (find-console-bin-dir)
                         (if (eq? 'windows (system-type))
                             "raco.exe"
                             "raco")))

(void
 (system* raco "planet" "link" "racket-tester" "p1.plt" "1" "0"
          (path->string (collection-path "tests" "compiler" "embed" "embed-planet-1"))))

(define (test expected got)
  (unless (equal? expected got)
    (error "failed")))

(define-syntax-rule (test/exn e)
  (test 'exn
        (with-handlers ([exn:fail? (lambda (exn) 'exn)])
          e)))

(test #f (module-declared? `(submod (planet racket-tester/p1) reader) #f))
(test #f (module-declared? `(planet racket-tester/p1) #f))
(test #f (module-declared? `(planet racket-tester/p1/none) #f))

(test/exn (module-declared? `(planet racket-tester/p1/none) #t))
(test #f (module-declared? `(submod (planet racket-tester/p1/none) reader) #t))


(test #f (module-declared? `(submod (planet racket-tester/p1) reader) #t))
(test #t (module-declared? `(planet racket-tester/p1) #f))

(test #f (module-declared? `(submod (planet racket-tester/p1/has-sub) the-sub) #f))
(test #t (module-declared? `(submod (planet racket-tester/p1/has-sub) the-sub) #t))

(void
 (system* raco "planet" "unlink" "racket-tester" "p1.plt" "1" "0"))

