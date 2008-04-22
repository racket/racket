(define files
  (list "basic-tests.ss"
        "area.ss"
        "barland.ss"
        "batched-queue.scm"
        "annotation-test.ss"
        "cl.ss"
        "do.ss"
        "foo.scm"
        "if-splitting-test.ss"
        "leftist-heap.ss"
        "let-values-tests.ss"
        "little-schemer.ss"
        "seasoned-schemer.ss"
        "manual-examples.ss"
        "mu-rec.ss"
        "struct-exec.ss"
        "pair-test.ss"
        "poly-struct.ss"
        "poly-tests.ss"
        "priority-queue.scm"
        "rec-types.ss"
        "require-tests.ss"
        #;"set-struct.ss"
        "typed-list.ss"
        "varargs-tests.ss"
        "vec-tests.ss"))

(define eli-files
  (map (lambda (s) (string-append "660-examples/" s))
       (list "slow.ss"
             "hw01.scm"
             "hw02.scm"
             "hw03.scm"
             "hw04.scm"
             "hw05.scm")))

(define (loader f) (with-handlers ([exn:fail? (lambda _ (printf "FAILED: ~a~n" f))])
                     (load f)))

(require (planet "io.ss" ("dherman" "io.plt" 1)))

(define (count-lines f)
  (length (read-lines (open-input-file f))))

(define (go)
  (for-each loader files)
  (for-each loader eli-files))

(apply + (map count-lines (append files eli-files)))

(time (go))
