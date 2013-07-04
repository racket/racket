#lang racket
(require setup/link
         compiler/find-exe
         racket/sandbox)

(define-syntax-rule (test expect expr)
  (do-test expect expr 'expr))

(define (do-test expect val expr)
  (unless (equal? expect val)
    (eprintf "test failed: ~.s; expected: ~e; actual: ~e\n" 
             expr expect val)))

;; ----------------------------------------
;; set up

(define work-dir (build-path (find-system-path 'temp-dir) "link-test"))
(when (directory-exists? work-dir)
  (delete-directory/files work-dir))
(make-directory work-dir)

(define link-file (build-path work-dir "links"))

;; ----------------------------------------
;; running Racket

(define racket-exe (find-exe #f))

(define (run-racket args)
  (let ([o (open-output-string)]
        [eo (open-output-string)])
    (with-handlers ([exn? (lambda (exn) (raise exn))])
      (parameterize ([current-output-port o]
                     [current-error-port eo])
        (apply system* 
               racket-exe
               (list* "-C"
                      link-file
                      args))))
    (values (get-output-string o)
            (get-output-string eo))))

(define (test-racket result-str args)
  (define-values (out err) (run-racket args))
  (unless (string=? "" err)
    (eprintf "test stderr: ~e\n" err))
  (let ([eo (open-output-string)])
    (display result-str eo)
    (newline eo)
    (do-test (get-output-string eo)
             out
             `(racket ,result-str ',args))))

(define (run-setup collection 
                   #:err [err-rx #f] 
                   #:no-docs? [no-docs? #f])
  (printf "setup ~s\n" collection)
  (define-values (out err)
    (run-racket (append `("-l-" "setup")
                        (if no-docs?
                            `("-D")
                            null)
                        `("-l" ,collection))))
  (cond
   [err-rx
    (unless (regexp-match err-rx err)
      (eprintf "setup non-matching stderr: ~e\n" err))]
   [else
    (unless (string=? "" err)
      (eprintf "setup stderr: ~e\n" err))]))

;; ----------------------------------------
;; check setup errs

(run-setup "Racket"
           #:err "collection not found|not in canonical form")

;; ----------------------------------------
;; simple collection link

(test null (links #:file link-file))
(test null (links #:file link-file #:root? #t))

(define c1-dir (build-path work-dir "c1"))
(make-directory c1-dir)
(test '("c1") (links c1-dir #:file link-file))

(test '("c1") (links #:file link-file))
(test (list (cons "c1" c1-dir)) (links #:with-path? #t #:file link-file))
(test null (links #:file link-file #:root? #t))

(with-output-to-file (build-path c1-dir "m1.rkt")
  (lambda ()
    (printf "#lang racket/base\n'm1\n")))

(test-racket "'m1" '("-l" "c1/m1"))

(run-setup "c1")
(test #t (file-exists? (build-path c1-dir "compiled" "m1_rkt.zo")))

;; ----------------------------------------
;; splicing with "mzlib"

(define mzlib-dir (build-path work-dir "mzlib"))
(make-directory mzlib-dir)
(test '("c1" "mzlib") (sort (links mzlib-dir #:file link-file) string<?))

(test '("c1" "mzlib") (sort (links #:file link-file) string<?))
(test null (links #:file link-file #:root? #t))

(with-output-to-file (build-path mzlib-dir "m1.rkt")
  (lambda ()
    (printf "#lang racket/base\n'mz1\n")))

(test-racket "'mz1" '("-l" "mzlib/m1"))

(test-racket "#<channel>" '("-l" "racket/base" "-l" "mzlib/cml" "-e" "(channel)"))

(run-setup "mzlib" #:no-docs? #t)
(test #t (file-exists? (build-path mzlib-dir "compiled" "m1_rkt.zo")))

;; ----------------------------------------
;; splicing via new root directory

(define new-root-dir (build-path work-dir "collects"))
(make-directory new-root-dir)

(define another-c1-dir (build-path new-root-dir "c1"))
(make-directory another-c1-dir)

(with-output-to-file (build-path another-c1-dir "m2.rkt")
  (lambda ()
    (printf "#lang racket/base\n'm2\n")))
(with-output-to-file (build-path c1-dir "m3.rkt")
  (lambda ()
    (printf "#lang racket/base\n'm3\n")))

(test null (links #:file link-file #:root? #t))
(test (list new-root-dir) (links new-root-dir #:file link-file #:root? #t))

(test-racket "'m1" '("-l" "c1/m1"))
(test-racket "'m2" '("-l" "c1/m2"))
(test-racket "'m3" '("-l" "c1/m3"))

(run-setup "c1")
(test #t (file-exists? (build-path c1-dir "compiled" "m1_rkt.zo")))
(test #t (file-exists? (build-path another-c1-dir "compiled" "m2_rkt.zo")))
(test #t (file-exists? (build-path c1-dir "compiled" "m3_rkt.zo")))

;; original "c1" should take precdence over the new addition,
;; just based on the order of addition

(with-output-to-file (build-path another-c1-dir "m3.rkt")
  (lambda ()
    (printf "#lang racket/base\n'bad-m3\n")))
(test-racket "'m3" '("-l" "c1/m3"))

(run-setup "c1")
;; questionable: maybe modules unreachable via `require' shouldn't be compiled:
(test #t (file-exists? (build-path another-c1-dir "compiled" "m3_rkt.zo")))

(with-output-to-file (build-path another-c1-dir "m4.rkt")
  (lambda ()
    (printf "#lang racket/base\n(require c1/m1)\n")))
(test-racket "'m1" '("-l" "c1/m4"))

(with-output-to-file (build-path another-c1-dir "m5.rkt")
  (lambda ()
    (printf "#lang racket/base\n(require c1/m3)\n")))
(test-racket "'m3" '("-l" "c1/m5"))

;; Relative path accesses a relative file --- which is inconsistent
;; with module-path collapses, so this shouldn't be done, but
;; check it anyway:
(with-output-to-file (build-path another-c1-dir "m6.rkt")
  (lambda ()
    (printf "#lang racket/base\n(require \"m3.rkt\")\n")))
(test-racket "'bad-m3" '("-l" "c1/m6"))

;; ----------------------------------------
;; omits in linked collections:

(with-output-to-file (build-path c1-dir "info.rkt")
  (lambda ()
    (printf "#lang info\n(define compile-omit-paths '(\"b1.rkt\"))\n")))
(with-output-to-file (build-path c1-dir "b1.rkt")
  (lambda ()
    (printf "#lang racket/base\n'b1\n")))

(with-output-to-file (build-path another-c1-dir "info.rkt")
  (lambda ()
    (printf "#lang info\n(define compile-omit-paths '(\"b2.rkt\"))\n")))
(with-output-to-file (build-path another-c1-dir "b2.rkt")
  (lambda ()
    (printf "#lang racket/base\n'b2\n")))

(run-setup "c1")

(test #f (file-exists? (build-path c1-dir "compiled" "b1_rkt.zo")))
(test #f (file-exists? (build-path another-c1-dir "compiled" "b2_rkt.zo")))

;; ----------------------------------------
;; subcollections:

(define c1/s1-dir (build-path c1-dir "s1"))
(make-directory c1/s1-dir)

(define c1/s2-dir (build-path another-c1-dir "s2"))
(make-directory c1/s2-dir)

(with-output-to-file (build-path c1/s1-dir "n1.rkt")
  (lambda ()
    (printf "#lang racket/base\n'n1\n")))

(with-output-to-file (build-path c1/s2-dir "n2.rkt")
  (lambda ()
    (printf "#lang racket/base\n'n2\n")))

(test-racket "'n1" '("-l" "c1/s1/n1"))
(test-racket "'n2" '("-l" "c1/s2/n2"))

(run-setup "c1/s1")
(test #t (file-exists? (build-path c1/s1-dir "compiled" "n1_rkt.zo")))
(run-setup "c1/s2")
(test #t (file-exists? (build-path c1/s2-dir "compiled" "n2_rkt.zo")))

;; ----------------------------------------
;; sandbox:

(test-racket "#f" '("-l" "racket/base" "-l" "racket/sandbox" "-e" "(void? (make-evaluator 'racket/base))"))
(test-racket "'mz1" '("-l" "racket/base" "-l" "racket/sandbox" 
                      "-e" "(sandbox-output current-output-port)"
                      "-e" "(define e (make-evaluator 'racket/base))" 
                      "-e" "(e '(require mzlib/m1))"))
(test-racket "'m2" '("-l" "racket/base" "-l" "racket/sandbox" 
                     "-e" "(sandbox-output current-output-port)"
                     "-e" "(define e (make-evaluator 'racket/base))" 
                     "-e" "(e '(require c1/m2))"))
(test-racket "'n1" '("-l" "racket/base" "-l" "racket/sandbox" 
                     "-e" "(sandbox-output current-output-port)"
                     "-e" "(define e (make-evaluator 'racket/base))" 
                     "-e" "(e '(require c1/s1/n1))"))

;; ----------------------------------------
;; docs in a linked collection:

(with-output-to-file (build-path c1-dir "c1.scrbl")
  (lambda ()
    (printf "#lang scribble/manual\n@title{C1}@defmodule[c1]\n")))
(with-output-to-file (build-path c1-dir "info.rkt")
  #:exists 'truncate
  (lambda ()
    (printf "#lang setup/infotab\n(define scribblings '((\"c1.scrbl\")))\n")))

(run-setup "c1")
(test #t (file-exists? (build-path c1-dir "doc" "c1" "index.html")))

;; ----------------------------------------
;; clean up

;(delete-directory/files work-dir)
