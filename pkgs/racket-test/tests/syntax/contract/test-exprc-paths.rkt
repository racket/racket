#lang racket/base
(require (for-syntax racket/base)
         racket/file
         rackunit
         racket/runtime-path)

;; Check that the compiled example files (macro*.rkt, client*.rkt) do
;; not contain absolute paths. In particular, we look for the fragment
;; "pkgs/racket-test".

(define bad (path->bytes (build-path "pkgs" "racket-test")))
(define bad-rx (regexp-quote bad))

(define-runtime-path compiled "compiled")
(define test-files
  '("macro1_rkt.zo" "client1-1_rkt.zo" "client1-2_rkt.zo" "macro2_rkt.zo"))

(for ([file0 test-files])
  (define file (build-path compiled file0))
  (cond [(file-exists? file)
         (define code-b (file->bytes file))
         (check-false (regexp-match bad-rx code-b))]
        [else
         (printf "skipping ~e, does not exist\n" file)]))
