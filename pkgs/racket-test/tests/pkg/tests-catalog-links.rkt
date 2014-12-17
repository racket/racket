#lang racket/base
(require rackunit
         racket/file
         racket/format
         "util.rkt"
         "shelly.rkt")

(this-test-is-run-by-the-main-test)

(define (make-cat-entry #:name name
                        #:source source
                        #:deps [deps null]
                        #:checksum [checksum "0"])
  `#hash((name . ,name)
         (checksum . ,checksum)
         (source . ,source)
         (author . "test@racket-lang.org")
         (description . ,(string-upcase name))
         (tags . ())
         (dependencies . ,deps)
         (modules . ())))

(pkg-tests
 (define dir (make-temporary-file "~a-tree" 'directory))

 (with-fake-root
  (shelly-wind
   (set-file (build-path dir "test-pkg-1" "main.rkt")
             "#lang racket/base 'one")
   (define (make-one-cat-entry checksum)
     (make-cat-entry #:name "test-pkg-1"
                     #:source "../test-pkg-1" ; <<<< not a link
                     #:checksum checksum))
   (set-file (build-path dir "catalog" "pkg" "test-pkg-1")
             (~s (make-one-cat-entry "0")))

   (set-file (build-path dir "test-pkg-2" "main.rkt")
             "#lang racket/base (require test-pkg-1) 'two")
   (set-file (build-path dir "test-pkg-2" "info.rkt")
             "#lang info (define deps '(\"test-pkg-1\"))")
   (set-file (build-path dir "catalog" "pkg" "test-pkg-2")
             (~s (make-cat-entry
                  #:name "test-pkg-2"
                  #:deps '("test-pkg-1")
                  #:source "../test-pkg-2?type=link"))) ; <<<< a link
   
   (set-file (build-path dir "test-pkg-3" "main.rkt")
             "#lang racket/base (require test-pkg-2) 'three")
   (set-file (build-path dir "test-pkg-3" "info.rkt")
             "#lang info (define deps '(\"test-pkg-2\"))")
   (define (make-three-cat-entry checksum)
     (make-cat-entry #:name "test-pkg-3"
                     #:deps '("test-pkg-2")
                     #:source "../test-pkg-3" ; <<<< not a link
                     #:checksum checksum))
   (set-file (build-path dir "catalog" "pkg" "test-pkg-3")
             (~s (make-three-cat-entry "0")))
   
   $ (~a "raco pkg install --auto --catalog file://" (build-path dir "catalog") " test-pkg-3")
   $ "racket -l test-pkg-1" =stdout> "'one\n"
   $ "racket -l test-pkg-2" =stdout> "'one\n'two\n"
   $ "racket -l test-pkg-3" =stdout> "'one\n'two\n'three\n"

   ;; Change 2, change is immediately visible:
   (set-file (build-path dir "test-pkg-2" "main.rkt")
             "#lang racket/base (require test-pkg-1) 'TWO")
   $ "racket -l test-pkg-3" =stdout> "'one\n'TWO\n'three\n"

   ;; Change 1 and 3, changes are not immediately visible, since not linked:
   (set-file (build-path dir "test-pkg-1" "main.rkt")
             "#lang racket/base 'ONE")
   (set-file (build-path dir "test-pkg-3" "main.rkt")
             "#lang racket/base (require test-pkg-2) 'THREE")
   (set-file (build-path dir "catalog" "pkg" "test-pkg-1")
             (~s (make-one-cat-entry "1")))
   (set-file (build-path dir "catalog" "pkg" "test-pkg-3")
             (~s (make-three-cat-entry "1")))
   $ "racket -l test-pkg-3" =stdout> "'one\n'TWO\n'three\n"
   
   $ (~a "raco pkg update --auto --catalog file://" (build-path dir "catalog") " test-pkg-3")
   $ "racket -l test-pkg-3" =stdout> "'ONE\n'TWO\n'THREE\n"

   (finally
    (delete-directory/files dir)))))
