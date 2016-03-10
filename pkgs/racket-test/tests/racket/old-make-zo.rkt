#lang racket/base
(require setup/dirs
         racket/path
         racket/file
         racket/system)

(define orig-src (collection-file-path "old-make-zo.rkt" "tests" "racket"))

(define tmp-dir (make-temporary-file "old-zo~a" 'directory))
(define src (build-path tmp-dir (file-name-from-path orig-src)))
(copy-file orig-src src)

(define (exe s)
  (if (eq? (system-type) 'windows)
      (string-append s ".exe")
      s))

(define (check auto-dir? v)
  (unless v (error "failed"))
  (let ([src (if auto-dir?
                 (let-values ([(base name dir?) (split-path src)])
                   (build-path base "compiled" name))
                 src)])
    (delete-file (path-add-suffix src #".zo"))))

(check
 #f
 (system* (build-path (find-console-bin-dir) (exe "mzc"))
          "-z"
          (path->string src)))
 
(check
 #t
 (system* (build-path (find-console-bin-dir) (exe "raco"))
          "make"
          "--no-deps"
          (path->string src)))

(delete-directory/files tmp-dir)
