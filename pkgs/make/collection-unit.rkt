#lang mzscheme

(require mzlib/unit
         mzlib/list
         mzlib/file
         "collection-sig.rkt"
         "make-sig.rkt"
         compiler/sig
         dynext/file-sig)

(provide make:collection@)

(define-unit make:collection@
  (import make^
          dynext:file^
          compiler^)
  (export make:collection^)

  (define (make-collection collection-name collection-files argv)
    (printf "building collection ~a: ~a\n" collection-name collection-files)
    (let* ([zo-compiler #f]
           [src-dir (current-directory)]
           [sses (sort collection-files
                       (lambda (a b)
                         (string-ci<? (path->string a) (path->string b))))]
           [zos (map (lambda (ss)
                       (build-path "compiled" (append-zo-suffix ss)))
		     sses)]
           [ss->zo-list
            (map (lambda (ss zo)
                   `(,zo (,ss)
                     ,(lambda ()
                        (unless zo-compiler
                          (set! zo-compiler (compile-zos #f)))
                        (zo-compiler (list ss) "compiled"))))
                 sses zos)])
      (unless (directory-exists? "compiled") (make-directory "compiled"))
      (make/proc (append `(("zo" ,zos)) ss->zo-list) argv))))
