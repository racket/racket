#lang racket/base
(require racket/path racket/system)

(define from (vector-ref (current-command-line-arguments) 0))
(define to
  (path->string (simplify-path (build-path (collection-path "racket")
                                           'up 'up "lib/")
                               #f)))

(define libs
  '("libffi.5"
    "libgio-2.0.0"
    "libgmodule-2.0.0"
    "libgthread-2.0.0"
    "libglib-2.0.0"
    "libgobject-2.0.0"
    "libintl.8"
    "libpango-1.0.0"
    "libpangocairo-1.0.0"
    "libcairo.2"
    "libpixman-1.0"
    "libpng15.15"
    "libgmp.10"
    "libmpfr.4"
    "libjpeg.62"))

(define (fixup p p-new)
  (printf "Fixing ~s\n" p-new)
  (system (format "install_name_tool -id ~a ~a" (file-name-from-path p-new) p-new))
  (for-each (lambda (s)
              (system (format "install_name_tool -change ~a @loader_path/~a ~a" 
                              (format "~a/~a.dylib" from s)
                              (format "~a.dylib" s)
                              p-new)))
            libs))

(define (install p)
  (let* ([p (format "~a.dylib" p)]
         [dest (string-append to p)])
    (when (file-exists? dest) (delete-file dest))
    (copy-file (build-path from p) dest)
    (fixup p dest)))

(for-each install libs)
