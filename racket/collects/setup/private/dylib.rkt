#lang racket/base
(require compiler/private/mach-o
         setup/dirs)

(provide adjust-dylib-path/install
         adjust-dylib-path/uninstall)

(define (adjust-dylib-path p adjust)
  (cond
   [(directory-exists? p)
    ;; Find Mach-O files in the framework and adjust them:
    (for ([f (in-list (directory-list p #:build? #t))])
      (adjust-dylib-path f adjust))]
   [(file-exists? p)
    (define magic (call-with-input-file*
                   p
                   (lambda (i)
                     (define bstr (read-bytes 4 i))
                     (and (bytes? bstr)
                          (= 4 (bytes-length bstr))
                          (integer-bytes->integer bstr #f)))))
    (case magic
      [(#xfeedface #xfeedfacf)
       ;; Found a Mach-o file; get a list of all referenced dylibs,
       ;; and adjust each one:
       (define libs (get/set-dylib-path p #rx"." #f))
       (define-values (base name dir?) (split-path p))
       (for ([lib (in-list libs)])
         (define new-lib (adjust lib base))
         (when new-lib
           (get/set-dylib-path p (regexp-quote lib) new-lib)))])]))

;; ----------------------------------------

(define (adjust-dylib-path/install p)
  (adjust-dylib-path p relative-to-absolute))

(define (relative-to-absolute ref dir)
  (and (regexp-match? #rx#"^@loader_path/" ref)
       (let ()
         (define p (bytes->path (subbytes ref 13)))
         (and (not (file-exists? (build-path dir p)))
              (for/or ([dir (in-list (get-lib-search-dirs))])
                (define full-p (build-path dir p))
                (and (file-exists? full-p)
                     (path->bytes full-p)))))))

;; ----------------------------------------

(define (adjust-dylib-path/uninstall p)
  (adjust-dylib-path p absolute-to-relative))

(define (absolute-to-relative ref in-dir)
  (for/or ([dir (in-list (get-lib-search-dirs))])
    (define dir-bstr (path->bytes dir))
    (and (regexp-match? (bytes-append #"^" (regexp-quote dir-bstr))
                        ref)
         (bytes-append #"@loader_path"
                       (subbytes ref (bytes-length dir-bstr))))))
