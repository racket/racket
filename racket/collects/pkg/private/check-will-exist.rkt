#lang racket/base
(require compiler/compilation-path
         setup/collection-search
         racket/list
         racket/string
         "get-info.rkt")

(provide check-found-module-will-exist)

;; Check whether the given path exists (as the resolved form of the
;; given module path, which must be a normalized 'lib path).
;; Make sure it will continue to exist after `raco setup` is run
;; -- that is, that it's not a bytecode file without source so
;; that the file will be deleted.
;; If `f` refers to a bytecode file that will be deleted, look for
;; another resolution of the module path that will stick around.
(define (check-found-module-will-exist f mp metadata-ns)
  (define v (check-one-found-module-will-exist f mp metadata-ns #:deleted-result 'going))
  (cond
   [(eq? v 'going) (find-module/slow-way mp metadata-ns)]
   [else v]))

;; Check one particular path, with capability to report that a
;; byetcode file won't count because it should be deleted.
(define (check-one-found-module-will-exist f mp metadata-ns
                                           #:deleted-result [deleted-result #f])
  (define v
    (or (file-exists? f)
        (file-exists? (path-replace-suffix f #".ss"))
        (and (or (file-exists? (get-compilation-bytecode-file f))
                 (file-exists? (get-compilation-bytecode-file (path-replace-suffix f #".ss"))))
             ;; found bytecode; make sure it won't be deleted by `raco setup`
             (or (bytecode-will-stick-around? f mp metadata-ns)
                 deleted-result))))
  (if (eq? v #t)
      f
      v))

;; Given that a bytecode's source file was not around, check whether
;; the bytecode will stick around as a result of an
;; 'assume-virtual-sources in an "info.rkt" file.
(define (bytecode-will-stick-around? f mp metadata-ns)
  (unless (and (pair? mp)
               (eq? 'lib (car mp))
               (null? (cddr mp)))
    (error 'bytecode-will-stick-around? "expected a normalized 'lib path"))
  (define cols (drop-right (string-split (cadr mp) "/")
                           1))
  (define-values (dir name dir?) (split-path f))
  (let loop ([cols cols] [dir dir])
    (cond
     [(null? cols) #f]
     [else
      (define info (get-pkg-info dir metadata-ns))
      (or (and info
               (info 'assume-virtual-sources (lambda () #f)))
          (let-values ([(base name dir?) (split-path dir)])
            (loop (cdr cols) base)))])))

;; We tried a fast way to find a module as existing, but it
;; didn't work, because the one will found will go away when
;; `raco setup` is run, and so we don't want to count that
;; one. Search manually. The given `mp` must be a normalized
;; 'lib path.
(define (find-module/slow-way mp metadata-ns)
  (collection-search mp
                     #:combine (lambda (r f)
                                 (when r (log-error "oops ~s" r))
                                 (check-one-found-module-will-exist f mp metadata-ns))
                     #:break? (lambda (r) r)))
