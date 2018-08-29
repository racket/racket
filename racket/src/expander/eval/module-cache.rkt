#lang racket/base
(require racket/private/place-local)

;; The module cache lets us avoid reloading ".zo" files when
;; we have the relevant data handy in memory. The "eval/module.rkt"
;; module installs entries, and the default load handler in
;; "boot/load-handler.rkt" consults the cache.

(provide make-module-cache-key
         module-cache-set!
         module-cache-ref
         module-cache-place-init!)

(define-place-local module-cache (make-weak-hasheq))

(define (module-cache-place-init!)
  (set! module-cache (make-weak-hasheq)))

(define (make-module-cache-key hash-code)
  ;; The result is preserved to retain the cache entry, and
  ;; found in `module-cache-ref` by `equal?` comparsion.
  ;; The current load-relative directory is part of the
  ;; key because the bytecode form can have bulk bindings
  ;; in syntax objects that refer to `require`s that are
  ;; relative to the enclosing module, and that part of
  ;; the syntax object is unmarshaled once and used for
  ;; all instances of the module.
  (and hash-code
       ;; Encode as a symbol so we can use an eq?-based hash table
       ;; (i.e., explot the low-level lock on the symbol table)
       (string->symbol (format "~s" (list hash-code (current-load-relative-directory))))))

(define (module-cache-set! key proc)
  (hash-set! module-cache key (make-ephemeron key proc)))

(define (module-cache-ref key)
  (define e (hash-ref module-cache key #f))
  (and e (ephemeron-value e)))
