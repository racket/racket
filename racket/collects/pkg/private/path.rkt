#lang racket/base
(require racket/path
         racket/file
         racket/list
         racket/function)

(provide (all-defined-out))

(define (path->bytes* pkg)
  (cond
    [(path? pkg)
     (path->bytes pkg)]
    [(string? pkg)
     (path->bytes (string->path pkg))]
    [(bytes? pkg)
     pkg]))

(define (directory-path-no-slash pkg)
  (bytes->path (regexp-replace* #rx#"/$" (path->bytes* pkg) #"")))

(define (directory-list* d)
  (append-map
   (λ (pp)
     (define p (build-path d pp))
     (if (directory-exists? p)
       (map (curry build-path pp)
            (directory-list* p))
       (list pp)))
   (directory-list d)))

(define (simple-form-path* p)
  (path->string (simple-form-path p)))

(define (pretty-module-path mod)
  (if (and (list? mod)
           (= 2 (length mod))
           (eq? (car mod) 'lib)
           (regexp-match? #rx"[.]rkt$" (cadr mod)))
      (string->symbol (regexp-replace #rx"[.]rkt$" (cadr mod) ""))
      mod))

(define (lift-directory-content pkg-dir path)
  (define orig-sub (let ([s (car path)])
                     (if (string? s)
                         (string->path s)
                         s)))
  ;; Delete everything except `orig-sub`:
  (for ([f (in-list (directory-list pkg-dir))])
    (unless (equal? f orig-sub)
      (delete-directory/files (build-path pkg-dir f))))
  ;; Get list of files and directories to move:
  (define sub-l (directory-list (apply build-path pkg-dir path)))
  ;; Make sure `sub` doesn't match a name we want to move here:
  (define sub
    (let loop ([sub orig-sub] [i 0])
      (cond
       [(member sub sub-l)
        ;; pick a new name:
        (loop (string->path (format "sub~a" i)) (add1 i))]
       [(not (equal? sub orig-sub))
        (rename-file-or-directory (build-path pkg-dir orig-sub)
                                  (build-path pkg-dir sub))
        sub]
       [else sub])))
  ;; Move content of `sub` out:
  (define sub-path (apply build-path (cons sub (cdr path))))
  (for ([f (in-list sub-l)])
    (rename-file-or-directory (build-path pkg-dir sub-path f)
                              (build-path pkg-dir f)))
  ;; Remove directory that we moved files out of:
  (delete-directory/files (build-path pkg-dir sub)))

(define (remove-extra-directory-layer pkg-dir)
  ;; Treat a single directory produced in `pkg-dir`
  ;; as having the content of the package, instead of
  ;; being included itself in the package content.
  (define l (directory-list pkg-dir))
  (when (= 1 (length l))
    (define orig-sub (car l))
    (when (directory-exists? (build-path pkg-dir orig-sub))
      (lift-directory-content pkg-dir (list orig-sub)))))
