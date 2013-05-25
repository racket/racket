#lang racket/base
(require racket/promise)

(provide make-relativize)

(define (make-relativize find-root-dir tag to-rel-name from-rel-name)

  ;; Historical note: this module is based on the old "plthome.ss"

  ;; The `path->relative' and `relative->path' functions that this
  ;; generates are used to store paths that are relative to the root
  ;; directory (specified by `find-root-dir'), such as in .dep files.
  ;; This means that if the racket tree is moved, .dep files still
  ;; work.  It is generally fine if `path->relative' misses some
  ;; usages, as long as it works when we prepare a distribution tree.
  ;; (If it misses, things will continue to work fine and .dep files
  ;; will contain absolute path names.)

  ;; We need to compare paths to find when something is in the racket
  ;; tree, so we explode the paths. This is slower than the old way
  ;; (by a factor of 2 or so), but it's simpler and more portable.
  (define (explode-path path)
    (let loop ([path (simplify-path (path->complete-path path))]
               [rest null])
      (let-values ([(base name dir?) (split-path path)])
        (if (path? base)
          (loop base (cons name rest))
          (cons name rest)))))

  (define exploded-root
    (delay (cond [(find-root-dir) => explode-path] [else #f])))

  ;; path->relative : path-or-bytes -> datum-containing-bytes-or-path
  (define (path->relative path0)
    (define path1
      (cond [(bytes? path0) (bytes->path path0)]
            [(path-string? path0) path0]
            [else (raise-type-error to-rel-name "path, string, or bytes"
                                    path0)]))
    (let loop ([path (explode-path path1)] [root (force exploded-root)])
      (cond [(not root) path0]
            [(null? root) (cons tag (map (lambda (pe) (datum-intern-literal (path-element->bytes pe)))
                                         path))]
            ;; Note: in some cases this returns the input path as is, which
            ;; could be a byte string -- it should be possible to return
            ;; `path1', but that messes up the xform compilation somehow, by
            ;; having #<path...> vaules written into dep files.
            [(null? path) path0]
            [(equal? (car path) (car root)) (loop (cdr path) (cdr root))]
            [else path0])))

  (define root-or-orig
    (delay (or (find-root-dir)
               ;; No main "collects"/"doc"/whatever => use the
               ;; original working directory:
               (find-system-path 'orig-dir))))

  ;; relative->path : datum-containing-bytes-or-path -> path
  (define (relative->path path)
    (cond [(and (pair? path) (eq? tag (car path))
                (and (list? (cdr path)) (andmap bytes? (cdr path))))
           (apply build-path (force root-or-orig)
                  (map bytes->path-element (cdr path)))]
          [(path?   path) path]
          [(bytes?  path) (bytes->path path)]
          [(string? path) (string->path path)]
          [else (raise-type-error
                 from-rel-name
                 (format "path, string, bytes, or a list beginning with ~a" tag)
                 path)]))

  (values path->relative relative->path))
