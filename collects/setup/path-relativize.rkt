#lang racket/base
(require racket/promise)

(provide make-relativize)

(define (make-relativize find-main-dir tag to-rel-name from-rel-name)

  ;; Historical note: this module is based on the old "plthome.ss"

  ;; The `path->main-relative' and `main-relative->path' functions
  ;; that this generates are used to store paths that are relative to
  ;; the main directory (specified by `find-main-dir'), such as in
  ;; .dep files.  This means that if the plt tree is moved, .dep files
  ;; still work.  It is generally fine if `path->main-relative' misses
  ;; some usages, as long as it works when we prepare a distribution
  ;; tree.  (If it misses, things will continue to work fine and .dep
  ;; files will contain absolute path names.)

  ;; We need to compare paths to find when something is in the plt
  ;; tree, so we explode the paths. This slower than the old way (by
  ;; a factor of 2 or so), but it's simpler and more portable.
  (define (explode-path* path)
    (explode-path (simplify-path (path->complete-path path))))

  (define (explode-path orig-path)
    (let loop ([path orig-path][rest null])
      (let-values ([(base name dir?) (split-path path)])
        (if (path? base)
          (loop base (cons name rest))
          (cons name rest)))))

  (define main-dir/
    (delay (let ([dir (find-main-dir)])
             (and dir (explode-path* dir)))))

  ;; path->main-relative* : path-or-bytes -> datum-containing-bytes-or-path
  (define (path->main-relative* path)
    (let loop ([exploded
                (explode-path*
                 (cond [(bytes? path) (bytes->path path)]
                       [(path-string? path) path]
                       [else (raise-type-error
                              to-rel-name "path, string, or bytes" path)]))]
               [main-exploded (force main-dir/)])
      (cond [(null? main-exploded)
             (cons tag (map path-element->bytes exploded))]
            [(null? exploded) path]
            [(equal? (normal-case-path (car exploded))
                     (normal-case-path (car main-exploded)))
             (loop (cdr exploded) (cdr main-exploded))]
            [else path])))

  ;; main-relative->path* : datum-containing-bytes-or-path -> path
  (define (main-relative->path* path)
    (cond [(and (pair? path)
                (eq? tag (car path))
                (or (bytes? (cdr path)) ; backward compatibility
                    (and (list? (cdr path)) (andmap bytes? (cdr path)))))
           (let ([dir (or (find-main-dir)
                          ;; No main "collects"/"doc"/whatever?  Use
                          ;; original working directory:
                          (find-system-path 'orig-dir))])
             (if (bytes? (cdr path))
               ;; backward compatibilty:
               (if (equal? (cdr path) #"")
                 dir
                 (build-path dir (bytes->path (cdr path))))
               ;; Normal mode:
               (apply build-path dir
                      (map bytes->path-element (cdr path)))))]
          [(path?   path) path]
          [(bytes?  path) (bytes->path path)]
          [(string? path) (string->path path)]
          [else (raise-type-error
                 from-rel-name
                 (format "path, string, bytes, or a list beginning with ~a" tag)
                 path)]))

  (values path->main-relative*
          main-relative->path*))
