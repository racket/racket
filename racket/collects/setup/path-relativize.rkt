#lang racket/base
(require racket/promise)

(provide make-relativize)

(define (make-relativize find-roots-dir ; can return #f, one path, or a list of paths to try
                         tag
                         to-rel-name
                         from-rel-name)

  ;; Historical note: this module is based on the old "plthome.ss"

  ;; The `path->relative' and `relative->path' functions that this
  ;; generates are used to store paths that are relative to the root
  ;; directory (specified by `find-root-dir'), such as in .dep files.
  ;; This means that if the racket tree is moved, .dep files still
  ;; work.  It is generally fine if `path->relative' misses some
  ;; usages, as long as it works when we prepare a distribution tree.
  ;; (If it misses, things will continue to work fine and .dep files
  ;; will contain absolute path names.)

  ;; If `find-roots-dir` returns a list of roots, then a path is
  ;; converted as relative for the first root path where that's
  ;; possible, and a relative is converted back to a path for the
  ;; first one that exists (as a file, directory, or link) or
  ;; the first one if none exists. An empty roots list and a #f
  ;; from `find-root-dir` are treated as the same.

  ;; We need to compare paths to find when something is in the racket
  ;; tree, so we explode the paths. This is slower than the old way
  ;; (by a factor of 2 or so), but it's simpler and more portable.
  (define (explode-path* path)
    (explode-path (simplify-path (path->complete-path path))))

  (define exploded-roots
    (delay (cond [(find-roots-dir)
                  => (lambda (p)
                       (if (list? p)
                           (map explode-path* p)
                           (list (explode-path* p))))]
                 [else '()])))

  ;; path->relative : path-or-bytes -> datum-containing-bytes-or-path
  (define (path->relative path0)
    (define path1
      (cond [(bytes? path0) (bytes->path path0)]
            [(path-string? path0) path0]
            [else (raise-argument-error to-rel-name
                                        "(or/c path-string? bytes?)"
                                        path0)]))
    (define orig-path (explode-path* path1))
    (define roots (force exploded-roots))
    (let loop ([path orig-path]
               [root (and (pair? roots) (car roots))]
               [roots (if (pair? roots) (cdr roots) '())])
      (cond [(not root) path0]
            [(null? root) (cons tag (map (lambda (pe)
                                           (datum-intern-literal
                                            (path-element->bytes pe)))
                                         path))]
            ;; Note: in some cases this returns the input path as is, which
            ;; could be a byte string -- it should be possible to return
            ;; `path1', but that messes up the xform compilation somehow, by
            ;; having #<path...> values written into dep files.
            [(null? path)
             (cond
               [(null? roots) path0]
               [else (loop orig-path (car roots) (cdr roots))])]
            [(equal? (normal-case-path (car path)) (normal-case-path (car root)))
	     (loop (cdr path) (cdr root) roots)]
            [else
             (cond
               [(null? roots) path0]
               [else (loop orig-path (car roots) (cdr roots))])])))

  (define roots-or-orig
    (delay (or (let ([r (find-roots-dir)])
                 (and r
                      (if (list? r)
                          (and (pair? r) r)
                          (list r))))
               ;; No main "collects"/"doc"/whatever => use the
               ;; original working directory:
               (list (find-system-path 'orig-dir)))))

  ;; relative->path : datum-containing-bytes-or-path -> path
  (define (relative->path path)
    (cond [(and (pair? path) (eq? tag (car path))
                (and (list? (cdr path)) (andmap bytes? (cdr path))))
           (define roots (force roots-or-orig))
           (define elems (map bytes->path-element (cdr path)))
           (define default-p (apply build-path (car roots) elems))
           (define (exists? p) (file-or-directory-type p))
           (cond
             [(or (null? (cdr roots))
                  (exists? default-p))
              default-p]
             [else
              (let loop ([roots (cdr roots)])
                (cond
                  [(null? roots) default-p]
                  [else
                   (define p (apply build-path (car roots) elems))
                   (or (and (exists? p)
                            p)
                       (loop (cdr roots)))]))])]
          [(path?   path) path]
          [(bytes?  path) (bytes->path path)]
          [(string? path) (string->path path)]
          [else (raise-argument-error
                 from-rel-name
                 (format "(or/c path? bytes? (cons '~a (non-empty-listof bytes?)))" tag)
                 path)]))

  (values path->relative relative->path))
