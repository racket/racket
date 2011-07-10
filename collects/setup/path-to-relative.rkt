#lang racket/base

;; intended for use in racket/contract, so don't try to add contracts!
;; (and try to generally minimize dependencies)

(require "dirs.rkt" "path-relativize.rkt"
         (only-in planet/config [CACHE-DIR find-planet-dir]))

(provide make-path->relative-string
         path->relative-string/setup
         path->relative-string/library)

(define (make-path->relative-string
         dirs [default (lambda (x) (if (path? x) (path->string x) x))])
  (unless (and (list? dirs)
               (andmap (lambda (x)
                         (and (pair? x)
                              (and (procedure? (car x))
                                   (procedure-arity-includes? (car x) 0)
                                   (string? (cdr x)))))
                       dirs))
    (raise-type-error 'make-path->relative-string
      "a list of thunk and string pairs" dirs))
  (define prefixes (map cdr dirs))
  (define path->relatives
    (map (lambda (x)
           (let-values ([(path->relative _)
                         (make-relativize (car x) '_ 'path->relative '_)])
             path->relative))
         dirs))
  (define (path->relative-string path [default default])
    (unless (path-string? path)
      (raise-type-error 'path->relative-string "path or string" path))
    (or (and (complete-path? path)
             (for/or ([prefix         (in-list prefixes)]
                      [path->relative (in-list path->relatives)])
               (define exploded (path->relative path))
               (and (pair? exploded)
                    (let* ([r (cdr exploded)]
                           ;; note: use "/"s, to get paths as in `require's
                           [r (map (lambda (p) (list #"/" p)) r)]
                           [r (apply bytes-append (cdr (apply append r)))])
                      (string-append prefix (bytes->string/locale r))))))
        (if (procedure? default) (default path) default)))
  path->relative-string)

(define path->relative-string/library
  (make-path->relative-string
   (list (cons find-collects-dir      "<collects>/")
         (cons find-user-collects-dir "<user-collects>/")
         (cons find-planet-dir        "<planet>/"))))

(define path->relative-string/setup
  (make-path->relative-string
   (list (cons find-collects-dir      "")
         (cons find-user-collects-dir "<user>/")
         (cons find-planet-dir        "<planet>/"))))
