#lang racket/base
(require setup/dirs)

(provide so-spec? so-find)

(define (so-spec? p)
  (and (list? p)
       (or (= 2 (length p))
           (= 3 (length p)))
       (eq? 'so (car p))
       (string? (cadr p))
       (or (= 2 (length p))
           (let ([s (caddr p)])
             (define (vers? s) (or (not s) (string? s)))
             (or (vers? s)
                 (and (list? s) (andmap vers? s)))))))

(define (path-extra-suffix p sfx)
  ;; Library names may have a version number preceded
  ;; by a ".", which looks like a suffix, so add the
  ;; shared-library suffix using plain-old bytes append:
  (let-values ([(base name dir?) (split-path p)])
    (let ([name (bytes->path (bytes-append (path->bytes name) sfx))])
      (if (path? base)
          (build-path base name)
          name))))

(define (so-find p)
  (let ([verss (cons 'no-suffix
                     (if (= (length p) 3)
                         (let ([s (caddr p)])
                           (if (list? s) s (list s)))
                         '(#f)))]
        [suffix-before-version? (not (equal? (system-type 'so-suffix)
                                             #".dylib"))])
    (ormap (lambda (dir)
             (ormap (lambda (vers)
                      (let ([f (if (eq? vers 'no-suffix)
                                   (cadr p)
                                   (path-extra-suffix (cadr p)
                                                      (if (string? vers)
                                                          (if suffix-before-version?
                                                              (bytes-append (system-type 'so-suffix)
                                                                            #"."
                                                                            (string->bytes/utf-8 vers))
                                                              (bytes-append #"."
                                                                            (string->bytes/utf-8 vers)
                                                                            (system-type 'so-suffix)))
                                                          (system-type 'so-suffix))))])
                        (let ([p (build-path dir f)])
                          (and (or (file-exists? p)
                                   (directory-exists? p))
                               p))))
                    verss))
           (get-lib-search-dirs))))
