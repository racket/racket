
;; Used by Mac OS install to clear out old frameworks versions,
;; leaving other variants for the current version intact.

;; written in #%kernel because it's loaded with -c (ie, no compiled files)

(module collects-path '#%kernel
  (let-values ([(fw-dir) (vector-ref (current-command-line-arguments) 0)])
    (let-values ([(versions-dir) (build-path fw-dir "Versions")])
      (letrec-values ([(clean-files)
                       (lambda (l)
                         (if (null? l)
                             (void)
                             (if (starts-this-version? (path->string (car l)))
                                 (clean-files (cdr l))
                                 (begin
                                   (delete-all versions-dir (car l))
                                   (clean-files (cdr l))))))]
                      [(delete-all)
                       (lambda (base p)
                         (let-values ([(p) (build-path base p)])
                           (if (file-exists? p)
                               (delete-file p)
                               (if (directory-exists? p)
                                   (begin
                                     (delete-all* p (directory-list p))
                                     (delete-directory p))
                                   (void)))))]
                      [(delete-all*)
                       (lambda (base l)
                         (if (null? l)
                             (void)
                             (begin
                               (delete-all base (car l))
                               (delete-all* base (cdr l)))))]
                      [(starts-this-version?)
                       (lambda (s)
                         (let-values ([(len) (string-length (version))]
                                      [(s-len) (string-length s)])
                           (if (s-len . >= . len)
                               (if (equal? (version) (substring s 0 len))
                                   (if (= s-len len)
                                       #t
                                       (if (equal? #\_ (string-ref s len))
                                           #t
                                           #f))
                                   #f)
                               #f)))])
        (if (directory-exists? versions-dir)
            (clean-files (directory-list versions-dir))
            (void))))))
