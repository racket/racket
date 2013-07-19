#lang scheme/base

(require (prefix-in planet: planet/config) scheme/path scheme/list)

;; `library-roots' is an alist of root-path, and the number of levels
;; below that which indicates a "library".  This is hard-wired now to
;; the different roots, and should probably be improved at some point.
;; `path->library-root' takes in a path and returns a path to the root
;; of its library, defined by the above.  (Assumes that none of the
;; roots is a subdirectory of another.)

(provide library-roots path->library-root)

(define library-roots
  `(,@(map (lambda (p) (cons p 1)) (current-library-collection-paths))
    ,(cons (planet:CACHE-DIR) 4)
    ;; add planet links, each as a root (if there is a change in
    ;; the format, this will just ignore these paths, but these
    ;; collections will throw an error in setup-plt)
    ,@(with-handlers ([exn? (lambda (e)
                              (printf "WARNING: bad planet links at ~a:\n ~a"
                                      (planet:HARD-LINK-FILE) (exn-message e))
                              '())])
        (if (not (file-exists? (planet:HARD-LINK-FILE)))
          '()
          (with-input-from-file (planet:HARD-LINK-FILE)
            (lambda ()
              (let loop ([r '()])
                (let ([x (read)])
                  (if (eof-object? x)
                    (reverse r)
                    (let ([x (and (list? x) (= 7 (length x)) (list-ref x 4))])
                      (loop (if (bytes? x)
                              (cons (cons (simple-form-path (bytes->path x)) 0) r)
                              r))))))))))))

(define path->library-root
  (let ([t #f])
    (define (init-table)
      (set! t (make-hash))
      (for ([x (in-list library-roots)])
        (hash-set! t (reverse (explode-path (car x))) (cdr x))))
    (lambda (path)
      (unless (complete-path? path)
        (raise-type-error 'path->library-root "complete-path" path))
      (unless t (init-table))
      (let loop ([rpath (reverse (explode-path (simple-form-path path)))]
                 [subdir '()])
        (let ([x (hash-ref t rpath #f)])
          (cond [(and x ((length subdir) . >= . x))
                 (apply build-path (append (reverse rpath) (take subdir x)))]
                [(or x (null? rpath)) #f]
                [else (loop (cdr rpath) (cons (car rpath) subdir))]))))))
