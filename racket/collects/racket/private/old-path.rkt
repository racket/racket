;; Old variants of `path-replace-extension` and
;; `path-add-extension` that do the wrong thing with
;; file names that start "."
(module path '#%kernel
  (#%require "qq-and-or.rkt" "define-et-al.rkt")

  (#%provide path-replace-suffix
             path-add-suffix)
  
  (define-values (path-string?)
    (lambda (s)
      (or (path? s) 
          (and (string? s)
               (or (relative-path? s)
                   (absolute-path? s))))))
  
  (define-values (check-suffix-call)
    (lambda (s sfx who)
      (unless (or (path-for-some-system? s)
                  (path-string? s))
        (raise-argument-error who "(or/c path-for-some-system? path-string?)" 0 s sfx))
      (unless (or (string? sfx) (bytes? sfx))
        (raise-argument-error who "(or/c string? bytes?)" 1 s sfx))
      (let-values ([(base name dir?) (split-path s)])
        (when (not base)
          (raise-mismatch-error who "cannot add a suffix to a root path: " s))
        (values base name))))

  (define-values (path-adjust-suffix)
    (lambda (name sep rest-bytes s sfx)
      (let-values ([(base name) (check-suffix-call s sfx name)])
        (-define bs (path-element->bytes name))
        (-define finish
          (lambda (i sep i2)
            (bytes->path-element
             (let ([res (bytes-append
                         (subbytes bs 0 i)
                         sep
                         (rest-bytes bs i2)
                         (if (string? sfx)
                             (string->bytes/locale sfx (char->integer #\?))
                             sfx))])
               (if (zero? (bytes-length res))
                   (raise-arguments-error 'path-replace-suffix
                                          "removing suffix makes path element empty"
                                          "given path" s)
                   res))
             (if (path-for-some-system? s)
                 (path-convention-type s)
                 (system-path-convention-type)))))
        (let ([new-name (letrec-values ([(loop)
                                         (lambda (i)
                                           (if (zero? i)
                                               (finish (bytes-length bs) #"" (bytes-length bs))
                                               (let-values ([(i) (sub1 i)])
                                                 (if (eq? (char->integer #\.) (bytes-ref bs i))
                                                     (finish i sep (add1 i))
                                                     (loop i)))))])
                          (loop (bytes-length bs)))])
          (if (path-for-some-system? base)
              (build-path base new-name)
              new-name)))))

  (define-values (path-replace-suffix)
    (lambda (s sfx)
      (path-adjust-suffix 'path-replace-suffix #"" (lambda (bs i) #"") s sfx)))

  (define-values (path-add-suffix)
    (lambda (s sfx)
      (path-adjust-suffix 'path-add-suffix #"_" subbytes s sfx))))
