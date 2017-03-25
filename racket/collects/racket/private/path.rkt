(module path '#%kernel
  (#%require "qq-and-or.rkt" "cond.rkt" "define-et-al.rkt")

  (#%provide path-string?
             normal-case-path
             path-replace-extension
             path-add-extension
             reroot-path)

  (define-values (path-string?)
    (lambda (s)
      (or (path? s) 
          (and (string? s)
               (or (relative-path? s)
                   (absolute-path? s))))))

  (define-values (bsbs) (string #\u5C #\u5C))

  (define-values (normal-case-path)
    (lambda (s)
      (unless (or (path-for-some-system? s)
                  (path-string? s))
        (raise-argument-error 'normal-path-case "(or/c path-for-some-system? path-string?)" s))
      (cond
       [(if (path-for-some-system? s)
            (eq? (path-convention-type s) 'windows)
            (eq? (system-type) 'windows))
        (let ([str (if (string? s) s (bytes->string/locale (path->bytes s)))])
          (if (regexp-match? #rx"^[\u5C][\u5C][?][\u5C]" str)
              (if (string? s)
                  (string->path s)
                  s)
              (let ([s (string-locale-downcase str)])
                (bytes->path 
                 (string->bytes/locale
                  (regexp-replace* #rx"/" 
                                   (if (regexp-match? #rx"[/\u5C][. ]+[/\u5C]*$" s)
                                       ;; Just "." or ".." in last path element - don't remove
                                       s
                                       (regexp-replace* #rx"\u5B .\u5D+([/\u5C]*)$" s "\u005C1"))
                                   bsbs))
                 'windows))))]
       [(string? s) (string->path s)]
       [else s])))
  
  ;; ----------------------------------------
  
  (define-values (check-extension-call)
    (lambda (s sfx who sep trust-sep?)
      (let-values ([(err-msg err-index)
                    (cond
                     [(not (or (path-for-some-system? s) (path-string? s)))
                      (values "(or/c path-for-some-system? path-string?)" 0)]
                     [(not (or (string? sfx) (bytes? sfx)))
                      (values "(or/c string? bytes?)" 1)]
                     [(not (or trust-sep? (string? sep) (bytes? sep)))
                      (values "(or/c string? bytes?)" 2)]
                     [else
                      (values #f #f)])])
        (when err-msg
          (if trust-sep?
            (raise-argument-error who err-msg err-index s sfx)
            (raise-argument-error who err-msg err-index s sfx sep))))
      (let-values ([(base name dir?) (split-path s)])
        (when (not base)
          (raise-mismatch-error who "cannot add an extension to a root path: " s))
        (values base name))))

  (define-values (path-adjust-extension)
    (lambda (name sep rest-bytes s sfx trust-sep?)
      (let-values ([(base name) (check-extension-call s sfx name sep trust-sep?)])
        (-define bs (path-element->bytes name))
        (-define finish
          (lambda (i sep i2)
            (bytes->path-element
             (bytes-append
              (subbytes bs 0 i)
              (if (string? sep)
                  (string->bytes/locale sep (char->integer #\?))
                  sep)
              (rest-bytes bs i2)
              (if (string? sfx)
                  (string->bytes/locale sfx (char->integer #\?))
                  sfx))
             (if (path-for-some-system? s)
                 (path-convention-type s)
                 (system-path-convention-type)))))
        (let ([new-name (letrec-values ([(loop)
                                         (lambda (i)
                                           (if (zero? i)
                                               (finish (bytes-length bs) #"" (bytes-length bs))
                                               (let-values ([(i) (sub1 i)])
                                                 (if (and (not (zero? i))
                                                          (eq? (char->integer #\.) (bytes-ref bs i)))
                                                     (finish i sep (add1 i))
                                                     (loop i)))))])
                                       (loop (bytes-length bs)))])
          (if (path-for-some-system? base)
              (build-path base new-name)
              new-name)))))

  (define-values (path-replace-extension)
    (lambda (s sfx)
      (path-adjust-extension 'path-replace-extension #"" (lambda (bs i) #"") s sfx #t)))

  (define-values (path-add-extension)
    (case-lambda
     [(s sfx)
      (path-adjust-extension 'path-add-extension #"_" subbytes s sfx #t)]
     [(s sfx sep)
      (path-adjust-extension 'path-add-extension sep subbytes s sfx #f)]))

  ;; ----------------------------------------
  
  

  (define-values (reroot-path)
    (lambda (p root)
      (unless (or (path-string? p) (path-for-some-system? p))
        (raise-argument-error 'reroot-path "(or/c path-string? path-for-some-system?)" 0 p root))
      (unless (or (path-string? root) (path-for-some-system? root))
        (raise-argument-error 'reroot-path "(or/c path-string? path-for-some-system?)" 1 p root))
      (-define conv (if (path-for-some-system? p)
                        (path-convention-type p)
                        (system-path-convention-type)))
      (unless (or (complete-path? p)
                  (eq? (system-path-convention-type) conv))
        (raise-arguments-error 'reroot-path
                               "path is not complete and not the platform's convention"
                               "path" p
                               "platform convention type" (system-path-convention-type)))
      (unless (eq? (if (path-for-some-system? root)
                       (path-convention-type root)
                       (system-path-convention-type))
                   conv)
        (raise-arguments-error 'reroot-path
                               "given paths use different conventions"
                               "path" p
                               "root path" root))
      (-define c-p (normal-case-path (cleanse-path (if (complete-path? p)
                                                       p
                                                       (path->complete-path p)))))
      (-define bstr (path->bytes c-p))
      (cond 
       [(eq? conv 'unix) 
        (if (bytes=? bstr #"/")
            (if (path-for-some-system? root)
                root
                (string->path root))
            (build-path root (bytes->path (subbytes (path->bytes c-p) 1) conv)))]
       [(eq? conv 'windows)
        (build-path
         root
         (bytes->path
          (cond
           ((regexp-match? #rx"^\\\\\\\\[?]\\\\[a-z]:" bstr)
            (bytes-append #"\\\\?\\REL\\" (subbytes bstr 4 5) #"\\" (subbytes bstr 6)))
           ((regexp-match? #rx"^\\\\\\\\[?]\\\\UNC\\\\" bstr)
            (bytes-append #"\\\\?\\REL\\" (subbytes bstr 4)))
           ((regexp-match? #rx"^\\\\\\\\[?]\\\\UNC\\\\" bstr)
            (bytes-append #"\\\\?\\REL\\" (subbytes bstr 4)))
           ((regexp-match? #rx"^\\\\\\\\" bstr)
            (bytes-append #"UNC\\" (subbytes bstr 2)))
           ((regexp-match? #rx"^[a-z]:" bstr)
            (bytes-append (subbytes bstr 0 1) (subbytes bstr 2))))
          conv))]))))
