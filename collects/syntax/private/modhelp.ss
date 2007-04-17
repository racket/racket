
(module modhelp mzscheme
  (require (lib "string.ss"))

  (provide explode-relpath-string
           module-path-v?
           module-path-v-string?)

  (define (explode-relpath-string p)
    (map (lambda (p)
           (cond [(assoc p '((#"." . same) (#".." . up))) => cdr]
                 [else (bytes->path p)]))
         (regexp-split #rx#"/+" (string->bytes/utf-8 p))))

  (define (module-path-v-string? v)
    (and (regexp-match? #rx"^[-a-zA-Z0-9./]+$" v)
         (not (regexp-match? #rx"^/" v))
         (not (regexp-match? #rx"/$" v))))

  (define (module-path-v? v)
    (cond [(path? v) #t]
          [(string? v) (module-path-v-string? v)]
          [(pair? v)
           (case (car v)
             [(file) (and (pair? (cdr v))
                          (path-string? (cadr v))
                          (null? (cddr v)))]
             [(lib) (and (pair? (cdr v))
                         (list? (cdr v))
                         (andmap module-path-v-string? (cdr v)))]
             [(planet) #t]
             [else #f])]
          [else #f])))
