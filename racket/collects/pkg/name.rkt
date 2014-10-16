#lang racket/base
(require racket/list
         racket/contract
         racket/format
         racket/string
         net/url)

(provide
 package-source-format?
 (contract-out
  [package-source->name+type (->* (string? (or/c #f package-source-format?))
                                  (#:complain (-> string? string? any)
                                              #:must-infer-name? boolean?
                                              #:link-dirs? boolean?)
                                  (values (or/c #f string?) (or/c #f package-source-format?)))]
  [package-source->name (->* (string?)
                             ((or/c #f package-source-format?))
                             (or/c #f string?))]
  [package-source->path (->* (string?)
                             ((or/c #f 'file 'dir 'link 'static-link))
                             path?)]))

(define rx:package-name #rx"^[-_a-zA-Z0-9]+$")
(define rx:archive #rx"[.](plt|zip|tar|tgz|tar[.]gz)$")

(define package-source-format?
  (or/c 'name 'file 'dir 'github 'file-url 'dir-url 'link 'static-link))

(define (validate-name name complain inferred?)
  (and name
       (cond
        [(regexp-match? rx:package-name name)
         name]
        [(equal? name "")
         (complain (~a (if inferred? "inferred " "")
                       "package name is empty"))
         #f]
        [else
         (complain (~a (if inferred? "inferred " "")
                       "package name includes disallowed characters"))
         #f])))

(define (extract-archive-name name+ext complain)
  (validate-name
   (path->string
    (if (regexp-match #rx#"[.]tar[.]gz$" (if (path? name+ext)
                                             (path->bytes name+ext)
                                             name+ext))       
        (path-replace-suffix (path-replace-suffix name+ext #"") #"")
        (path-replace-suffix name+ext #"")))
   complain
   #t))

(define (last-non-empty p)
  (cond
   [(null? p) #f]
   [else (or (last-non-empty (cdr p))
             (and (not (equal? "" (path/param-path (car p))))
                  (path/param-path (car p))))]))

(define-syntax-rule (cor v complain)
  (or v (begin complain #f)))

(define (package-source->name+type s type 
                                   #:link-dirs? [link-dirs? #f]
                                   #:complain [complain-proc void]
                                   #:must-infer-name? [must-infer-name? #f])
  ;; returns (values inferred-name inferred-type);
  ;; if `type' is given it should be returned, but name can be #f;
  ;; type should not be #f for a non-#f name
  (define (complain msg)
    (complain-proc s msg))
  (define complain-name
    (if must-infer-name? complain void))
  (define (parse-path s)
    (cond
     [(if type
          (eq? type 'file)
          (and (path-string? s)
               (regexp-match rx:archive s)))
      (unless (path-string? s)
        (complain "ill-formed path"))
      (unless (regexp-match rx:archive s)
        (complain "path does not end with a recognized archive suffix"))
      (define-values (base name+ext dir?) (if (path-string? s)
                                              (split-path s)
                                              (values #f #f #f)))
      (define name (and name+ext (extract-archive-name name+ext complain-name)))
      (values name 'file)]
     [(if type
          (or (eq? type 'dir) 
              (eq? type 'link)
              (eq? type 'static-link))
          (path-string? s))
      (unless (path-string? s)
        (complain "ill-formed path"))
      (define-values (base name dir?) (if (path-string? s)
                                          (split-path s)
                                          (values #f #f #f)))
      (define dir-name (and (cor (path? name) 
                                 (if (not name)
                                     (complain "no elements in path")
                                     (complain "ending path element is not a name")))
                            (path->string name)))
      (values (validate-name dir-name complain-name #t)
              (or type (and dir-name (if link-dirs? 'link 'dir))))]
     [else
      (complain "ill-formed path")
      (values #f #f)]))
  (cond
   [(if type
        (eq? type 'name)
        (regexp-match? rx:package-name s))
    (validate-name s complain #f)
    (values (and (regexp-match? rx:package-name s) s) 'name)]
   [(and (eq? type 'github)
         (not (regexp-match? #rx"^git(?:hub)?://" s)))
    (package-source->name+type 
     (string-append "git://github.com/" s)
     'github)]
   [(if type
        (or (eq? type 'github)
            (eq? type 'file-url)
            (eq? type 'dir-url))
        (regexp-match? #rx"^(https?|github|git)://" s))
    (define url (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (string->url s)))
    (define-values (name name-type)
      (if url
          (let ([p (url-path url)])
            (cond
             [(if type
                  (eq? type 'github)
                  (or (equal? (url-scheme url) "github")
                      (equal? (url-scheme url) "git")))
              (unless (or (equal? (url-scheme url) "github")
                          (equal? (url-scheme url) "git"))
                (complain "URL scheme is not 'git' or 'github'"))
              (define name
                (and (cor (pair? p)
                          (complain "URL path is empty"))
                     (cor (equal? "github.com" (url-host url))
                          (complain "URL host is not 'github.com'"))
                     (if (equal? (url-scheme url) "git")
                         ;; git://
                         (and (cor (or (= (length p) 2)
                                       (and (= (length p) 3)
                                            (equal? "" (path/param-path (caddr p)))))
                                   (complain "URL does not have two path elements (name and repo)"))
                              (let ([a (assoc 'path (url-query url))])
                                (define sub (and a (cdr a) (string-split (cdr a) "/")))
                                (if (pair? sub)
                                    (validate-name (last sub) complain-name #t)
                                    (let ([s (path/param-path (cadr p))])
                                      (validate-name (regexp-replace #rx"[.]git$" s "") complain-name #t)))))
                         ;; github://
                         (let ([p (if (equal? "" (path/param-path (last p)))
                                      (reverse (cdr (reverse p)))
                                      p)])
                           (and (cor ((length p) . >= . 3)
                                     (complain "URL does not have at least three path elements"))
                                (validate-name
                                 (if (= (length p) 3)
                                     (path/param-path (second (reverse p)))
                                     (last-non-empty p))
                                 complain-name
                                 #t))))))
              (values name (or type 'github))]
             [(if type
                  (eq? type 'file-url)
                  (and (pair? p)
                       (path/param? (last p))
                       (regexp-match? rx:archive (path/param-path (last p)))))
              (unless (pair? p)
                (complain "URL path is empty"))
              (when (pair? p)
                (unless (path/param? (last p))
                  (complain "URL's last path element is missing"))
                (unless (regexp-match? rx:archive (path/param-path (last p)))
                  (complain "URL does not end with a recognized archive suffix")))
              (values (and (pair? p)
                           (extract-archive-name (last-non-empty p) complain-name))
                      'file-url)]
             [else
              (unless (pair? p)
                (complain "URL path is empty"))
              (when (pair? p)
                (unless (path/param? (last p))
                  (complain "URL's last path element is missing")))
              (values (validate-name (last-non-empty p) complain-name #t) 'dir-url)]))
          (values #f #f)))
    (values (validate-name name complain-name #f) (or type (and name-type)))]
   [(and (not type)
         (regexp-match #rx"^file://(.*)$" s))
    => (lambda (m) (parse-path (cadr m)))]
   [(and (not type)
         (regexp-match? #rx"^[a-zA-Z]*://" s))
    (complain "unreognized URL scheme")
    (values #f #f)]
   [else
    (parse-path s)]))

(define (package-source->name s [given-type #f])
  (define-values (name type) (package-source->name+type s given-type))
  name)

(define (package-source->path s [type #f])
  ((if (memq type '(dir link static-link))
       path->directory-path
       values)
   (cond
    [(regexp-match? #rx"^file://" s)
     (url->path (string->url s))]
    [else
     (string->path s)])))
