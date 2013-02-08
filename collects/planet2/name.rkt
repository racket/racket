#lang racket/base
(require racket/list
         net/url)

(provide package-source->name+type
         package-source->name)

(define rx:package-name #rx"^[-_a-zA-Z0-9]+$")
(define rx:archive #rx"[.](plt|zip|tar|tgz|tar[.]gz)$")

(define (validate-name name)
  (and name
       (regexp-match? rx:package-name name)
       name))

(define (extract-archive-name name+ext)
  (validate-name
   (path->string
    (if (regexp-match #rx#"[.]tar[.]gz$" (if (path? name+ext)
                                             (path->bytes name+ext)
                                             name+ext))       
        (path-replace-suffix (path-replace-suffix name+ext #"") #"")
        (path-replace-suffix name+ext #"")))))

(define (last-non-empty p)
  (cond
   [(null? p) #f]
   [else (or (last-non-empty (cdr p))
             (and (not (equal? "" (path/param-path (car p))))
                  (car p)))]))

(define (package-source->name+type s type)
  ;; returns (values inferred-name inferred-type);
  ;; if `type' is given it should be returned, but name can be #f;
  ;; type should not be #f for a non-#f name
  (cond
   [(if type
        (eq? type 'name)
        (regexp-match? rx:package-name s))
    (values (and (regexp-match? rx:package-name s) s) 'name)]
   [(and (eq? type 'github)
         (not (regexp-match? #rx"^github://" s)))
    (package-source->name+type 
     (string-append "github://github.com/" s)
     'github)]
   [(if type
        (or (eq? type 'github)
            (eq? type 'file-url)
            (eq? type 'dir-url))
        (regexp-match? #rx"^(https?|github)://" s))
    (define url (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (string->url s)))
    (define-values (name name-type)
      (if url
          (let ([p (url-path url)])
            (cond
             [(if type
                  (eq? type 'github)
                  (equal? (url-scheme url) "github"))
              (define name
                (and (pair? p)
                     (let ([p (if (equal? "" (path/param-path (last p)))
                                  (reverse (cdr (reverse p)))
                                  p)])
                       (and ((length p) . >= . 3)
                            (validate-name 
                             (if (= (length p) 3)
                                 (path/param-path (second (reverse p)))
                                 (path/param-path (last-non-empty p))))))))
              (values name (or type 'github))]
             [(if type
                  (eq? type 'file-url)
                  (and (pair? p)
                       (regexp-match? rx:archive (path/param-path (last p)))))
              (values (and (pair? p)
                           (extract-archive-name (path/param-path (last-non-empty p))))
                      'file-url)]
             [else
              (values (validate-name (path/param-path (last-non-empty p))) 'dir-url)]))
          (values #f #f)))
    (values (validate-name name) (or type (and name-type)))]
   [(and (not type)
         (regexp-match? #rx"^[a-zA-Z]*://" s))
    (values #f #f)]
   [(if type
        (eq? type 'file)
        (and (path-string? s)
             (regexp-match rx:archive s)))
    (define-values (base name+ext dir?) (split-path s))
    (define name (extract-archive-name name+ext))
    (values name 'file)]
   [(if type
        (or (eq? type 'dir) (eq? type 'link))
        (path-string? s))
    (define-values (base name dir?) (split-path s))
    (define dir-name (and (path? name) (path->string name)))
    (values (validate-name dir-name) (or type (and dir-name 'dir)))]
   [else
    (values #f #f)]))

(define (package-source->name s)
  (define-values (name type) (package-source->name+type s #f))
  name)
