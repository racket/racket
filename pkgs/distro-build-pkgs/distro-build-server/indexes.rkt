#lang racket/base
(require racket/string
         scribble/html
         plt-web)

(provide generate-index-html)

(define (generate-index-html dest-dir sub-dir www-site)
  (define content
    (for/list ([f (directory-list (build-path dest-dir sub-dir))])
      (define fp (build-path dest-dir sub-dir f))
      (if (file-exists? fp)
          (cons f (file-size fp))
          (cons f 'dir))))
  (cond
   [www-site
    (define web-dir (string-join (map path-element->string (explode-path sub-dir)) "/"))
    (log-error "web ~s" web-dir)
    (define s
      (site web-dir
            #:url "http://index.racket-lang.org"
            #:share-from www-site
            #:always-abs-url? #f))
    (define is (index-site s))
    (index-page is 'same content)
    (void)]
   [else
    (define page-content
      (html (head (title "Index"))
            (body (table
                   (for/list ([c (in-list content)])
                     (tr (td (a href: (car c)
                                ((if (eq? 'dir (cdr c))
                                     (lambda (p)
                                       (format "[~a]" p))
                                     values)
                                 (car c))))))))))
    (call-with-output-file*
     (build-path dest-dir sub-dir "index.html")
     (lambda (o)
       (output-xml page-content o)))]))
   
