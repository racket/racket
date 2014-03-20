#lang at-exp racket/base
(require scribble/html
         racket/format
         racket/runtime-path
         "layout.rkt"
         "style.rkt")

(provide make-indexes
         (rename-out [mk-index-site index-site])
         index-site?
         index-page)

(define-runtime-path file-png "resources/file.png")
(define-runtime-path folder-png "resources/folder.png")

(struct index-site (site file-icon folder-icon))

(define (index-page is dir content
                    #:html-only? [html-only? #f])
  (page #:site (index-site-site is)
        #:file (if (eq? dir 'same)
                   "index.html"
                   (path->string (build-path dir "index.html")))
        #:title "Index"
        #:html-only? html-only?
        @columns[10 #:row? #t]{
              @table{@(for/list ([p+k (in-list content)])
                        (define p (let ([p (car p+k)])
                                    (if (path? p)
                                        (path->string p)
                                        p)))
                        (define k (cdr p+k))
                        @tr{@td{@a[href: p]{@;
                                 @img[src: (if (number? k)
                                              (index-site-file-icon is)
                                              (index-site-folder-icon is))
                                     width: "16" height: "16"] @;
                                 @nbsp @;
                                 @p}}
                            @td{@(if (number? k)
                                     (~a (ceiling (/ k 1024)) "k")
                                     "")}})}}))

(define mk-index-site
  (let ([index-site
         (lambda (site)
           (define file-icon (copyfile #:site site file-png))
           (define folder-icon (copyfile #:site site folder-png))
           (index-site site file-icon folder-icon))])
    index-site))

(define (build is root-dir p)
  (unless (file-exists? (build-path root-dir p "index.html"))
    (index-page is p
                (for/list ([i (in-list (directory-list (build-path root-dir p)))])
                  (define f (build-path root-dir p i))
                  (if (file-exists? f)
                      (cons i (file-size f))
                      (cons i 'dir))))))

(define (make-indexes site [dir 'same] 
                      #:depth [depth #f]
                      #:use-dir? [use-dir? (lambda (p) #t)])
  (define is (mk-index-site site))
  (let loop ([dir dir] [depth depth])
    (build is (current-directory) dir)
    (when (and (or (not depth) (positive? depth))
               (use-dir? dir))
      (for ([d (in-list (directory-list (if (eq? dir 'same)
                                            (current-directory)
                                            dir)))])
        (define p (if (eq? dir 'same) d (build-path dir d)))
        (when (directory-exists? p)
          (loop p (and depth (sub1 depth))))))))
