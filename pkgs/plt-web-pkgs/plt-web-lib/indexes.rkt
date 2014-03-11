#lang at-exp racket/base
(require scribble/html
         racket/format
         racket/runtime-path
         "layout.rkt"
         "style.rkt")

(provide make-indexes)

(define-runtime-path file-png "resources/file.png")
(define-runtime-path folder-png "resources/folder.png")

(define (build site p file-icon folder-icon)
  (let ([dir (current-directory)])
    (unless (file-exists? (build-path dir p "index.html"))
      (page #:site site
            #:file (if (eq? p 'same)
                       "index.html"
                       (path->string (build-path p "index.html")))
            #:title "Index"
            @columns[10 #:row? #t]{
              @table{@(for/list ([i (in-list
                                     (directory-list (build-path dir p)))])
                        @tr{@td{@a[href: (path->string i)]{@;
                                 @img[src: (if (file-exists? (build-path dir p i))
                                              file-icon
                                              folder-icon)
                                     width: "16" height: "16"] @;
                                 @nbsp @;
                                 @(path->string i)}}
                            @td{@(let ([i (build-path dir p i)])
                                   (if (file-exists? i)
                                       (let ([s (file-size i)])
                                         (~a (ceiling (/ s 1024)) "k"))
                                       ""))}})}}))))

(define (make-indexes site [dir 'same] 
                      #:depth [depth #f]
                      #:use-dir? [use-dir? (lambda (p) #t)])
  (define file-icon (copyfile #:site site file-png))
  (define folder-icon (copyfile #:site site folder-png))
  (let loop ([dir dir] [depth depth])
    (build site dir file-icon folder-icon)
    (when (and (or (not depth) (positive? depth))
               (use-dir? dir))
      (for ([d (in-list (directory-list (if (eq? dir 'same)
                                            (current-directory)
                                            dir)))])
        (define p (if (eq? dir 'same) d (build-path dir d)))
        (when (directory-exists? p)
          (loop p (and depth (sub1 depth))))))))
