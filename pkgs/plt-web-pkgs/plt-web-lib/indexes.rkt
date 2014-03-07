#lang at-exp racket/base
(require scribble/html
         racket/format
         "layout.rkt")

(provide make-indexes)

(define (build site p)
  (let ([dir (current-directory)])
    (unless (file-exists? (build-path dir p "index.html"))
      (page #:site site
            #:file (if (eq? p 'same)
                       "index.html"
                       (path->string (build-path p "index.html")))
            #:title "Index"
            @table{@(for/list ([i (in-list
                                   (directory-list (build-path dir p)))])
                      @tr{@td{@a[href: (path->string i)]{@(path->string i)}}
                          @td{@(let ([i (build-path dir p i)])
                                 (if (file-exists? i)
                                     (let ([s (file-size i)])
                                       (~a (ceiling (/ s 1024)) "k"))
                                     ""))}})}))))

(define (make-indexes site [dir 'same] 
                      #:depth [depth #f])
  (build site dir)
  (when (or (not depth) (positive? depth))
    (for ([d (in-list (directory-list (if (eq? dir 'same)
                                          (current-directory)
                                          dir)))])
      (define p (if (eq? dir 'same) d (build-path dir d)))
      (when (directory-exists? p)
        (make-indexes site p #:depth (and depth (sub1 depth)))))))
