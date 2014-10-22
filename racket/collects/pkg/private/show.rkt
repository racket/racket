#lang racket/base
(require racket/list
         racket/match
         racket/format
         racket/function
         "../path.rkt"
         "pkg-db.rkt")

(provide pkg-show)

(define (pkg-show indent 
                  #:directory? [dir? #f]
                  #:auto? [show-auto? #f])
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (if (null? pkgs)
        (printf " [none]\n")
        (begin
          (table-display
           (list*
            (append
             (list (format "~aPackage~a"
                           indent 
                           (if show-auto? "[*=auto]" ""))
                   "Checksum"
                   "Source")
             (if dir?
                 (list "Directory")
                 empty))
            (for/list ([pkg (in-list pkgs)]
                       #:when (or show-auto?
                                  (not (pkg-info-auto? (hash-ref db pkg)))))
              (match-define (pkg-info orig-pkg checksum auto?) (hash-ref db pkg))
              (append
               (list (format "~a~a~a"
                             indent
                             pkg
                             (if auto? "*" ""))
                     (format "~a" checksum)
                     (format "~a" orig-pkg))
               (if dir?
                   (list (~a (pkg-directory* pkg #:db db)))
                   empty)))))
          (unless show-auto?
            (define n (for/sum ([pkg (in-list pkgs)] 
                                #:when (pkg-info-auto? (hash-ref db pkg)))
                               1))
            (unless (zero? n)
              (printf "~a[~a auto-installed package~a not shown]\n"
                      indent
                      n
                      (if (= n 1) "" "s"))))))))

(define (table-display l)
  (define how-many-cols (length (first l)))
  (define max-widths
    (for/list ([col (in-range how-many-cols)])
      (apply max (map (compose string-length (curryr list-ref col)) l))))
  (for ([row (in-list l)])
    (for ([col (in-list row)]
          [i (in-naturals 1)]
          [width (in-list max-widths)])
      (printf "~a~a"
              col
              (if (= i how-many-cols)
                ""
                (make-string (+ (- width (string-length col)) 4) #\space))))
    (printf "\n")))
