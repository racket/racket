#lang racket/base
(require racket/list
         racket/match
         racket/format
         racket/function
         racket/path
         "../path.rkt"
         "dirs.rkt"
         "pkg-db.rkt")

(provide pkg-show)

(define (pkg-show indent 
                  #:directory? [dir? #f]
                  #:auto? [show-auto? #f]
                  #:long? [long? #t])
  (let ()
    (define db (read-pkg-db))
    (define pkgs (sort (hash-keys db) string-ci<=?))
    (if (null? pkgs)
        (printf " [none]\n")
        (begin
          (table-display
           long?
           (append '(right right middle)
                   (if dir?
                       '(left)
                       '()))
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
                     (if (or checksum long?)
                         (format "~a" checksum)
                         "")
                     (let ([src (case (car orig-pkg)
                                  [(link static-link clone)
                                   (list* (car orig-pkg)
                                          (path->string
                                           (simple-form-path
                                            (path->complete-path (cadr orig-pkg)
                                                                 (pkg-installed-dir))))
                                          (cddr orig-pkg))]
                                  [else orig-pkg])])
                       (if long?
                           (~s src)
                           (apply ~a #:separator " " src))))
               (if dir?
                   (let ([p (path->string
                             (simple-form-path
                              (pkg-directory* pkg #:db db)))])
                     (list (if long?
                               (~s p)
                               (~a p))))
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

(define (table-display long? dots-poses l)
  (define how-many-cols (length (first l)))
  (define full-max-widths
    (for/list ([col (in-range how-many-cols)])
      (apply max (map (compose string-length (curryr list-ref col)) l))))
  (define sep (if long? 4 2))
  (define COLUMNS (or (cond
                       [long? 80]
                       [(getenv "COLUMNS")
                        => (lambda (s)
                             (define v (string->number s))
                             (and (exact-positive-integer? v) v))]
                       [else #f])
                      80))
  (define max-widths
    (cond
     [(or long?
          ((apply + full-max-widths) . < . (- COLUMNS (* sep (sub1 how-many-cols)))))
      full-max-widths]
     [else
      (define avail (- COLUMNS
                       (car full-max-widths)
                       (* sep (sub1 how-many-cols))))
      (cons (car full-max-widths)
            (for/list ([c (in-list (cdr full-max-widths))]
                       [i (in-naturals 1)])
              (define frac
                ;; Give last column twice the space:
                (if (= i (sub1 how-many-cols))
                    (/ 2 how-many-cols)
                    (/ 1 how-many-cols)))
              (max 3
                   (floor (* avail frac)))))]))
  (for ([row (in-list l)])
    (for ([col (in-list row)]
          [i (in-naturals 1)]
          [width (in-list max-widths)]
          [dots-pos (in-list dots-poses)])
      (define col-width (string-length col))
      (printf "~a~a"
              (if (col-width . <= . width)
                  col
                  (case dots-pos
                   [(right)
                    ;; Checksum: show prefix:
                    (~a (substring col 0 (- width 3))
                        "...")]
                   [(middle)
                    ;; Source
                    ;; To start "..." at a space:
                    (define m (regexp-match-positions #rx" " col))
                    (define left
                      (cond
                       [(and m
                             ((caar m) . < . (- width 3)))
                        ;; Dots at space:
                        (caar m)]
                       [else
                        ;; Put dots in middle:
                        (quotient (- width 3) 2)]))
                    (~a (substring col 0 left)
                        "..."
                        (substring col (+ (- col-width width)
                                          3 left)))]
                   [(left)
                    ;; Put dots at start:
                    (~a "..."
                        (substring col (min col-width (- col-width width -3))))]))
              (if (= i how-many-cols)
                ""
                (let ([len (min (string-length col)
                                width)])
                  (make-string (+ (- width len) sep) #\space)))))
    (printf "\n")))
