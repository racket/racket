#lang racket/base

  (require rackunit
           racket/file
           pkg/db)

  (define (pkg<? a b)
    (if (string=? (pkg-name a) (pkg-name b))
        (string<? (pkg-catalog a) (pkg-catalog b))
        (string<? (pkg-name a) (pkg-name b))))

  (parameterize ([current-pkg-catalog-file (make-temporary-file
                                          "~a.sqlite")])
    (check-equal? (get-catalogs) '())

    (set-catalogs! '("http://a" "http://b"))
    (check-equal? (get-catalogs)
                  '("http://a" "http://b"))
    
    (check-equal? (get-pkgs) '())

    
    (set-pkgs! "http://a" '("p1"))
    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pkgs! "http://b" '("p2"))
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p1" "http://a" "" "" "" "")
                   (pkg "p2" "http://b" "" "" "" "")))
    (check-equal? (get-pkgs #:catalog "http://a") 
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    (check-equal? (get-pkgs #:name "p1")
                  (list
                   (pkg "p1" "http://a" "" "" "" "")))
    
    (set-pkg! "p1" "http://a" "adam" "git:a" "123" "the first package")
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p1" "http://a" "adam" "git:a" "123" "the first package")
                   (pkg "p2" "http://b" "" "" "" "")))

    ;; reverse order of catalogs:
    (set-catalogs! '("http://b" "http://a"))
    (check-equal? (get-catalogs)
                  '("http://b" "http://a"))
    (check-equal? (get-pkgs)
                  (list
                   (pkg "p2" "http://b" "" "" "" "")
                   (pkg "p1" "http://a" "adam" "git:a" "123" "the first package")))

    (check-equal? (get-pkg-tags "p2" "http://b")
                  '())
    (set-pkg-tags! "p2" "http://b" '("2x" "2y" "2z"))
    (check-equal? (sort (get-pkg-tags "p2" "http://b") string<?)
                  '("2x" "2y" "2z"))
    (check-equal? (get-pkg-tags "p1" "http://a")
                  '())
    
    (set-pkg-modules! "p1" "http://a" "123" (list '(lib "lib1/main.rkt")
                                                  '(lib "lib2/main.rkt")))
    (check-equal? (sort (get-pkg-modules "p1" "http://a" "123")
                        string<?
                        #:key cadr)
                  (list '(lib "lib1/main.rkt")
                        '(lib "lib2/main.rkt")))
    (check-equal? (get-module-pkgs '(lib "lib1/main.rkt"))
                  (list
                   (pkg "p1" "http://a" "" "" "123" "")))

    (set-pkg-dependencies! "p1" "http://a" "123" (list "p7"
                                                       '("p8" "8.0")
                                                       '("p9" #:version "9.0")
                                                       '("p10" #:platform #rx"linux")
                                                       '("p11" #:platform windows)
                                                       '("p12" #:version "1.2" #:platform macosx)
                                                       '("p13" #:platform unix #:version "1.3.2")
                                                       '("p14" #:platform "")))
    (check-equal? (sort (get-pkg-dependencies "p1" "http://a" "123")
                        string<?
                        #:key car)
                  '(("p10" #:platform #rx"linux")
                    ("p11" #:platform windows)
                    ("p12" #:version "1.2" #:platform macosx)
                    ("p13" #:version "1.3.2" #:platform unix)
                    ("p14" #:platform "")
                    ("p7")
                    ("p8" #:version "8.0")
                    ("p9" #:version "9.0")))

    (set-catalogs! '("http://a" "http://c"))
    (check-equal? (sort (get-catalogs) string<?) 
                  '("http://a" "http://c"))

    (check-equal? (get-pkgs) 
                  (list
                   (pkg "p1" "http://a" "adam" "git:a" "123" "the first package")))

    (delete-file (current-pkg-catalog-file)))
