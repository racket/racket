#lang info

(define collection 'multi)

(define deps '("base"
               "scribble-lib"))
(define build-deps '("scheme-lib"
                     "at-exp-lib"))

(define pkg-desc "Racket Documentation driver")

(define pkg-authors '(eli jay matthias mflatt robby ryanc samth))

;; We need to be able to re-render this documentation even in
;; binary mode, since that's how we list new documentation:
(define binary-keep-files '("scribblings"
                            "scribblings/main/compiled/acks_scrbl.zo"
                            "scribblings/main/compiled/license_scrbl.zo"
                            "scribblings/main/compiled/local-redirect_scrbl.zo"
                            "scribblings/main/compiled/release_scrbl.zo"
                            "scribblings/main/compiled/search_scrbl.zo"
                            "scribblings/main/compiled/start_scrbl.zo"
                            "scribblings/main/user/compiled/local-redirect_scrbl.zo"
                            "scribblings/main/user/compiled/release_scrbl.zo"
                            "scribblings/main/user/compiled/search_scrbl.zo"
                            "scribblings/main/user/compiled/start_scrbl.zo"))
