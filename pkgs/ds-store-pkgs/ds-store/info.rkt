#lang info

(define collection "ds-store")

(define deps '("ds-store-lib"
               "ds-store-doc"
               "base"))
(define implies '("ds-store-lib"
                  "ds-store-doc"))

(define pkg-desc "Libraries for manipulating \".DS_Store\" files")

(define pkg-authors '(mflatt))
