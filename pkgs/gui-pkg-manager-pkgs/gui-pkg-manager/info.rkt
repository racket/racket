#lang info

(define collection 'multi)

(define deps '("gui-pkg-manager-lib"
               "gui-pkg-manager-doc"
               "gui-lib"
               "base"))

(define implies '("gui-pkg-manager-lib"
                  "gui-pkg-manager-doc"))

(define pkg-desc "Graphical tool for managing Racket package installations")

(define pkg-authors '(mflatt robby))
