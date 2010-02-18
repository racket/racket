#lang setup/infotab

(define name "SchemeUnit")

(define blurb '((p "SchemeUnit is a unit testing framework based on the "
                   " Extreme Programming unit test frameworks")))

(define repositories '("4.x"))
(define required-core-version "4.0.0.0")
(define categories '(devtools))
(define can-be-loaded-with 'all)

(define homepage "http://schematics.sourceforge.net/")
(define url "http://schematics.sourceforge.net/")

(define primary-file "main.ss")
(define scribblings '(("scribblings/schemeunit.scrbl" (multi-page) (tool))))

(define release-notes
  '((p "Correctly handle arbitrary expressions in test suites and fix Scribble errors.")))

;;
;; The data below applies to the graphical UI.  It is kept
;; around for when the GUI is ported to the current
;; SchemeUnit
;;

;;  (define tools '[("drscheme-ui-tool.ss" "plt" "gui")])
;;  (define tool-names '["SchemeUnit DrScheme integration"])
  
  ;; Information about SchemeUnit tests for package
  
;;  (define schemeunit:test-name 'all-schemeunit-tests)
;;  (define schemeunit:test-module "all-schemeunit-tests.ss")

  ;; Information about distribution
  
;;  (define distribution-method 'planet)
;;  (define distribution-package-spec '("schematics" "schemeunit.plt" 3 0))




