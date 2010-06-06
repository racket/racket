#lang at-exp s-exp "../common.rkt"

(define-context "stubs/pre")

(provide pre-root)
(define pre-root
  (page #:file "" #:title "Prebuilt materials"
    "This is a stub page to get the header for the nightly builds root."))

(provide pre-installers)
(define pre-installers
  (page #:file "installers/" #:title "Nightly build installers"
    "This is a stub page to get the header for the nightly installers."))
