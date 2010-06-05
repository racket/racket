#lang at-exp s-exp "shared.rkt"

(provide pre-root)
(define pre-root
  (page #:file "pre/" #:title "Prebuilt materials"
    "This is a stub page to get the header for the nightly builds root."))

(provide pre-installers)
(define pre-installers
  (page #:file "pre/installers/" #:title "Nightly build installers"
    "This is a stub page to get the header for the nightly installers."))
