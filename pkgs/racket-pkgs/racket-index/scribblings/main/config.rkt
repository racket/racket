#lang scheme/base

(provide (all-defined-out))

;; Configuration of various parts of the main pages

(define bug-url "http://bugs.racket-lang.org/")

;; Link definitions: (id-sym title root-sym/#f-for-url subpath/url),
;; or a `---' for a spacer; the root-sym can be `plt' for standard
;; pages, or `user' for pages that have an installation and a
;; user-specific version (and navigating to them should get to the
;; user-specific pages using cookies).  (Note: the subpath must match
;; where the corresponding document is generated, this is a hack.)
(define links
  `((start   "Racket Documentation" user "index.html")
    (search  "Search Manuals"   user "search/index.html")
    ---
    (license "License"          plt  "license/index.html")
    (acks    "Acknowledgements" plt  "acks/index.html")
    (release "Release Notes"    user  "release/index.html")
    ---
    (bugreport "Report a Bug"   #f ,(format "~a?v=~a" bug-url (version)))))

;; Section definitions for manuals that appear on the start page.
(define manual-sections
  '((getting-started #f)
    (tutorial        "Tutorials")
    (racket-core     "Racket Language and Core Libraries")
    (teaching        "Teaching")
    (language        (elem "Other "
                           (link "Languages"
                                 (lib "scribblings/guide/guide.scrbl")
                                 "dialects")
                           " in the Racket Environment "))
    (tool            "Tools")
    (gui-library     "GUI and Graphics Libraries")
    (net-library     "Network Libraries")
    (parsing-library "Parsing Libraries")
    (tool-library    "Tool Libraries")
    (foreign         "Low-Level APIs")
    (interop         "Interoperability")
    (library         "Miscellaneous Libraries")
    (experimental    "Experimental Languages and Libraries")
    (legacy          "Legacy Languages and Libraries")
    (other           "Other")))
