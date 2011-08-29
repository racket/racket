#lang setup/infotab

(require string-constants)

(define name "HtDP Languages")
(define tools (list "htdp-langs.rkt"))
(define tool-icons (list '("htdp-icon.gif" "icons")))
(define tool-names (list "How to Design Programs"))
(define tool-urls (list "http://www.htdp.org/"))

(define compile-omit-paths
  '("test-error.rkt"))

(define textbook-pls
  (list (list '("htdp-icon.gif" "icons")
              "How to Design Programs"
              (string-constant teaching-languages)
              (string-constant how-to-design-programs)
              (string-constant beginning-student))))
