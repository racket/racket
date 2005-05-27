
(module info (lib "infotab.ss" "setup")
  (define doc.txt "doc.txt")
  (define name "Slideshow")
  (define tools (list '("tool.ss")))
  (define tool-names (list "Slideshow"))
  (define tool-icons (list (list "slideshow.png" "slideshow")))
  (define tool-urls (list "http://www.plt-scheme.org/software/slideshow/"))
  (define mred-launcher-libraries (list "start.ss"))
  (define mred-launcher-names (list "Slideshow"))
  (define compile-omit-files
    (list "tutorial-show.ss" "initial-ones.ss" "pict-snipclass.ss")))
