#lang setup/infotab

(define name "Test Suites")
(define tools '(("time-keystrokes.ss" "drracket")))
(define tool-names '("Time Keystrokes"))

(define compile-omit-paths
  '("2htdp"
    "aligned-pasteboard"
    "deinprogramm"
    "errortrace"
    "future"
    "gracket"
    "honu"
    "match"
    "macro-debugger"
    "mysterx"
    "mzcom"
    "racket"
    "plai"
    "planet"
    "plot"
    "r6rs"
    "rackunit"
    "srfi"
    "srpersist"
    "stepper"
    "stxparse"
    "syntax-color"
    "typed-scheme"
    "units"
    "unstable"
    "xml"
    "html"
    "web-server"))
