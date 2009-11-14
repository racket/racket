#lang setup/infotab

(define name "Test Suites")
(define tools '(("tool.ss" "drscheme") ("time-keystrokes.ss" "drscheme")))
(define tool-names '("DrScheme Test Suites" "Time Keystrokes"))

(define compile-omit-paths
  '("2htdp"
    "aligned-pasteboard"
    "deinprogramm"
    "honu"
    "match"
    "macro-debugger"
    "mred"
    "mysterx"
    "mzcom"
    "mzscheme"
    "plai"
    "planet"
    "plot"
    "profj"
    "r6rs"
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
