#lang scheme/base

(provide (all-defined-out))

;; Configuration of various parts of the main pages

(define manual-sections
  '((getting-started "Getting Started")
    (language        "Languages")
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
