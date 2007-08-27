(module what mzscheme
  (require "../private/util.ss"
           "../../private/manuals.ss"
           "../private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (standout-text s)
    (with-color "forestgreen" `(B ,s)))
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html
         (head ,hd-css ,@hd-links (title "Scheme Languages"))
         (body
          (h1 "Scheme Languages")
          (a ([name "scheme"] [value "Language Family"]))
          (a ([name "r5rs"] [value "r5rs"]))
          (a ([name "language levels"] [value "language levels"]))
          "From the introduction of " ,(main-manual-page "r5rs") " (R5RS):"
          (p)
          (dl (dd "Scheme is a statically scoped and properly tail-recursive"
                  " dialect of the Lisp programming language [...] designed to"
                  " have an exceptionally clear and simple semantics and few"
                  " different ways to form expressions.  A wide variety of"
                  " programming paradigms, including imperative, functional, and"
                  " message passing styles, find convenient expression in"
                  " Scheme."))
          (p)
          "DrScheme supports many dialects of Scheme.  The following dialects are"
          " specifically designed for teaching computer science.  In DrScheme's "
          (a ([href "/servlets/scheme/what.ss#lang-sel"])
             "language selection menu")
          ", they are found under the heading " (b "How to Design Programs") "."
          (ul (li (a ([name "beg"] [value "Beginning Student language"]))
                  ,(standout-text "Beginning Student")
                  " is a pedagogical version of Scheme that is tailored for"
                  " beginning computer science students.")
              (li (a ([name "begla"]
                      [value "Beginning Student with List Abbreviations language"]))
                  ,(standout-text "Beginning Student with List Abbreviations")
                  " extends Beginning Student with convenient (but potentially"
                  " confusing) ways to write lists, including quasiquote.")
              (li (a ([name "int"] [value "Intermediate Student language"]))
                  ,(standout-text "Intermediate Student")
                  " adds local bindings and higher-order functions.")
              (li (a ([name "intlam"]
                      [value "Intermediate Student with Lambda language"]))
                  ,(standout-text "Intermediate Student with Lambda")
                  " adds anonymous functions.")
              (li (a ([name "adv"] [value "Advanced Student language"]))
                  ,(standout-text "Advanced Student")
                  " adds mutable state."))
          "The "
          ,(standout-text "Essentials of Programming Languages")
          " language is designed for use with the MIT Press textbook with that"
          " name."
          (p)
          "Other dialects are designed for practicing programmers. The "
          (a ([name "r5rs2"] [value "R5RS Scheme language"]))
          ,(standout-text "R5RS")
          " language is a standard dialect of Scheme that is defined by the "
          ,(main-manual-page "r5rs") ". "
          (a ([name "full"] [value "PLT Scheme language"]))
          "In DrScheme's "
          (a ([href "/servlets/scheme/what.ss#lang-sel"])
             "language selection menu")
          ", the following languages are found under the heading " (b "PLT") ":"
          (ul (li ,(standout-text "Textual (MzScheme)") " is a superset of R5RS"
                  " Scheme.  In addition to the the base Scheme language, PLT"
                  " Scheme provides exceptions, threads, objects, modules,"
                  " components, regular expressions, TCP support, filesystem"
                  " utilities, and process control operations. This language is"
                  " defined in " ,(main-manual-page "mzscheme") ". ")
              (li ,(standout-text "Graphical (MrEd)") " includes the "
                  (standout-text "Textual (MzScheme)") " language and adds a"
                  " graphical toolbox, described in "
                  ,(main-manual-page "mred") ".")
              (li ,(standout-text "Pretty Big") " is a superset of the "
                  (standout-text "Graphical (MrEd)")
                  " language, and adds forms from the "
                  (standout-text "Pretty Big") " language.  For those forms that"
                  " are in both languages, Pretty Big behaves like Graphical"
                  " (MrEd)."))
          "The " (a ([name "module"] [value "module"]))
          ,(standout-text "(module ...)")
          " language supports development using PLT Scheme's "
          ,(manual-entry "mzscheme" "modules" `(code "module"))
          " form, where the module's language is explicitly declared in the code."
          (p)
          "See " ,(manual-entry "drscheme" "language levels" "the DrScheme manual")
          " for further details on the languages, especially the teaching"
          " languages."
          (p)
          "DrScheme's set of languages can be extended, so the above list"
          " mentions only the languages installed by default.  Documentation for"
          " all languages is available through the "
          (a ([href "/servlets/manuals.ss"]) "manuals page") "."
          (p)
          (a ([name "lang-sel"] [value "language, setting"]))
          "To change the"
          " language, select the " (b "Choose Language...") " item in the "
          (B "Language") " menu."))))))
