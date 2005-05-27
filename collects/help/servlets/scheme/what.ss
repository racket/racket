(module what mzscheme
  (require "../private/util.ss"
           "../../private/manuals.ss")
  (require "../private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (define stupid-internal-define-syntax (report-errors-to-browser send/finish))
    
    (define (standout-text s)
      (color-with "forestgreen"
                  `(B ,s)))
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Scheme Languages"))
      (BODY 
       (H1  "Scheme Languages")  
       (A ((NAME "scheme") (VALUE "Language Family")))
       (A ((NAME "r5rs") (VALUE "r5rs")))
       (A ((NAME "language levels") (VALUE "language levels")))
       "From the introduction of "
       ,(main-manual-page "r5rs") " (R5RS):" 
       (P)
       (DL  
        (DD  
         "Scheme is a statically scoped and properly "
         "tail-recursive dialect of the Lisp programming "
         "language [...] designed to have an exceptionally "
         "clear and simple semantics and few different ways "
         "to form expressions. A wide variety of programming "
         "paradigms, including imperative, functional, and "
         "message passing styles, find convenient expression "
         "in Scheme."))
       (P)
       "DrScheme supports many dialects of Scheme. "
       "The following dialects are specifically designed for "
       "teaching computer science.  In DrScheme's "
       (A ((HREF "/servlets/scheme/what.ss#lang-sel")) "language selection menu")
       ", they are found under the heading " (B (TT "How to Design Programs")) "."
       (UL  
        (LI 
         (A ((NAME "beg") (VALUE "Beginning Student language"))) 
         ,(standout-text "Beginning Student")
         " is a pedagogical version of Scheme "
         "that is tailored for beginning computer "
         "science students.") 
        (LI  
         (A ((NAME "begla") (VALUE "Beginning Student with List Abbreviations language")))
         ,(standout-text "Beginning Student with List Abbreviations")
         " extends Beginning Student with convenient "
         "(but potentially confusing) ways to write lists, "
         "including quasiquote.") 
        (LI  
         (A ((NAME "int") (VALUE "Intermediate Student language")))
         ,(standout-text "Intermediate Student")
         " adds local bindings and higher-order functions.") 
        (LI  
         (A ((NAME "intlam") (VALUE "Intermediate Student with Lambda language")))
         ,(standout-text "Intermediate Student with Lambda")
         " adds anonymous functions.") 
        (LI  
         (A ((NAME "adv") (VALUE "Advanced Student language")))
         ,(standout-text "Advanced Student")
         " adds mutable state."))
       "The "
       ,(standout-text "Essentials of Programming Languages")
       " language is designed for use with the MIT Press "
       "textbook with that name."
       (P)
       "Other dialects are designed for practicing programmers. " 
       "The " (A ((NAME "r5rs2") (VALUE "R5RS Scheme language")))
       ,(standout-text "R5RS")
       " language is a standard dialect of Scheme that is "
       "defined by the " 
       ,(main-manual-page "r5rs") ". "
       (A ((NAME "full") (VALUE "PLT Scheme language")))
       "In DrScheme's "
       (A ((HREF "/servlets/scheme/what.ss#lang-sel")) "language selection menu")
       ", the following languages "
       "are found under the heading " (B (TT "PLT")) ":"
       (UL
        (LI  
         ,(standout-text "Textual (MzScheme)")
         " is a superset of R5RS Scheme. "
         "In addition to the the base Scheme language, "
         "PLT Scheme provides exceptions, threads, "
         "objects, modules, components, regular expressions, "
         "TCP support, filesystem utilities, and process "
         "control operations. This language is defined in " 
         ,(main-manual-page "mzscheme")
         ". ")
        (LI
         ,(standout-text "Graphical (MrEd)")
         " includes the " (standout-text "Textual (MzScheme)") " language "
         "and adds a graphical toolbox, "
         "described in "
         ,(main-manual-page "mred"))
        (LI
         ,(standout-text "Pretty Big")
         " is a superset of the "
	 (standout-text "Graphical (MrEd)")
         " language, and adds forms from the "
         (standout-text "Pretty Big")
	 " language."
	 " For those forms that are in both languages,"
	 " Pretty Big behaves like Graphical (MrEd)."))
       "The "
       (A ((NAME "module") (VALUE "module")))
       ,(standout-text "module")
       " language supports development using PLT Scheme's "
       ,(manual-entry "mzscheme" "modules" `(CODE "module"))
       " form, where the module's language is explicitly "
       "declared in the code."
       (P)
       "See "
       ,(manual-entry "drscheme" "language levels" "the DrScheme manual")
       " for further details on the languages, "
       "especially the teaching languages." 
       (P)
       "DrScheme's set of languages can be extended, "
       "so the above list mentions only the languages installed "
       "by default. "
       "Documentation for all languages is available "
       "through the " 
       (A ((HREF "/servlets/manuals.ss")) "manuals page") "."
       (P)
       (A ((NAME "lang-sel") (VALUE "language, setting")))
       "DrScheme's default language is Beginning Student. "
       "To change the language, select the " 
       (B (TT  "Set Language...")) 
       " item in the " 
       (B (TT "Language")) " menu."))))