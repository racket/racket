(module langlevels mzscheme
  (require "../private/headelts.ss"
           "../../private/manuals.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "A Note on Language Levels"))
      (body
       (h1 "A Note on Language Levels")
       (a ([name "language levels"] [value "language levels"]))
       (p)
       "DrScheme presents Scheme via a hierarchy of "
       ,(manual-entry "drscheme" "languages" "language levels") "."
       (p)
       "We designed the teaching languages based upon our observations of"
       " students in classes and labs over several years. Beginning students"
       " tend to make small notational mistakes that produce "
       (em  "syntactically legal") " Scheme programs with a "
       (em "radically different meaning") " than the one intended."
       " Even the best students are then surprised by error messages, which"
       " might mention concepts not covered in classes, or other unexpected"
       " behavior."
       (p)
       "The teaching levels are not ideal for instructors.  They are"
       " particularly unhelpful for implementing libraries to support course"
       " material.  But the levels were not designed for this purpose."
       " Instead, in order to protect students from unwanted mistakes and to"
       " provide them with libraries based on language constructs outside of"
       " their knowledge, DrScheme provides an interface designed specially"
       " for instructors: "
       ,(manual-entry "drscheme" "DrScheme Teachpacks" "Teachpacks") "."
       " A Teachpack is a "
       ,(manual-entry "mzscheme" "modules" "module")
       " that is implemented in Full Scheme; it imports the functions from the"
       " teaching languages and the graphics run-time library.  The provided"
       " values are automatically imported to the run-time of the"
       " read-eval-print loop when the student clicks the "
       ,(manual-entry "drscheme" "Execute button" "Execute") "."
       " In short, Teachpacks provide students the best of both worlds:"
       " protection from wanton error messages and unexpected behavior, and"
       " powerful support from the instructor."
       (p)
       "We strongly encourage instructors to employ language levels and"
       " Teachpacks. In our experience, the restriction of the teaching"
       " languages do not interfere with students' programming needs up to,"
       " and including, junior-level courses on programming languages.  It"
       " gives students a more productive learning experience than raw Scheme,"
       " and simplifies the interface between library and user code."
       (p)
       "We also strongly encourage students to point out this page to their"
       " instructors."
       (p)
       "Please follow the links on this page for more information.  If you"
       " have additional questions or comments, please contact us at "
       (a ((href "mailto:scheme@plt-scheme.org")) "scheme@plt-scheme.org")
       "."))))
