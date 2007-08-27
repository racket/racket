(module batch mzscheme
  (require "../../private/headelts.ss"
           "../../private/util.ss")

  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)

  (define (start initial-request)
    (with-errors-to-browser
     send/finish
     (lambda ()
    `(html
      (head ,hd-css
            ,@hd-links
            (title  "How to write Windows batch files"))
      (body
       (h1 "How to write Windows batch files")
       (a ((name "sh") (value "Batch files")))
       (a ((name "sh2") (value ".bat files")))
       "You can put MzScheme code in a Windows batch file, that is, a "
       "file with a .BAT extension.  Batch files can be executed "
       "directly from the command line.  In Windows 95, 98, and Me, "
       "the batch file looks like:"
       (pre 
        " ; @echo off" (br)
        " ; d:\\plt\\mzscheme -r %0 %1 %2 %3 %4 %5 %6 %7 %8 %9" (br)
        " ; goto :end" (br)
        "   ... " (i  "scheme-program") " ..." (br)
        " ; :end")
       "With this code, your batch file can use as many as nine "
       "parameters."
       (p)
       "In Windows NT, Windows 2000, and Windows XP, you can instead write "
       (pre
        " ; @echo off" (br)
        " ; d:\\plt\\mzscheme -r %0 %*" (br)
        " ; goto :end" (br)
        "   ... " (i "scheme-program") " ..." (br)
        " ; :end")
       "This code allows an arbitrary number of parameters to your " 
       "batch file."
       (p)
       "The batch file code works by combining both batch and MzScheme "
       "syntax in a single file.  When invoked from the command line, "
       "the semicolons are ignored.  The second line invokes MzScheme "
       "with the batch file as an argument.  MzScheme interprets the "
       "lines beginning with semicolons as comments, and runs the "
       "Scheme code.  When the Scheme program is "
       "done, control returns to the batch file, and the "
       (tt  "goto") " jumps around the Scheme code."))))))
