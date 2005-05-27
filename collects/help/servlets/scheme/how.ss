(module how mzscheme
  (require (lib "launcher.ss" "launcher")
           "../private/util.ss"
           "../../private/manuals.ss"
           "../private/headelts.ss"
           "../../private/installed-components.ss"
           (lib "servlet.ss" "web-server"))
  
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (define stupid-internal-define-syntax (report-errors-to-browser send/finish))
    
    (define soft-page
      `(HTML 
        (HEAD ,hd-css
              ,@hd-links 
              (TITLE "Software & Components"))
        (BODY 
         (H1  "Software & Components")
         ,(color-highlight `(H2  "DrScheme"))
         (A ((NAME "dr2") (VALUE "DrScheme programming environment")))
         (A ((NAME "dr3") (VALUE "Running Scheme")))
         (B  "DrScheme") 
         " is a user-friendly environment for creating and running "
         "Scheme programs." 
         (P)
         "DrScheme's default " 
         (A ((HREF "/servlets/scheme/what.ss")) "language")
         " is Beginning Student. To change the language, select "
         "the " (B  (TT  "Choose Language...")) " item in the " 
         (B  (TT  "Language")) " menu."
         (P)
         "On this machine, the DrScheme program is " 
         (TT  ,(path->string (mred-program-launcher-path "DrScheme"))) "."
         (P)
         "For more information, see " 
         (A ((HREF "/servlets/howtodrscheme.ss")) "DrScheme") "."
         (P)
         ,(color-highlight `(H2  "MzScheme and MrEd"))
         (A ((NAME "mz") (VALUE "MzScheme interpreter")))
         (A ((NAME "mr") (VALUE "MrEd interpreter")))
         "The " (B  "MzScheme") 
         " and " (B  "MrEd") " executables run programs written "
         "in the MzScheme and MrEd variants, respectively, of the "
         "PLT Scheme " (A ((HREF "/servlets/scheme/what.ss")) "language")
         "." 
         (P)
         "Create a MzScheme or MrEd program using the DrScheme "
         "development environment. Then, use the MzScheme or MrEd "
         "executable to run the program in its deployed setting."
         (P)
         "On this machine, the MzScheme program is at " 
         (TT  ,(path->string (mzscheme-program-launcher-path "MzScheme"))) ", and "
         "MrEd is at " 
         (TT  ,(path->string (mred-program-launcher-path "MrEd"))) "."
         (P)
         "For more information, see " 
         ,(main-manual-page "mzscheme")
         " and " 
         ,(main-manual-page "mred")
         (P)
         ,(color-highlight `(H2  "mzc"))
         (A ((NAME "mzc2") (VALUE "mzc compiler")))
         (A ((NAME "mzc3") (VALUE "Compiling")))
         "The " (B  "mzc") " executable compiles MzScheme and "
         "MrEd programs to native code using a C compiler "
         "(which your system must provide).  The resulting native "
         "code can be loaded into MrEd or MzScheme.  The mzc "
         "compiler also provides limited support for building "
         "stand-alone executables from Scheme code." 
         (P)
         "On this machine, the mzc program is at " 
         (TT  ,(path->string (mzscheme-program-launcher-path "mzc"))) "."
         (P)
         "For more information, see " 
         ,(main-manual-page "mzc") ". "
         (P)
         (A ((NAME "help") (VALUE "help-desk")))
         ,(color-highlight `(H2  "Help Desk"))
         "Help Desk provides information about PLT Software in a "
         "user-friendly, searchable environment. "
         "Help Desk can run by itself, or within DrScheme "
         "(via the " 
         (B  (TT  "Help")) " menu)."  
         "You are currently reading this text in Help Desk."
         (P)
         "On this machine, the Help Desk program is at " 
         (TT  ,(path->string (mred-program-launcher-path "Help Desk"))) "."
         (P)
         (A ((NAME "setup-plt"))) 
         ,(color-highlight `(H2  "Setup PLT"))
         (A ((NAME "setup") (VALUE "Setup PLT program")))
         (A ((NAME "setup2") (VALUE "setup-plt program")))
         (A ((HREF ,(format "/servlets/doc-anchor.ss?file=~a&name=~a&caption=~a"
                            (hexify-string
                             (path->string
                              (simplify-path
                               (build-path (collection-path "mzlib") 'up "setup" "doc.txt"))))
                            "Setup PLT"
                            "Document for the setup collection")))
            "Setup PLT")
         " performs certain installation duties, such as compiling "
         "DrScheme's source code to make DrScheme start faster." 
         (P)
         "Setup PLT also unpacks and installs downloadable " 
         (TT  ".plt") " distributions, such as the MrFlow "
         "distribution archive. However, Help Desk automatically "
         "runs Setup PLT when you use it to download a " 
         (tt  ".plt") " file."
         (P)
         "On this machine, the Setup PLT program is at " 
         (TT  ,(path->string (mzscheme-program-launcher-path "Setup PLT"))) "."
         (P)
         (A ((NAME "installed-components") (VALUE "Installed Components")))
         ,(color-highlight `(H2  "Additional Installed Components"))
         (A ((NAME "installed-components")))
         (I  
          "The list below was generated by searching the set "
          "of installed libraries.")
         (UL ,@(help-desk:installed-components)))))
    
    (send/finish soft-page)))