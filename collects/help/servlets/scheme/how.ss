(module how mzscheme
  (require (lib "launcher.ss" "launcher")
           "../private/util.ss"
           "../../private/manuals.ss"
           "../private/headelts.ss"
           "../../private/installed-components.ss"
           (lib "uri-codec.ss" "net")
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
    (send/finish
     `(html
       (head ,hd-css ,@hd-links (title "Software & Components"))
       (body
        (h1 "Software & Components")
        ,(color-highlight `(h2 "DrScheme"))
        (a ([name "dr2"] [value "DrScheme programming environment"]))
        (a ([name "dr3"] [value "Running Scheme"]))
        (b "DrScheme")
        " is a user-friendly environment for creating and running"
        " Scheme programs."
        (p)
        "DrScheme's default "
        (a ((href "/servlets/scheme/what.ss")) "language")
        " is Beginning Student.  To change the language, select the "
        (b (tt "Choose Language...")) " item in the "
        (b (tt "Language")) " menu."
        (p)
        "On this machine, the DrScheme program is "
        (tt ,(path->string (mred-program-launcher-path "DrScheme"))) "."
        (p)
        "For more information, see "
        (a ((href "/servlets/howtodrscheme.ss")) "DrScheme") "."
        (p)
        ,(color-highlight `(h2 "MzScheme and MrEd"))
        (a ((name "mz") (value "MzScheme interpreter")))
        (a ((name "mr") (value "MrEd interpreter")))
        "The " (b "MzScheme") " and " (b "MrEd")
        " executables run programs written in the MzScheme and MrEd variants,"
        " respectively, of the PLT Scheme "
        (a ((href "/servlets/scheme/what.ss")) "language") "."
        (p)
        "Create a MzScheme or MrEd program using the DrScheme development"
        " environment. Then, use the MzScheme or MrEd executable to run the"
        " program in its deployed setting."
        (p)
        "On this machine, the MzScheme program is at "
        (tt ,(path->string (mzscheme-program-launcher-path "MzScheme")))
        ", and MrEd is at "
        (tt ,(path->string (mred-program-launcher-path "MrEd"))) "."
        (p)
        "For more information, see " ,(main-manual-page "mzscheme")
        " and " ,(main-manual-page "mred")
        (p)
        ,(color-highlight `(h2 "mzc"))
        (a ((name "mzc2") (value "mzc compiler")))
        (a ((name "mzc3") (value "Compiling")))
        "The " (b "mzc") " command-line tool creates stand-alone executables,"
        " compiles MzScheme and MrEd programs to byte-code files, compiles"
        " programs to native code using a C compiler "
        ,(if (memq (system-type) '(macosx windows))
           "(not useful on this machine, since MzScheme's just-in-time compiler works), "
           "(useful on on machines where MzScheme's just-in-time compiler is unavailable), ")
        "bundles distribution archives, and performs many other tasks."
        (p)
        "On this machine, the mzc program is at "
        (tt ,(path->string (mzscheme-program-launcher-path "mzc"))) "."
        (p)
        "For more information, see "
        ,(main-manual-page "mzc") ". "
        (p)
        (a ((name "help") (value "help-desk")))
        ,(color-highlight `(h2 "Help Desk"))
        "Help Desk provides information about PLT Software in a user-friendly,"
        " searchable environment.  Help Desk can run by itself, or within"
        " DrScheme (via the " (b (tt "Help")) " menu)."
        "You are currently reading this text in Help Desk."
        (p)
        "On this machine, the Help Desk program is at "
        (tt ,(path->string (mred-program-launcher-path "Help Desk"))) "."
        (p)
        (a ((name "setup-plt")))
        ,(color-highlight `(h2 "Setup PLT"))
        (a ((name "setup") (value "Setup PLT program")))
        (a ((name "setup2") (value "setup-plt program")))
        (a ((href ,(format "/servlets/doc-anchor.ss?file=~a&name=~a&caption=~a"
                           (uri-encode
                            (path->string
                             (simplify-path
                              (build-path (collection-path "mzlib")
                                          'up "setup" "doc.txt"))))
                           "Setup PLT"
                           "Document for the setup collection")))
           "Setup PLT")
        " performs certain installation duties, such as compiling DrScheme's"
        " source code to make DrScheme start faster."
        (p)
        "Setup PLT also unpacks and installs downloadable "
        (tt ".plt") " distributions, such as the MrFlow "
        "distribution archive. However, Help Desk automatically runs Setup PLT"
        " when you use it to download a "
        (tt ".plt") " file."
        (p)
        "On this machine, the Setup PLT program is at "
        (tt ,(path->string (mzscheme-program-launcher-path "Setup PLT"))) "."
        (p)
        (a ((name "installed-components") (value "Installed Components")))
        ,(color-highlight `(h2 "Additional Installed Components"))
        (a ((name "installed-components")))
        (i "The list below was generated by searching the set of installed"
           " libraries.")
        (ul ,@(help-desk:installed-components)))))))))