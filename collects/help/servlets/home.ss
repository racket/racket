(module home mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "match.ss")
           (lib "acks.ss" "drscheme")
           (lib "uri-codec.ss" "net")
           (lib "dirs.ss" "setup")
           (lib "list.ss")
           (lib "url.ss" "net")
           "../private/manuals.ss"
           "private/util.ss" ; for plt-version
           "private/url.ss"
           "private/html.ss"
           "private/split-screen.ss"
           "../private/options.ss")

  (define copyright-year 2007)

  (provide interface-version timeout start
           generate-index-for-static-pages)
  
  (define interface-version 'v1)
  (define timeout +inf.0)

  ;; html-subpage : xexprs -> xexpr
  (define (html-subpage . xs)
    (apply (case (helpdesk-platform)
             [(internal-browser-simple) make-simple-page/internal-browser]
             [(internal-browser) make-split-page/internal-browser]
             [else make-split-page])
           xs))

  (define (start initial-request)
    ;; Note : DrScheme preferences calls start with a #f argument,
    ;;        so initial-request can be either a request structure or #f
    (unless initial-request
      (set! initial-request
            (make-request 'get (string->url "") '() '() #f "localhost"
                          (internal-port) "localhost")))
    (with-errors-to-browser
     send/finish
     (lambda ()
       (let* ([bindings (request-bindings initial-request)]
              [subpage  (if (exists-binding? 'subpage bindings)
                          (extract-binding/single 'subpage bindings)
                          "home")])
         ;; dispatch on subpage
         ;;   the dynamic ones (manuals and release) are handled are here,
         ;;   the static pages below
         (match subpage
           ["manuals"
            (html-subpage
             "PLT Scheme Help Desk: Installed Manuals"
             (html-top initial-request) (left-items) ""
             `(,@(if (eq? (helpdesk-platform) 'external-browser)
                   '((h3 "NOTE")
                     (p "To see the list of manuals installed on " (i "your") " computer,"
                        " use the Help Desk from within DrScheme. This list of manuals reflects"
                        " what is installed on this Help Desk server only."))
                   '())
               (VERBATIM ,(find-manuals))
               (p (i "Version: " ,(plt-version)))))]
           ["release"
            (let ([link-stuff (lambda (url txt)
                                `(li (b (a ([href ,url]) ,txt))))])
              (html-subpage
               "PLT Scheme Help Desk: Release Info"
               (html-top initial-request) (left-items) ""
               `((VERBATIM
                  ((h3 "NOTE")
                   (p "To see the release information for your installation,"
                      " use the Help Desk from within DrScheme."
                      " The following information reflects the installation on"
                      " this server only.")
                   (h1 "Release Information")
                   (p (i "Version: " ,(plt-version)))
                   (ul ,(link-stuff url-helpdesk-license "License")
                       ,(link-stuff url-helpdesk-release-notes "Release Notes")
                       ,(link-stuff url-helpdesk-known-bugs "Known Bugs")
                       ,(link-stuff url-helpdesk-patches "Downloadable Patches"))
                   (p "The PLT software is installed on this machine at" (br)
                      (pre nbsp nbsp ,(path->string (find-collects-dir)))))))))]
           [_
            (let-values ([(right-header right-items)
                          (page-tag->title+items subpage)])
              (cond
                [(and (eq? (helpdesk-platform) 'internal-browser-simple)
                      (equal? subpage "home"))
                 ;; change the "home" page for internal HelpDesk with no menus
                 (html-subpage "PLT Scheme Help Desk: Home"
                               (html-top initial-request)
                               "home"
                               right-header
                               (append (left-items)
                                       `(((p (i "Version: " ,(plt-version)))))))]
                [else
                 (html-subpage "PLT Scheme Help Desk: Home"
                               (html-top initial-request)
                               (left-items)
                               right-header right-items)]))])))))
  

  (define (left-items)
    `(-- -- -- -- --
      ("Get help: "
       nbsp nbsp nbsp nbsp
       (b (a ((href ,url-helpdesk-help)) "Help Desk"))
       ,@(br*))
      -- --
      ("Learn to program in Scheme: "
       nbsp nbsp nbsp nbsp
       "Reference: "
       'nbsp
       (a ((href ,url-helpdesk-manuals))   "Manuals") ", "
       (a ((href ,url-helpdesk-libraries))  "Libraries")
       (br) nbsp nbsp nbsp nbsp
       (b (a ((href ,url-helpdesk-program-design)) "Program Design: "))
       'nbsp
       (a ((href ,url-helpdesk-books)) "Books") ", "
       (a ((href ,url-helpdesk-languages)) "Languages") ", "
       (a ((href ,url-helpdesk-teachpacks)) "Teachpacks")
       ,@(br*))
      -- --
      ("How to run programs: "
       nbsp nbsp nbsp nbsp (b (a ((href ,url-helpdesk-software)) "Software: "))
       'nbsp
       (a ((href ,url-helpdesk-tour))      "Tour") ", "
       (a ((href ,url-helpdesk-drscheme))  "DrScheme") ", "
       (a ((href ,url-helpdesk-release))   "Release")
       ,@(br*)
       ;; (a ((href ,url-helpdesk-drscheme-faq)) "FAQ") ; Moved to DrScheme page
       )
      -- --
      ("Get involved:"
       nbsp nbsp nbsp nbsp
       (a ((href ,url-helpdesk-mailing-lists)) "Mailing Lists")
       ,@(case (helpdesk-platform)
           [(external-browser)
            `(", " (a ((href ,url-external-send-bug-report)) "Send a bug report"))]
           [else '()])
       ,@(br*))
      -- --
      (""
       " " " "
       ,@(case (helpdesk-platform)
           [(internal-browser internal-browser-simple)
            '((b (a ([mzscheme
                      "((dynamic-require '(lib |bug-report.ss| |help|) 'help-desk:report-bug))"])
                    (font ([color "forestgreen"]) "Send a bug report")))
              nbsp nbsp)]
           [else `()])
       ;; DrScheme Acknowledgements
       ,@(case (helpdesk-platform)
           [(internal-browser internal-browser-simple)
            `((b (a ((href ,url-helpdesk-acknowledge))
                    (font ([color "forestgreen"])"Acknowledgments"))))]
           [else '()]))
      -- -- -- --))

  ;; page-tag->title+items : string -> (values string list-of-right-items)
  (define (page-tag->title+items page-tag)
    (match (assoc page-tag easy-pages)
      [#f                 (page-tag->title+items "home")]
      [(tag header body)  (values header body)]))
  
  ;; generate-index-for-static-pages : -> list-of-index-entries
  ; used by install.ss to generate hdindex
  (define (generate-index-for-static-pages)
    ; (<indexed-item> <url> <label-within-html-file> <page-title>)
    (map (match-lambda
           [(subpage page-title . more)
            (let ([url (format "/servlets/home.ss?subpage=~a" subpage)])
              `(,page-title ,url "" ,page-title))])
         easy-pages))
  
  ;; static subpages
  ;;  - In ALPHABETICAL order
  (define easy-pages
    `(("about-the-master-index" "About the Master Index"
       ((p "The master index is a list of all keywords present in the html documentation.")
        (p (a ([href ,url-helpdesk-master-index]) "The Master Index"))))
      ;;
      ("acknowledge" "Acknowledgements"
       ((p ,(get-general-acks))
        (p ,(get-translating-acks))))
      ;;
      ("activex" "How to use ActiveX components"
       ((p  ; (a ([name "com"] [value "COM"]))
            ;(a ([name "activex"] [value "ActiveX"]))
         "If you run Windows, you can use MysterX, a library for "
         "controlling COM and ActiveX components within DrScheme, "
         "MzScheme, or MrEd.  MysterX is available from ")
        (pre
         nbsp nbsp
         (a ([href "http://www.plt-scheme.org/software/mysterx/"])
            "http://www.plt-scheme.org/software/mysterx/"))
        #;(p ,(collection-doc-link "mysterx" "The MysterX collection"))))
      ;;
      ("batch" "How to write Windows batch files"
       ((p "You can put MzScheme code in a Windows batch file, that is, a "
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
           "parameters.")
        (p "In Windows NT, Windows 2000, and Windows XP, you can instead write "
           (pre
            " ; @echo off" (br)
            " ; d:\\plt\\mzscheme -r %0 %*" (br)
            " ; goto :end" (br)
            "   ... " (i "scheme-program") " ..." (br)
            " ; :end")
           "This code allows an arbitrary number of parameters to your " 
           "batch file.")
        (p "The batch file code works by combining both batch and MzScheme "
           "syntax in a single file.  When invoked from the command line, "
           "the semicolons are ignored.  The second line invokes MzScheme "
           "with the batch file as an argument.  MzScheme interprets the "
           "lines beginning with semicolons as comments, and runs the "
           "Scheme code.  When the Scheme program is "
           "done, control returns to the batch file, and the "
           (tt  "goto") " jumps around the Scheme code.")))
      ;;
      ("books" "Books"
       ((h3 "HTDP - How to Design Programs")
        (p (a ([href "http://www.htdp.org/"]) "'How to Design Programs -"
              " An Introduction to Programming and Computing'") 
           (br) " by Matthias Felleisen, Robert Bruce Findler, Matthew Flatt, and Shriram Krishnamurthi")
        (p (a ([href "http://www.ccs.neu.edu/home/matthias/htdp-plus.html"]) "HTDP+")
           (br) " Supplemental Materials for 'How to Design Programs'")
        (h3 "Teach Yourself Scheme in Fixnum Days")
        (p (a ((href, url-helpdesk-teach-yourself)) " Teach Yourself Scheme in Fixnum Days")
           (br) "- an introduction to Scheme by Dorai Sitaram")))
      ;;
      ("cgi" "How to write CGI scripts"
       ((p "Type " (tt  "CGI") " in the " (b "Search for") " "
           "field in Help Desk and click on the "
           (b (tt "Search")) " button to get information "
           "on CGI-related functions.")
        (p "A CGI script is merely a program with funny inputs and "
           "outputs.  Input comes either from an environment variable "
           "or through the standard input port, in a special format. "
           "Output consists of a MIME header followed by the content.  "
           "Everything in-between is pure program.")
        (p "MzScheme comes with a CGI library that is designed to "
           "make it easy to write such scripts.  In the mini-tutorial "
           "below, we'll walk you through the "
           "construction of such a script.  If you have questions or "
           "comments, send email to "
           (a ((href "mailto:sk@plt-scheme.org")) "sk@plt-scheme.org") ".")
        (hr)
        (p "Let's write a simple \"finger server\" in MzScheme. "
           "The front-end will be a Web form that accepts a username. "
           "The form should supply a username in the field `name'. "
           "The CGI script fingers that user.")
        (p "First, make sure you have MzScheme installed on the host "
           "where your Web server is located.")
        (p "A CGI script must be an executable.  Each OS has different "
           "ways of launching an application.  Under Unix, it's "
           "probably easiest to make them simple shell scripts.  "
           "Therefore, place the following magic incantation at the "
           "top of your script:")
        (p (pre " #!/bin/sh" (br)
                " string=? ; exec /usr/local/bin/mzscheme -r $0 \"$@\""))
        (p "Make sure the path to MzScheme is specified correctly.")
        (p "Now we're in Scheme-land.  First, let's load the Scheme "
           "CGI library and define where `finger' resides.")
        (p (pre " (require (lib \"cgi.ss\" \"net\"))" (br)
                " (define finger-program \"/usr/bin/finger\")"))
        (p "Next we must get the names bound by the form, and "
           "extract the username field.")
        (p (pre  " (let ((bindings (get-bindings)))" (br)
                 "   (let ((name (extract-binding/single 'name bindings)))"))
        (p "We use extract-binding/single to make sure only one name "
           "field was bound.  (You can bind the same field multiple "
           "times using check-boxes.  This is just one kind of "
           "error-checking; a robust CGI script will do more.")
        (p "Next we invoke the finger program using `process*'.  "
           "If no username was specified, we just run finger on the host.")
        (p (pre " (let ((results (if (string=? name \"\"))" (br)
                "   (process* finger-program)" (br)
                "   (process* finger-program name))))"))
        (p "The `process*' function returns a list of several values. "
           "The first of these is the output port.  Let's pull this "
           "out and name it.")
        (p (pre " (let ((proc->self (car results)))"))
        (p "Now we extract the output of running finger into a "
           "list of strings.")
        (p (pre " (let ((strings (let loop " (br)
                "   (let ((l (read-line proc->self)))" (br)
                "     (if (eof-object? l)" (br)
                "        null" (br)
                "        (cons l (loop))))))))"))
        (p "All that's left is to print this out to the user.  "
           "We use the `generate-html-output' procedure to do that, "
           "which takes care of generating the appropriate MIME header "
           "(as required of CGI scripts). "
           "Note that the <pre> tag of HTML doesn't prevent its "
           "contents from being processed.  To avoid this "
           "(i.e., to generate truly verbatim output), "
           "we use `string->html', which knows about HTML quoting "
           "conventions.")
        (p (pre " (generate-html-output \"Finger Gateway Output\"" (br)
                "   (append " (br)
                "    '(\"<pre>\")" (br)
                "    (map string->html strings)" (br)
                "    '(\"</pre>\"))))))))"))
        (p "That's all!  This program will work irrespective of "
           "whether the form uses a GET or POST method to send its "
           "data over, which gives designers additional flexibility "
           "(GET provides a weak form of persistence, while "
           "POST is more robust and better suited to large volumes of "
           "data).")
        (p "Here's the entire program, once again:"
           (pre " #!/bin/sh" (br)
                " string=? ; exec /usr/local/bin/mzscheme -r $0 \"$@\"" (br)
                "" (br)
                " (require (lib \"cgi.ss\" \"net\"))" (br)
                " (define finger-program \"/usr/bin/finger\")" (br)
                "" (br)
                " (let ((bindings (get-bindings)))" (br)
                "   (let ((name (extract-binding/single 'name bindings)))" (br)
                "    (let ((results (if (string=? name "")" (br)
                "      (process* finger-program)" (br)
                "     (process* finger-program name))))" (br)
                "     (let ((proc->self (car results)))" (br)
                "       (let ((strings (let loop " (br)
                "                        (let ((l (read-line proc->self)))" (br)
                "                          (if (eof-object? l)" (br)
                "                            null" (br)
                "                            (cons l (loop)))))))" (br)
                "         (generate-html-output \"Finger Gateway Output\"" (br)
                "         (append" (br)
                "           '(\"<pre>\")" (br)
                "           (map string->html strings)" (br)
                "           '(\"</pre>\"))))))))"))))
      ;;
      ("databases" "Databases"
       ((p "For ODBC databases see " (a ([href ,url-helpdesk-srpersist]) "SrPersist") ".")
        (p "For bindings to MySQL, SQLite, PostGreSQL, and more see " 
           (a ([href ,url-external-planet]) "PLaneT") ".")))
      ;; was: /servlets/scheme/doc.ss
      ("documentation" "Documentation"
       (,(make-green-header-text "How to use DrScheme")
        (p (a ([href ,url-helpdesk-drscheme]) "DrScheme")
           " provides information about using the DrScheme development environment.")
        ,(make-green-header-text "Languages and Libraries")
        (p "Language and library documentation is distributed among several"
           " manuals, plus a number of plain-text files describing small library"
           " collections.")
        (p "When you " (a ([href ,url-helpdesk-how-to-search]) "search") ","
           " Help Desk groups the results by manual and collection.  The manuals"
           " are ordered from the most-used documentation (e.g., R5RS Scheme) to"
           " the least-used (e.g., MzScheme internals), and all manuals precede"
           " library collections.")
        (p "The PLT distribution archive includes a partial set of documentation."
           "  A hyperlink in this partial set may refer to a manual that is"
           " missing from the distribution.  If you follow such a link, Help Desk"
           " provides a special page for automatically downloading and installing"
           " the missing manual.  For certain manuals, the PLT distribution"
           " includes a searchable index file rather than the whole manual, so a"
           " search result link might refer to a missing manual.")
        (ul (li (b (a ([href ,url-helpdesk-manuals]) "Manuals"))
                ": List the currently installed and uninstalled manuals"))
        ,(make-green-header-text "Searching")
        (p (a ([href ,url-helpdesk-how-to-search]) "Searching")
           " in Help Desk finds documenation from all sources, including ")
        (p (a ([href ,url-helpdesk-drscheme]) "DrScheme")
           " and the language and library documentation.")))
      ;;
      ("drscheme" "DrScheme"
       ((p "DrScheme is PLT's flagship programming environment")
        (ul (li (a ([href ,url-helpdesk-tour])
                   (b "Tour: ") "An introduction to DrScheme"))
            (li (a ([href ,url-helpdesk-interface-essentials])
                   "Quick-start jump into the user manual"))
            (li (a ([href ,url-helpdesk-languages])
                   "Languages: ")
                "supported by DrScheme")
            (li (a ([href ,url-helpdesk-drscheme-manual])
                   "PLT DrScheme: Programming Environment Manual")
                (br)
                "The complete user manual")
            (li (a ([href ,url-helpdesk-drscheme-faq]) "FAQ")
                ": DrScheme Frequently asked questions")
            (li (a ([href ,url-helpdesk-why-drscheme])
                   "Why DrScheme?")))))
      ;;
      ("graphics" "How to write graphics programs"
       ((p ; (a ([name "gfx"] [value "Graphics"]))
           ; (a ([name "gui"] [value "GUIs"]))
           ; (a ([name "gui2"] [value "Graphical User Interfaces"]))
         "To write graphics programs, use DrScheme with the "
         "Graphical (MrEd) flavor of the PLT " 
         (a ([href "/servlets/scheme/what.ss"]) " language") ". "
         "MrEd provides a complete GUI toolbox that is described "
         "in the manual for MrEd." 
         ; TODO: make MrEd a link ,(main-manual-page "mred")
         (p "For simple graphics programs, you may also use the "
            "viewport-based graphics library, which is described in "
            ,(manual-entry "misclib" "viewport" "Viewport Graphics") ". "
            "The following declaration loads viewport graphics into MrEd:"
            (pre " (require (lib \"graphics.ss\" \"graphics\"))")))))
      ;;
      ("home" "PLT Help Desk Home"
       ((p "The Help Desk is a complete source of information about PLT software, "
           "including DrScheme, MzScheme and MrEd.")
        (p "There are two ways to find information in the Help Desk: searching and browsing.")
        (h3 "Search the Help Desk")
        (p "Search for keywords, index entries or raw text in the documentation pages"
           (ul (li (i "Keywords: ") "are Scheme names, such as " (b "define") " and " (b "cons")".")
               (li (i "Index entries: ") "are topical phrases, such as 'lists'.")
               (li (i "Raw text: ") "are fragments of text from the documentation pages. "
                   "Use only as a last resort."))
           "The Help Desk search results are sorted according to their source.")
        (h3 "Browse the Help Desk")
        (ul (li "The " (b "Home")    " link will take you back to this page.")
            (li "The " (b "Manuals") " link displays a list of manuals and other documentation")
            #;(li "The " (b "Send a bug report") " link allows you to submit a bug report to PLT."))))
      ;;
      ("how-to-do-things-in-scheme" "How to do things in Scheme"
       ((p (ul 
            (li (a ([href ,url-helpdesk-stand-alone]) "How to build a stand-alone executable"))
            (li (a ([href ,url-helpdesk-graphics])   "How to write graphics programs"))
            (li (a ([href ,url-helpdesk-script])     "How to write Unix shell scripts"))
            (li (a ([href ,url-helpdesk-batch])      "How to write Windows batch files"))
            (li (a ([href ,url-helpdesk-cgi])        "How to write CGI scripts"))
            (li (a ([href ,url-helpdesk-databases])  "How to connect to databases"))
            (li (a ([href ,url-helpdesk-system])     "How to call low-level system routines"))))
        (p "If you didn't find what you're looking for in the list above, try "
           "searching in Help Desk.  Also, check "
           (a ((href "http://www.htus.org/")) (i "How to Use Scheme")) ".")))
      ;;
      ("how-to-search" "PLT Help Desk"
       ((p "The Help Desk is a complete source of information about PLT software, "
           "including DrScheme, MzScheme and MrEd.")
        (p "There are two ways to find information in the Help Desk: searching and browsing.")
        (h3 "Search the Help Desk")
        (p "Search for keywords, index entries or raw text in the documentation pages"
           (ul (li (i "Keywords: ") "are Scheme names, such as " (b "define") " and " (b "cons")".")
               (li (i "Index entries: ") "are topical phrases, such as 'lists'.")
               (li (i "Raw text: ") "are fragments of text from the documentation pages. "
                   "Use only as a last resort."))
           "The Help Desk search results are sorted according to their source.")
        (h3 "Browse the Help Desk")
        (ul (li "The " (b "Home")    " link will take you back to this page.")
            (li "The " (b "Manuals") " link displays a list of manuals and other documentation")
            #;(li "The " (b "Send a bug report") " link allows you to submit a bug report to PLT."))))
      ;;
      ("known-bugs" "Known Bugs"
       ((p "For an up-to-date list of bug reports, see the "
           (a ([href "http://bugs.plt-scheme.org/query/"] [target "_top"])
              "PLT bug report query page") ".")))
      ;;
      ("languages" "Scheme Languages"
       ((p "DrScheme supports many dialects of Scheme. "
           "The following dialects are specifically designed for teaching "
           "computer science. In DrScheme's "
           ;; TODO: (a ([href "/servlets/scheme/what.ss#lang-sel"]) "language selection menu")
           (b "Language selection menu") ", "
           "they are found under the heading "
           (b "How to Design Programs") "."
           (ul (li (b "Beginning Student") " is a pedagogical version of Scheme "
                   "that is tailored for beginning computer science students.")
               (li (b "Beginning Student with List Abbreviations") " extends Beginning Student "
                   "with convenient (but potentially confusing) "
                   "ways to write lists, including quasiquote.")
               (li (b "Intermediate Student") " adds local bindings and higher-order functions.")
               (li (b "Intermediate Student with Lambda") " adds anonymous functions.")
               (li (b "Advanced Student") " adds mutable state.")))
        (p "The " (b "The Essentials of Programming Languages")
           " language is designed for use with the MIT Press textbook with that name.")
        (p "Other dialects are designed for practicing programmers. "
           "The R5RS language is a standard dialect of Scheme that is defined by the "
           "Revised^5 Report on the Algorithmic Language Scheme. "
           "In DrScheme's language selection menu, the following languages are found under the heading PLT: ")
        (ul (li (b "Textual (MzScheme)") " is a superset of R5RS Scheme. "
                "In addition to the base Scheme language, PLT Scheme provides "
                "exceptions, threads, objects, modules, components, regular expressions, "
                "TCP support, filesystem utilities, and process control operations. "
                "This language is defined in PLT MzScheme: Language Manual. ")
            (li (b "Graphical (MrEd)") " includes the Textual (MzScheme) language "
                "and adds a graphical toolbox, described in PLT MrEd: Graphical Toolbox Manual.")
            (li (b "Pretty Big") " is a superset of the Graphical (MrEd) language, "
                "and adds forms from the Pretty Big language. "
                "For those forms that are in both languages, Pretty Big behaves like Graphical (MrEd)."))
        (p "The " (b "module language") " supports development using PLT Scheme's module form, "
           "where the module's language is explicitly declared in the code.")
        (p "See the DrScheme manual for further details on the languages, especially the "
           "teaching languages.")
        (p "DrScheme's set of languages can be extended, so the above list mentions only "
           "the languages installed by default. "
           "Documentation for all languages is available through the manuals page.")))
      ;;
      ("libraries" "Libraries"
       ((h3 "Built-in Libraries")
        (p "PLT Scheme has a lot of libraries. The core libraries are described in "
           (a ((href ,url-helpdesk-mzlib)) "PLT MzLib: Libraries Manual"))
        (p "See the " (a ((href ,url-helpdesk-manuals)) "Manuals") " page for more.")
        (h3 "User / PLaneT Libraries")
        (p (a ((href ,url-external-planet)) "PLaneT") " is the repository for user contributed libraries. "
           "Join the PLaneT announcement mailing list to get notified on new PLaneT packages.")))
      ;;
      ("license" "License"
       ((h2 "PLT Software") 
        (b ,(format "Copyright (c) ~a PLT Scheme Inc." copyright-year))
        (p "PLT software is distributed under the GNU Library General Public "
           " License (LGPL).  This means you can link PLT software (such as "
           "MzScheme or MrEd) into proprietary applications, provided you follow "
           "the specific rules stated in the LGPL.  You can also modify PLT "
           "software; if you distribute a modified version, you must distribute it "
           "under the terms of the LGPL, which in particular means that you must "
           "release the source code for the modified software. See "
           (a ([href ,(format "/servlets/doc-anchor.ss?~a&file=~a"
                              "name=COPYING.LIB&caption=Copying PLT software"
                              (uri-encode
                               (path->string
                                (simplify-path (build-path (find-doc-dir)
                                                           "release-notes"
                                                           "COPYING.LIB")))))])
              "COPYING.LIB")
           " for more information.")
        (p "PLT software includes or extends the following copyrighted material:"
           ,@(map
              (lambda (ss) `(ul (li ,@(map (lambda (s) `(div ,s (br))) ss))))
              `(("DrScheme"
                 ,(format "Copyright (c) 1995-~a PLT" copyright-year)
                 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("MrEd"
                 ,(format "Copyright (c) 1995-~a PLT" copyright-year)
                 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("MzScheme"
                 ,(format "Copyright (c) 1995-~a PLT" copyright-year)
                 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("libscheme"
                 "Copyright (c) 1994 Brent Benson"
                 "All rights reserved.")
                ("wxWindows"
                 ,(string-append "Copyright (c) 1994 Artificial Intelligence Applications Institute, "
                                 "The University of Edinburgh")
                 "All rights reserved.")
                ("wxWindows Xt"
                 ,(string-append "Copyright (c) 1994 Artificial Intelligence Applications Institute, "
                                 "The University of Edinburgh")
                 "Copyright (c) 1995 GNU (Markus Holzem)"
                 "All rights reserved.")
                ("Conservative garbage collector"
                 "Copyright (c) 1988, 1989 Hans-J. Boehm, Alan J. Demers"
                 "Copyright (c) 1991-1996 Xerox Corporation"
                 "Copyright (c) 1996-1999 Silicon Graphics"
                 "Copyright (c) 1999-2001 by Hewlett-Packard Company"
                 "All rights reserved.")
                ("Collector C++ extension by Jesse Hull and John Ellis"
                 "Copyright (c) 1994 Xerox Corporation"
                 "All rights reserved.")
                ("The A List"
                 "Copyright (c) 1997-2000 Kyle Hammond."
                 "All rights reserved.")
                ("Independent JPEG Group library"
                 "Copyright (c) 1991-1998 Thomas G. Lane."
                 "All rights reserved.")
                ("libpng"
                 "Copyright (c) 2000-2002 Glenn Randers-Pehrson"
                 "All rights reserved.")
                ("zlib"
                 "Copyright (c) 1995-2002 Jean-loup Gailly and Mark Adler"
                 "All rights reserved.")
                ("GNU MP Library"
                 "Copyright (c) 1992, 1993, 1994, 1996 by Free Software Foundation, Inc.")
                ("GNU lightning"
                 "Copyright (c) 1994, 1995, 1996, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.")
                ("GNU Classpath"
                 "GNU Public License with special exception"))))))
      ;;
      ("mailing-lists" "Mailing Lists"
       ((p "There are two mailing lists: the discussion list and the announcements only list.")
        (h3 "Archives")
        (p "The lists are archived:"
           (ul (li (a ([href ,url-external-discussion-list-archive]) "Discussions")
                   " - " (a ([href ,url-external-discussion-list-archive-old]) "(old archive)"))
               (li (a ([href ,url-external-announcement-list-archive]) "Announcements only"))))
        (h3 "Subscribing")
        (p "Visit the "
           (a ((href ,url-external-mailing-list-subscription))
              "subscription page")
           " to join the mailing lists.")))
      ;;
      ("mrflow" "MrFlow"
       ((p "MrFlow is a user friendly, interactive static debugger for DrScheme that"
           (ul (li "highlights operations that may cause errors;")
               (li "computes invariants describing the set of values each program expression can assume; and")
               (li "provides a graphical explanation for each invariant.")))
        (p "The programmer can browse this information, and then resume program development "
           "with an improved understanding of the program's execution behavior, and in "
           "particular of potential run-time errors.")
        (p "See the " (a ([href ,url-external-mrflow]) "MrFlow") " web-site.")))
      ;;
      ("mrspidey" "MrSpidey"
       ((p "MrSpidey is a static debugger for DrScheme v103p1.")
        (p "See the " (a ([href ,url-external-mrspidey]) "MrSpidey") " web-site.")))
      ;;
      ("mzcom" "MzCom"
       ((p "MzCOM is a COM class containing an embedded MzScheme. With MzCOM, "
           "you can run Scheme code from your favorite COM host environment, "
           "such as Visual BASIC, Delphi, Visual FoxPro, Visual C++, or even PLT's MysterX.")
        (p "See the " (a ([href ,url-external-mzcom]) "MzCom") " web-site.")))
      ;;
      ("mysterx" "MysterX"
       ((p "MysterX (\"Mister X\") is a toolkit for building Windows applications "
           "within DrScheme or MzScheme using ActiveX and COM components. "
           "Dynamic HTML is used for component presentation and event-handling.")
        (p "See the " (a ([href ,url-external-mysterx]) "MysterX") " web-site.")))
      ;; 
      ("note-on-language-levels" "A Note on Language Levels"
       ((p "DrScheme presents Scheme via a hierarchy of "
           (a ([href ,url-helpdesk-languages]) "language levels") ".")
        (p "We designed the teaching languages based upon our observations of"
           " students in classes and labs over several years. Beginning students"
           " tend to make small notational mistakes that produce "
           (em  "syntactically legal") " Scheme programs with a "
           (em "radically different meaning") " than the one intended."
           " Even the best students are then surprised by error messages, which"
           " might mention concepts not covered in classes, or other unexpected"
           " behavior.")
        (p "The teaching levels are not ideal for instructors.  They are"
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
           " read-eval-print loop when the student clicks the Execute button."
           " In short, Teachpacks provide students the best of both worlds:"
           " protection from wanton error messages and unexpected behavior, and"
           " powerful support from the instructor.")
        (p "We strongly encourage instructors to employ language levels and"
           " Teachpacks. In our experience, the restriction of the teaching"
           " languages do not interfere with students' programming needs up to,"
           " and including, junior-level courses on programming languages.  It"
           " gives students a more productive learning experience than raw Scheme,"
           " and simplifies the interface between library and user code.")
        (p "We also strongly encourage students to point out this page to their"
           " instructors.")
        (p "Please follow the links on this page for more information.  If you"
           " have additional questions or comments, please contact us at "
           (a ((href "mailto:scheme@plt-scheme.org")) "scheme@plt-scheme.org") ".")))
      ;;
      ("odbc" "ODBC"
       ((p "See " (a ([href ,url-helpdesk-srpersist]) "SrPersist") ".")))
      ;;
      ("patches" "Downloadable Patches"
       ((p "The following Web page may contain downloadable patches to fix "
           "serious bugs in version " ,(version) " of the PLT software:")
        (p nbsp nbsp
           ,(let ([url (format "http://download.plt-scheme.org/patches/~a/"
                               (version))])
              `(a ([href ,url] [target "_top"]) ,url)))))
      ;;
      ("program-design" "Program Design"
       ((h3 "For Students")
        (p "The textbook " (a ((href "http://www.htdp.org")) "How to Design Programs")
           " provides an introduction to programming using the DrScheme environment. "
           "The Help Desk provides the following interactve support for the text book: "
           (a ((href ,url-helpdesk-teachpacks)) "Teachpack documentation"))
        (h3 "For Experienced Programmers")
        (p (a ((href ,url-helpdesk-teach-yourself)) "Teach Yourself Scheme in a Fixnum Days")
           ": For programmers with lots of experience in other languages")
        (h3 "For Teachers and Researchers")
        (p (a ((href ,url-helpdesk-why-drscheme)) "PLT's vision"))))
      ;;
      ("release-notes" "Release Notes"
       ((h1 "Release Notes for PLT Scheme version " ,(version))
        (a ([name "relnotes"] [VALUE "Release notes"]))
        (p "Detailed release notes:"
           (ul
            ,@(let ()
                (define (make-release-notes-entry s)
                  (match s
                    [(label dir filename)
                     (let ([file (build-path (find-doc-dir) "release-notes" dir filename)])
                       (if (file-exists? file)
                         `(li (a ([href ,(format
                                          "/servlets/doc-anchor.ss?file=~a&name=~a&caption=~a"
                                          (uri-encode (path->string file))
                                          filename
                                          label)])
                                 ,label))
                         #f))]))
                (filter
                 values ; delete #f entries
                 (map make-release-notes-entry
                      '(("DrScheme release notes"     "drscheme"  "HISTORY")
                        ("Teachpack release notes"    "teachpack" "HISTORY")
                        ("MzScheme version 300 notes" "mzscheme"  "MzScheme_300.txt")
                        ("MzScheme release notes"     "mzscheme"  "HISTORY")
                        ("MrEd release notes"         "mred"      "HISTORY")
                        ("Stepper release notes"      "stepper"   "HISTORY")
                        ("MrFlow release notes"       "mrflow"    "HISTORY")))))))))
      ;;
      ("script" "How to write Unix shell scripts"
       ((p "When MzScheme is installed as part of the standard Unix "
           "PLT distribution, " (TT "plt/bin/mzscheme") " and  " 
           (TT "plt/bin/mred") " are binary executables.") 
        (p "Thus, they can be used with Unix's " (TT  "#!") 
           " convention as follows:"
           (pre "  #! /usr/local/lib/plt/bin/mzscheme -r  ... " (br) 
                " " (I  "scheme-program") " ...")
           "assuming that the " (tt  "plt") " tree is installed as " 
           (tt "/usr/local/lib/plt") ". "
           "To avoid specifying an absolute path, use " 
           (tt "/usr/bin/env") ":"
           (pre " #! /usr/bin/env mzscheme -r  ... " (br)
                " " (i  "scheme-program") " ...")
           (p "The above works when " 
              (tt  "mzscheme") " is in the user's path. "
              "The " (tt "mred") " executable can be used in the "
              "same way for GUI scripts.")
           (p "Within " (i "scheme-program") ", " 
              (tt  "(current-command-line-arguments)") 
              " produces a vector of strings for the arguments "
              "passed to the script. The vector is also available as " 
              (tt "argv") "."))))
      ;;
      ("srpersist" "SrPersist"
       ((p "SrPersist (\"Sister Persist\") is a set of Scheme bindings for the Open "
           "Database Connectivity (ODBC) standard.")
        (p "See the " (a ([href ,url-external-srpersist ]) "SrPersist") " web-site.")))
      ;;
      ("software" "Software"
       ((ul (li (a ((href ,url-helpdesk-drscheme)) "DrScheme") ": The programming environment")
            (li (a ((href ,url-helpdesk-languages)) "Languages") ": The family of languages "
                "supported by PLT Software")
            ;; (li (a ((href ,url-helpdesk-documentation)) "Documentation")
            ;;      ": Organization and manuals")
            ;; (li (a ((href ,url-helpdesk-hints)) "Hints")
            ;;      ": How to do things in Scheme")
            )))
      ;;
      ("stand-alone" "How to build a stand-alone Executable"
       ((p "To create stand-alone executables, use DrScheme's "
           (tt "Scheme | Create Executable ...")
           " menu item. This menu is sensitive to the language levels; "
           "the " (tt "module") " language permits the most flexibility "
           "in creating executables.")
        (p "The mzc compiler provides a more low-level interface "
           "to stand-alone executables creation. "
           "See " ,(main-manual-page "mzc") " for more information.")))
      ;;
      ("system" "How to call low-level system routines"
       ((p "To call low-level system routines, you must write "
           "an extension to MzScheme using the C programming language. "
           "See Inside MzScheme"
           ; TODO: #;,(main-manual-page "insidemz") 
           " for details.")))
      ;;
      ("teachpacks" "Teachpacks"
       ((ul (li (a ((href ,url-helpdesk-teachpacks-for-htdp))
                   "Teachpacks for 'How to Design Programs'"))
            (li (a ((href ,url-helpdesk-teachpacks-for-htdc))
                   "Teachpacks for 'How to Design Classes'")))))
      ;;
      ("teachscheme" "TeachScheme"
       ((h2  "TeachScheme! Workshops")
        (p "TeachScheme! is a free summer workshop for high school teachers. "
           "Its goal is to bridge the gulf between high school and "
           "college-level computing curricula.  In the workshop, programming "
           "is taught as an algebraic problem-solving process, and computing "
           "is the natural generalization of grade-school level calculating." )
        (p "Students who learn to design programs properly learn to "
           "analyze a problem statement; express its essence, abstractly "
           "and with examples; formulate statements and comments in a "
           "precise language; evaluate and revise these activities in "
           "light of checks and tests; and pay attention to details. "
           "As a result, all students benefit, those who wish to study computing "
           "as well as those who just wish to explore the subject.")
        (p "For more information, see the "
           (a ([href "http://www.teach-scheme.org/Workshops/"]
               [TARGET "_top"])
              "TeachScheme! Workshops page") ".")))
      ;;
      ("tour" "Tour of DrScheme"
       ((p "Take a " (a ([href ,url-external-tour-of-drscheme]) "Tour of DrScheme")
           " and discover the wealth of features of the interactive, "
           "integrated programming environment.")))
      ;;
      ("why-drscheme" "Why DrScheme?"
       ((p "Teaching introductory computing courses with Scheme, or any other "
           "functional programming language, facilitates many conceptual tasks "
           "and greatly enhances the appeal of computer science. Specifically, "
           "students can implement many interesting programs with just a small "
           "subset of the language. The execution "
           "of a functional program can be explained with simple reduction "
           "rules that students mostly know from "
           "secondary school. Interactive implementations allow for quick "
           "feedback to the programmers andmake the "
           "development of small functions a pleasant experience.")
        (p "Unfortunately, the poor quality of the available environments "
           "for functional languages negates these advantages. Typical "
           "implementations accept too many definitions, that is, definitions "
           "that are syntactically well-formed in the sense of the full "
           "language but meaningless for beginners. The results are inexplicable "
           "behavior, incomprehensible run-time errors, or confusing type "
           "error messages. The imperative nature of "
           "read-eval-print loops often introduces subtle bugs into otherwise "
           "perfect program developments. Scheme, in "
           "particular, suffers from an adherence to Lisp's output traditions, "
           "which often produces confusing effects. "
           "In many cases students, especially those familiar with commercial C++ "
           "environments, mistake these problems "
           "for problems with the functional approach and reject the approach itself. ")
        (p "To overcome this obstacle, we have developed a new programming "
           "environment for Scheme. It fully integrates a "
           "(graphics-enriched) editor, a multi-lingual parser that can process a "
           "hierarchy of syntactically restrictive "
           "variants of Scheme, a functional read-eval-print loop, and an "
           "algebraically sensible printer. The environment "
           "catches the typical syntactic mistakes of beginners and pinpoints "
           "the exact source location of run-time "
           "exceptions. The new programming environment also provides an "
           "algebraic stepper and a static debugger. The "
           "former reduces Scheme programs, including programs with assignment "
           "and control effects, to values (and effects). "
           "The static debugger infers what set of values an expression may "
           "produce and how values flow from expressions "
           "into variables. It exposes potential safety violations and, upon "
           "demand from the programmer, explains its "
           "reasoning by drawing value flowgraphs over the program text. "
           "Preliminary experience with the environment shows "
           "that students find it helpful and that they greatly prefer it to "
           "shell-based or Emacs-based systems.")
        (p "A paper that discusses DrScheme in more detail is available in the paper: "
           (a ((href "http://www.ccs.neu.edu/scheme/pubs/#jfp01-fcffksf"))
              "DrScheme: A Programming Environment for Scheme."))))
      )))
