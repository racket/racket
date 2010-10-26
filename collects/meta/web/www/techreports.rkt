#lang meta/web

(require "resources.rkt" "people.rkt" "../download/data.rkt"
         "bib.rkt" (prefix-in - version/utils))

;; New style TR entries -------------------------------------------------------

(define (-all-techreports-)
  (list (TR 1 'reference "Reference: Racket" '(mflatt plt)
            #:description "Racket Programming Language")
        (TR 2 'drracket "DrRacket: Programming Environment" '(robby plt)
            #:description "DrRacket API"
            #:note
            @list{
              Preferred generic citation:
              @blockquote[style: "margin-top: 0.5ex; margin-bottom: 0.5ex;"]{
                R. B. Findler, J. Clements, C. Flanagan, M. Flatt,
                S. Krishnamurthi, P. Steckler and M. Felleisen. @br
                @i{DrScheme: A programming environment for Scheme.} @br
                Journal of Functional Programming, 12(2): 159–182, March 2002.}
              Please cite the DrRacket technical report only if internal
              details of DrRacket are concerned, otherwise use the DrScheme
              reference.})
        (TR 3 'gui "GUI: Racket Graphics Toolkit" '(mflatt robby clements)
            #:description "Racket Graphics Toolkit")))

(define (doc-url doc [fmt 'html] [ver 'recent])
  (format "http://download.racket-lang.org/docs/~a/~a/~a~a" ver fmt doc
          (if (eq? 'html fmt) "" (format ".~a" fmt))))

(define (TR num docname title authors* #:description [desc title]
            #:note [note #f])
  (define tr-name @list{PLT-TR-2010-@num})
  (define author-strings
    (map (lambda (a) (if (eq? 'plt a) "PLT" (person-bibname (find-person a))))
         authors*))
  (define (link fmt [ver 'recent]) @a[href: (doc-url docname fmt ver)]{[@fmt]})
  (define (title-line link?)
    @list{@big{@b{@(if link? cite-page values){@tr-name}}}
          @|nbsp nbsp| @small{@link['html] @link['pdf]}})
  (define (content)
    @list{@(add-between author-strings " and ") @br
          @i{@title} @br
          PLT Technical Report #@num @br
          @(and note @div[style: "margin-top: 1ex; font-size: small;"]{
                       @note})})
  (define ((refblock . title) . body)
    @list{@h2{@title}
          @blockquote{@PRE{@body}}})
  (define cite-page
    @page[#:file (format "tr~a/" num) #:title tr-name #:part-of 'learning]{
      @h1{@title-line[#f]}
      @p*{@blockquote{@big{@content}}
          @~ For citations of the @desc, please use @TT{\cite{plt-tr@num}} in
             LaTeX, or @TT|{@cite[plt-tr1]}| in Scribble, using the definitions
             below.
          @~ For references to specific releases and/or chapters, use
             @TT{\cite[Version M.N]{plt-tr@num}} or
             @TT|{@cite[(in-bib plt-tr@num "Version M.N")]}| instead.  The year
             in the bibliographic entry should be 2010 regardless of the
             version's date.}
      @@refblock{BibTeX}|{
        @techreport{plt-tr@num,
          title       = {|@title},
          author      = {|@(add-between author-strings " and ")},
          number      = {|@tr-name},
          institution = {PLT Inc.},
          year        = {2010},
          note        = {\url{|@(get-resource-path cite-page #t)}}
        }}|
      @@refblock{Scribble}|{
        (define plt-tr@num
          (make-bib #:title    "|@title"
                    #:author   |@(format "~s" (cons 'authors author-strings))
                    #:date     "2010"
                    #:location (techrpt-location "|@tr-name")
                    #:url      "|@(get-resource-path cite-page #t)"))}|
      @h2{Specific Versions}
      @blockquote{
        @table[frame: 'box rules: 'rows cellpadding: 10]{
          @(for/list ([v (remove-duplicates
                          (map installer-version all-installers))])
             @tr{@td{@b{@v} @br @small{(@version->date[v])}}
                 @td{@TT{\cite[Version @v]{plt-tr@num}} @br
                     @TT|{@cite[(in-bib plt-tr@num "Version |@v")]}|}
                 @td{@link['html v], @link['pdf v]}})}
        @p{@small{Reminder: the release dates should not be included in the
                  entry or the citation.}}}})
  @list{@dt{@title-line[#t]}
        @dd[style: "margin-bottom: 1.5ex;"]{
          @content}})

(provide techreports)
(define techreports
  @page[#:file "tr/" #:title "PLT Technical Reports" #:part-of 'learning]{
    @p*{
    @~ For citations of generic pieces of the Racket infrastructure, please use
       @TT{\cite{plt-tr1}}, @TT{\cite{plt-tr2}}, etc. in LaTeX, or
       @TT|{@cite[plt-tr1]}|, @TT|{@cite[plt-tr2]}|, etc. in Scribble, with the
       BibTeX and Scribble entries provided in the web pages below.
    @~ For references to specific releases and/or chapters of the language, use
       @TT{\cite[Version M.N]{plt-tr1}} or
       @TT|{@cite[(in-bib plt-tr1 "Version M.N")]}|
       instead.}
    @dl{@(add-newlines (-all-techreports-))}})

;; Old style TR entries, for compatibility ------------------------------------

(define last-old-tr "4.9")

(define authors
  '([plt      "PLT"]
    [ff       mflatt robby]
    [fplt     mflatt plt]
    [rplt     robby plt]
    [ffplt    ff plt]
    [ffc      ff clements]
    [fb       mflatt eli]
    [dorai    "Dorai Sitaram"]
    [wright   "Andrew K. Wright"]
    [flanagan "Cormac Flanagan"]
    [web      burns gregp jay]
    [burns    "Mike Burns"]
    [gregp    "Greg Pettyjohn"]
    [dyoo     "Danny Yoo"]
    [ym       dyoo jay]
    [kathyg   "Kathryn E. Gray"]
    [jacobm   "Jacob Matthews"]
    [sowens   "Scott Owens"]
    [plot     alex raymond]
    [alex     "Alexander Friedman"]
    [raymond  "Jamie Raymond"]
    [dskippy  "Mike T. McHenry"]
    [ptg      "Paul Graunke"]
    [ryanc    "Ryan Culpepper"]
    [steck    "Paul Steckler"]
    [samth    "Sam Tobin-Hochstadt"]
    [gcooper  "Greg Cooper"]))

(define doc-defs
  ;; each item is:
  ;;   (docname [docnum] versions author title . attrs)
  ;; docname: a symbol for the name of the document
  ;; docnum: an optional integer used in the TR number (docname used otherwise)
  ;; versions: version range specification (see `versions->pred' below)
  ;; author: is a symbol from the `authors' table
  ;; title: is a string (no braces)
  `(;; old versions
    (mzscheme 1 "...*" mflatt "PLT MzScheme: Language Manual")
    (mred     2 "...*" ff     "PLT MrEd: Graphical Toolbox Manual")
    (drscheme 3 "...*" robby  "PLT DrScheme: Programming Environment Manual"
              [#:note ("See also:"
                       "R. B. Findler, J. Clements, C. Flanagan, M. Flatt,"
                       "S. Krishnamurthi, P. Steckler and M. Felleisen."
                       "DrScheme: A programming environment for Scheme,"
                       "Journal of Functional Programming,"
                       "12(2):159--182, March 2002."
                       "http://www.ccs.neu.edu/scheme/pubs/")])
    (mzlib 4    "200...*" plt    "PLT MzLib: Libraries Manual")
    (framework  "...*"    plt    "PLT Framework: GUI Application Framework")
    (tools      "...*"    robby  "PLT Tools: DrScheme Extension Manual")
    (web-server "300...*" web    "Web Server Manual")
    (mrlib      "207...*" plt    "PLT MrLib: Graphical Libraries Manual")
    (plot       "207...*" plot   "PLoT Manual")
    (mzc        "...*"    plt    "PLT mzc: MzScheme Compiler Manual")
    (insidemz   "...*"    mflatt "Inside PLT MzScheme")
    (foreign    "...*"    eli    "PLT Foreign Interface Manual")
    (misclib    "...*"    plt   "PLT Miscellaneous Libraries: Reference Manual")
    (tex2page   "200...*" dorai  "TeX2page")
    (t-y-scheme "200...*" dorai  "Teach Yourself Scheme in Fixnum Days"
                [type "Introduction"])
    (match      "103p1"   wright "Pattern Matching for Scheme")
    (mrspidey   "103p1"   flanagan "PLT MrSpidey: Static Debugger Manual")
    ;; new Scheme versions
    (quick      "*...!"    mflatt
                "Quick: An Introduction to PLT Scheme with Pictures"
                [type "Introduction"])
    (more       "*...!"    plt    "More: Systems Programming with PLT Scheme"
     [type "Introduction"])
    (continue   "4.1.1..." ym    "Continue: Web Applications in PLT Scheme"
     [type "Introduction"])
    (guide      "*...!"    ffplt  "Guide: PLT Scheme" [type "Introduction"])
    (reference  "*...!"    fplt   "Reference: PLT Scheme")
    (htdp-langs "*...!"    plt    "How to Design Programs Languages")
    (htdc       "*...!"    kathyg "How to Design Classes Languages")
    (teachpack  "*...!"    plt    "Teachpacks")
    (eopl       "*...!"    plt    "Essentials of Programming Languages Language")
    (drscheme   "*...!"    rplt   "DrScheme: PLT Programming Environment")
    (mzc        "*...!"    plt    "mzc: PLT Compilation and Packaging")
    (setup-plt  "*...!"    plt    "setup-plt: PLT Configuration and Installation")
    (planet     "*...!"    jacobm "PLaneT: Automatic Package Distribution")
    (redex      "4.1...!"  robby  "Redex: Debugging Operational Semantics")
    (scribble   "*...!"    fb     "Scribble: PLT Documentation Tool")
    (slideshow  "*...!"    ff     "Slideshow: PLT Figure and Presentation Tools")
    (web-server "*...!"    jay    "Web Server: PLT HTTP Server")
    (tools      "*...!"    robby  "Plugins: Extending DrScheme")
    (gui        "*...!"    ffc    "GUI: PLT Graphics Toolkit")
    (framework  "*...!"    ff     "Framework: PLT GUI Application Framework")
    (sgl        "*...!"    sowens "GL: 3-D Graphics")
    (plot       "*...!"    plot   "PLoT: Graph Plotting")
    (browser    "*...!"    plt    "Browser: Simple HTML Rendering")
    (cards      "*...!"    plt    "Cards: Virtual Playing Cards Library")
    (embedded-gui "*...!"  dskippy "Embedded GUI: Widgets within editor<%>")
    (games      "*...!"    plt    "Games: Fun Examples")
    (gl-board-game "*...!" plt    "GL Board Game: 3-D Game Support")
    (mrlib      "*...!"    plt    "MrLib: Extra GUI Libraries")
    (string-constants "*...!" plt "String Constants: GUI Internationalization")
    (syntax-color "*...!"  sowens "Syntax Color: Utilities")
    (turtles    "*...!"    plt    "Turtle Graphics")
    (net        "*...!"    plt    "Net: PLT Networking Libraries")
    (openssl    "*...!"    plt    "OpenSSL")
    (file       "*...!"    plt    "File: PLT File Format Libraries")
    (html       "*...!"    plt    "HTML: Parsing Library")
    (parser-tools "*...!"  sowens "Parser Tools: lex and yacc-style Parsing")
    (xml        "*...!"    ptg    "XML: Parsing and Writing")
    (config     "*...!"    plt    "Config: Installation and Search Paths")
    (dynext     "*...!"    plt    "Dynext: Running a C Compiler/Linker")
    (errortrace "*...!"    plt    "Errortrace: Debugging and Profiling")
    (macro-debugger "*...!" ryanc "Macro Debugger")
    (make       "*...!"    plt    "Make: Dependency Manager")
    (readline   "*...!"    plt    "Readline: Terminal Interaction")
    (slatex-wrap "*...!"   plt    "SLaTeX Wrapper")
    (trace      "*...!"    plt   "Trace: Instrumentation to Show Function Calls")
    (version    "*...!"    plt    "Version: PLT Version Checking")
    (foreign    "*...!"    eli    "FFI: PLT Scheme Foreign Interface")
    (inside     "*...!"    mflatt "Inside: PLT Scheme C API")
    (cffi       "*...!"    mflatt "c-lambda: C FFI via mzc")
    (mysterx    "*...!"    steck  "MysterX: Using Windows COM Objects in Scheme")
    (mzcom      "*...!"    steck  "MzCOM: Scheme as a Windows COM Object")
    (srfi       "*...!"    plt    "SRFIs: Libraries")
    (htdp-lib   "*...!"    plt    "HtDP: Languages as Libraries")
    (swindle    "*...!"    plt    "Swindle")
    (syntax     "*...!"    plt    "Syntax: Meta-Programming Helpers")
    (typed-scheme "*...!"  samth  "Typed Scheme: Scheme with Static Types")
    (frtime     "*...!"    gcooper "FrTime: A Language for Reactive Programs")
    (lazy       "*...!"    eli    "Lazy Scheme")
    (r5rs       "*...!"    plt    "R5RS: Legacy Standard Language"
                [#:note ("See also:"
                         "Rickard Kelsey and William Clinger"
                         "and Jonathan Rees (Editors)"
                         "Revised$^5$ Report of the Algorithmic Language Scheme"
                         "ACM SIGPLAN Notices"
                         "33(9):26--76, 1998")])
    (graphics   "*...!"    plt    "Graphics: Legacy Library")
    (mzlib      "*...!"    plt    "MzLib: Legacy PLT Libraries")
    (preprocessor "*...!"  eli    "mzpp and mztext: Preprocessors")
    (mzscheme   "*...!"    plt    "MzScheme: Legacy Module Language")
    (algol60    "*...!"    plt    "Algol 60")
    (honu       "*...!"    plt    "Honu")
    (test-box-recovery "*...!" plt "Test Box Recovery Tool")

    ;; Racket versions
    (quick      "!..."     mflatt
                "Quick: An Introduction to Racket with Pictures"
                [type "Introduction"])
    (more       "!..."    plt    "More: Systems Programming with Racket"
                [type "Introduction"])
    (guide      "!..."    ffplt  "Guide: Racket" [type "Introduction"])
    (reference  "!..."    fplt   "Reference: Racket")
    (drracket   "!..."    rplt   "DrRacket: Programming Environment")
    (scribble   "!..."    fb     "Scribble: Racket Documentation Tool")
    (slideshow  "!..."    ff     "Slideshow: Racket Figure and Presentation Tools")
    (web-server "!..."    jay    "Web Server: Racket HTTP Server")
    (foreign    "!..."    eli    "FFI: Racket Foreign Interface")
    (inside     "!..."    mflatt "Inside: Racket C API")

    ;; Both Scheme and Racket
    (r6rs       "*..."    plt    "R6RS: Standard Language"
                [#:note ("See also:"
                         "Michael Sperber and R. Kent Dybvig and Matthew Flatt"
                         "and Anton Van Straaten (Editors)"
                         "Revised$^6$ Report of the Algorithmic Language Scheme"
                         "September 2007")])

     ;; (article fcffksf:drscheme
     ;;   [author "Robert Bruce Findler and John Clements and Cormac Flanagan"
     ;;           "and Matthew Flatt and Shriram Krishnamurthi"
     ;;           "and Paul Steckler and Matthias Felleisen"]
     ;;   [title "{DrScheme}: A Programming Environment for {Scheme}"]
     ;;   [volume 12]
     ;;   [number 2]
     ;;   [pages "159--182"]
     ;;   [month "March"]
     ;;   [journal "Journal of Functional Programming"]
     ;;   [year "2002"])
    ))

;; Use this instead of the built-in one, to make sure that we only deal
;; with known released version.
(define version->integer
  (let ([t (for*/hash ([v+d (in-list versions+dates)]
                       [v (in-value (car v+d))])
             (values v (-version->integer (regexp-replace #rx"^0+" v ""))))])
    (lambda (ver)
      (hash-ref t ver (lambda ()
                        (error 'version->integer
                               "unknown pltreport version: ~e" ver))))))

(define (date->year+month date)
  (let ([m (regexp-match #rx"^([A-Z][a-z]+) +([0-9]+)$" date)])
    (if m
      (values (caddr m) (cadr m))
      (error 'date->year+month "unexpected date string: ~.a" date))))

;; "V...V"        version range
;; "...V", "V..." open-ended version range
;; "..."          all versions
;; "V"            specific version
;; ""             no versions
;; V can be `*': a number between the v3 docs and the v4 docs
;; V can be `!': a number between the last PLT Scheme and the first Racket
(define v:3->4 (-version->integer "379"))
(define v:4->5 (-version->integer "4.3"))
(define (versions->pred str)
  (let* ([str (regexp-replace* #rx"  +" str " ")]
         [str (regexp-replace #rx"^ +" str "")]
         [str (regexp-replace #rx" +$" str "")]
         [l (regexp-split #rx" *[.][.][.] *" str)]
         [l (map (lambda (x)
                   (cond [(equal? "" x) #f]
                         [(equal? "*"  x) v:3->4]
                         [(equal? "!"  x) v:4->5]
                         [(version->integer x)]
                         [else (error 'versions->pred "bad version: ~e" x)]))
                 l)])
    (apply
     (case-lambda [(ver)
                   (if ver
                     (lambda (v) (equal? ver (version->integer v)))
                     (lambda (v) #f))]
                  [(from to)
                   (let ([from (or from -inf.0)]
                         [to   (or to   +inf.0)])
                     (lambda (v) (<= from (version->integer v) to)))]
                  [_ (error 'versions->pred "bad versions spec: ~e" str)])
     l)))

(define authors*
  (let ([t (make-hasheq)])
    (for ([a authors])
      (hash-set! t (car a)
                 ((if (and (= 2 (length a)) (string? (cadr a))) cadr cdr) a)))
    t))

(define (author->string author)
  (let ([r (hash-ref authors* author
                     (lambda () (person-bibname (find-person author))))])
    (if (string? r)
      r
      (let ([r (string-join (map author->string r) " and ")])
        (hash-set! authors* author r)
        r))))

(define doc-defs*
  (for/list ([d doc-defs])
    (apply
     (lambda (docname docnum versions author title . attrs)
       `(,(versions->pred versions)
         [#:docname ,docname]
         [#:number-template ,(format "PLT-TR~~a-~a-v~~a" (or docnum docname))]
         [title     ,(with-braces title)]
         [author    ,(author->string author)]
         ,@attrs))
     (if (number? (cadr d)) d (list* (car d) #f (cdr d))))))

(define bibs
  (for*/list ([ver (filter (let ([last (-version->integer last-old-tr)])
                             (lambda (v) (<= (version->integer v) last)))
                           (reverse all-versions))]
              [doc doc-defs*]
              #:when ((car doc) ver))
    (define attrs (cdr doc))
    (define (get key [dflt #f]) (cond [(assq key attrs) => cadr] [else dflt]))
    (define docname (get '#:docname))
    (define-values (year month) (date->year+month (version->date ver)))
    (define number (format (get '#:number-template) year ver))
    (define key (regexp-replace* #rx":" (string-downcase number) "_"))
    (define old? (< (version->integer ver) v:4->5))
    (define site (if old? "plt-scheme" "racket-lang"))
    (define note
      (let ([n1 (format "\\url{http://~a.org/techreports/}" site)]
            [n2 (get '#:note #f)])
        (if n2 (cons (string-append n1 ";") n2) (list n1))))
    (define maybe-s (if old? "" "s"))
    (define (mk-url dir sfx)
      (format "http://download.~a.org/doc~a/~a/~a/~a~a"
              site maybe-s ver dir docname sfx))
    (define pdf-url  (mk-url "pdf" ".pdf"))
    (define pdf-html (mk-url "html" "/"))
    (bib 'techreport key
         `(,@attrs
           [year        ,year]
           [month       ,month]
           [institution "PLT Scheme Inc."]
           [type        "Reference Manual"]
           [number      ,number]
           [#:version   ,ver]
           [url         ,pdf-url]
           [#:pdf-url   ,pdf-url]
           [#:html-url  ,pdf-html]
           [note        ,@note]))))

(define old-techreports
  @page[#:file "techreports/" #:title "Old PLT Technical Reports"
        #:part-of 'learning
        #:extra-headers
        @script/inline[type: "text/javascript"]{
          function show_bib(n) {
            var s = document.getElementById("bibrow"+n).style;
            s.display = (s.display == "table-row") ? "none" : "table-row";
          }}]{
    @p{@strong{Note:} the entries on this page are outdated, please see the new
       @techreports page.}
    @p{PLT publishes technical reports about some of its tools and libraries so
       that scholars who wish to give proper credit to some of our innovations
       have a definite citation.  Each entry below provides the full pdf and a
       bibtex entry; some of the bibtex entries provide additional citations to
       published papers.}
    @table[width: "98%" cellspacing: 0 cellpadding: 6 border: 0
           align: 'center style: "font-size: 75%;"]{
      @(for/list ([bib (in-list bibs)] [n (in-naturals)])
         (define bgcolor (if (even? n) "#e0e0e0" "white"))
         (define bibtext
           (parameterize ([current-output-port (open-output-string)])
             (display-bib bib)
             (get-output-string (current-output-port))))
         @list{
           @tr[valign: 'top bgcolor: bgcolor]{
             @td[style: "white-space: nowrap;"]{@(hash-ref bib 'number)}
             @td[align: 'left]{@i{@(without-braces (hash-ref bib 'title))}}
             @td{@(bib-author bib)}
             @td{@a[href: @list{javascript: show_bib(@n)@";"}]{[bib]}@|nbsp|@;
                 @a[href: (hash-ref bib '#:pdf-url)]{[pdf]}@|nbsp|@;
                 @a[href: (hash-ref bib '#:html-url)]{[html]}}}
           @tr[valign: 'top bgcolor: bgcolor
               id: @list{bibrow@n} style: "display: none;"]{
             @td{}@td[colspan: 3]{@pre{@bibtext}}}})}})
