#lang meta/web

;; Old style TR entries

;; This code is intended only to keep the old techreports/ page up for
;; old links.

(require "resources.rkt" "techreports.rkt" "people.rkt"
         (prefix-in - version/utils))

;; ----------------------------------------------------------------------------
;; Data

(define last-old-tr "4.9")

;; This list is fixed to these versions (since this is an old page, no
;; need to update with new versions)
(define versions+dates
  '(["4.2.5" "April" "2010"]
    ["4.2.4" "January" "2010"]
    ["4.2.3" "December" "2009"]
    ["4.2.2" "October" "2009"]
    ["4.2.1" "July" "2009"]
    ["4.2"   "June" "2009"]
    ["4.1.5" "March" "2009"]
    ["4.1.4" "January" "2009"]
    ["4.1.3" "November" "2008"]
    ["4.1.2" "October" "2008"]
    ["4.1.1" "October" "2008"]
    ["4.1"   "August" "2008"]
    ["4.0.2" "July" "2008"]
    ["4.0.1" "June" "2008"]
    ["4.0"   "June" "2008"]
    ["372"   "December" "2007"]
    ["371"   "August" "2007"]
    ["370"   "May" "2007"]
    ["360"   "November" "2006"]
    ["352"   "July" "2006"]
    ["351"   "July" "2006"]
    ["350"   "June" "2006"]
    ["301"   "January" "2006"]
    ["300"   "December" "2005"]
    ["209"   "December" "2004"]
    ["208"   "August" "2004"]
    ["207"   "May" "2004"]
    ["206p1" "January" "2004"]
    ["206"   "January" "2004"]
    ["205"   "August" "2003"]
    ["204"   "May" "2003"]
    ["203"   "December" "2002"]
    ["202"   "August" "2002"]
    ["201"   "July" "2002"]
    ["200"   "June" "2002"]
    ["103p1" "August" "2001"]
    ["103"   "September" "2000"]
    ["053"   "July" "1998"]
    ))

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

;; ----------------------------------------------------------------------------
;; Bib stuff

;; bib values are hash tables mapping field names (symbols) to strings.
;; Keywords can also be used for the field names, which makes them meta-fields
;; that are not included in the usual bib printout.  Two of them are required:
;;   - #:type  the type of the entry (a symbol: 'article, 'techreport, etc)
;;   - #:key   the label for the entry

(define bib-fields
  '(author editor title booktitle journal
    edition volume number series
    chapter pages
    type
    school institution organization
    publisher howpublished
    address
    month year
    key
    crossref
    url
    note
    eprint))

(define meta-field? keyword?)

(define key->number
  (let ([t (for/hash ([k bib-fields] [i (in-naturals)]) (values k i))])
    (λ (key) (hash-ref t key
               (λ () (error 'key->number "unknown field name: ~e" key))))))

;; converts the hash to an alist with the order specified by bib-fields
(define (bib->alist bib)
  (sort (filter-not (compose meta-field? car) (hash-map bib cons))
        < #:key (compose key->number car) #:cache-keys? #t))

(define (display* . xs)
  (for-each display xs))

(define (display-attr attr)
  (let* ([prefix (format "  ~a = {" (car attr))]
         [sep (lazy (string-append "\n" (make-string (string-length prefix)
                                                     #\space)))])
    (display* prefix
              (if (regexp-match? #rx"\n" (cdr attr))
                (regexp-replace* #rx"\n" (cdr attr) (force sep))
                (cdr attr))
              "}")))

(define (display-bib bib)
  (display* "@" (hash-ref bib '#:type) "{" (hash-ref bib '#:key))
  (for ([attr (bib->alist bib)]) (display* ",\n") (display-attr attr))
  (display* "\n}\n"))

(define (with-braces str) (regexp-replace* #px"\\b\\w+[A-Z]\\w*" str "{\\0}"))
(define (without-braces str) (regexp-replace* #rx"[{}]+" str ""))

(define (bib-author bib)
  (let ([authors (regexp-split #rx"[ \t\n]+and[ \t\n]+"
                               (hash-ref bib 'author))])
    (case (length authors)
      [(0) "???"]
      [(1) (car authors)]
      [(2) (apply format "~a and ~a" authors)]
      [(3) (apply format "~a, ~a, and ~a" authors)]
      [else (format "~a et al" (car authors))])))

;; processes the (key val ...) alist to a hash of (key . val) by combining the
;; possibly multiple values for each key (each value becomes a line)
(define (bib type key attrs)
  (define t (make-hasheq))
  (hash-set! t '#:type type)
  (hash-set! t '#:key  key)
  (for ([a attrs])
    (define (err) (error 'make-bib "bad attribute: ~e" a))
    (unless (and (pair? a) (pair? (cdr a)) (list? (cdr a))) (err))
    (let ([key (car a)])
      (unless (hash-ref t key #f) ; previous keys take precedence
        (cond [(symbol? key)
               ;; turn non-strings to strings, join multiple strings, normalize
               ;; spaces
               (let* ([val (cdr a)]
                      [val (map (λ (x) (if (string? x) x (format "~a" x))) val)]
                      [val (string-append* (add-between val "\n"))]
                      [val (regexp-replace* #rx"\t" val " ")]
                      [val (regexp-replace* #rx"  +" val " ")]
                      [val (regexp-replace #rx"^ +" val "")]
                      [val (regexp-replace #rx" +$" val "")]
                      [val (regexp-replace* #rx"(?: *\r?\n *)+" val "\n")])
                 (hash-set! t key val))]
              [(and (meta-field? key) (null? (cddr a)))
               (hash-set! t key (cadr a))]
              [else (err)]))))
  t)

;; ----------------------------------------------------------------------------
;; Version etc

;; Use this instead of the built-in one, to make sure that we only deal
;; with known versions.
(define version->integer
  (let ([t (for*/hash ([v (in-list (map car versions+dates))])
             (values v (-version->integer (regexp-replace #rx"^0+" v ""))))])
    (λ (ver)
      (hash-ref t ver
        (λ () (error 'version->integer "unknown pltreport version: ~e" ver))))))

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
         [l (map (λ (x)
                   (cond [(equal? "" x) #f]
                         [(equal? "*"  x) v:3->4]
                         [(equal? "!"  x) v:4->5]
                         [(version->integer x)]
                         [else (error 'versions->pred "bad version: ~e" x)]))
                 l)])
    (apply
     (case-lambda [(ver)
                   (if ver
                     (λ (v) (equal? ver (version->integer v)))
                     (λ (v) #f))]
                  [(from to)
                   (let ([from (or from -inf.0)]
                         [to   (or to   +inf.0)])
                     (λ (v) (<= from (version->integer v) to)))]
                  [_ (error 'versions->pred "bad versions spec: ~e" str)])
     l)))

;; ----------------------------------------------------------------------------
;; Authors

(define authors*
  (let ([t (make-hasheq)])
    (for ([a authors])
      (hash-set! t (car a)
                 ((if (and (= 2 (length a)) (string? (cadr a))) cadr cdr) a)))
    t))

(define (author->string author)
  (let ([r (hash-ref authors* author
                     (λ () (person-bibname (find-person author))))])
    (if (string? r)
      r
      (let ([r (string-join (map author->string r) " and ")])
        (hash-set! authors* author r)
        r))))

;; ----------------------------------------------------------------------------
;; Main

(define doc-defs*
  (for/list ([d doc-defs])
    (apply
     (λ (docname docnum versions author title . attrs)
       `(,(versions->pred versions)
         [#:docname ,docname]
         [#:number-template ,(format "PLT-TR~~a-~a-v~~a" (or docnum docname))]
         [title     ,(with-braces title)]
         [author    ,(author->string author)]
         ,@attrs))
     (if (number? (cadr d)) d (list* (car d) #f (cdr d))))))

(define last-old-tr-version (-version->integer last-old-tr))

(define bibs
  (for/list ([ver   (in-list (map car versions+dates))]
             [month (in-list (map cadr versions+dates))]
             [year  (in-list (map caddr versions+dates))]
             #:when (<= (version->integer ver) last-old-tr-version)
             [doc (in-list doc-defs*)]
             #:when ((car doc) ver))
    (define attrs (cdr doc))
    (define (get key [dflt #f]) (cond [(assq key attrs) => cadr] [else dflt]))
    (define docname (get '#:docname))
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
           [institution "PLT Design Inc."]
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
