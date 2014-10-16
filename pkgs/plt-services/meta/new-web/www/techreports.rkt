#lang plt-web

;; New style TR entries

(require "resources.rkt" "people.rkt" "../download/data.rkt"
         plt-web/style)

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
    (map (λ (a) (if (eq? 'plt a) "PLT" (person-bibname (find-person a))))
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
    @list{@h4{@title}
          @blockquote{@PRE{@body}}})
  (define cite-page
    @page[#:site www-site #:file (format "tr~a/" num) #:title tr-name #:part-of 'learning]{
      @h3{@title-line[#f]}
      @p*{@blockquote{@big{@content}}
          @~ For citations of the @desc, please use @TT{\cite{plt-tr@num}} in
             LaTeX, or @TT|{@cite[plt-tr1]}| in Scribble, using the definitions
             below.
          @~ For references to specific releases and/or chapters, use
             @TT{\cite[Version M.N]{plt-tr@num}} or
             @TT|{@cite[(in-bib plt-tr|@num "Version M.N")]}| instead.  The
             year in the bibliographic entry should be 2010 regardless of the
             version's date.}
      @@refblock{BibTeX}|{
        @techreport{plt-tr|@num,
          title       = {|@title},
          author      = {|@(add-between author-strings " and ")},
          number      = {|@tr-name},
          institution = {PLT Design Inc.},
          year        = {2010},
          note        = {\url{|@(url-of cite-page #t)}}
        }}|
      @@refblock{Scribble}|{
        (define plt-tr|@num
          (make-bib #:title    "|@title"
                    #:author   |@(format "~s" (cons 'authors author-strings))
                    #:date     "2010"
                    #:location (techrpt-location #:institution "PLT Design Inc."
                                                 #:number "|@tr-name")
                    #:url      "|@(url-of cite-page #t)"))}|
      @h4{Specific Versions}
      @blockquote{
        @table[frame: 'box rules: 'rows cellpadding: 10]{
          @(for/list ([r (in-list all-releases)])
             (define v (release-version r))
             @tr{@td{@b{@v} @br @small{(@(release-date-string r))}}
                 @td{@TT{\cite[Version @v]{plt-tr@num}} @br
                     @TT|{@cite[(in-bib plt-tr|@num "Version |@v")]}|}
                 @td{@link['html v] @link['pdf v]}})}
        @p{@small{Reminder: the release dates should not be included in the
                  entry or the citation.}}}})
  @list{@dt{@title-line[#t]}
        @dd[style: "margin-bottom: 1.5ex; margin-left: 40px;"]{
          @content}})

(provide techreports)
(define techreports
  @page[#:site www-site #:file "tr/" #:title "PLT Technical Reports" #:part-of 'learning]{
    @columns[10 #:row? #t]{
    @p*{
    @~ For citations of generic pieces of the Racket infrastructure, please use
       @TT{\cite{plt-tr1}}, @TT{\cite{plt-tr2}}, etc. in LaTeX, or
       @TT|{@cite[plt-tr1]}|, @TT|{@cite[plt-tr2]}|, etc. in Scribble, with the
       BibTeX and Scribble entries provided in the web pages below.
    @~ For references to specific releases and/or chapters of the language, use
       @TT{\cite[Version M.N]{plt-tr1}} or
       @TT|{@cite[(in-bib plt-tr1 "Version M.N")]}|
       instead.}
    @dl{@(add-newlines (-all-techreports-))}}})
