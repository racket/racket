#lang plt-web

(require "resources.rkt" "data.rkt" "installer-pages.rkt" "symlinks.rkt" plt-web/style
         (prefix-in pre: "../minis/pre.rkt"))

(define docs "docs")

(provide render-download-page)
(define (render-download-page [release current-release] [package 'racket])
  (define version (release-version release))
  (define all-packages (sort (hash-map (for/hash ([i (in-list all-installers)]
                                                  #:when (equal? release (installer-release i)))
                                         (values (installer-package i)
                                                 (installer->page i 'render-package-option)))
                                       cons)
                             (lambda (a b)
                               (cond
                                [(equal? (car a) package) #t]
                                [(equal? (car b) package) #f]
                                [else (string<? (cdr a) (cdr b))]))))
  (define note-style '("font-size: 85%; display: none;"
                       " margin-top: 1ex;"
                       " padding: 1ex 1ex 1ex 1ex;"
                       " text-align: left;"
                       " line-height: 1.5em; "
                       " background-color: #edd"))
  (list
   @columns[10 #:center? #t #:row? #t #:center-text? #t]{
    @h3[style: "text-align: center"]{Version @version (@(release-date-string release))}
    @div[id: "download_panel" align: "center" style: "display: none; margin-bottom: 20px;"]{
      @div[align: "center"]{
        Distribution:
        @select[id: "package_selector"
                onchange:   "selection_changed();"
                onkeypress: "selection_changed();"]{
          @(for/list ([i (in-list all-packages)])
             (cdr i))}
        @(for/list ([i (in-list all-packages)]
                    [n (in-naturals)])
           (define this-package (car i))
           @div[id: (format "platform_selector_panel_~a" this-package)
                style: (if (zero? n) "display: block;" "display: none;")]{
              Platform:
              @select[id: (format "platform_selector_~a" this-package)
                      onchange:   "selection_changed();"
                      onkeypress: "selection_changed();"]{
                @(for/list ([i (in-list all-installers)]
                            #:when (and (equal? release (installer-release i))
                                        (equal? this-package (installer-package i))))
                   (installer->page i 'render-direct-option))}})}
      @br
      @navigation-button[@(a href: (resource "download/" #f)
                             id: "download_link"
                             "Download")]
      @br
      or @a[href: (resource "download/" #f) id: "mirror_link"]{mirror}}}
  @columns[8 #:center? #t #:center-text? #t #:row? #t]{
      @(let* ([sep   @list{@nbsp @bull @nbsp}]
              [links (λ links @(div style: "margin: 1ex 4ex;" (add-between links sep)))]
              [docs  @list{@|docs|/@|version|/html}])
         (list
          @row{
            @links[@list{Release: @nbsp @(release-page release){Announcement}}
                       @a[href: @list{@|docs|/release/}]{Notes}
                       @a[href: @docs]{Documentation}]}
          @row{@links[@license{License}
                       all-version-pages
                       @pre:installers{Snapshot Builds}]}))}
  @columns[6 #:center? #t #:center-text? #t #:row? #t]{
      @div[id: "linux_explain"
           style: note-style]{
        @div{@b{About the Linux installers:}} If you don't see an option for
        your particular platform, try other Linux installers, starting from
        similar ones.  Very often, a build on one Linux variant will work on
        others too.}
      @div[id: "builtpkgs_explain"
           style: note-style]{
        @div{@b{About source distributions:}} The @b{Source + built packages}
           distribution is recommend, instead of the selected @b{Source} distribution.
           The @b{Source + built packages} distribution includes pre-built,
           platform-independent bytecode@";" it installs much faster than
           plain source, and it is also compatible with fast installs of
           additional Racket packages.}
      @div[id: "source_explain"
           style: note-style]{
        @div{@b{About sources for Windows and Mac OS X:}} To build from source for
           Windows or Mac OS X, download and build a @b{Minimal Racket} distribution
           instead of a @b{Racket} distribution, and then install packages
           with @div{@tt{raco pkg install -i main-distribution}}}
    @downloader-script[package (map car all-packages)]
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}}))

(define (release-page* rel)
  (define ver (release-version rel))
  (define title @list{v@ver Release Notes})
  @page[#:site download-site
        #:file (format "v~a.html" ver) #:title title #:part-of 'download]{
    @table[align: 'center]{
      @tr{@td{@h2{Release Announcement for Version @ver}}}
      @tr{@td{@pre{@release-announcement[rel]}}}}
  })
(define release-page
  (let ([t (make-hash)])
    (λ (rel) (hash-ref! t rel (λ () (release-page* rel))))))

(define all-version-pages
  (let ()
    (define (make-page rel pkg)
      (define ver   (release-version rel))
      (define file  (format "~a-v~a.html" pkg ver))
      (define title @list{Download @(package->name pkg) v@ver})
      @page[#:site download-site #:file file #:title title #:width 'full #:part-of 'download]{
        @(render-download-page rel pkg)})
    (define style
      @style/inline[type: 'text/css]{
       })
    (define-values (main-package alt-packages)
      (cond [(null? all-packages)
             (eprintf "Warning: all-packages is empty\n")
             (values 'racket null)]
            [else
             (values (car all-packages) (cdr all-packages))]))
    @page[#:site download-site
          #:id 'all-versions #:title "All Versions" #:part-of 'download
          #:extra-headers style #:width 'full]{
     @columns[10 #:center? #t #:row? #t]{
      @table[class: "striped rounded"]{
        @thead{
          @tr{@th{Version}
              @th{Announcement}
              @th{Download}
              @th{Alternative}
              @th{Documentation}}}
       @tbody{
        @(let ([sep null])
           ;; release=>packages : hash[release => (listof package)]
           ;; Indicates what packages actually exist (have installers) for a given release.
           (define release=>packages (make-hash))
           (for ([i (in-list all-installers)])
             (define r (installer-release i))
             (define prev-packages (hash-ref release=>packages r null))
             (unless (member (installer-package i) prev-packages)
               (hash-set! release=>packages r (cons (installer-package i) prev-packages))))
           (define (cell rel pkg)
             @td[align: 'center]{
               @(make-page rel pkg){@(package->name main-package)}})
             @sep
             @(for/list ([r (in-list all-releases)])
                (define ver (release-version r))
                  @list{
                    @tr[class: 'version-row]{
                      @td{@strong{Version @ver}}
                      @td{@span[style: "font-size: 80%"]{@(release-page r){@release-date-string[r]}}}
                      @(if (member main-package (hash-ref release=>packages r))
                           (cell r main-package)
                           @td[])
                      @td[align: 'center]{
                        @(add-between
                          (for/list ([p (in-list alt-packages)]
                                     #:when (member p (hash-ref release=>packages r)))
                            ((make-page r p) (package->name p)))
                          " ")}
                      @td{@a[href: @list{@|docs|/@|ver|/html}]{[HTML]} @;
                          @nbsp @;
                          @a[href: @list{@|docs|/@|ver|/pdf}]{[PDF]}}}
                    @sep}))
          @tr[class: 'version-row]{
            @td{Development}
            @td{}
            @td{@pre:installers{Snapshots}}
            @td{}
            @td{}}}}}}))

(define license
  @page[#:site download-site
        #:title "Software License" #:part-of 'download]{
    @columns[10 #:center? #t #:row? #t]{
    @p*{
    @~ Racket is distributed under the
       @a[href: "http://www.gnu.org/licenses/lgpl-3.0.html"]{
         GNU Lesser General Public License (LGPL)}.
    @~ Our primary goal is to help as many people as possible use and
       contribute to Racket.  We encourage anyone to develop any kind of
       software, with any kind of license, using Racket.
    @~ We have chosen the LGPL as the license for Racket, which makes it
       possible for for people to create software with Racket, and to allow us
       to build on existing libraries that use the LGPL, such as the Lightning
       assembler and the GMP math library.  The basic requirement of the LGPL
       is that you make your changes to Racket available, and that you let
       other people use your software with new versions of Racket.
    @~ Since the LGPL license that Racket uses was originally designed for C
       programs, parts of it require some interpretation to apply to Racket in
       detail.  The following is how the Racket maintainers interpret the
       license.
    @~ @blockquote{
        @p{First, if you distribute your Racket application in source form or as
          compiled bytecode files, the Racket license does not restrict you at
          all.}
        @p{Second, if you distribute your Racket application as compiled binary
          generated by @tt{raco exe}, there are no requirements placed on the
          licensing of your software.  However, the LGPL requires that you make
          it possible to re-link your software with modified versions of
          Racket.  This means, basically, that you need to provide the compiled
          bytecode files used to produce the compiled binary, if requested by
          someone who got your software from you.  Note that this does @em{not}
          mean that your software has to be made open source, nor do you have
          to give the source code to anyone, nor do you have to make the
          compiled bytecode files available to the public or let other people
          redistribute them.  Furthermore, this is not revealing any more of
          your source code than the @tt{raco exe} format, since the bytecode is
          embedded in an extractable way in the resulting executable.}}
    @~ We are, of course, not lawyers, and this should not be taken as legal
       advice.  However, we wanted to make it clear that Racket is an
       appropriate building block for all kinds of software, and to clarify how
       we view the license of Racket.}}})

(define (downloader-script package packages)
  @script/inline[type: 'text/javascript]{@||
    var do_jump, selection_changed;
    var packages = [@(string-join (map (lambda (s) (format "\"~s\"" s)) packages) ", ")];
    var current_package = "@|package|";
    (function() {
    // show the download panel, since JS is obviously working
    document.getElementById("download_panel").style.display = "block";
    //
    var selector = document.getElementById("platform_selector_@|package|");
    // jump to the selected item
    do_jump = function() {
      location.href = selector[selector.selectedIndex].value;
    }
    // returns an ordering for the platform names, an array of regexps
    // note that the entries are sorted in a good order, so return an order
    // that only brings the locally desired entries to the top
    function getPlatformOrder() {
      var p = navigator.platform;
      var l = function(str) { return p.indexOf(str) != -1@";" }
      var Win      = /Windows/,
          Mac      = /Mac/,
          MacIntel = /Mac.*Intel/,
          MacPPC   = /Mac.*PPC/,
          Linux    = /Linux/,
          Linux64  = /Linux.*x86_64/,
          Linux32  = /Linux.*i386/,
          Unix     = /Unix/,
          Solaris  = /Solaris/;
      if (p == null) return [];
      else if (l("SunOS")) return [Solaris, Unix];
      @; Note about Windows 64: seems like the best guess could be done by
      @; checking that the platform has "Win64" or navigator.userAgent has
      @; either "Win64" or "WOW64".  But the 32 build might be better for many
      @; people anyway, so keep things as is for now.  (With the `Win' filter
      @; which will get the 32 version first and the 64 second.)
      else if (l("Win"))   return [Win];
      else if (l("Mac"))   return [(l("Intel")?MacIntel:MacPPC), Mac, Unix];
      else if (l("Linux")) {
        // also show the linux explanation if it's a linux
        document.getElementById("linux_explain").style.display = "block";
        return [(l("_64")?Linux64:Linux32), Linux, Unix];
      } else return [];
    }
    // show the linux explanation on change too (do it with a timeout so it
    // changes even when the arrow keys are used to move the selection -- since
    // then onchange is called only on blur)
    linux_expl_s = document.getElementById("linux_explain").style;
    source_expl_s = document.getElementById("source_explain").style;
    builtpkgs_expl_s = document.getElementById("builtpkgs_explain").style;
    selection_changed_timer = false;
    selection_changed = function() {
      var package_selector = document.getElementById("package_selector");
      var package = packages[package_selector.selectedIndex];
      if (current_package != package) {
         var panel = document.getElementById("platform_selector_panel_" + current_package);
         panel.style.display = "none";
         current_package = package;
         panel = document.getElementById("platform_selector_panel_" + package);
         panel.style.display = "block";
         selector = document.getElementById("platform_selector_" + package);
      }
      var download_link = document.getElementById("download_link");
      var selected = selector[selector.selectedIndex];
      var path = selected.value;
      download_link.href = path;
      download_link.innerHTML = path.replace(/.*\//, "") + " (" + selected.getAttribute("x-installer-size") + ")";
      var mirror_link = document.getElementById("mirror_link");
      mirror_link.href = selected.getAttribute("x-mirror");
      if (selection_changed_timer) clearTimeout(selection_changed_timer);
      selection_changed_timer = setTimeout(do_selection_changed, 250);
    }
    function some_selector_matches(rx) {
       for (i = 0@";" i < selector.length@";" i++) {
         if (selector[i].text.search(rx) >= 0)
          return true;
       }
       return false;
    }
    function do_selection_changed() {
      linux_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Linux/) >= 0)
        ? "block"
        : "none";
      source_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Unix Source/) >= 0
         && !some_selector_matches(/(Windows|Mac OS X) Source/))
        ? "block"
        : "none";
      builtpkgs_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Source/) >= 0
         && selector[selector.selectedIndex].text.search(/built/) < 0
         && some_selector_matches(/built/))
        ? "block"
        : "none";
    }
    //
    function initialize_selector(selector) {
      var opts = selector.options;
      var len = opts.length;
      // get the order and a make a sorting function
      var order = getPlatformOrder();
      function getOrder(str) {
        for (var i=0@";" i<order.length@";" i++)
          if (str.search(order[i]) >= 0) return i;
        return 999;
      }
      function isBetter(opt1,opt2) {
        // sort first by the order, then by how they were placed originally
        var ord1 = getOrder(opt1[0]), ord2 = getOrder(opt2[0]);
             if (ord1 < ord2)       return -1;
        else if (ord1 > ord2)       return +1;
        else if (opt1[4] < opt2[4]) return -1;
        else if (opt1[4] > opt2[4]) return +1;
        else                        return  0;
      }
      // sort the options, need to use a temporary array
      var tmps = new Array(len);
      for (var i=0@";" i<len@";" i++)
        tmps[i]=[opts[i].text,
                 opts[i].value,
                 opts[i].getAttribute("x-installer-size"),
                 opts[i].getAttribute("x-mirror"),
                 i];
      tmps.sort(isBetter);
      for (var i=0@";" i<len@";" i++) {
        opts[i].text  = tmps[i][0];
        opts[i].value = tmps[i][1];
        opts[i].setAttribute("x-installer-size", tmps[i][2]);
        opts[i].setAttribute("x-mirror", tmps[i][3]);
      }
      opts.selectedIndex = 0;
    }
    for (var i = 0; i < packages.length; i++) {
      initialize_selector(document.getElementById("platform_selector_" + packages[i]));
    }
    selection_changed();
    })();
    @||})
