#lang meta/web

(require "resources.rkt" "data.rkt" "installer-pages.rkt" "symlinks.rkt"
         (prefix-in pre: "../stubs/pre.rkt"))

(provide render-download-page)
(define (render-download-page [release current-release] [package 'racket])
  (define version (release-version release))
  @center-div{
    @h2{@(package->name package) v@version (@(release-date-string release))}
    @div[id: "download_panel" align: "center" style: "display: none;"]{
      @input[type: 'submit value: "Download" onclick: "do_jump();"
             style: '("font-size: 200%; font-weight: bolder;"
                      " letter-spacing: 0.2em;"
                      " margin: 0.5ex 0 1ex 0; width: 100%;")]
      @br
      @div{
        Platform:
        @select[id: "platform_selector"
                onchange:   "selection_changed();"
                onkeypress: "selection_changed();"]{
          @(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             (installer->page i 'render-option))}}
      @|br br|
      @(let* ([sep   @list{@nbsp @bull @nbsp}]
              [links (λ links @(tr (td (div style: "margin: 1ex 4ex;"
                                            (add-between links sep)))))]
              [docs  @list{@|docs|/@|version|/html}])
        @table[style: "text-align: center; font-size: small;"
               frame: 'hsides rules: 'rows]{
          @links[@list{Release: @nbsp @(release-page release){Announcement}}
                 @a[href: @list{@|docs|/release/}]{Notes}
                 @a[href: @docs]{Documentation}]
          @links[@license{License}
                 all-version-pages
                 @pre:installers{Nightly Installers}]})
      @br
      @div[id: "linux_explain"
           style: '("font-size: 75%; display: none; width: 28em;"
                    " margin-top: 1ex; text-align: center;")]{
        @b{Note about the Linux installers:} if you don't see an option for
        your particular platform, try other Linux installers, starting from
        similar ones.  Very often, a build on one Linux variant will work on
        others too.}}
    @downloader-script
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? release (installer-release i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}})

(define (release-page* rel)
  (define ver (release-version rel))
  (define title @list{v@ver Release Notes})
  @page[#:file (format "v~a.html" ver) #:title title #:part-of 'download]{
    @table[align: 'center]{
      @tr{@td{@h2{Release Announcements for Version @ver}}}
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
      @page[#:file file #:title title #:part-of 'download]{
        @(render-download-page rel pkg)})
    (define style
      @style/inline[type: 'text/css]{
        .version-row {
          background-color: #ffffc0;
        }
        .version-row:hover {
          background-color: #e0e0a0;
        }
        .version-row a {
          text-decoration: none;
        }
        .version-row a:hover {
          background-color: #eeee22;
        }})
    (define-values (main-package alt-packages)
      (cond [(null? all-packages)
             (eprintf "Warning: all-packages is empty\n")
             (values 'racket null)]
            [else
             (values (car all-packages) (cdr all-packages))]))
    @page[#:id 'all-versions #:title "All Versions" #:part-of 'download
          #:extra-headers style]{
      @table[align: 'center cellspacing: 0 cellpadding: 4 frame: 'box
             rules: 'groups]{
        @thead{
          @tr{@td{@nbsp @strong{Version & Release Notes}}
              @th[align: 'center]{@(package->name main-package)}
              @th[align: 'center]{Alternative Distributions}
              @td{@strong{Documentation}}}}
        @(let ([sep (tr style: "height: 4px; margin: 0; padding: 0;"
                        (td) (map (λ (_) (td)) all-packages))])
           ;; release=>packages : hash[release => (listof package)]
           ;; Indicates what packages actually exist (have installers) for a given release.
           (define release=>packages (make-hash))
           (for ([i (in-list all-installers)])
             (define r (installer-release i))
             (define prev-packages (hash-ref release=>packages r null))
             (unless (member (installer-package i) prev-packages)
               (hash-set! release=>packages r (cons (installer-package i) prev-packages))))
           (define (cell rel pkg)
             @td[align: 'center
                 @nbsp @(make-page rel pkg){[download]} @nbsp])
           @tbody{
             @sep
             @(for/list ([r (in-list all-releases)])
                (define ver (release-version r))
                  @list{
                    @tr[class: 'version-row]{
                      @td{@|nbsp nbsp| @strong{Version @ver},
                          @(release-page r){@release-date-string[r]}@;
                          @nbsp}
                      @(if (member main-package (hash-ref release=>packages r))
                           (cell r main-package)
                           @td[])
                      @td[align: 'center]{
                        @nbsp
                        @(add-between
                          (for/list ([p (in-list alt-packages)]
                                     #:when (member p (hash-ref release=>packages r)))
                            ((make-page r p) (format "[download ~a]" (package->name p))))
                          @nbsp)
                        @nbsp}
                      @td[align: 'center]{
                          @nbsp @a[href: @list{@|docs|/@|ver|/html}]{[HTML]} @;
                          @nbsp @a[href: @list{@|docs|/@|ver|/pdf}]{[PDF]} @;
                          @nbsp}}
                    @sep})})
        @tfoot{
          @tr[class: 'version-row]{
            @td[align: 'center colspan: 3]{@pre:installers}
            @td{@nbsp @pre:docs[#:sub 'html]{[HTML]} @;
                @nbsp @pre:docs[#:sub 'pdf]{[PDF]} @nbsp}}}}}))

(define license
  @page[#:title "Software License" #:part-of 'download]{
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
    @~ @ul*{
       @~ First, if you distribute your Racket application in source form or as
          compiled bytecode files, the Racket license does not restrict you at
          all.
       @~ Second, if you distribute your Racket application as compiled binary
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
          embedded in an extractable way in the resulting executable.}
    @~ We are, of course, not lawyers, and this should not be taken as legal
       advice.  However, we wanted to make it clear that Racket is an
       appropriate building block for all kinds of software, and to clarify how
       we view the license of Racket.}})

(define downloader-script
  @script/inline[type: 'text/javascript]{@||
    var do_jump, selection_changed;
    (function() {
    // show the download panel, since JS is obviously working
    document.getElementById("download_panel").style.display = "block";
    //
    var selector = document.getElementById("platform_selector");
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
    selection_changed_timer = false;
    selection_changed = function() {
      if (selection_changed_timer) clearTimeout(selection_changed_timer);
      selection_changed_timer = setTimeout(do_selection_changed, 250);
    }
    function do_selection_changed() {
      linux_expl_s.display =
        (selector[selector.selectedIndex].text.search(/Linux/) >= 0) ?
          "block" : "none";
    }
    //
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
      else if (opt1[2] < opt2[2]) return -1;
      else if (opt1[2] > opt2[2]) return +1;
      else                        return  0;
    }
    // sort the options, need to use a temporary array
    var tmps = new Array(len);
    for (var i=0@";" i<len@";" i++)
      tmps[i]=[opts[i].text,opts[i].value,i];
    tmps.sort(isBetter);
    for (var i=0@";" i<len@";" i++) {
      opts[i].text  = tmps[i][0];
      opts[i].value = tmps[i][1];
    }
    opts.selectedIndex = 0;
    })();
    @||})
