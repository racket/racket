#lang meta/web

(require "shared.rkt" "data.rkt" "installer-pages.rkt"
         (prefix-in pre: "../stubs/pre.rkt"))

(provide render-download-page)

(define (render-download-page [version current-version] [package 'racket])
  @center-div{
    @h2{Download @(package->name package)
                 v@version (@(version->date version))}
    @div[id: "download_panel" style: "display: none;"]{
      Platform:
      @select[id: "platform_selector"
              onchange: "selection_changed();"
              onkeypress: "selection_changed();"]{
        @(for/list ([i (in-list all-installers)]
                    #:when (and (equal? version (installer-version i))
                                (equal? package (installer-package i))))
           (installer->page i 'render-option))}
      @input[type: 'submit value: "Download" onclick: "do_jump();"]
      @|br hr|
      @div[align: "center"]{
        @small{@all-version-pages @nbsp @bull @nbsp
               @license @nbsp @bull @nbsp @pre:installers}}
      @hr
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
                      #:when (and (equal? version (installer-version i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}})

(define all-version-pages
  (let ()
    (define all-versions
      (remove-duplicates (map installer-version all-installers)))
    (define all-packages
      (remove-duplicates (map installer-package all-installers)))
    (define (make-page ver pkg)
      (define file  (format "~a-v~a.html" pkg ver))
      (define title @list{@(package->name pkg) v@ver})
      (define label @list{v@ver @small{(@(version->date ver))}})
      (define the-page
        @page[#:file file #:title title #:part-of 'download]{
          @(render-download-page ver pkg)})
      (the-page label))
    @page[#:id 'all-versions #:title "All Versions" #:part-of 'download]{
      @table[width: "90%" align: 'center cellspacing: 10 cellpadding: 10
             rules: 'cols frame: 'box]{
        @thead{
          @tr[style: "border-bottom: 1px solid;"]{
            @(map (lambda (p)
                    @th[width: "50%" align: 'center]{@(package->name p)})
                  all-packages)}}
        @tbody{
          @(map (lambda (v)
                  (tr (map (lambda (p) (td align: 'center (make-page v p)))
                           all-packages)))
                all-versions)}}}))

(define license
  @page[#:title "Software License" #:part-of 'download]{
    @p{Racket is distributed under the
       @a[href: "http://www.gnu.org/copyleft/lesser.html"]{
         GNU Lesser General Public License (LGPL)}.
       This means that you can link parts of Racket (such as racket or gracket)
       into proprietary applications, provided that you follow the specific
       rules stated in the LGPL.  You can also modify Racket software; if you
       distribute a modified version, you must distribute it under the terms of
       the LGPL, which in particular means that you must release the source
       code for the modified Racket software.}})

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
    function getPlatformOrder() {
      var p = navigator.platform;
      var l = function(str) { return p.indexOf(str) != -1@";" }
      var Win      = /Windows/,
          Mac      = /Macintosh/,
          MacIntel = /Macintosh.*Intel/,
          MacPPC   = /Macintosh.*PPC/,
          Linux    = /Linux/,
          Linux64  = /Linux.*x86_64/,
          Linux32  = /Linux.*i386/,
          Unix     = /Unix/,
          Solaris  = /Solaris/;
      var default_order = [Win, Mac, Linux, Unix];
      // The default is the common case
      if (p == null) return default_order;
      else if (l("SunOS")) return [Solaris, Unix, Linux, Mac, Win];
      else if (l("Win"))   return [Win, Mac, Linux, Unix];
      else if (l("Mac"))
      return [(l("Intel")?MacIntel:MacPPC), Mac, Unix, Linux, Win];
      else if (l("Linux")) {
        // also show the linux explanation if it's a linux
        document.getElementById("linux_explain").style.display = "block";
        return [(l("_64")?Linux64:Linux32), Linux, Unix, Mac, Win];
      } else return default_order;
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
    var tmps = new Array(len); // temp array to sort the options
    // get the order and a make a sorting function
    var order = getPlatformOrder();
    function getOrder(str) {
      for (var i=0@";" i<len@";" i++)
        if (str.search(order[i]) >= 0) return i * 10;
      return 999;
    }
    function isBetter(opt1,opt2) {
      var ord1 = getOrder(opt1[0]), ord2 = getOrder(opt2[0]);
      // prefer non-source
      if (opt1[0].search("source")>=0) ord1 += 1;
      if (opt2[0].search("source")>=0) ord2 += 1;
           if (ord1 < ord2)       return -1;
      else if (ord1 > ord2)       return +1;
      else if (opt1[0] < opt2[0]) return -1;
      else if (opt1[0] > opt2[0]) return +1;
      else                        return  0;
    }
    // sort the options, need to use a temporary array
    for (var i=0@";" i<len@";" i++)
      tmps[i]=[opts[i].text,opts[i].value];
    tmps.sort(isBetter);
    for (var i=0@";" i<len@";" i++) {
      opts[i].text  = tmps[i][0];
      opts[i].value = tmps[i][1];
    }
    opts.selectedIndex = 0;
    })();
    @||})
