#lang at-exp s-exp "shared.rkt"

(require "data.rkt" "installer-pages.rkt")

(provide render-download-page)

(define (render-download-page [version current-version] [package 'racket])
  @center-div{
    @h2{Download @(package->name package)
                 v@version (@(version->date version))}
    @div[id: "download_panel" style: "display: none;"]{
      Platform:
      @select[id: "platform_selector"]{
        @(for/list ([i (in-list all-installers)]
                    #:when (and (equal? version (installer-version i))
                                (equal? package (installer-package i))))
           (installer->page i 'render-option))}
      @input[type: 'submit value: "Download" onclick: "do_jump();"]}
    @script/inline[type: 'text/javascript]{
      document.getElementById("download_panel").style.display = "block";
      function do_jump() {
        var sel = document.getElementById("platform_selector");
        location.href = sel[sel.selectedIndex].value;
      }
      @platform-script
    }
    @noscript{
      Installers are available for the following platforms:
      @ul{@(for/list ([i (in-list all-installers)]
                      #:when (and (equal? version (installer-version i))
                                  (equal? package (installer-package i))))
             @li{@(installer->page i 'only-platform)})}}})

(define platform-script
  @literal{@||
    (function() {
    var opts = document.getElementById("platform_selector").options;
    var len = opts.length;
    // returns a platform name, doubles as a regexp too
    function getPlatform() {
      var p = navigator.platform;
      var l = function(str) { return p.indexOf(str) != -1@";" }
      // The default is the common case
      return (p == null)  ? "Windows" :
             l("Linux") ? (l("_64") ? "Linux x86_64" : "Linux i386")   :
             l("SunOS") ? "Solaris" :
             l("Mac") ? (l("Intel") ? "Mac OS X Intel" : "Mac OS X PPC") :
             "Windows";
    }
    // convert name to a regexp by splitting on words
    var rx = new RegExp(getPlatform().replace(/ +/g,".*"));
    for (var i=0@";" i<len@";" i++) {
      if (opts[i].text.search(rx) >= 0) {
        opts.selectedIndex = i; break;
      }
    }
    })();
    @||})
