#lang meta/web

(define-context "bugs")

(define planet-bugs "http://planet.racket-lang.org/trac/newticket")

;; a starred option is the default
(define (platform-option-links)
  (mk-options '([* "...or choose" ""]
                ["Windows Vista" "windows-vista"]
                ["Windows XP" "windows-xp"]
                ["Windows 2000" "windows-2000"]
                ["Windows NT" "windows-nt"]
                ["Windows 95/98/Me" "windows-9x"]
                ["Macintosh PowerPC (MacOS X)" "mac-ppc-osx"]
                ["Macintosh Intel (MacOS X)" "mac-i386-osx"]
                ["Linux, Fedora/RedHat based" "linux-fedora"]
                ["Linux, Ubuntu/Debian based" "linux-ubuntu"]
                ["Linux, other distro" "linux-other"]
                ["Sun Solaris" "solaris"]
                ;; ["Sun Solaris 8" "solaris-8"]
                ;; ["Sun Solaris, other version" "solaris-other"]
                ["Other Unix" "unix-other"]
                ;; ["Macintosh PowerPC (MacOS Classic)" "mac-ppc-macos"]
                ;; ["Macintosh 68K" "mac-68k"]
                ;; ["BeOS" "beos"]
                ;; ["MzScheme kernel" "mzkernel"]
                )))
(define (severity-option-links)
  (mk-options '(["Critical" "critical"]
                [* "Serious" "serious"]
                ["Non-critical" "non-critical"])))
(define (bug-class-option-links)
  (mk-options '([* "Software Bug" "sw-bug"]
                ["Documentation Bug" "doc-bug"]
                ["Change Request" "change-request"]
                ["Support" "support"])))

(define (mk-options opts)
  (for/list ([s (in-list opts)])
    (if (eq? '* (car s))
      @option[selected: 'true value: (caddr s)]{@(cadr s)}
      @option[value: (cadr s)]{@(car s)})))

(require (only-in "../www/all.rkt" download))

(define (cgi-link from . to)
  (apply symlink (format "/www/cgi-bin/~a" from) to))
(define bug-report-cgi (cgi-link "bug-report" "bug-report.cgi"))
(define bug-report-captcha (cgi-link "bug-report-captcha"))

(provide captcha-file)
(define captcha-file (make-parameter #f))

(define query (cgi-link "gnatsweb" "query"))

(define (bugs-script)
  @script/inline{
    var bugform = null;
    var browser_platform = "";
    var params  = new Array();
    var cookies = new Array();
    @||
    function initBugData() {
      bugform = document.getElementById("BugForm");
      if (navigator.platform && navigator.platform != "")
        browser_platform = navigator.platform;
      if (navigator.cpuClass && navigator.cpuClass != "")
        browser_platform += " / " + navigator.cpuClass;
      if (navigator.userAgent && navigator.userAgent != "")
        browser_platform += " / " + navigator.userAgent;
      if (location.search.length > 0) {
        var paramstrs = location.search.substring(1).split(@"/[;&]/");
        for (var i in paramstrs) {
          var param = paramstrs[i].split(/=/);
          if (param.length == 2)
            params[param[0]] = unescape(param[1]).replace(/\+/g," ");
        }
      }
      if (document.cookie.length > 0) {
        var cookiestrs = document.cookie.split(@"/; */");
        for (var i in cookiestrs) {
          var eql = cookiestrs[i].indexOf('=');
          if (eql >= 0)
            cookies[cookiestrs[i].substring(0,eql)] =
              unescape(cookiestrs[i].substring(eql+1));
        }
      }
      if (params["v"]) bugform.version.value = params["v"];
      DoUserFields(RestoreUserField);
      if (bugform.platform.value == "") {
        if (bugform.platform_options.selectedIndex == 0) UpdatePlatformUser();
        else UpdatePlatformOptions();
      }
      if (bugform.email.value == "")     bugform.email.focus();
      else if (bugform.name.value == "") bugform.name.focus();
      else                               bugform.subject.focus();
    }
    @||
    function SaveUserField(field, name) {
      if (field.value != "") {
        var d = new Date();
        d.setTime(d.getTime()+(365*24*60*60*1000));
        document.cookie = name + "=" + escape(field.value)
                        + "; expires="+ d.toGMTString()
                        + "; path=/";
      }
    }
    function RestoreUserField(field, name) {
      if (field.value == "" && cookies[name]) field.value = cookies[name];
    }
    function DoUserFields(op) {
      op(bugform.email, "email");
      op(bugform.name, "name");
      op(bugform.version, "version");
      op(bugform.platform, "platform");
      op(bugform.platform_user, "platform_user");
      op(bugform.platform_options, "platform_options");
    }
    @||
    function CheckSubmit() {
      DoUserFields(SaveUserField);
      if (bugform.email.value == "") {
        window.alert("Please enter an Email");
        return false;
      }
      if (bugform.subject.value == "" && bugform.description.value == "") {
        window.alert("Please enter a summary and/or a description"
                     + " of your bug");
        return false;
      }
      if (bugform.version.value == "") {
        window.alert("Please enter your Racket version");
        return false;
      }
      return true;
    }
    @||
    var old_platform_user = null;
    function UpdatePlatformUser() {
      var newval = bugform.platform_user.value;
      if (old_platform_user != newval) {
        if (newval == "" && old_platform_user != browser_platform) {
          newval = browser_platform;
          bugform.platform_user.value = browser_platform;
          bugform.platform_user.select();
        }
        bugform.platform.value = newval;
        bugform.platform_options.selectedIndex = 0;
        bugform.platform_user.focus();
        old_platform_user = newval;
      }
    }
    @||
    function UpdatePlatformOptions() {
      var d = new Date();
      var opts = bugform.platform_options;
      var newval = opts.options[opts.selectedIndex].value;
      if (newval == "") {
        bugform.platform.value = browser_platform;
        bugform.platform_user.value = old_platform_user = browser_platform;
      } else {
        bugform.platform.value = newval;
        bugform.platform_user.value = old_platform_user = "...or type";
      }
      bugform.platform_user.select();
      opts.focus();
    }})

(define index
  @page[#:title "Bug Reports" #:extra-headers bugs-script
        #:extra-body-attrs `(onLoad: "initBugData();")]{
    @p[style: '("padding: 5px; color: #a00; background-color: #ffe;"
                " border: 1px solid; font-weight: bold;")]{
         If you can, please use the “Submit Bug Report” item in DrRacket's Help
         menu.  It works better than this page, because it helps you supply
         precise information about your Racket installation and working
         environment.}
    @p{@strong{Note:} Bug reports for PLaneT packages are submitted on the
       @a[href: planet-bugs]{PLaneT server}.}
    @p{Before submitting a bug report, you may wish to:
       @ul{@li{Consult the @-docs,}
           @li{@download a newer Racket version if there is one (Racket
               displays its version number on startup),}
           @li{@a[href: (list query "/")]{Query existing bug reports}.}}}
    @(define (field mode title . input)
       (let ([title (b title ":")])
         (case mode
           [(line) (list title " " input br)]
           [(br)   (list title br input br br)]
           [(tr)   (tr (td title) (td input))]
           [else (error 'field "internal error")])))
    @form[action: bug-report-cgi method: 'post id: 'BugForm
          ;; enctype: "multipart/form-data"
          style: '("border: 2px solid #44f; padding: 6px;"
                   " background-color: #eef;")
          onsubmit: "return CheckSubmit();"]{
      @input[type: 'hidden name: 'cont value: thanks]
      @field['br "Your name"]{
        @input[type: 'text name: 'name value: "" size: 40]}
      @field['br "Your e-mail address"]{
        @input[type: 'text name: 'email value: "" size: 40]}
      @field['br "Summary of the problem"]{
        @input[type: 'text name: 'subject value: "" size: 60]}
      @table{
        @field['tr "Version"]{
          @input[type: 'text name: 'version value: "" size: 14]}
        @field['tr "Platform"]{
          @input[type: 'text name: 'platform_user size: 30
                 onchange: "UpdatePlatformUser();"
                 onkeyup: "UpdatePlatformUser();"]@;
          @|nbsp|@;
          @select[name: 'platform_options
                  onchange: "UpdatePlatformOptions();"]{
            @platform-option-links}@;
          @input[type: 'hidden name: 'platform]}
        @field['tr "Severity"]{
          @select[name: 'severity]{@severity-option-links}}
        @field['tr "Class"]{
          @select[name: 'class]{@bug-class-option-links}}}
      @br
      @field['br "Description of the problem"]{
        @textarea[name: 'description rows: 12 cols: 70
          style: "font-family: monospace;"]{}}
      @field['br '("If possible, please give a short sequence of steps to"
                   " reproduce the problem")]{
        @textarea[name: 'how-to-repeat rows: 8 cols: 70
                  style: "font-family: monospace;"]{}}
      @; An attachement requires a cgi script that can deal with input
      @; that is in "multipart/form-data" format.
      @; @field['line "Attachment"]{
      @;   @input[type: 'file name: 'attachment size: 20]}
      @(let* ([c (captcha-file)]
              [c (and c (img src: (cgi-link c) align: "middle"))])
         (and c @field['line @list{Please type @c}]{
                  @input[type: 'text name: 'captcha value: "" size: 10]}))
      @input[type: 'submit value: "Submit"]}})

(define thanks
  @page[#:title "Thanks!" #:extra-headers bugs-script #:referrer values]{
    @p{@strong{Thanks!}}
    @p{Your Racket bug report has been submitted.}
    @p{You should receive an email confirming your report in a few minutes.
       The confirmation will be sent to the email address you specified in
       your report.}
    @p{If you do not receive such confirmation, please report the problem to
       @tt{@small{racket@"@"racket-lang.org}}
       or to the Racket
       @a[href: "http://lists.racket-lang.org/users/"]{mailing list}.}})
