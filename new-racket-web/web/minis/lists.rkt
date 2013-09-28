#lang meta/web

(provide mailing-lists-quick)

(define-context "lists")

(struct ML (name gmane-name google-name description))

(define MLs
  (list (ML "users" "user" "racket-users"
            @text{A discussion list for all things related to Racket.
                  Ask your questions here!})
        (ML "announce" "announce" #f
            @text{A low-volume, moderated list for announcements, only.
                  @small{(These are posted on the @TT{users} list too, no need
                         to subscribe to both.)}})
        (ML "dev" "devel" #f
            @text{A mailing list for Racket development.
                  @small{(For people who want to see how the sausages are made
                         — and help make them.)}})))

(define lists
  @page[#:title "Mailing Lists" #:file "" #:part-of 'community
        #:description
        @'{Racket mailing lists for users, developers, announcements, and more.}
    (define (list-cells what) (map (λ (r) (r what)) list-renderers))
    ]{
    @p{This is the Racket mailing list server.  We have several public mailing
       lists, some are listed below with several mirrors for each one.  The
       complete list of public mailing lists is available on
       @a[href: "listinfo"]{this page}.}
    @(define gap1 (tr (map (λ (_) @td{@div[style: "height: 1ex;"]{}}) MLs)))
    @(define gap2 (tr (map (λ (_) @td{}) MLs)))
    @(define (sec . text)
       @list{@gap1
             @tr{@td[style: '("background-color: #dddddd; font-weight: bold;"
                              " padding: 0;")
                     colspan: (length MLs)]{@text}}
             @gap2})
    @table[style: "width: 100%; margin: auto; text-align: center;"
           frame: 'box rules: 'cols cellpadding: 5]{
      @tr[style: "border-bottom: 1px solid; background-color: #ccccff;"]{
        @(list-cells 'header-cell)}
      @tr[valign: 'top style: "text-align: left;"]{@(list-cells 'description)}
      @tr{@(list-cells 'main-page-cell)}
      @tr{@(list-cells 'graph-cell)}
      @sec{Subscription: enter your email here to subscribe to a mailing list}
      @tr{@(list-cells 'subscribe-cell)}
      @sec{Gmane Mirror}
      @tr{@(list-cells 'gmane-cell)}
      @sec{Archive at mail-archive.com}
      @tr{@(list-cells 'mail-archive-cell)}
      @sec{Google group mirror}
      @tr{@(list-cells 'google-cell)}}})

(define (list-renderer ml)
  (define name (ML-name ml))
  (define at-domain "@racket-lang.org")
  (define email (list name at-domain))
  (define description (ML-description ml))
  (define gmane
    (let ([gm (ML-gmane-name ml)])
      (and gm @list{gmane.comp.lang.racket.@(ML-gmane-name ml)})))
  (define (gmane-link base . body)
    (unless gmane
      (error 'list-renderer "internal error: no gmane info for ~a" name))
    (let* ([path (if (pair? base) (list "/" (cdr base) "/") "/")]
           [base (if (pair? base) (car base) base)]
           [pfx (if (regexp-match? #rx"://" base) base (list "http://" base))])
      @a[href: (list pfx ".gmane.org" path gmane)]{@body}))
  (define (mail-archive-link suffix . body)
    @a[href: (list "http://www.mail-archive.com/" email "/" suffix)]{@body})
  (define google-groups-url
    (let ([g (ML-google-name ml)])
      (and g (list "http://groups.google.com/group/" g "/"))))
  (define ((mk-form make) url #:method [method 'get] . body)
    (make @form[action: url method: method
                style: "display: inline; clear: none;"]{
            @div{@body}}))
  (define (mk-subscribe mk)
    @(mk-form mk)[(list (url-of lists #t) name "/subscribe") #:method 'post]{
      @input[type: 'text name: 'email size: 20 value: ""
             placeholder: "Email to Subscribe"
             title: @list{Enter your email to subscribe
                          to the "@name" mailing list.}]})
  (define form-cell (mk-form td))
  (λ (what)
    (case what
      [(header-cell)
       @th[style: "width: 33%;"]{
         @; the mixed styles will help against some spam harvesting too
         @span[style: '("font-size: x-large;"
                        " font-family: monospace; font-weight: bold;")]{
           @TT{@name}@;
           @span[style: "font-size: small;"]{@at-domain}}}]
      [(description) @td{@description}]
      [(main-page-cell)
       @td{@a[href: (list name "/")]{@big{@b{@TT{@name}}} page}
           @bull
           @a[href: (list name "/archive/")]{archive}}]
      [(graph-cell)
       @td{@img[src: (list "http://gmane.org/plot-rate.php?group=" gmane)
                style: "width: 80%;"]}]
      [(subscribe-cell) (mk-subscribe td)]
      [(google-cell)
       (if google-groups-url
         @form-cell[(list google-groups-url "search")]{
           @a[href: google-groups-url]{
             @(string-titlecase
               (regexp-replace* #rx"-" (ML-google-name ml) " "))}
           @br
           @span[style: "white-space: nowrap;"]{
             Search: @input[type: 'text name: 'q value: "" size: 20].}}
         @td{@small{—none—}})]
      [(mail-archive-cell)
       @form-cell["http://www.mail-archive.com/search"]{
         @input[type: 'hidden name: 'l value: email]
         @mail-archive-link["info.html"]{Archive}
         @bull
         @mail-archive-link[""]{Browse}
         @bull
         @mail-archive-link["maillist.xml"]{RSS}
         @br
         @span[style: "white-space: nowrap;"]{
           Search: @input[type: 'text name: 'q value: "" size: 20].}}]
      [(gmane-cell)
       @form-cell["http://search.gmane.org/"]{
         @input[type: 'hidden name: 'group value: gmane]
         @gmane-link["dir"]{@TT{@gmane}}
         @br
         @gmane-link["news"]{threaded}
         @bull
         @gmane-link["blog"]{blog}
         @bull
         @gmane-link["nntp://news"]{newsgroup}
         @br
         Feed:
         @gmane-link['("rss" "messages/complete")]{messages}
         @bull
         @gmane-link['("rss" "topics/complete")]{topics}
         @br
         @;Excerpt feed:
         @;@gmane-link['("rss" "messages/excerpts")]{messages}
         @;@bull
         @;@gmane-link['("rss" "topics/excerpts")]{topics}
         @;@br
         @span[style: "white-space: nowrap;"]{
           Search: @input[type: 'text name: 'query value: "" size: 20].}}]
      [(quick)
       @text{
         @big{@TT{@b{@name}}@small{@tt{@at-domain}}}
         @div[style: "margin-left: 2em;"]{
           @description
           @br
           @div[style: "float: right;"]{@(mk-subscribe values)}
           [@a[href: (list (url-of lists) name "/")]{list page},
            @gmane-link["dir"]{gmane mirror},
            @mail-archive-link[""]{mail-archive}@;
            @(and google-groups-url
                  @text{, @a[href: google-groups-url]{google group mirror}})]}}]
      [else (error 'list-cell "internal error")])))

(define list-renderers (map list-renderer MLs))

(define (mailing-lists-quick)
  @text{@(apply parlist @strong{Mailing Lists}
                (map (λ (r) (r 'quick)) list-renderers))
        @p{See the @lists{mailing list server} for more details.}})
