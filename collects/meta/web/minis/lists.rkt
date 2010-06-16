#lang at-exp s-exp "../common.rkt"

(provide mailing-lists-quick)

(define-context "lists")

(struct ML (name gmane-name google-name description))

(define MLs
  (list (ML "users" "user" "racket-users"
            @text{A discussion list for all things related to Racket.
                  Ask your questions here!})
        (ML "announce" "announce" #f
            @text{A low-volume, moderated list for announcements, only.})
        (ML "dev" "devel" #f
            @text{A mailing list for Racket development, for people who want to
                  see how the sausages are made @mdash and help make them.})))

(define lists
  @page[#:title "Mailing Lists" #:file "" #:part-of 'community]{
    @p{This is the Racket mailing list server.  We have three public mailing
       lists listed below, with several mirrors for each one.}
    @(map show-list MLs)})

(define (show-list ml)
  @text{
    @(define name (ML-name ml))
    @(define at-domain "@racket-lang.org")
    @(define email (list name at-domain))
    @; the mixed styles will help against some spam harvesting too
    @h1[style: '("margin-top: 2.5ex;"
                 " font-family: monospace;"
                 " text-decoration: underline;")]{
      @a[href: (list name "/")]{
        @span[style: "background-color: #dde; text-decoration: underline;"]{
          @name}@;
        @span[style: "font-size: 82.5%;"]{@at-domain}}}
    @p{@ML-description[ml]}
    @form[action: (list name "/subscribe") method: 'post]{
      @p[style: "white-space: nowrap;"]{
        Quick subscribe: @input[type: 'text name: 'email size: 20 value: ""]}}
    @(cond [(ML-google-name ml)
            => (lambda (g)
                 @p{Mirrord on Google Groups as the
                    @a[href: @text{http://groups.google.com/group/@|g|/}]{
                      @(string-titlecase (regexp-replace* #rx"-" g " "))
                      group.}})])
    @(let* ([gmane @list{gmane.comp.lang.racket.@(ML-gmane-name ml)}]
            [G "gmane.org"])
       @form[action: @`{http://search.@,|G|/} method: 'get]{
         @p{@input[type: 'hidden name: 'group value: gmane]
           Mirrord as the
           @a[href: @`{http://dir.@,|G|/@,gmane}]{@TT{@gmane}}
           newsgroup on Gmane:
           Browse using
           @a[href: @`{http://news.@,|G|/@,gmane}]{a threaded interface},
           @a[href: @`{http://blog.@,|G|/@,gmane}]{a blog-like interface},
           or
           @a[href: @`{nntp://news.@,|G|/@,gmane}]{as a newsgroup};
           RSS of complete
           @a[href: @`{http://rss.@,|G|/messages/complete/@,gmane}]{messages}
           or
           @a[href: @`{http://rss.@,|G|/topics/complete/@,gmane}]{topics},
           and RSS of excerpted
           @a[href: @`{http://rss.@,|G|/messages/excerpts/@,gmane}]{messages}
           or
           @a[href: @`{http://rss.@,|G|/topics/excerpts/@,gmane}]{topics};
           @span[style: "white-space: nowrap;"]{
             Search: @input[type: 'text name: 'query value: "" size: 20].}}})
    @(let ([url @list{http://www.mail-archive.com/@|email|/}])
       @form[action: "http://www.mail-archive.com/search" method: 'get
             style: "display: inline; clear: none;"]{
         @p{@input[type: 'hidden name: 'l value: email]
           @a[href: (list url "info.html")]{Archived}
           at @tt{mail-archive.com}:
           @a[href: url]{Browse},
           @a[href: (list url "maillist.xml")]{RSS},
           @span[style: "white-space: nowrap;"]{
             Search: @input[type: 'text name: 'q value: "" size: 20].}}})})

;; TODO: improve this using the above, *and* link to the above

(define (maillist-email name)
  @TT{@big{@strong{@name}}@"@"racket-lang.org})
(define (maillist-url name)
  (define url "http://lists.racket-lang.org/")
  @text{@a[href: `(,url ,name "/")]{Subscribe}
        or @a[href: `(,url ,name "/archive/")]{browse}})

(define (mailing-lists-quick)
  @parlist[@strong{Mailing Lists}
    @text{@maillist-email{users} @mdash a discussion list for all things
      related to Racket.  Ask your questions here!
      (@maillist-url{users}.)
      @; These are not set up yet
      @; also via @gmane{racket} and @|google-groups|).
      }
    @text{@maillist-email{announce} @mdash a low-volume, moderated list
      for announcements, only.  (@maillist-url{announce}.)}
    @text{@maillist-email{dev} @mdash a mailing list for Racket development,
      for the people who want to see how the sausages are made and help make
      them.  (@maillist-url{dev}.)
      @; @";" also on @gmane{plt.dev}.)
      }])
