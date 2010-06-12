#lang at-exp s-exp "../common.rkt"

(define-context "stubs/mailman")

(define (MM  . tag) @literal{<MM-@|tag|>})
(define (MM/ . tag) @literal{</MM-@|tag|>})
(define (MMform name . body)
  (list @MM{@(and name (list name "-"))Form-Start} body @MM{Form-End}))

(define style-header
  @style/inline{
    h1 {
      text-align: center;
      background-color: #9cf;
      padding: 0.5em;
      font-size: 150%;
      font-weight: bold;
    }
    h2 {
      text-align: left;
      padding: 0.1em 0.5em;
      background-color: #fda;
      font-size: 120%;
      font-weight: bold;
    }
    .subp {
      margin: 0.5ex 0 0.5ex 1em;
    }})

(define (subp . body) (apply div class: 'subp body))
(define (graytd . body) (apply td bgcolor: "#ddd" body))

(define listinfo
  @page[#:title @list{Mailing lists: @MM{List-Name}}
        #:extra-headers style-header]{
    @; --------------------
    @comment{@||
      Based on the Mailman file "listinfo.html", revision: 5865
      Modified to fit the racket pages, add a mail-archive searchbox
      @||}
    @; --------------------
    @h1{@MM{List-Name}: @MM{List-Description}}
    @; --------------------
    @h2{About @MM{List-Name}
        @span[style: '("float: right; font-weight: normal; font-size: 80%;"
                       " margin-top: 4px;")]{
          @MMform['Lang]{@MM{displang-box} @MM{list-langs}}}}
    @subp{@MM{List-Info}}
    @subp{To see the collection of prior postings to the list, visit the
      @MM{Archive}@MM{List-Name} Archives@MM/{Archive}.
      @MM{Restricted-List-Message}}
    @; This is the mail-archive search box
    @form[action: "http://www.mail-archive.com/search" method: 'get]{@subp{
      @input[type: 'hidden name: 'l value: @MM{Posting-Addr}]
      Archives are also available at
      @a[href: @list{http://www.mail-archive.com/@MM{Posting-Addr}/}]{
        @tt{mail-archive}},
      search it here:
      @input[type: 'text name: 'q value: "" size: 16]}}
    @subp{(@a[href: "/"]{More information} on other ways to use this list and
      other public Racket lists.)}
    @; --------------------
    @h2{Using @MM{List-Name}}
    @subp{To post a message to all the list members, send email to
      @a[href: @list{mailto:@MM{Posting-Addr}}]{@MM{Posting-Addr}}.}
    @subp{You can subscribe to the list, or change your existing subscription,
      in the sections below.}
    @; --------------------
    @h2{Subscribing to @MM{List-Name}}
    @subp{Subscribe to @MM{List-Name} by filling out the following form.
      @MM{List-Subscription-Msg}}
    @subp{
      @MMform['Subscribe]{
        @table[cellspacing: 2 cellpadding: 2 width: "90%" align: 'center]{
          @tr{@graytd{Your email address:}
              @td{@MM{Subscribe-Box}}
              @td{@nbsp}}
          @tr{@graytd{Your name (optional):}
              @td{@MM{fullname-box}}
              @td{@nbsp}}
          @tr{@td[colspan: 3 style: "font-size: 82.5%;"]{
                  @p{You may enter a privacy password below.  This provides
                    only mild security, but should prevent others from messing
                    with your subscription. @b{Do not use a valuable password}
                    as it will occasionally be emailed back to you in
                    cleartext.}
                  @p{If you choose not to enter a password, one will be
                    automatically generated for you, and it will be sent to you
                    once you've confirmed your subscription.  You can always
                    request a mail-back of your password when you edit your
                    personal options.}
                  @MM{Reminder}}}
          @tr{@graytd{Pick a password:}
              @td{@MM{New-Password-Box}}
              @td{@nbsp}}
          @tr{@graytd{Reenter password to confirm:}
              @td{@MM{Confirm-Password}}
              @td{@nbsp}}
          @tr{@graytd{Which language do you prefer to display your messages?}
              @td{@MM{list-langs}}
              @td{@nbsp}}
          @MM{digest-question-start}
          @tr{@td{Would you like to receive list mail batched in a daily
                  digest?}
              @td{@MM{Undigest-Radio-Button} No
                  @MM{Digest-Radio-Button}   Yes}
              @td{@nbsp}}
          @MM{digest-question-end}
          @tr{@td[colspan: 3]{
                @p[style: "text-align: center;"]{
                  @MM{Subscribe-Button}}}}}}}
    @; --------------------
    @h2{@a[name: 'subscribers]{@MM{List-Name} Subscribers}}
    @subp{@MMform['Options]{@MM{Editing-Options}}}
    @; not needed: @subp{@MMform['Roster]{@MM{Roster-Option}}}
    @; --------------------
    @h2{@nbsp}
    @MM{Mailman-Footer}})

(define subscribe
  @page[#:title @list{@MM{List-Name} Subscription results}
        #:extra-headers style-header]{
    @; --------------------
    @comment{@||
      Based on the Mailman file "subscribe.html", revision: 3550
      Modified to fit the racket pages
      @||}
    @; --------------------
    @h1{@MM{List-Name} Subscription results}
    @subp{@MM{Results}}
    @MM{Mailman-Footer}})

(define options
  @page[#:title @list{@MM{Presentable-User} membership configuration
                      for @MM{List-Name}}
        #:extra-headers style-header]{
    @; --------------------
    @comment{@||
      Based on the Mailman file "options.html" (no revision specified)
      Modified to fit the racket pages
      @||}
    @; --------------------
    @h1{@MM{List-Name} mailing list membership configuration for
        @MM{Presentable-User}}
    @subp{@div[style: "float: right;"]{@MMform[#f]{@MM{logout-button}}}
      @b{@MM{Presentable-User}}'s subscription status, password, and options
      for the @MM{List-Name} mailing list.}
    @subp{@MM{Case-Preserved-User}
          @MM{Disabled-Notice}
          @div[style: "background-color: #faa;"]{@MM{Results}}}
    @(define (tablesec . body)
       (apply table cellspacing: 5 cellpadding: 3 width: "100%" align: 'center
              body))
    @(define (h2sub . body)
       @h2[style: "margin-bottom: 0; font-size: 100%;"]{@body})
    @(define (center . body)
       @div[style: "text-align: center; margin-top: 1ex;"]{@body})
    @(define (global-checkbox button-name)
       @div[align: 'right]{@MM{global-@|button-name|-button}@i{Set globally}})
    @MMform[#f]{
      @tablesec{
        @tr{@td[colspan: 2]{
              @h2sub{Changing your @MM{List-Name} membership information}
              @subp{You can change the address that you are subscribed to the
                mailing list with by entering the new address in the fields
                below.  Note that a confirmation email will be sent to the new
                address, and the change must be confirmed before it is
                processed.
                @small{(Confirmations time out after about
                       @MM{pending-days}.)}}
              @subp{You can also optionally set or change your real name
                (i.e. @em{John Smith}).}
              @subp{If you want to make the membership changes for all the
                lists that you are subscribed to at @MM{host}, turn on the
                @em{Change globally} check box.}}}
        @tr{@td[align: 'center]{
              @table[cellspacing: 2 cellpadding: 2 width: "80%"]{
                @tr{@graytd[align: 'right]{New address:}
                    @td{@MM{new-address-box}}}
                @tr{@graytd[align: 'right]{Again to confirm:}
                    @td{@MM{confirm-address-box}}}}}
            @td[align: 'center]{
              @table[cellspacing: 2 cellpadding: 2 width: "80%"]{
                @tr{@graytd[align: 'right]{Your name (optional):}
                    @td{@MM{fullname-box}}}}}}
        @tr{@td[colspan: 2 align: 'center]{@MM{change-address-button}}}
        @tr{@td[colspan: 2 align: 'center]{
              @MM{global-change-of-address}Change globally}}}
      @tablesec{
        @tr{@td[width: "50%"]{@h2sub{Unsubscribing from @MM{List-Name}}}
            @td[width: "50%"]{@h2sub{Your other @MM{Host} subscriptions}}}
        @tr{@td{Turn on the confirmation checkbox and hit this button to
                unsubscribe from this mailing list.
                @br
                @strong{Warning:} This action will be taken immediately!
                @center{@MM{Unsubscribe-Button}}}
            @td{You can view a list of all the other mailing lists at
                @MM{host} for which you are a member.  Use this if you want to
                make the same membership option changes to this other
                subscriptions.
                @center{@MM{Other-Subscriptions-Submit}}}}}
      @tablesec{
        @tr{@td[colspan: 2]{@h2sub{Your @MM{List-Name} Password}}}
        @tr[valign: 'top]{
          @td[width: "50%"]{
            @h3{@a[name: "reminder"]{Forgotten Your Password?}}
            Click this button to have your password emailed to your membership
            address.
            @br
            @MM{Umbrella-Notice}
            @center{@MM{Email-My-Pw}}}
          @td[width: "50%"]{
            @h3{@a[name: "changepw"]{Change Your Password}}
            @table[cellspacing: 2 cellpadding: 2 width: "70%" align: 'center]{
              @tr{@graytd[align: 'right]{New password:}
                  @td{@MM{New-Pass-Box}}}
              @tr{@graytd[align: 'right]{Again to confirm:}
                  @td{@MM{Confirm-Pass-Box}}}}
            @center{@MM{Change-Pass-Button}}
            @center{@MM{global-pw-changes-button}Change globally}}}}
      @tablesec{
        @tr{@td[colspan: 2]{@h2sub{Your @MM{List-Name} Subscription Options}}}
        @tr{@td[colspan: 2]{
              @subp{@em{Current values are checked.}}
              @subp{Note that some of the options have a @em{Set globally}
                checkbox.  Checking this field will cause the changes to be
                made to every mailing list that you are a member of on
                @MM{host}.  Click on @em{List my other subscriptions} above to
                see which other mailing lists you are subscribed to.}}}
        @tr{@graytd[width: "80%"]{
              @a[name: "disable"]{@strong{Mail delivery}} @br
              Set this option to @em{Enabled} to receive messages posted to
              this mailing list.  Set it to @em{Disabled} if you want to stay
              subscribed, but don't want mail delivered to you for a while
              (e.g. you're reading the list elsewhere).}
            @graytd[width: "20%"]{
              @MM{delivery-enable-button}Enabled @br
              @MM{delivery-disable-button}Disabled @br
              @global-checkbox{deliver}}}
        @tr{@graytd{@strong{Set Digest Mode} @br
              If you turn digest mode on, you'll get posts bundled together
              (usually one per day but possibly more on busy lists), instead of
              singly when they're sent.  If digest mode is changed from on to
              off, you may receive one last digest.}
            @graytd{
              @MM{Undigest-Radio-Button}Off @br
              @MM{Digest-Radio-Button}On}}
        @tr{@graytd{@strong{Get MIME or Plain Text Digests?} @br
              Your mail reader may or may not support MIME digests.  In general
              MIME digests are preferred, but if you have a problem reading
              them, select plain text digests.}
            @graytd{
              @MM{Mime-Digests-Button}MIME @br
              @MM{Plain-Digests-Button}Plain Text @br
              @global-checkbox{mime}}}
        @tr{@graytd{@strong{Receive your own posts to the list?} @br
              Ordinarily, you will get a copy of every message you post to the
              list.  If you don't want to receive this copy, set this option to
              @em{No}.}
            @graytd{@MM{dont-receive-own-mail-button}No @br
                    @MM{receive-own-mail-button}Yes}}
        @tr{@graytd{@strong{Receive acknowledgement mail when you send mail to
              the list?}}
            @graytd{
              @MM{dont-ack-posts-button}No @br
              @MM{ack-posts-button}Yes}}
        @tr{@graytd{@strong{Get password reminder email for this list?} @br
              Once a month, you will get an email containing a password
              reminder for every list at this host to which you are subscribed.
              You can turn this off on a per-list basis by selecting @em{No}
              for this option.  If you turn off password reminders for all the
              lists you are subscribed to, no reminder email will be sent to
              you.}
            @graytd{
              @MM{dont-get-password-reminder-button}No @br
              @MM{get-password-reminder-button}Yes @p
              @global-checkbox{remind}}}
        @tr{@graytd{@strong{Conceal yourself from subscriber list?} @br
              When someone views the list membership, your email address is
              normally shown (in an obscured fashion to thwart spam
              harvesters).  If you do not want your email address to show up on
              this membership roster at all, select @em{Yes} for this option.}
            @graytd{
              @MM{Public-Subscription-Button}No @br
              @MM{Hide-Subscription-Button}Yes}}
        @tr{@graytd{@strong{What language do you prefer?}}
            @graytd{@MM{list-langs}}}
        @tr{@graytd{@strong{Which topic categories would you like to subscribe
              to?} @br
              By selecting one or more topics, you can filter the traffic on
              the mailing list, so as to receive only a subset of the messages.
              If a message matches one of your selected topics, then you will
              get the message,otherwise you will not.
              @br
              If a message does not match any topic, the delivery rule depends
              on the setting of the option below.  If you do not select any
              topics of interest, you will get all the messages sent to the
              mailing list.}
            @graytd{@MM{topics}}}
        @tr{@graytd{@strong{Do you want to receive messages that do not match
              any topic filter?} @br
              This option only takes effect if you've subscribed to at least
              one topic above.  It describes what the default delivery rule is
              for messages that don't match any topic filter.  Selecting
              @em{No} says that if the message does not match any topic
              filters, then you won't get the message, while selecting @em{Yes}
              says to deliver such non-matching messages to you.
              @br
              If no topics of interest are selected above, then you will
              receive every message sent to the mailing list.}
            @graytd{
              @MM{suppress-nonmatching-topics}No @br
              @MM{receive-nonmatching-topics}Yes}}
        @tr{@graytd{@strong{Avoid duplicate copies of messages?} @br
               When you are listed explicitly in the @tt{To:} or @tt{Cc:}
               headers of a list message, you can opt to not receive another
               copy from the mailing list.  Select @em{Yes} to avoid receiving
               copies from the mailing list; select @em{No} to receive copies.
               @br
               If the list has member personalized messages enabled, and you
               elect to receive copies, every copy will have a
               @tt{X-Mailman-Copy: yes} header added to it.}
            @graytd{
              @MM{receive-duplicates-button}No @br
              @MM{dont-receive-duplicates-button}Yes @br
              @global-checkbox{nodupes}}}
        @tr{@td[colspan: 2 align: 'center]{@MM{options-Submit-button}}}}}
    @MM{Mailman-Footer}})
