#lang plt-web

(require "resources.rkt"
         plt-web/style)

(define name i)

(define (url str) (tt (a href: str str)))

(define styles
  @style/inline{
    .nestedheading {
      text-align: center;
      font-weight: bold;
      font-size: large;
    }
    .nested {
      margin-left: auto;
      margin-right: auto;
      margin-top: 1em;
      margin-bottom: 1em;
    }
    .faqsection {
      margin-left: auto;
      margin-right: auto;
      margin-top: 2em;
    }
    .faqques {
      font-weight: bold;
      margin-top: 1em;
      margin-bottom: 1em;
    }
    .faqans {
      margin-left: 1em;
    }})

(define ((FAQ tag . ques) . answer)
  @div[class: 'faqentry]{
    @div[class: 'faqques]{@a[name: tag]{@ques}}
    @div[class: 'faqans]{@answer}})

(define (heading . text)
  (apply div class: 'nestedheading text))
(define (nested . text)
  (apply div class: 'nested text))

(define new-name
  @page[#:site www-site
        #:title "From PLT Scheme to Racket"]{
    @styles

  @columns[9 #:row? #t]{

    @heading{PLT Scheme is a Racket}

    @nested{Sure, it has parentheses, uses the keyword @tt{lambda}, provides
      lexical scope, and emphasizes macros — but don't be fooled.  PLT Scheme
      is no minimalist embodiment of 1930s math or 1970s technology.  PLT
      Scheme is a cover for a gang of academic hackers who want to fuse
      cutting-edge programming-language research with everyday programming.
      They draw you in with the promise of a simple and polite little Scheme,
      but soon you'll find yourself using modules, contracts, keyword
      arguments, classes, static types, and even curly braces.}


    @heading{Racket is a Scheme}

    @nested{Racket is still a dialect of Lisp and a descendant of Scheme.  The
      tools developed by PLT will continue to support R5RS, R6RS, the old
      @tt{mzscheme} environment, Typed Scheme, and more.  At the same time,
      instead of having to say “PLT's main variant of Scheme,” programmers can
      now simply say “Racket” to refer to the specific descendant of Scheme
      that powers PLT's languages and libraries.}


    @div[class: 'faqsection]{
      @heading{Anticipated Questions}


      @@FAQ['why]{Why change the name?}{
      @p*{
        The @name{Scheme} part of the name @name{PLT Scheme} is misleading, and
        it is often an obstacle to explaining and promoting PLT research and
        tools.
      @~
        For example, when you type “scheme” into Google, the first hit is a
        Wikipedia entry written from an R5RS perspective.  That's appropriate
        for a Wikipedia page on Scheme, but it's not a good introduction to PLT
        Scheme. As long as we call our language @name{Scheme}, we struggle to
        explain our language, and we are usually forced to start the
        explanation with a disclaimer.  At the same time, to the degree that
        the PLT community has defined @name{Scheme} through market share,
        publications, and educational outreach, we interfere with everyone
        else's ability to define @name{Scheme} — and many have a valid claim to
        that ability.
      @~
        By switching to @name{Racket}, we expect to resolve this communication
        problem.}}


      @@FAQ['what]{What will the change look like?}{
      @p*{
        @name{DrScheme} becomes @name{DrRacket}.  The @tt{mzscheme} executable
        becomes @tt{racket}, and @tt{mred} becomes @tt{gracket} (following a
        common convention for “GUI @tt{racket}”).  We change each @tt{#lang
        scheme} to @tt{#lang racket} in the Racket distribution, although
        @tt{#lang scheme} will be supported for backward compatibility. The
        @url{http://plt-scheme.org} site will be replaced by
        @url{http://racket-lang.org}.  The @tt{plt-scheme} mailing list becomes
        the @tt{racket} mailing list (@tt|{users@racket-lang.org}|).
      @~
        The Racket site and documentation will note that Racket is a descendant
        of Scheme, but most current uses of the word “Scheme” (which implicitly
        mean PLT Scheme) will be replaced by “Racket.”
      @~
        Racket programmers are @name{Racketeers}, of course.}}


      @@FAQ['scheme]{Does this mean that PLT will no longer implement Scheme?}{
      @p*{
        There will be little difference between the current @tt{#lang scheme}
        and the new @tt{#lang racket}, but the latter will become the preferred
        form.
      @~
        In addition, PLT will continue to support standards such as R5RS and
        R6RS.  The transition from/to various Scheme languages to/from Racket
        will be as easy/difficult as before.}}


      @@FAQ['transition]{What happens to all of the old programs, scripts,
        address books, bookmarks, papers, etc. that refer to @name{PLT Scheme}
        instead of @name{Racket}?}{
      @p*{
        Old executables, web sites, mailing addresses, and module names will
        forward to the new ones.  We will work to make the transition as
        painless as possible and to preserve old references for as long as
        possible.}}

      @@FAQ['edu]{How can I tell my department that we should teach with Racket
        instead of Scheme?  They've never heard of @name{Racket}.}{
      @p*{
        If you felt comfortable claiming that PLT Scheme was Scheme before,
        then you can still say that you want to teach with Scheme, even if the
        environment is called @name{DrRacket}.  Racket is a descendant of
        Scheme, just like PLT Scheme was.}}


      @@FAQ['brand]{Aren't you worried that you will lose brand recognition by
        switching from the name @name{Scheme} to @name{Racket}?}{
      @p*{
        Yes.  Nevertheless, we think the long-term benefit of a new name will
        outweigh the short-term difficulties of changing.}}


      @@FAQ['plt]{Instead of picking a new name, why not just call the language
        @name{PLT}?}{
      @p*{
        Some of us tried that, informally.  It felt awkward, because we use
        @name{PLT} to refer to a group of people, and because we have used
        @name{PLT} as a modifier for @name{Scheme} and other nouns.  Switching
        the language name from one noun to another sounds better, it's clearer,
        and it's easier to explain.}}


      @@FAQ['suggestions]{Couldn't you find a better name?
        @name{[Insert name here]} would be better.}{
      @p*{
        Thank you for the suggestion.  The name @name{Racket} meets some basic
        criteria: it isn't used already, it's easy to pronounce and spell, and
        it has a vague connection to the word “scheme.”  Mostly, though, we
        just like it.}}

      }}})
