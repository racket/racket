#lang at-exp racket/base
(require scribble/manual
         scribble/core
         scribble/html-properties
         setup/dirs
         "private/utils.rkt"
         "private/manuals.rkt")

(provide build-contents
         make-default-doc-properties)

(define path-info-style (style "RootPathInfo" (list (attributes '((id . "rootPathInfo"))))))
(define go-style (style "RootPathAction" (list (attributes '((onclick . "return GoToRootPath();"))))))
(define disable-style (style "RootPathAction" (list (attributes '((onclick . "return DisableRootPath();"))))))

(define (build-contents #:user? [user? #f]
                        #:supplant [supplant #f]
                        #:style [style-in #f]
                        #:main-language-family [main-language-family (get-main-language-family)]
                        #:title-content [title-content (list main-language-family
                                                             (element (style #f '(aux)) " Documentation"))]
                        #:self-path [self-path #f]
                        #:bug-url [bug-url #f]
                        #:default-category [default-category '(language)]
                        #:doc-properties [doc-properties (make-default-doc-properties #:language-family main-language-family
                                                                                      #:default-category default-category
                                                                                      #:supplant supplant)]
                        #:default-language-family [default-language-family #f]
                        #:version [doc-version #f]
                        #:date [doc-date #f])
  (list
   @main-page['start (not user?)
                     #:style style-in
                     #:family-navigation? #t
                     #:show-root-info? (not user?)
                     #:title-content title-content
                     #:self-path self-path
                     #:bug-url bug-url
                     #:doc-properties doc-properties
                     #:default-language-family default-language-family
                     #:version doc-version
                     #:date doc-date]

   (if user?
       @margin-note{This is an installation- and user-specific listing,
             including documentation for installed
             packages.}
      @margin-note{
        @not-on-the-web{This is an installation-specific listing.}
        Running @exec{raco docs}
        (or @exec{Racket Documentation} on Windows or Mac OS)
        may open a different page with local and user-specific
        documentation, including documentation for installed packages.
        @elem[#:style path-info-style]{Searching or following a
         ``top'' link will go to a different starting point that
         includes user-specific information.
         @hyperlink["#"]{@elem[#:style go-style]{[Go to user-specific start]}}
         @hyperlink["#"]{@elem[#:style disable-style]{[Forget user-specific start]}}}})

   @(make-start-page user? #:main-language-family main-language-family)))

(define (make-default-doc-properties #:language-family [main-language-family (get-main-language-family)]
                                     #:default-category [default-category '(language)]
                                     #:supplant [supplant #f])
  (let ([ht (hash 'category (hash main-language-family '(omit)
                                  'default default-category))])
    (if supplant
        (hash-set ht 'supplant supplant)
        ht)))
