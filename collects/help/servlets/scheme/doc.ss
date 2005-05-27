(module doc mzscheme
  (require "../private/headelts.ss"
           "../private/util.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (define (make-header-text s)
      (color-highlight `(H2 () ,s)))
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Documentation"))
      (BODY 
       (H1  "Documentation")
       (A ((NAME "docs") (VALUE "Documentation")))
       ,(make-header-text "How to use DrScheme")
       (A ((HREF "/servlets/howtodrscheme.ss")) "DrScheme") 
       " provides information about using the DrScheme development "
       "environment." 
       ,(make-header-text "Languages and Libraries") 
       "Language and library documentation is distributed among "
       "several manuals, plus a number of plain-text files "
       "describing small library collections." 
       (P)
       "When you " (A ((HREF "/servlets/howtouse.ss#search")) "search")
       ", Help Desk groups the results by manual and collection. "
       "The manuals are ordered from the most-used documentation "
       "(e.g., R5RS Scheme) to the least-used (e.g., MzScheme "
       "internals), and all manuals precede library collections." 
       (P)
       "The PLT distribution archive includes a partial set of "
       "documentation.  A hyperlink in this partial set may refer "
       "to a manual that is missing from the distribution. "
       "If you follow such a link, Help Desk provides a special "
       "page for automatically downloading and installing the "
       "missing manual. For certain manuals, the PLT distribution "
       "includes a searchable index file rather than the whole "
       "manual, so a search result link might refer to a "
       "missing manual." 
       (UL  (LI  (B (A ((href "/servlets/manuals.ss")) 
                       "Manuals")) 
                 ": List the currently installed and uninstalled manuals"))
       ,(make-header-text "Searching")
       (A ((HREF "/servlets/howtouse.ss#search")) "Searching") 
       " in Help Desk finds documenation from all sources, "
       "including " 
       (A ((HREF "/servlets/howtodrscheme.ss")) "DrScheme") 
       " and the language and library documentation."))))