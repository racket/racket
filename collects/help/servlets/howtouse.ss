(module howtouse mzscheme
  (require "private/util.ss"
           "private/headelts.ss"
           (lib "string-constant.ss" "string-constants"))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (TITLE "Help Desk")
      (HEAD ,hd-css
            ,@hd-links)
      (BODY 
       (H1 "Help Desk") 
       (P)
       (A ((NAME "helpme") (VALUE "Help Desk")))
       "Help Desk (the program you're currently running) is a "
       "complete source of information about PLT software, "
       "including DrScheme, MzScheme, and MrEd."
       (P)
       "Use Help Desk to find information in either of two ways:"
       (P)
       ,(color-highlight
         "1) Navigate the Help Desk information pages by "
         "clicking on hyperlinks.")
       (UL 
        (LI  "The " (B  (TT  ,(string-constant home))) " button "
             "at the top of the page always takes "
             "you back to the starting page.")
        (LI  "The " (B  (TT  "Manuals")) " link "
             " displays a list "
             " of manuals and other documentation.")
        (LI  "The " (B  (TT  "Send a bug report")) 
             " link "
             "allows you to submit bug reports to PLT."))
       (P)
       (A ((NAME "helpsearch") (VALUE "Searching in Help Desk")))
       (A ((NAME "search")))
       ,(color-highlight
         "2) Search for terms using the " 
         `(B  (TT  "Find docs for"))  
         " field at the bottom of Help Desk.")
       (UL  
        (LI  "Enter one or more terms into the " 
             (B  (TT  "Find docs for")) " field.")
        (LI  "Click the " (B  "Search") " button "
             "(or hit Enter) to start a search, "
             "or choose the " (B "Feeling Lucky") " menu item.")
        (LI  "If you click on the " (B "Search") " button, "
             "Help Desk scans the documentation pages and "
             "returns a list of hyperlinks for "
             (I  "keyword") ", "
             (I  "index entry") ", and "
             (I  "raw text")  " matches:"
             (UL 
              (LI  (I  "Keywords") " are Scheme names, "
                   "such as " (TT  "define") " and "
                   (TT  "cons") ".") 
              (LI  (I  "Index entries")
                   " are topical phrases, such as \"lists\".")
              (LI  (I  "Raw text") " results are fragments of "
                   "text from the documentation pages. "
                   "(Raw text results are useful only as "
                   "a last resort.)")))
        (LI "If you perform a lucky search, "
            "Help Desk goes directly to the first item of documentation "
            "that matches the search term, without displaying links to "
            "all relevant items."))
       (P)
       "Help Desk sorts search results according to their source."
       (p)
       "If you open Help Desk inside DrScheme, the search results are "
       "filtered based on the language you are using. Use " 
            (tt "Choose Language...")
            " menu item from the "
            (tt "Language")
            " menu to change the language."))))