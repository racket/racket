(module howtouse mzscheme
  (require "private/util.ss"
           "private/headelts.ss"
           (lib "string-constant.ss" "string-constants")
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html
         (head ,hd-css ,@hd-links (title "Help Desk"))
         (body
          (h1 "Help Desk")
          (p)
          (a ([name "helpme"] [value "Help Desk"]))
          "Help Desk (the program you're currently running) is a "
          "complete source of information about PLT software, "
          "including DrScheme, MzScheme, and MrEd."
          (p)
          "Use Help Desk to find information in either of two ways:"
          (p)
          ,(color-highlight
            "1. Navigate the Help Desk information pages by"
            " clicking on hyperlinks.")
          (ul
           (li "The " (b ,(string-constant home)) " button "
               "at the top of the page always takes "
               "you back to the starting page.")
           (li "The " (b "Manuals") " link (in the " (b "Software") " section on the main page) displays a list"
               " of manuals and other documentation.")
           (li "The " (b "Send a bug report")
               " link allows you to submit bug reports to PLT."))
          (p)
          (a ([name "helpsearch"] [value "Searching in Help Desk"]))
          (a ([name "search"]))
          ,(color-highlight
            "2. Search for terms using the "
            `(b "Find docs for") " field at the bottom of Help Desk.")
          (ul
           (li "Enter one or more terms into the " (b "Find docs for") " field.")
           (li "Click the " (b "Search") " button "
               "(or hit Enter) to start a search, "
               "or choose the " (b "Feeling Lucky") " menu item.")
           (li "If you click on the " (b "Search") " button, "
               "Help Desk scans the documentation pages and "
               "returns a list of hyperlinks for "
               (i "keyword") ", "
               (i "index entry") ", and "
               (i "raw text")  " matches:"
               (ul (li (i "Keywords") " are Scheme names, such as " (tt "define")
                       " and " (tt  "cons") ".")
                   (li (i "Index entries")
                       " are topical phrases, such as \"lists\".")
                   (li  (i  "Raw text") " results are fragments of "
                        "text from the documentation pages. "
                        "(Raw text results are useful only as "
                        "a last resort.)")))
           (li "If you perform a lucky search, "
               "Help Desk goes directly to the first item of documentation "
               "that matches the search term, without displaying links to "
               "all relevant items."))
          (p)
          "Help Desk sorts search results according to their source."
          (p)
          "If you open Help Desk inside DrScheme, the search results are "
          "filtered based on the language you are using. Use "
          (b "Choose Language...")
          " menu item from the "
          (b "Language")
          " menu to change the language."))))))