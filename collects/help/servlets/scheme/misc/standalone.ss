(module standalone mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "../../private/headelts.ss"
           "../../../private/manuals.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "How to build a stand-alone executable"))
      (BODY 
       (H1 "How to build a stand-alone executable")
       (A ((NAME "exec") (VALUE "Standalone executables")))
       (A ((name "exec2") (VALUE "Stand-alone executables")))
       "To create stand-alone executables, use DrScheme's "
       (tt "Scheme | Create Executable ...")
       " menu item. This menu is sensitive to the language levels; "
       "the " (tt "module") " language permits the most flexibility "
       "in creating executables."
       
       (p)
       "The mzc compiler provides a more low-level interface "
       "to stand-alone executables creation. "
       "See " 
       ,(main-manual-page "mzc")
       " for more information."))))