(module graphics mzscheme
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
            (TITLE "How to write graphics programs"))
      (BODY 
       (H1 "How to write graphics programs")
       (A ((NAME "gfx") (VALUE "Graphics")))
       (A ((NAME "gui") (VALUE "GUIs")))
       (A ((NAME "gui2") (VALUE "Graphical User Interfaces")))
       "To write graphics programs, use DrScheme with the "
       "Graphical (MrEd) flavor of the PLT " 
       (A ((HREF "/servlets/scheme/what.ss")) " language") ". "
       "MrEd provides a complete GUI toolbox that is described "
       "in " 
       ,(main-manual-page "mred") ". "
       (P)
       "For simple graphics programs, you may also use the "
       "viewport-based graphics library, which is described in "
       ,(manual-entry "misclib" "viewport" "Viewport Graphics") ". "
       "The following declaration loads viewport graphics into MrEd:"
       (PRE " (require (lib \"graphics.ss\" \"graphics\"))")))))