(module graphics mzscheme
  (require (lib "servlet.ss" "web-server"))
  (require "../../private/headelts.ss"
           "../../../private/manuals.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html 
         (head ,hd-css
               ,@hd-links
               (title "How to write graphics programs"))
         (body
          (h1 "How to write graphics programs")
          (a ([name "gfx"] [value "Graphics"]))
          (a ([name "gui"] [value "GUIs"]))
          (a ([name "gui2"] [value "Graphical User Interfaces"]))
          "To write graphics programs, use DrScheme with the "
          "Graphical (MrEd) flavor of the PLT " 
          (a ([href "/servlets/scheme/what.ss"]) " language") ". "
          "MrEd provides a complete GUI toolbox that is described "
          "in "
          ,(main-manual-page "mred") ". "
          (p)
          "For simple graphics programs, you may also use the "
          "viewport-based graphics library, which is described in "
          ,(manual-entry "misclib" "viewport" "Viewport Graphics") ". "
          "The following declaration loads viewport graphics into MrEd:"
          (pre " (require (lib \"graphics.ss\" \"graphics\"))")))))))
