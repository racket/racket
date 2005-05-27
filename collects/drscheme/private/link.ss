(module link mzscheme
  (require "modes.ss"
           "font.ss"
           "eval.ss"
           "module-overview.ss"
           "multi-file-search.ss"
           "debug.ss"
           "module-language.ss"
           "teachpack.ss"
	   "tools.ss"
           (lib "unitsig.ss")
	   "language.ss"
           "language-configuration.ss"
           "drsig.ss"
	   "init.ss"
           "text.ss"
           "app.ss"
           "main.ss"
           "rep.ss"
           "frame.ss"
           "unit.ss"
           "get-extend.ss"
           "help-desk.ss")
  (provide drscheme@)
  
  (define drscheme@
    (compound-unit/sig
      (import)
      (link [init : drscheme:init^ (init@)]
            [tools : drscheme:tools^ 
                   (tools@ frame unit rep get/extend language
                           (language-configuration : drscheme:language-configuration^)
                           help-desk init debug eval teachpack modes)]
            [modes : drscheme:modes^ (modes@)]
            [text : drscheme:text^ (text@)]
            [teachpack : drscheme:teachpack^ (teachpack@)]
            [eval : drscheme:eval^ (eval@ language-configuration rep init language teachpack)]
            [frame : drscheme:frame^ (frame@ unit app help-desk multi-file-search init)]
            [rep : drscheme:rep^
                 (rep@ init language-configuration language app 
                     frame unit text help-desk teachpack debug eval)]
            [language : drscheme:language^ (language@ debug teachpack tools help-desk)]
            [module-overview : drscheme:module-overview^ 
                             (module-overview@ frame eval language-configuration language)]
            [unit : drscheme:unit^ 
                  (unit@ help-desk app frame text rep language-configuration language
                       get/extend teachpack module-overview tools eval init
                       module-language modes)]
            [debug : drscheme:debug^
                   (debug@ rep frame unit language language-configuration init)]
            [multi-file-search : drscheme:multi-file-search^ (multi-file-search@ frame unit)]
            [get/extend : drscheme:get/extend^ (get-extend@ unit frame rep debug)]
            [language-configuration : drscheme:language-configuration/internal^ 
                                    (language-configuration@ unit rep teachpack
                                                           init language app
                                                           tools help-desk)]
            [font : drscheme:font^ (font@ language-configuration)]
            [module-language : drscheme:module-language^ 
                             (module-language@ language-configuration language unit rep)]
            [help-desk : drscheme:help-desk^ (help-desk@ frame language-configuration teachpack)]
	    [app : drscheme:app^ (app@ unit frame language-configuration help-desk tools)]
            [main : () (main@ 
                        app unit get/extend language-configuration language teachpack
                        module-language tools debug frame font
                        modes)])
      (export
       (unit teachpack drscheme:teachpack)
       (unit language-configuration drscheme:language-configuration)))))
