(module framework-sig mzscheme
  (require (lib "unitsig.ss")
	   "private/sig.ss")

  (provide framework^ framework-class^)

  (define-signature framework-class^
    ([unit application : framework:application-class^]
     [unit version : framework:version-class^]
     [unit color-model : framework:color-model-class^]
     [unit exn : framework:exn-class^]
     [unit exit : framework:exit-class^]
     [unit preferences : framework:preferences-class^]
     [unit number-snip : framework:number-snip-class^]
     [unit autosave : framework:autosave-class^]
     [unit handler : framework:handler-class^] 
     [unit keymap : framework:keymap-class^]
     [unit path-utils : framework:path-utils-class^]
     [unit icon : framework:icon-class^]
     [unit editor : framework:editor-class^]
     [unit pasteboard : framework:pasteboard-class^]
     [unit text : framework:text-class^]
     [unit finder : framework:finder-class^]
     [unit group : framework:group-class^]
     [unit canvas : framework:canvas-class^]
     [unit panel : framework:panel-class^]
     [unit menu : framework:menu-class^]
     [unit frame : framework:frame-class^]
     [unit color : framework:color-class^]
     [unit color-prefs : framework:color-prefs-class^]
     [unit scheme : framework:scheme-class^]
     [unit comment-box : framework:comment-box-class^]
     (unit mode : framework:mode-class^)
     [unit main : framework:main-class^]))

  (define-signature framework^
    ([unit application : framework:application^]
     [unit version : framework:version^]
     [unit color-model : framework:color-model^]
     [unit exn : framework:exn^]
     [unit exit : framework:exit^]
     [unit preferences : framework:preferences^]
     [unit number-snip : framework:number-snip^]
     [unit autosave : framework:autosave^]
     [unit handler : framework:handler^] 
     [unit keymap : framework:keymap^]
     [unit path-utils : framework:path-utils^]
     [unit icon : framework:icon^]
     [unit editor : framework:editor^]
     [unit pasteboard : framework:pasteboard^]
     [unit text : framework:text^]
     [unit finder : framework:finder^]
     [unit group : framework:group^]
     [unit canvas : framework:canvas^]
     [unit panel : framework:panel^]
     [unit menu : framework:menu^]
     [unit frame : framework:frame^]
     [unit color : framework:color^]
     [unit color-prefs : framework:color-prefs^]
     [unit scheme : framework:scheme^]
     [unit comment-box : framework:comment-box^]
     (unit mode : framework:mode^)
     [unit main : framework:main^])))
