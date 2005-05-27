
(module framework-unit mzscheme
  (require (lib "unitsig.ss")
	   (lib "mred-sig.ss" "mred"))

  (require "framework-sig.ss"
	   "private/sig.ss"
           "private/number-snip.ss"
           "private/comment-box.ss"
           "private/application.ss"
	   "private/version.ss"
	   "private/color-model.ss"
	   "private/exn.ss"
	   "private/exit.ss"
	   "private/menu.ss"
	   "private/preferences.ss"
	   "private/autosave.ss"
           "private/color.ss"
           "private/color-prefs.ss"
	   "private/handler.ss" 
	   "private/keymap.ss"
	   "private/path-utils.ss"
	   "private/icon.ss"
	   "private/editor.ss"
	   "private/pasteboard.ss"
	   "private/text.ss"
	   "private/finder.ss"
	   "private/group.ss"
	   "private/canvas.ss"
	   "private/panel.ss"
	   "private/frame.ss"
	   "private/scheme.ss"
	   "private/main.ss"
           "private/mode.ss")

  (provide framework@)

  (define framework@
    (compound-unit/sig
      (import [mred : mred^])
      (link [application : framework:application^ (application@)]
	    [version : framework:version^ (version@)]
	    [color-model : framework:color-model^ (color-model@ )]
	    [exn : framework:exn^ (exn@)]
	    [mode : framework:mode^ (mode@)]
            [exit : framework:exit^ (exit@ mred preferences)]
	    [menu : framework:menu^ (menu@ mred preferences)]
	    [preferences : framework:preferences^ (preferences@ mred exn exit panel frame)]
            [number-snip : framework:number-snip^ (number-snip@ mred preferences)]
	    [autosave : framework:autosave^ (autosave@ mred exit preferences frame
                                                     scheme editor text finder group)]
	    [path-utils : framework:path-utils^ (path-utils@)]
	    [icon : framework:icon^ (icon@ mred)]

	    [keymap : framework:keymap^
		    (keymap@ mred preferences finder handler frame editor)]
	    [editor : framework:editor^
		    (editor@ mred autosave finder path-utils keymap icon
			     preferences text pasteboard frame handler)]
	    [pasteboard : framework:pasteboard^ (pasteboard@ mred editor)]
	    [text : framework:text^
		  (text@ mred icon editor preferences keymap 
                         color-model frame scheme number-snip)]
            [color : framework:color^ (color@ preferences icon mode text color-prefs scheme)]
            [color-prefs : framework:color-prefs^ (color-prefs@ preferences editor panel canvas)]
            [comment-box : framework:comment-box^
                         (comment-box@ text scheme keymap)]
	    [finder : framework:finder^ (finder@ mred preferences keymap)]
	    [group : framework:group^ 
                   (group@ mred application frame preferences text canvas menu)]
	    [canvas : framework:canvas^ (canvas@ mred preferences frame text)]
	    [panel : framework:panel^ (panel@ icon mred)]
	    [frame : framework:frame^ 
		   (frame@ mred group preferences icon handler application panel
                           finder keymap text pasteboard editor canvas menu scheme exit
                           comment-box)]
	    [handler : framework:handler^
		     (handler@ mred finder group text preferences frame)]

	    [scheme : framework:scheme^ 
		    (scheme@ mred preferences 
                           icon keymap text editor frame comment-box mode color color-prefs)]
	    [main : framework:main^ (main@ mred preferences exit group handler editor color-prefs scheme)])
      (export (unit number-snip)
              (unit menu)
              (unit application)
              (unit version)
              (unit color-model)
              (unit exn)
              (unit exit)
              (unit preferences)
              (unit autosave)
              (unit handler) 
              (unit keymap)
              (unit path-utils)
              (unit icon)
              (unit editor)
              (unit pasteboard)
              (unit text)
              (unit color)
              (unit color-prefs)
              (unit comment-box)
              (unit finder)
              (unit group)
              (unit canvas)
              (unit panel)
              (unit frame)
              (unit scheme)
              (unit mode)
              (unit main)))))
