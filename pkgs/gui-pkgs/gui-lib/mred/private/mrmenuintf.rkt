(module mrmenuintf mzscheme
  (require mzlib/class)

  (provide menu-item<%>
	   labelled-menu-item<%>
	   submenu-item<%>
	   menu-item-container<%>
	   selectable-menu-item<%>)

  (define menu-item<%>
    (interface ()
      get-parent
      delete restore is-deleted?))

  (define labelled-menu-item<%>
    (interface (menu-item<%>)
      get-label set-label get-plain-label
      get-help-string set-help-string
      enable is-enabled?
      on-demand))

  (define submenu-item<%>
    (interface (labelled-menu-item<%>) 
      get-menu))

  (define selectable-menu-item<%>
    (interface (labelled-menu-item<%>)
      command
      get-shortcut set-shortcut
      get-shortcut-prefix set-shortcut-prefix))
  
  (define menu-item-container<%> 
    (interface () 
      get-items on-demand)))
