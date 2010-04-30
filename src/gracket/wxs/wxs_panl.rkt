  (define-class panel% window% () #f
    get-label-position
    set-label-position
    on-char
    on-event
    on-paint
    on-drop-file
    pre-on-event
    pre-on-char
    on-size
    on-set-focus
    on-kill-focus
    set-item-cursor
    get-item-cursor)
  (define-class dialog% window% () #f
    system-menu
    set-title
    on-drop-file
    pre-on-event
    pre-on-char
    on-size
    on-set-focus
    on-kill-focus
    enforce-size
    on-close
    on-activate)
