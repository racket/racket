  (define-class item% window% () #f
    set-label
    get-label
    command)
  (define-class message% item% () #f
    get-font
    set-label
    on-drop-file
    pre-on-event
    pre-on-char
    on-size
    on-set-focus
    on-kill-focus)
