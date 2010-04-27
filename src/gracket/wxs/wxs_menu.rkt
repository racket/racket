  (define-class menu% object% () #f
    select
    get-font
    set-width
    set-title
    set-label
    set-help-string
    number
    enable
    check
    checked?
    append-separator
    delete-by-position
    delete
    append)
  (define-class menu-bar% object% () #f
    set-label-top
    number
    enable-top
    delete
    append)
  (define-class menu-item% object% () #f
    id)
  (define-function id-to-menu-item)
