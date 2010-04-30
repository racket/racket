  (define-class timer% object% () ()
    stop
    start
    notify
    interval)
  (define-private-class clipboard% clipboard<%> object% () #f
    same-clipboard-client?
    get-clipboard-bitmap
    set-clipboard-bitmap
    get-clipboard-data
    get-clipboard-string
    set-clipboard-string
    set-clipboard-client)
  (define-function get-the-x-selection)
  (define-function get-the-clipboard)
  (define-class clipboard-client% object% () ()
    same-eventspace?
    get-types
    add-type
    get-data
    on-replaced)
  (define-class ps-setup% object% () ()
    copy-from
    set-margin
    set-editor-margin
    set-level-2
    set-paper-name
    set-translation
    set-scaling
    set-orientation
    set-mode
    set-preview-command
    set-file
    set-command
    get-margin
    get-editor-margin
    get-level-2
    get-paper-name
    get-translation
    get-scaling
    get-orientation
    get-mode
    get-preview-command
    get-file
    get-command)
  (define-function show-print-setup)
  (define-function can-show-print-setup?)
