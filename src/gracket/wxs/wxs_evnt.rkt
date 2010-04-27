  (define-class event% object% () ([time-stamp 0])
    get-time-stamp
    set-time-stamp)
  (define-class control-event% event% () (event-type [time-stamp 0])
    get-event-type
    set-event-type)
  (define-class popup-event% control-event% () #f
    get-menu-id
    set-menu-id)
  (define-class scroll-event% event% () ([event-type thumb] [direction vertical] [position 0] [time-stamp 0])
    get-event-type
    set-event-type
    get-direction
    set-direction
    get-position
    set-position)
  (define-class key-event% event% () ([key-code #\nul] [shift-down #f] [control-down #f] [meta-down #f] [alt-down #f] [x 0] [y 0] [time-stamp 0] [caps-down #f])
    set-other-caps-key-code
    get-other-caps-key-code
    set-other-shift-altgr-key-code
    get-other-shift-altgr-key-code
    set-other-altgr-key-code
    get-other-altgr-key-code
    set-other-shift-key-code
    get-other-shift-key-code
    get-key-code
    set-key-code
    get-key-release-code
    set-key-release-code
    get-shift-down
    set-shift-down
    get-control-down
    set-control-down
    get-meta-down
    set-meta-down
    get-alt-down
    set-alt-down
    get-caps-down
    set-caps-down
    get-x
    set-x
    get-y
    set-y)
  (define-function key-symbol-to-integer)
  (define-class mouse-event% event% () (event-type [left-down #f] [middle-down #f] [right-down #f] [x 0] [y 0] [shift-down #f] [control-down #f] [meta-down #f] [alt-down #f] [time-stamp 0] [caps-down #f])
    moving?
    leaving?
    entering?
    dragging?
    button-up?
    button-down?
    button-changed?
    get-event-type
    set-event-type
    get-left-down
    set-left-down
    get-middle-down
    set-middle-down
    get-right-down
    set-right-down
    get-shift-down
    set-shift-down
    get-control-down
    set-control-down
    get-meta-down
    set-meta-down
    get-alt-down
    set-alt-down
    get-caps-down
    set-caps-down
    get-x
    set-x
    get-y
    set-y)
