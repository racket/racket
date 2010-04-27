  (define-class font% object% () #f
    screen-glyph-exists?
    get-font-id
    get-size-in-pixels
    get-underlined
    get-smoothing
    get-weight
    get-point-size
    get-style
    get-face
    get-family)
  (define-class font-list% object% () #f
    find-or-create-font)
  (define-class color% object% () #f
    blue
    green
    red
    set
    ok?
    copy-from)
  (define-private-class color-database% color-database<%> object% () #f
    find-color)
  (define-class point% object% () #f
    get-x
    set-x
    get-y
    set-y)
  (define-class brush% object% () #f
    set-style
    get-style
    set-stipple
    get-stipple
    set-color
    get-color)
  (define-class brush-list% object% () #f
    find-or-create-brush)
  (define-class pen% object% () #f
    set-style
    get-style
    set-stipple
    get-stipple
    set-color
    get-color
    set-join
    get-join
    set-cap
    get-cap
    set-width
    get-width)
  (define-class pen-list% object% () #f
    find-or-create-pen)
  (define-class cursor% object% () #f
    ok?)
  (define-class region% object% () (dc)
    in-region?
    is-empty?
    get-bounding-box
    xor
    subtract
    intersect
    union
    set-path
    set-arc
    set-polygon
    set-ellipse
    set-rounded-rectangle
    set-rectangle
    get-dc)
  (define-class dc-path% object% () #f
    get-bounding-box
    append
    reverse
    rotate
    scale
    translate
    lines
    ellipse
    rounded-rectangle
    rectangle
    curve-to
    arc
    line-to
    move-to
    open?
    close
    reset)
  (define-private-class font-name-directory% font-name-directory<%> object% () #f
    find-family-default-font-id
    find-or-create-font-id
    get-family
    get-face-name
    get-font-id
    set-post-script-name
    set-screen-name
    get-post-script-name
    get-screen-name)
  (define-function get-control-font-size)
  (define-function get-the-font-name-directory)
  (define-function get-the-font-list)
  (define-function get-the-pen-list)
  (define-function get-the-brush-list)
  (define-function get-the-color-database)
