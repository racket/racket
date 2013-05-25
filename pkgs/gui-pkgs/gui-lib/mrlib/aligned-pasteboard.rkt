(module aligned-pasteboard mzscheme
  (require
    "private/aligned-pasteboard/aligned-pasteboard.rkt"
    "private/aligned-pasteboard/aligned-editor-container.rkt"
    "private/aligned-pasteboard/interface.rkt"
    "private/aligned-pasteboard/stretchable-editor-snip.rkt")
  (provide
    vertical-pasteboard%
    horizontal-pasteboard%
    aligned-editor-snip%
    aligned-editor-canvas%
    aligned-pasteboard<%>
    aligned-pasteboard-parent<%>
    stretchable-snip<%>
    stretchable-editor-snip%
    stretchable-editor-snip-mixin))
