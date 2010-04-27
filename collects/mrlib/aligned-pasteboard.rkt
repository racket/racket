(module aligned-pasteboard mzscheme
  (require
    "private/aligned-pasteboard/aligned-pasteboard.ss"
    "private/aligned-pasteboard/aligned-editor-container.ss"
    "private/aligned-pasteboard/interface.ss"
    "private/aligned-pasteboard/stretchable-editor-snip.ss")
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
