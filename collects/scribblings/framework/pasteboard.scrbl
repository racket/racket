#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Pasteboard}

@defclass[pasteboard:basic% (editor:basic-mixin pasteboard%) ()]{}
@defclass[pasteboard:standard-style-list% (editor:standard-style-list-mixin pasteboard:basic%) ()]{}
@defclass[pasteboard:keymap% (editor:keymap-mixin pasteboard:standard-style-list%) ()]{}
@defclass[pasteboard:file% (editor:file-mixin pasteboard:keymap%) ()]{}
@defclass[pasteboard:backup-autosave% (editor:backup-autosave-mixin pasteboard:file%) ()]{}
@defclass[pasteboard:info% (editor:info-mixin pasteboard:backup-autosave%) ()]{}

@(include-previously-extracted "main-extracts.rkt" #rx"^pasteboard:")
