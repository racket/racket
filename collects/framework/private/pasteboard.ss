(module pasteboard mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   (lib "mred-sig.ss" "mred"))

  (provide pasteboard@)

  (define pasteboard@
    (unit/sig framework:pasteboard^
      (import mred^
	      [editor : framework:editor^])

      (rename [-keymap% keymap%])

      (define basic% (editor:basic-mixin pasteboard%))
      (define standard-style-list% (editor:standard-style-list-mixin basic%))
      (define -keymap% (editor:keymap-mixin standard-style-list%))
      (define file% (editor:file-mixin -keymap%))
      (define backup-autosave% (editor:backup-autosave-mixin file%))
      (define info% (editor:info-mixin backup-autosave%)))))
