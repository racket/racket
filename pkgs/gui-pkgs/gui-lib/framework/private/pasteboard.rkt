#lang scheme/unit
  (require "sig.rkt" mred/mred-sig)

  (import mred^
          [prefix editor: framework:editor^])
  (export (rename framework:pasteboard^
                  [-keymap% keymap%]))
  (init-depend mred^ framework:editor^)


  (define basic% (editor:basic-mixin pasteboard%))
  (define standard-style-list% (editor:standard-style-list-mixin basic%))
  (define -keymap% (editor:keymap-mixin standard-style-list%))
  (define file% (editor:file-mixin -keymap%))
  (define backup-autosave% (editor:backup-autosave-mixin file%))
  (define info% (editor:info-mixin backup-autosave%))
