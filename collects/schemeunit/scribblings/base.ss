#lang scheme/base

(require
 scribble/eval
 scribble/manual

 (for-label scheme/base
            (file "../test.ss")
            (file "../text-ui.ss")))

(provide
 (all-from-out scribble/eval
               scribble/manual)
 (for-label (all-from-out scheme/base
                          (file "../test.ss")
                          (file "../text-ui.ss"))))