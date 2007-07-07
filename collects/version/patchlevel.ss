;; this file contains the current patch level of DrScheme
;; it is usually `0' in the repository, and changed only when a patch is made.

;; This PLT installation has a patch applied (v370p2), which fixes
;; * teachpacks that contain images
;; * picts in DrScheme's repl
;; * the "autosave disabled" bug
;; * a bug in the recently used language menu
;; Files that were patched:
;;   collects/version/patchlevel.ss
;;   collects/drscheme/private/module-language.ss
;;   collects/framework/private/scheme.ss
;;   collects/slideshow/tool.ss
;;   collects/lang/htdp-langs.ss
;;   collects/tests/drscheme/module-lang-test.ss
;;   collects/drscheme/private/unit.ss

(module patchlevel mzscheme
  (define patchlevel 2)
  (provide patchlevel))
