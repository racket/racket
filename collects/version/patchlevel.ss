;; this file contains the current patch level of DrScheme
;; it is usually `0' in the repository, and changed only when a patch is made.

;; This PLT installation has a patch applied (v370), which fixes the use of
;; teachpacks that contain images.  Files that were patched:
;;   collects/version/patchlevel.ss
;;   collects/drscheme/private/module-language.ss
;;   collects/framework/private/scheme.ss
;;   collects/slideshow/tool.ss
;;   collects/lang/htdp-langs.ss
;;   collects/tests/drscheme/module-lang-test.ss

(module patchlevel mzscheme
  (define patchlevel 1)
  (provide patchlevel))
