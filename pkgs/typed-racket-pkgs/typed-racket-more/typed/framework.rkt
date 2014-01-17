#lang s-exp typed-racket/base-env/extra-env-lang

;; A typed wrapper for the framework library

(require framework
         (for-syntax (only-in (rep type-rep)
                              make-Instance))
         "racket/private/gui-types.rkt"
         (for-syntax (submod "racket/private/gui-types.rkt" #%type-decl))
         "private/framework-types.rkt"
         (for-syntax (submod "private/framework-types.rkt" #%type-decl)))

(provide (all-from-out "private/framework-types.rkt"))

(begin-for-syntax
 (define -Button% (parse-type #'Button%))
 (define -Event% (parse-type #'Event%)))

(type-environment
 ;; 8 Canvas
 [canvas:basic% (parse-type #'Canvas:Basic%)]
 [canvas:wide-snip-mixin (parse-type #'Canvas:Wide-Snip-Mixin)]
 ;; 11 Editor
 [editor:get-standard-style-list
  (-> (make-Instance (parse-type #'Style-List%)))]
 ;; 14 Frame
 [frame:basic-mixin (parse-type #'Frame:Basic-Mixin)]
 [frame:focus-table-mixin (parse-type #'Frame:Focus-Table-Mixin)]
 [frame:size-pref-mixin (parse-type #'Frame:Size-Pref-Mixin)]
 [frame:register-group-mixin (parse-type #'Frame:Register-Group-Mixin)]
 [frame:status-line-mixin (parse-type #'Frame:Status-Line-Mixin)]
 ;; 16
 [gui-utils:ok/cancel-buttons
  (-> (make-Instance (parse-type #'Horizontal-Panel%))
      (-> (make-Instance -Button%) (make-Instance -Event%) -Void)
      (-> (make-Instance -Button%) (make-Instance -Event%) -Void)
      (-values (list Univ Univ)))]
 ;; 27
 [preferences:get (-> -Symbol -Sexp)]
 [preferences:set (-> -Symbol -Sexp -Void)]
 [preferences:set-default (-> -Symbol -Sexp (-> Univ -Boolean) -Void)]
 ;; 28
 [racket:text% (parse-type #'Text:Basic<%>)])
