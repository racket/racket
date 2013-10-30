#lang racket/base
(require racket/runtime-path 
         (for-syntax racket/base))
(provide 
 (protect-out (all-defined-out)))

(define-runtime-module-path-index platform-lib
  (let ([gtk-lib
         '(lib "mred/private/wx/gtk/platform.rkt")])
    (case (system-type)
      [(windows) (if (getenv "PLT_WIN_GTK")
                     gtk-lib
                     '(lib "mred/private/wx/win32/platform.rkt"))]
      [(macosx) '(lib "mred/private/wx/cocoa/platform.rkt")]
      [(unix) gtk-lib])))

(define-values (button%
                canvas%
                canvas-panel%
                check-box%
                choice%
                clipboard-driver%
                cursor-driver%
                dialog%
                frame%
                gauge%
                group-panel%
                item%
                list-box%
                menu%
                menu-bar%
                menu-item%
                message%
                panel%
                printer-dc%
                radio-box%
                slider%
                tab-panel%
                window%
                can-show-print-setup?
                show-print-setup
                id-to-menu-item
                file-selector
                is-color-display?
                get-display-depth
                has-x-selection?
                hide-cursor
                bell
                display-size
                display-origin
		display-count
                display-bitmap-resolution
                flush-display
                get-current-mouse-state
                fill-private-color
                cancel-quit
                get-control-font-face
                get-control-font-size
                get-control-font-size-in-pixels?
                get-double-click-time
                file-creator-and-type
                location->window
                shortcut-visible-in-label?
                unregister-collecting-blit
                register-collecting-blit
                find-graphical-system-path
                play-sound
                get-panel-background
                font-from-user-platform-mode
                get-font-from-user
                color-from-user-platform-mode
                get-color-from-user
                special-option-key
                special-control-key
                get-highlight-background-color
                get-highlight-text-color
                make-screen-bitmap
                make-gl-bitmap
                check-for-break
                key-symbol-to-menu-key
                needs-grow-box-spacer?)
  ((dynamic-require platform-lib 'platform-values)))
