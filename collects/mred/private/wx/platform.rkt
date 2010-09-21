#lang scheme/base
(require scheme/runtime-path (for-syntax scheme/base))
(provide (all-defined-out))

(define-runtime-path platform-lib
  (case (system-type)
    [(#;windows) '(lib "mred/private/wx/win32/platform.rkt")]
    [(macosx) '(lib "mred/private/wx/cocoa/platform.rkt")]
    [(windows unix) '(lib "mred/private/wx/gtk/platform.rkt")]))

(define-values (button%
                canvas%
                check-box%
                choice%
                clipboard-driver%
                cursor-driver%
                dialog%
                frame%
                gauge%
                gl-context%
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
                get-resource
                write-resource
                flush-display
                fill-private-color
                cancel-quit
                get-control-font-size
                get-double-click-time
                run-printout
                file-creator-and-type
                send-event
                location->window
                shortcut-visible-in-label?
                unregister-collecting-blit
                register-collecting-blit
                find-graphical-system-path
                play-sound
                get-panel-background
                get-font-from-user
                get-color-from-user
                special-option-key
                special-control-key
                get-highlight-background-color
                get-highlight-text-color
                make-screen-bitmap
                check-for-break)
  ((dynamic-require platform-lib 'platform-values)))
