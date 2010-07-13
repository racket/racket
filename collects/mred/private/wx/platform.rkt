#lang scheme/base
(require scheme/runtime-path (for-syntax scheme/base))
(provide (all-defined-out))

(define-runtime-path platform-lib
  (case (system-type)
    [() '(lib "mred/private/wx/win32/platform.rkt")]
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
                group-box%
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
                tab-group%
                tab-panel%
                window%
                can-show-print-setup?
                show-print-setup
                id-to-menu-item
                file-selector
                is-color-display?
                get-display-depth
                begin-busy-cursor
                is-busy?
                end-busy-cursor
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
                key-symbol-to-integer
                draw-tab-base
                draw-tab
                set-combo-box-font
                get-double-click-time
                run-printout
                end-refresh-sequence
                begin-refresh-sequence
                file-creator-and-type
                send-event
                set-executer
                set-dialogs
                location->window
                set-menu-tester
                in-atomic-region
                shortcut-visible-in-label?
                unregister-collecting-blit
                register-collecting-blit
                find-graphical-system-path
                check-for-break
                play-sound
                get-panel-background
                get-font-from-user
                get-color-from-user
                application-pref-handler
                application-about-handler
                application-quit-handler
                application-file-handler
                special-option-key
                special-control-key)
  ((dynamic-require platform-lib 'platform-values)))
