#lang scheme/base
(require scheme/class
          "../../syntax.rkt"
         "window.rkt")

(provide frame%)

(defclass frame% window%
  (def/public-unimplemented on-toolbar-click)
  (def/public-unimplemented on-menu-click)
  (def/public-unimplemented on-menu-command)
  (def/public-unimplemented on-mdi-activate)
  (def/public-unimplemented enforce-size)
  (def/public-unimplemented on-close)
  (def/public-unimplemented on-activate)
  (def/public-unimplemented designate-root-frame)
  (def/public-unimplemented system-menu)
  (def/public-unimplemented set-modified)
  (def/public-unimplemented is-maximized?)
  (def/public-unimplemented maximize)
  (def/public-unimplemented iconized?)
  (def/public-unimplemented get-menu-bar)
  (def/public-unimplemented set-menu-bar)
  (def/public-unimplemented set-icon)
  (def/public-unimplemented iconize)
  (def/public-unimplemented set-title)
  (super-new))
