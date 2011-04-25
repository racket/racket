#lang racket/base
(require tests/utils/docs-complete)
(check-docs (quote launcher))
(check-docs (quote launcher/launcher))
(check-docs (quote launcher/launcher-unit))
(check-docs (quote launcher/launcher-sig))
