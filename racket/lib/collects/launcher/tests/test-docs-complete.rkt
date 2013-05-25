#lang racket/base
(require rackunit/docs-complete)
(check-docs (quote launcher))
(check-docs (quote launcher/launcher))
(check-docs (quote launcher/launcher-unit))
(check-docs (quote launcher/launcher-sig))
