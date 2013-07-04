#lang racket/base
(require setup/infotab)
(provide (all-from-out setup/infotab))
(module reader syntax/module-reader setup/infotab)
