#lang scheme/base

(require scheme/modspec-forms)
(require (matching-identifiers-in #px"\\bu8vector\\b" scheme/foreign))
(provide (all-from-out scheme/foreign))
