#lang scheme/base

(require scheme/require)
(require (matching-identifiers-in #px"\\bu8vector\\b" scheme/foreign))
(provide (all-from-out scheme/foreign))
