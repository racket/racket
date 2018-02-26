#lang racket/base

(provide (struct-out linklet-info))

;; A linklet-info is a phase-specific slice of a module --- mainly a
;; linklet, but we group the linklet together with metadata from the
;; module's declaration linklet
(struct linklet-info (linklet          ; the implementation, or #f if the implementation is empty
                      imports          ; list of links: import "arguments"
                      re-exports       ; list of links: links whose variables are re-exported
                      variables        ; set of symbols: defined in the implementation, for detecting re-exports
                      in-variables     ; list of list of symbols: for each import, variables used from the import
                      side-effects?))  ; whether the implementaiton has side effects other than variable definition
