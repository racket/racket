#lang racket/base

;; Shows a gui of provided identifiers with some extra information such as
;;  contracts (works)
;;  typed racket types (doesn't work)

(require (prefix-in check: "check.rkt")
         framework/framework
         racket/gui/base
         racket/class)

(provide build-gui)

(define (build-gui gui-parent file)
  (define exports (check:get-exports file #true))
  (for ([provide (map check:provided-syntaxes exports)])
    (printf "syntaxes exports (~a): ~a\n" (length provide) (map check:print provide)))

  (for ([provide (map check:provided-variables exports)])
    (printf "variables (~a): ~a\n" (length provide) (map check:print provide)))

  #;
  (printf "exports: ~a\n" (map check:print
                               (map check:provided-syntaxes
                                    (check:get-exports "x.rkt" #true))))

  (define stuff (new vertical-pane% [parent gui-parent]))
  (new message% [parent stuff] [label "Contracts"])
  (define contract-pane (new horizontal-panel% [parent stuff]))
  (define contract-text (new racket:text%))
  (define contract-editor (new editor-canvas% [parent contract-pane] [editor contract-text]))
  (new message% [parent stuff] [label "No contracts"])
  (define non-contract-pane (new horizontal-panel% [parent stuff]))
  (define non-contract-text (new racket:text%))
  (define non-contract-editor (new editor-canvas% [parent non-contract-pane] [editor non-contract-text]))
  (for ([provide/phase (map check:provided-syntaxes exports)])
    (for ([symbol provide/phase])
      (send contract-text insert (check:print symbol))
      (send contract-text insert "\n")
      ))
  (for ([provide/phase (map check:provided-variables exports)])
    (for ([symbol provide/phase])
      (send non-contract-text insert (check:print symbol))
      (send non-contract-text insert "\n")
      ))
  )

#|
(let ([frame (new frame:basic% [label ""] [width 500] [height 500])])
  (build-gui (send frame get-area-container))
  (send frame show #true))
|#
