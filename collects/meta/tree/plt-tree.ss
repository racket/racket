#lang scheme/base

(require setup/dirs "tree.ss")

(define (get-plt-tree)
  (when absolute-installation?
    (error 'get-plt-tree "must be used from a relative installation"))
  (get-tree (build-path (find-collects-dir) 'up)))
