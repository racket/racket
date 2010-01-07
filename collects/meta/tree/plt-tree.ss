#lang scheme/base

(require "tree.ss" setup/dirs)

(define (get-plt-tree)
  (when absolute-installation?
    (error 'get-plt-tree "must be used from a relative installation"))
  (get-tree (build-path (find-collects-dir) 'up)))

#| good for benchmarking changes
(printf "getting tree ")
(define t (time (get-plt-tree)))
;;!!! (printf "adding deps ")
;;!!! (time (add-deps! t))
(printf "filtering x 1000 ")
(time
 (for ([i (in-range 1000)]) ; print-tree
   (tree-filter
    (not: (or: "**/.svn/" "**/compiled/"))
    ;; (get-tree "/home/scheme/plt/collects/scribble/.svn")
    t
    )))
|#
