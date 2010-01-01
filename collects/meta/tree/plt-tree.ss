#lang scheme/base

(require setup/dirs "tree.ss")

(define (get-plt-tree)
  (when absolute-installation?
    (error 'get-plt-tree "must be used from a relative installation"))
  (get-tree (build-path (find-collects-dir) 'up)))

#| good for benchmarking changes
(printf "getting tree\n")
(define t (get-plt-tree))
(printf "filtering\n")
(time
 (for ([i (in-range 1000)]) ; print-tree
   (tree-filter
    (not: (or: "**/.svn/" "**/compiled/"))
    ;; (get-tree "/home/scheme/plt/collects/scribble/.svn")
    t
    )))
|#
