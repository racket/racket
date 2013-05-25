#lang scheme/base

(provide get-file-tree get-plt-file-tree)

(require "tree.rkt" setup/dirs)

;; ----------------------------------------------------------------------------
;; Reading a tree from a directory

(define (get-file-tree path)
  (define path* (simplify-path path))
  (let loop ([path path*]
             [name (regexp-replace #rx#"/$" (path->bytes path*) #"")])
    (cond [(directory-exists? path)
           (make-tree
            (bytes-append name #"/")
            (parameterize ([current-directory path])
              (let* ([subs (map (lambda (sub)
                                  (cons (path-element->bytes sub) sub))
                                (directory-list))]
                     [subs (sort subs bytes<? #:key car)])
                (map (lambda (sub)
                       (loop (build-path path (cdr sub)) (car sub)))
                     subs)))
            path)]
          [(file-exists? path) (make-tree name #f path)]
          [else (error 'get-file-tree "bad path encountered: ~a/~a"
                       (current-directory) path)])))

;; ----------------------------------------------------------------------------
;; Reading the PLT tree

(define (get-plt-file-tree)
  (when (get-absolute-installation?)
    (error 'get-plt-tree "must be used from a relative installation"))
  (get-file-tree (build-path (find-collects-dir) 'up)))

#| good for benchmarking changes
(printf "getting tree ")
(define t (time (get-plt-file-tree)))
;;!!! (printf "adding deps ")
;;!!! (time (add-deps! t))
(printf "filtering x 1000 ")
(time
 (for ([i (in-range 1000)])
   (tree-filter
    (not: (or: "**/.svn/" "**/compiled/"))
    ;; (get-file-tree "/home/scheme/plt/collects/scribble/.svn")
    t
    )))
|#
