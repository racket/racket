#lang racket/base
(require "private/planet-shared.rkt"
         "config.rkt"
         "cachepath.rkt")

(provide get-installed-planet-archives
         get-hard-linked-packages
         get-all-planet-packages
         get-planet-cache-path)

;; get-installed-planet-archives : -> listof (list path[absolute, dir] string string (listof string) nat nat)
;; directories of all normally-installed planet archives [excluding hard links]
(define (get-installed-planet-archives)
  (with-handlers ((exn:fail:filesystem:no-directory? (lambda (e) '())))
    (tree-apply 
     (lambda (rep-name owner package maj min) 
       (let ((x (list
                 (build-path (CACHE-DIR) owner package (number->string maj) (number->string min))
                 owner
                 package
                 '()
                 maj 
                 min)))
         x))
     (repository-tree)
     3)))

;; get-hard-linked-packages : -> listof (list path[absolute, dir] string string (listof string) nat nat)
;; directories of all hard-linked packages
(define (get-hard-linked-packages)
  (map
   (lambda (row)
     (map (lambda (f) (f row))
          (list assoc-table-row->dir
                (lambda (r) (car (assoc-table-row->path r)))
                assoc-table-row->name
                (lambda (r) (cdr (assoc-table-row->path r)))
                assoc-table-row->maj
                assoc-table-row->min)))
   (get-hard-link-table)))

;; get-all-planet-packages : -> listof (list path[absolute, dir] string string (listof string) nat nat)
;; get every planet package, regardless of origin
(define (get-all-planet-packages)
  (append (get-installed-planet-archives)
          (get-hard-linked-packages)))
