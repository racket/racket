(module planet-archives mzscheme
  (require "private/planet-shared.ss"
	   (lib "file.ss")
           "config.ss"
           "cachepath.ss")
  
  (provide repository-tree 
           get-installed-planet-archives
           get-hard-linked-packages
           get-all-planet-packages
           get-planet-cache-path)
  
  (define (repository-tree)
    (define (id x) x)
    (filter-tree-by-pattern
     (directory->tree (CACHE-DIR)
                      (lambda (x)
                        (not (regexp-match #rx"/(CVS|[.]svn)$"
                                           (path->string x))))
                      4)
     (list id id id string->number string->number)))
  
  ;; get-installed-planet-dirs : -> listof (list path[absolute, dir] string string (listof string) nat nat)
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
  
  )
