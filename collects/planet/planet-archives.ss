(module planet-archives mzscheme
  (require "private/planet-shared.ss"
	   (lib "file.ss")
           "config.ss"
           "cachepath.ss")
  
  (provide repository-tree get-installed-planet-archives get-planet-cache-path)
  
  (define (repository-tree)
    (define (id x) x)
    (filter-tree-by-pattern
     (directory->tree (CACHE-DIR)
                      (lambda (x)
                        (not (regexp-match #rx"/(CVS|[.]svn)$"
                                           (path->string x))))
                      4)
     (list id id id string->number string->number)))
  
  ;; get-installed-planet-dirs : -> listof path[absolute, dir]
  ;; directories of all installed planet archives
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
       3))))
