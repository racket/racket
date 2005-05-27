(module profj-pref mzscheme
  
  (require (lib "file.ss")
           (lib "list.ss"))
  
  (provide reset-classpath add-to-classpath get-classpath)

  ;get-classpath: -> (list string)
  (define (get-classpath)
    (append (cons (build-path 'same)
                  (get-preference 'projf:classpath (lambda () null)))
            (map (lambda (p) (build-path p "profj" "libs"))
                 (current-library-collection-paths))
            (map (lambda (p) (build-path p "htdch"))
                 (current-library-collection-paths))))
  
  ;reset-classpath: -> void
  (define (reset-classpath)
    (put-preferences `(profj:classpath) (list null)))
  
  ;add-to-classpath: string -> void
  (define (add-to-classpath path)
    (let ((old-classpath (get-preference 'profj:classpath (lambda () null))))
      (put-preferences `(profj:classpath) (list (cons path old-classpath)))))
  
  ;remove-from-classpath: string -> void
  (define (remove-from-classpath path)
    (let ((old-classpath (get-preference 'profj:classpath (lambda () null))))
      (put-preferences `(profj:classpath) (list (remove path old-classpath)))))
  
)