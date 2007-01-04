;; This code is duplicated from the servelt-builder.ss file
(module find-scheme-menu mzscheme
  
  (provide find-scheme-menu)
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "string-constant.ss" "string-constants"))
  
  ; : menu% -> (U menu% #f)
  ; to crawl up and down the menu hierarcy to find the scheme menu
  ; This attempts to work even if
  ; a) the menus and menu items are in a different langauge
  ; b) the menus are in Philippe's language where they are all blank (and hence the same)
  ; It starts by selecting the menu by position to avoid problem b).
  ; Just to be paranoid, it looks in other positions, too.
  ; The scheme menu must have "Create Executable..." in some language as a menu item.
  (define (find-scheme-menu special-menu)
    (let* ([bar (send special-menu get-parent)]
           [menus (send bar get-items)]
           [ordered-menus (if (< (length menus) 5)
                              menus
                              (cons (car (cddddr menus)) menus))])
      (ormap (lambda (m)
               (and (string=? (label->plain-label (string-constant scheme-menu-name))
                              (send m get-plain-label))
                    (ormap is-create-executable-item? (send m get-items))
                    m))
             ordered-menus)))
  
  
  ; menu-item% -> bool
  (define (is-create-executable-item? item)
    (and (is-a? item labelled-menu-item<%>)
         (string=? (string-constant create-executable-menu-item-label)
                   (send item get-label))))
  )