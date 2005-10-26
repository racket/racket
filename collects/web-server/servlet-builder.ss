(module servlet-builder mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (lib "pretty.ss")
           (lib "sig.ss" "web-server")
           (lib "min-servlet.ss" "web-server")
           (lib "string-constant.ss" "string-constants")
           (all-except (lib "util.ss" "web-server") translate-escapes))
  
  (provide tool@)
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define phase1 void)
      (define phase2 void)
      
      ; language-prefs text% -> (listof s-expr)
      ; to read the program from the definitions window
      (define (read-program lang-prefs definitions-text)
        (let* ([language (drscheme:language-configuration:language-settings-language lang-prefs)]
               [settings (drscheme:language-configuration:language-settings-settings lang-prefs)]
               [front-end (send language front-end/complete-program
                                (drscheme:language:make-text/pos
                                 definitions-text
                                 0 ; (drscheme:language:get-post-hash-bang-start definitions-text)
                                 (send definitions-text last-position))
                                settings)])
          (let loop ()
            (let ([x (front-end)])
              (cond
                [(syntax? x) (cons (syntax-object->datum x) (loop))]
                [(eof-object? x) null]
                [else (cons x (loop))])))))
      
      ; : s-expr (listof str) (listof s-expr) -> (listof s-expr)
      (define (wrap language teachpacks program)
        (let-values ([(require-exprs other-exprs) (extract-requires program)])
          (let ([all-requires
                 (list* `(require
                          (lib "unitsig.ss")
                          (lib "sig.ss" "web-server")
                          (lib "min-servlet.ss" "web-server")
                          (lib "servlet-helpers.ss" "web-server"))
                        `(require ,language)
                        `(require
                          . ,(map (lambda (tp) `(file ,tp))
                                  (filter (lambda (tp)
                                            (not (servlet-teachpack? tp)))
                                          teachpacks)))
                        require-exprs)])
            (if (includes-servlet2? teachpacks)
                `(,@all-requires
                  (require (lib "servlet2-unit.ss" "htdp"))
                  (compound-unit/sig
                    (import (S1 : servlet^))
                    (link
                     [S2 : servlet2^ (servlet2@ S1)]
                     [U : () ((unit/sig ()
                                (import servlet^ servlet2^)
                                . ,other-exprs)
                              S1 S2)])
                    (export)))
                
                `(,@all-requires
                  (unit/sig ()
                    (import servlet^)
                    . ,other-exprs))))))
      
      ; : (listof s-expr) -> (listof s-expr)^2
      ; to separate a program into top level requires expressions and other expressions
      (define (extract-requires x)
        (let loop ([x x] [k (lambda (r o) (values r o))])
          (cond
            [(null? x) (k null null)]
            [else
             (let ([expr (car x)])
               (loop (cdr x)
                     (if (and (pair? expr) (eq? 'require (car expr)))
                         (lambda (r o) (k (cons expr r) o))
                         (lambda (r o) (k r (cons expr o))))))])))
      
      ; : str -> bool
      ; to check in a full path to a teachpack refers to a servlet teachpack
      ; FIX this - mimic how drscheme finds teachpacks
      (define (servlet-teachpack? tp)
        (let-values ([(base name must-be-dir?) (split-path tp)])
          (and (string? name) (or (string=? name "servlet.ss") (string=? name "servlet2.ss")))))
      
      ; : (listof str) -> bool
      (define (includes-servlet2? tps)
        (ormap (lambda (tp)
                 (let-values ([(base name must-be-dir?) (split-path tp)])
                   (and (string? name) (string=? name "servlet2.ss"))))
               tps))
      
      ; exn:unknown-language = (make-exn:unknown-language str mark-set str)
      (define-struct (exn:unknown-language exn) (lang))
      
      ; language-prefs -> s-expr
      ; to find the module to require for the language
      (define (find-language-require lang-prefs)
        (let ([lang-name
               (send (drscheme:language-configuration:language-settings-language lang-prefs)
                     get-language-name)])
          (hash-table-get
           language-table (string->symbol lang-name)
           (lambda ()
             (raise (make-exn:unknown-language
                     (format "Unsupported servlet language: ~e" lang-name)
                     (current-continuation-marks)
                     lang-name))))))
      
      (define language-table
        (let ([table (make-hash-table)])
          (for-each (lambda (name-req)
                      (hash-table-put! table (string->symbol (car name-req)) (cadr name-req)))
                    (list 
                     (list (string-constant r5rs-lang-name)
                           `(lib "lang.ss" "r5rs"))
                     (list (string-constant beginning-student)
                           `(lib "htdp-beginner.ss" "lang"))
                     (list (string-constant beginning-student/abbrev)
                           `(lib "htdp-beginner-abbr.ss" "lang"))
                     (list (string-constant intermediate-student)
                           `(lib "htdp-intermediate.ss" "lang"))
                     (list (string-constant intermediate-student/lambda)
                           `(lib "htdp-intermediate-lambda.ss" "lang"))
                     (list (string-constant advanced-student)
                           `(lib "htdp-advanced.ss" "lang"))
                     (list "Essentials of Programming Languages"
                           `(lib "eopl.ss" "eopl"))
                     (list (string-constant mzscheme-w/debug)
                           `(lib "plt-mzscheme.ss" "lang"))
                     (list (string-constant mred-w/debug)
                           `(lib "plt-mred.ss" "lang"))
                     (list (string-constant pretty-big-scheme)
                           `(lib "plt-pretty-big.ss" "lang")))
                    ; teachpacks don't work with the module language
                    ; algol60?
                    )
          table))
      
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
                   (and (string=? (string-constant scheme-menu-name)
                                  (send m get-label))
                        (ormap is-create-executable-item? (send m get-items))
                        m))
                 ordered-menus)))
      
      ; : menu% menu-item% -> void
      ; to move all the menu items between "Create Executable..." and "Create Servlet..." to the end.
      (define (arrange-scheme-menu scheme-menu servlet-item)
        (let ([between-items
               (let delete ([kids (let skip-first-few ([kids (send scheme-menu get-items)])
                                    (cond
                                      [(is-create-executable-item? (car kids)) (cdr kids)]
                                      [else (skip-first-few (cdr kids))]))])
                 (cond
                   [(eq? servlet-item (car kids)) null]
                   [else (cons (car kids) (delete (cdr kids)))]))])
          (for-each (lambda (item) (send item delete) (send item restore)) between-items)))
      
      ; menu-item% -> bool
      (define (is-create-executable-item? item)
        (and (is-a? item labelled-menu-item<%>)
             (string=? (string-constant create-executable-menu-item-label)
                       (send item get-label))))
      
      (drscheme:get/extend:extend-unit-frame
       (lambda (super%)
         (class super%
           (super-instantiate ())
           (inherit get-definitions-text get-special-menu)
           
           (let ([scheme-menu (find-scheme-menu (get-special-menu))])
             (when scheme-menu
               (arrange-scheme-menu
                scheme-menu
                (instantiate menu-item% ()
                  (label (string-constant create-servlet))
                  (parent scheme-menu)
                  (callback
                   (lambda (me event)
                     (with-handlers ([exn:unknown-language?
                                      (lambda (exn)
                                        (message-box (string-constant create-executable-menu-item-label)
                                                     (format (string-constant create-servlet-unsupported-language)
                                                             (exn:unknown-language-lang exn))
                                                     #f
                                                     '(ok stop)))])
                       (let* ([lang-prefs
                               (preferences:get
                                (drscheme:language-configuration:get-settings-preferences-symbol))]
                              [program (read-program lang-prefs (get-definitions-text))]
                              [teachpacks (drscheme:teachpack:teachpack-cache-filenames (preferences:get 'drscheme:teachpacks))]
                              [wrapped (wrap (find-language-require lang-prefs) teachpacks program)]
                              ; FIX - use file name as default file name
                              [file-name (put-file (string-constant create-servlet) this
                                                   (build-path (collection-path "web-server")
                                                               "default-web-root" "servlets")
                                                   #f ".ss")])
                         (when file-name
                           (call-with-output-file file-name
                             (lambda (out)
                               (for-each (lambda (x) (write x out)) wrapped))
                             'truncate))))))))))))))))
