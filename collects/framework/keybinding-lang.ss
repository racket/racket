(module keybinding-lang mzscheme
  (require (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "class.ss"))
  
  (provide (rename kl-module-begin #%module-begin)
           (all-from-except mzscheme #%module-begin)
           (all-from (lib "framework.ss" "framework"))
           (all-from (lib "mred.ss" "mred"))
           (all-from (lib "class.ss")))
  
  (define-syntax (kl-module-begin stx)
    (syntax-case stx ()
      [(mb a ...) 
       (with-syntax ([#%keymap (datum->syntax-object (syntax mb) '#%keymap)]
                     [#%keybinding (datum->syntax-object (syntax mb) '#%keybinding)]
                     [keybinding (datum->syntax-object (syntax mb) 'keybinding)])
         (syntax (#%plain-module-begin 
                  (define #%keymap (make-object keymap:aug-keymap%))
                  (provide #%keymap)
                  (define counter 0)
                  (define (#%keybinding key proc src line col pos)
                    (unless (string? key)
                      (error 'keybinding "expected string as first argument, got ~e (other arg ~e)" key proc))
                    (unless (and (procedure? proc)
                                 (procedure-arity-includes? proc 2))
                      (error 'keybinding "expected procedure of two arguments as second argument, got ~e (other arg ~e)"
                             proc
                             key))
                    (set! counter (+ counter 1))
                    (let ([name 
                           (cond
                             [(symbol? (object-name proc))
                              (format "~a" (object-name proc))]
                             [(and line col)
                              (format "~a:~a.~a:~a" src line col counter)]
                             [else
                              (format "~a:~a:~a" src pos counter)])])
                      (send #%keymap add-function name 
                            (λ (x y)
                              (let ([end-edit-sequence
                                     (λ ()
                                       (when (is-a? x editor<%>)
                                         (let loop ()
                                           (when (send x in-edit-sequence?)
                                             (send x end-edit-sequence)
                                             (loop)))))])
                              (with-handlers ([exn:fail? 
                                               (λ (x)
                                                 (end-edit-sequence)
                                                 (message-box (string-constant drscheme)
                                                              (format (string-constant user-defined-keybinding-error) 
                                                                      name
                                                                      (exn-message x))))])
                                (proc x y)
                                (when (is-a? x editor<%>)
                                  (when (send x in-edit-sequence?)
                                    (end-edit-sequence)
                                    (message-box (string-constant drscheme)
                                                 (format (string-constant user-defined-keybinding-error) 
                                                                      name
                                                                      "Editor left in edit-sequence"))))))))
                      (send #%keymap map-function key name)))
                  (define-syntax (keybinding stx)
                    (syntax-case stx ()
                      [(_ key val)
                       (with-syntax ([src (syntax-source stx)]
                                     [line (syntax-line stx)]
                                     [col (syntax-column stx)]
                                     [pos (syntax-position stx)])
                         (syntax (#%keybinding key val 'src 'line 'col 'pos)))]))
                  a ...)))])))
