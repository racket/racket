#lang scheme
(require mred
         string-constants
         framework
         scheme/class)

(provide (rename-out (kl-module-begin #%module-begin))
         (except-out (all-from-out scheme) #%module-begin)
         (all-from-out framework
                       mred
                       scheme/class))

(define-syntax (kl-module-begin stx)
  (syntax-case stx ()
    [(mb a ...) 
     (with-syntax ([#%keymap (datum->syntax (syntax mb) '#%keymap)]
                   [#%keybinding (datum->syntax (syntax mb) '#%keybinding)]
                   [keybinding (datum->syntax (syntax mb) 'keybinding)])
       (syntax (#%plain-module-begin 
                (define #%keymap (make-object keymap:aug-keymap%))
                (provide #%keymap)
                (define name-counter (make-hash))
                (define (unique-name raw-name)
                  (let ([last-number (hash-ref name-counter raw-name #f)])
                    (cond
                      [last-number
                       (hash-set! name-counter raw-name (+ last-number 1))
                       (format "~a:~a" raw-name last-number)]
                      [else
                       (hash-set! name-counter raw-name 2)
                       raw-name])))
                (define (#%keybinding key proc src line col pos)
                  (unless (string? key)
                    (error 'keybinding "expected string as first argument, got ~e (other arg ~e)" key proc))
                  (unless (and (procedure? proc)
                               (procedure-arity-includes? proc 2))
                    (error 'keybinding "expected procedure of two arguments as second argument, got ~e (other arg ~e)"
                           proc
                           key))
                  (let ([name 
                         (unique-name
                          (cond
                            [(symbol? (object-name proc))
                             (format "~a" (object-name proc))]
                            [(and line col)
                             (format "~a:~a.~a" src line col)]
                            [else
                             (format "~a:~a" src pos)]))])
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
                a ...)))]))
