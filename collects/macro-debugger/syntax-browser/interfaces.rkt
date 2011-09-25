#lang racket/base
(require unstable/class-iop
         (for-syntax racket/base))
(provide (all-defined-out))

;; Helpers

(define-for-syntax (join . args)
  (define (->string x)
    (cond [(string? x) x]
          [(symbol? x) (symbol->string x)]
          [(identifier? x) (symbol->string (syntax-e x))]
          [else (error '->string)]))
  (string->symbol (apply string-append (map ->string args))))

;; not in notify.rkt because notify depends on gui
(define-interface-expander methods:notify
  (lambda (stx)
    (syntax-case stx ()
      [(_ name ...)
       (datum->syntax #f
         (apply append
                (for/list ([name (syntax->list #'(name ...))])
                  (list ;; (join "init-" #'name)
                   (join "get-" name)
                   (join "set-" name)
                   (join "listen-" name)))))])))

;; Interfaces

;; config<%>
(define-interface config<%> ()
  ((methods:notify suffix-option
                   syntax-font-size
                   colors
                   width
                   height
                   props-percentage
                   props-shown?)))

;; displays-manager<%>
(define-interface displays-manager<%> ()
  (;; add-syntax-display : display<%> -> void
   add-syntax-display

   ;; remove-all-syntax-displays : -> void
   remove-all-syntax-displays))

;; selection-manager<%>
(define-interface selection-manager<%> ()
  (;; selected-syntax : notify-box of syntax/#f
   (methods:notify selected-syntax)))

;; mark-manager<%>
;; Manages marks, mappings from marks to colors
(define-interface mark-manager<%> ()
  (;; get-primary-partition : -> partition
   get-primary-partition

   ;; reset-primary-partition : -> void
   reset-primary-partition))

;; secondary-relation<%>
(define-interface secondary-relation<%> ()
  (;; identifier=? : notify-box of (cons string (U #f (id id -> bool)))
   (methods:notify identifier=?)))

;; controller<%>
(define-interface controller<%> (displays-manager<%>
                                 selection-manager<%>
                                 mark-manager<%>
                                 secondary-relation<%>)
  ())


;; host<%>
(define-interface host<%> ()
  (;; get-controller : -> controller<%>
   get-controller

   ;; add-keymap : text snip
   add-keymap))

;; keymap/popup<%>
(define-interface keymap/popup<%> ()
  (;; add-context-menu-items : popup-menu -> void
   add-context-menu-items))

;; display<%>
(define-interface display<%> ()
  (;; refresh : -> void
   refresh

   ;; highlight-syntaxes : (list-of syntax) color -> void
   highlight-syntaxes

   ;; underline-syntaxes : (listof syntax) -> void
   underline-syntaxes

   ;; get-start-position : -> number
   get-start-position

   ;; get-end-position : -> number
   get-end-position

   ;; get-range : -> range<%>
   get-range))

;; range<%>
(define-interface range<%> ()
  (;; get-ranges : datum -> (list-of (cons number number))
   get-ranges

   ;; get-treeranges : -> (listof TreeRange)
   get-treeranges

   ;; all-ranges : (list-of Range)
   ;; Sorted outermost-first
   all-ranges

   ;; get-identifier-list : (list-of identifier)
   get-identifier-list))


;; A Range is (make-range datum number number)
(define-struct range (obj start end))

;; A TreeRange is (make-treerange syntax nat nat (listof TreeRange))
;; where subs are disjoint, in order, and all contained within [start, end]
(define-struct treerange (obj start end subs))

;; syntax-prefs<%>
(define-interface syntax-prefs<%> ()
  (pref:width
   pref:height
   pref:props-percentage
   pref:props-shown?))

;; widget-hooks<%>
(define-interface widget-hooks<%> ()
  (;; setup-keymap : -> void
   setup-keymap

   ;; shutdown : -> void
   shutdown))

;; keymap-hooks<%>
(define-interface keymap-hooks<%> ()
  (;; make-context-menu : -> context-menu<%>
   make-context-menu

   ;; get-context-menu% : -> class
   get-context-menu%))

;; context-menu-hooks<%>
(define-interface context-menu-hooks<%> ()
  (add-edit-items
   after-edit-items
   add-selection-items
   after-selection-items
   add-partition-items
   after-partition-items))


;;----------

;; Convenience widget, specialized for displaying stx and not much else
(define-interface syntax-browser<%> ()
  (add-syntax
   add-text
   add-error-text
   add-clickback
   add-separator
   erase-all
   get-controller
   get-text))

(define-interface partition<%> ()
  (;; get-partition : any -> number
   get-partition

   ;; same-partition? : any any -> number
   same-partition?

   ;; count : -> number
   count))
