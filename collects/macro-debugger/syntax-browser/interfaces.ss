#lang scheme/base
(require scheme/class
         macro-debugger/util/class-iop)
(provide (all-defined-out))

;; displays-manager<%>
(define-interface displays-manager<%> ()
  (;; add-syntax-display : display<%> -> void
   add-syntax-display

   ;; remove-all-syntax-displays : -> void
   remove-all-syntax-displays))

;; selection-manager<%>
(define-interface selection-manager<%> ()
  (;; selected-syntax : syntax/#f
   set-selected-syntax
   get-selected-syntax
   listen-selected-syntax))

;; mark-manager<%>
;; Manages marks, mappings from marks to colors
(define-interface mark-manager<%> ()
  (;; get-primary-partition : -> partition
   get-primary-partition

   ;; reset-primary-partition : -> void
   reset-primary-partition))

;; secondary-partition<%>
(define-interface secondary-partition<%> ()
  (;; get-secondary-partition : -> partition<%>
   get-secondary-partition

   ;; set-secondary-partition : partition<%> -> void
   set-secondary-partition

   ;; listen-secondary-partition : (partition<%> -> void) -> void
   listen-secondary-partition

   ;; get-identifier=? : -> (cons string procedure)
   get-identifier=?

   ;; set-identifier=? : (cons string procedure) -> void
   set-identifier=?

   ;; listen-identifier=? : ((cons string procedure) -> void) -> void
   listen-identifier=?))

;; controller<%>
(define-interface controller<%> (displays-manager<%>
                                 selection-manager<%>
                                 mark-manager<%>
                                 secondary-partition<%>)
  ())


;; host<%>
(define-interface host<%> ()
  (;; get-controller : -> controller<%>
   get-controller

   ;; add-keymap : text snip
   add-keymap))

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

   ;; all-ranges : (list-of Range)
   ;; Sorted outermost-first
   all-ranges

   ;; get-identifier-list : (list-of identifier)
   get-identifier-list))


;; A Range is (make-range datum number number)
(define-struct range (obj start end))


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
   get-text))

(define-interface partition<%> ()
  (;; get-partition : any -> number
   get-partition

   ;; same-partition? : any any -> number
   same-partition?

   ;; count : -> number
   count))
