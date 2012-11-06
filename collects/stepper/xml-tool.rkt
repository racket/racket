#lang racket/base

(require "private/xml-snip-helpers.rkt"
         "private/find-tag.rkt"
         "private/xml-sig.rkt"
         racket/unit
         racket/gui
         framework
         drracket/tool
         xml/xml
         string-constants)
  
  (provide xml-tool@)
  
  (define orig (current-output-port))
  (define-unit xml-tool@
    (import drracket:tool^)
    (export xml^)
    
    ;; these were necessary when this was a stand-alone tool:
    #;(define (phase1) (void))
    #;(define (phase2) (void))
    
    (preferences:set-default 'drracket:xml-eliminate-whitespace #t boolean?)
      
      (define xml-box-color "forest green")
      (define scheme-splice-box-color "blue")
      (define scheme-box-color "purple")
      
      ;; get-bm : string -> (union (is-a?/c bitmap%) false?)
      (define (get-bm name)
        (let ([bm (make-object bitmap% (build-path (collection-path "icons") name))])
          (unless (send bm ok?)
            (error 'xml-box "bitmap ~a failed to load" name))
          bm))
      
      (define scheme-box-bm (get-bm "scheme-box.jpg"))
      (define scheme-splice-box-bm (get-bm "scheme-splice-box.jpg"))
      (define xml-box-bm (get-bm "xml-box.jpg"))
      (define xml-box-open-bm (get-bm "xml-box-open.jpg"))
      
      (define xml-snip%
        (class* decorated-editor-snip% (readable-snip<%> xml-snip<%>)
          (inherit get-editor)
          
          (init-field eliminate-whitespace-in-empty-tags?)
          
          (define/override (make-editor) (make-object (get-xml-text%)))
          (define/override (get-corner-bitmap) 
            (if eliminate-whitespace-in-empty-tags?
                xml-box-bm
                xml-box-open-bm))
          
          (define/override (get-menu)
            (let* ([menu (instantiate popup-menu% ()
                           (title (string-constant xml-tool-xml-box)))]
                   [leave-alone-item
                    (make-object checkable-menu-item%
                      (string-constant xml-tool-leave-whitespace-alone)
                      menu
                      (lambda (x y)
                        (set-eliminate-whitespace-in-empty-tags? #f)))]
                   [eliminate-item
                    (make-object checkable-menu-item%
                      (string-constant xml-tool-eliminate-whitespace-in-empty-tags)
                      menu
                      (lambda (x y)
                        (set-eliminate-whitespace-in-empty-tags? #t)))])
              (send leave-alone-item check (not eliminate-whitespace-in-empty-tags?))
              (send eliminate-item check eliminate-whitespace-in-empty-tags?)
              menu))
          
          (inherit get-admin reset-min-sizes)
          (define/private (set-eliminate-whitespace-in-empty-tags? new)
            (unless (eq? eliminate-whitespace-in-empty-tags? new)
              (set! eliminate-whitespace-in-empty-tags? new)
              (preferences:set 'drracket:xml-eliminate-whitespace new)
              (reset-min-sizes)
              (let ([admin (get-admin)])
                (when admin
                  (send admin resized this #t)))))
          
          (define/public (read-special source line col pos)
            (xml-read-special eliminate-whitespace-in-empty-tags?
			      this
			      source
			      line
			      col
			      pos))
          
          (define/override (write stream-out)
            (send stream-out put (if eliminate-whitespace-in-empty-tags?
                                     0
                                     1))
            (send (get-editor) write-to-file stream-out 0 'eof))
          (define/override (make-snip)
            (instantiate xml-snip% ()
              [eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?]))
          
          (define/override (get-color) xml-box-color)
          
          (inherit show-border set-snipclass)
          (super-instantiate ())
          (show-border #t)
          (set-snipclass lib-xml-snipclass)))
      
      (define xml-snipclass%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in)
            (instantiate xml-snip% ()
              [eliminate-whitespace-in-empty-tags?
               (preferences:get 'drracket:xml-eliminate-whitespace)]))
          (super-instantiate ())))
      
      ;; this snipclass is for old, saved files (no snip has it set)
      (define xml-snipclass (make-object xml-snipclass%))
      (send xml-snipclass set-version 1)
      (send xml-snipclass set-classname "drscheme:xml-snip")
      (send (get-the-snip-class-list) add xml-snipclass)
      
      ;; this snipclass overrides the actual one in xml/xml-snipclass
      ;; as a full-fledged snipclass, for use in DrRacket.
      
      (define lib-xml-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([eliminate-whitespace-in-empty-tags? (zero? (send stream-in get-exact))]
                   [snip (instantiate xml-snip% ()
                           [eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?])])
              (send (send snip get-editor) read-from-file stream-in #f)
              snip))
          (super-instantiate ())))
      
      (define lib-xml-snipclass (make-object lib-xml-snipclass%))
      (send lib-xml-snipclass set-version 1)
      (send lib-xml-snipclass set-classname (format "~s" '(lib "xml-snipclass.ss" "xml")))
      (send (get-the-snip-class-list) add lib-xml-snipclass)
      
      (define scheme-snip%
        (class* decorated-editor-snip% (scheme-snip<%> readable-snip<%>)
          (init-field splice?)
          (define/public (get-splice?) splice?)
          
          (define/override (get-corner-bitmap) 
            (if splice? 
                scheme-splice-box-bm
                scheme-box-bm))
          
          (define/override (get-menu)
            (let ([menu (instantiate popup-menu% ()
                          (title (if splice?
                                     (string-constant xml-tool-scheme-splice-box)
                                     (string-constant xml-tool-scheme-box))))])
              (instantiate menu-item% ()
                (label
                 (if splice?
                     (string-constant xml-tool-switch-to-scheme)
                     (string-constant xml-tool-switch-to-scheme-splice)))
                (parent menu)
                (callback (lambda (x y) (toggle-splice))))
              menu))
          
          (inherit get-admin reset-min-sizes)
          (define/private (toggle-splice)
            (let ([admin (get-admin)])
              (set! splice? (not splice?))
              (reset-min-sizes)
              (when admin
                (send admin resized this #t))))
          
          (inherit get-editor)
          
          (define/public (read-special source line col pos)
            (scheme-read-special this source line col pos))
          
          (define/override (make-editor) (new (get-scheme-box-text%)))
          
          (define/override (make-snip) 
            (instantiate scheme-snip% () (splice? splice?)))
          
          (define/override (write stream-out)
            (send stream-out put (if splice? 0 1))
            (super write stream-out))
          
          (inherit show-border set-snipclass)
          (define/override (get-color)
            (if splice?
                scheme-splice-box-color
                scheme-box-color))
          
          (super-instantiate ())
          (show-border #t)
          (set-snipclass lib-scheme-snipclass)))

      (define scheme-box-text% #f)
      (define (get-scheme-box-text%)
        (unless scheme-box-text%
          (set! scheme-box-text%
                (class ((drracket:unit:get-program-editor-mixin)
                        (add-file-keymap-mixin
                         racket:text%))
                  (inherit copy-self-to)
                  (define/override (copy-self)
                    (let ([t (new scheme-box-text%)])
                      (copy-self-to t)
                      t))
                  (super-new)
                  (inherit set-max-undo-history)
                  (set-max-undo-history 'forever))))
        scheme-box-text%)
        
      (define (add-file-keymap-mixin %)
        (class %
          (define/override (get-keymaps)
            (cons (keymap:get-file) (super get-keymaps)))
          (super-instantiate ())))
      
      (define scheme-snipclass%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in)
            (let* ([splice? (zero? (send stream-in get-exact))])
              (instantiate scheme-snip% () (splice? splice?))))
          (super-instantiate ())))
      
      ;; this snipclass is for old, saved files (no snip has it set)
      (define scheme-snipclass (make-object scheme-snipclass%))
      (send scheme-snipclass set-version 2)
      (send scheme-snipclass set-classname "drscheme:scheme-snip")
      (send (get-the-snip-class-list) add scheme-snipclass)
      
      ;; this snipclass overrides the one in xml/scheme-snipclass
      ;; as a full-fledged snipclass, for use in DrRacket.
      (define lib-scheme-snipclass (make-object scheme-snipclass%))
      (send lib-scheme-snipclass set-version 1)
      (send lib-scheme-snipclass set-classname (format "~s" '(lib "scheme-snipclass.ss" "xml")))
      (send (get-the-snip-class-list) add lib-scheme-snipclass)
      
      (define plain-text%
        (class text:keymap% 
          (init-field [delta (make-object style-delta%)])
          (inherit change-style copy-self-to)
          (inherit begin-edit-sequence end-edit-sequence)
          (define/override (copy-self)
            (let ([t (make-object plain-text% delta)])
              (copy-self-to t)
              t))
          (define/augment (on-insert x y)
            (begin-edit-sequence)
            (inner (void) on-insert x y))
          (define/augment (after-insert x y)
            (change-style delta x (+ x y))
            (end-edit-sequence)
            (inner (void) after-insert x y))
          
          (define/override (get-keymaps)
            (cons (keymap:get-file) (super get-keymaps)))
          
          (inherit set-styles-sticky)
          (super-instantiate ())
          (set-styles-sticky #f)))
      
      (define xml-keymap (make-object keymap%))
      (send xml-keymap add-function 
            "matching-xml" 
            (lambda (x e) 
              (when (is-a? x text%)
                (matching-xml x))))
      (send xml-keymap map-function ">" "matching-xml")
      
      ;; The editor standard style list's Standard size changes
      ;; according to the font size preference. So,
      ;; we create an XML style based on that stule
      ;; for the XML boxes, so they change size too.
      (let* ([style-list (editor:get-standard-style-list)]
             [style (send style-list find-named-style "XML")]) 
        (unless style
          (let ([xml-delta (make-object style-delta% 'change-family 'default)])
            (send style-list new-named-style "XML" 
                  (send style-list find-or-create-style 
                        (send style-list find-named-style (editor:get-default-color-style-name))
                        xml-delta)))))

      (define xml-text-mixin
        (mixin (editor:keymap<%> editor:standard-style-list<%> (class->interface text%)) ()
          (define/override (get-keymaps)
            (cons xml-keymap (super get-keymaps)))
          
          (inherit begin-edit-sequence end-edit-sequence
                   change-style get-style-list)
          (define/augment (on-insert start rng)
            (begin-edit-sequence)
            (inner (void) on-insert start rng))
          (define/augment (after-insert start rng)
            (inner (void) after-insert start rng)
            (change-style (send (get-style-list) find-named-style "XML")
                          start 
                          (+ start rng))
            (end-edit-sequence))
          
          (super-instantiate ())
          
          (inherit set-style-list)
          ))
      
      (define get-xml-text%
	(let ([xml-text% #f])
	  (lambda ()
	    (unless xml-text%
	      (set! xml-text% (class ((drracket:unit:get-program-editor-mixin)
                                      (xml-text-mixin
                                       plain-text%))
                                (inherit copy-self-to)
                                (define/override (copy-self)
                                  (let ([t (new xml-text%)])
                                    (copy-self-to t)
                                    t))
                                (super-new)
                                (inherit set-max-undo-history)
                                (set-max-undo-history 'forever))))
            xml-text%)))

      ;; matching-xml : (is-a?/c text) -> void
      ;; inserts > and if there is an XML tag just
      ;; before the caret, inserts the corresponding
      ;; close XML tag after the caret.
      (define (matching-xml text)
        (send text begin-edit-sequence)
        (send text insert ">")
        (let* ([start (send text get-start-position)]
               [tagname (find-tag text start)])
          (when tagname
            (send text insert "</")
            (send text insert tagname)
            (send text insert ">")
            (send text set-position start)))
        (send text end-edit-sequence))

    (define (xml-box-frame-extension super%)
        (class super%
          (inherit get-editor register-capability-menu-item get-insert-menu get-edit-target-object)
          
          (super-new)
          
          (let* ([menu (get-insert-menu)]
                 [find-insertion-point ;; -> (union #f editor<%>)
                  ;; returns the editor (if there is one) with the keyboard focus
                  (lambda ()
                    (let ([editor (get-edit-target-object)])
                      (and editor
                           (is-a? editor editor<%>)
                           (let loop ([editor editor])
                             (let ([focused (send editor get-focus-snip)])
                               (if (and focused
                                        (is-a? focused editor-snip%))
                                   (loop (send focused get-editor))
                                   editor))))))]
                 [insert-snip
                  (lambda (make-obj)
                    (let ([editor (find-insertion-point)])
                      (when editor
                        (let ([snip (make-obj)])
                          (send editor insert snip)
                          (send editor set-caret-owner snip 'display)))))]
                 [demand-callback ;; : menu-item% -> void
                  ;; enables the menu item when there is an editor available.
                  (lambda (item)
                    (send item enable (find-insertion-point)))])
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-xml-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback
               (lambda (menu evt)
                 (insert-snip
                  (lambda () 
                    (instantiate xml-snip% ()
                      [eliminate-whitespace-in-empty-tags?
                       (preferences:get 'drracket:xml-eliminate-whitespace)]))))))
            (register-capability-menu-item 'drracket:special:xml-menus (get-insert-menu))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-scheme-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback 
               (lambda (menu evt)
                 (insert-snip 
                  (lambda () (instantiate scheme-snip% () (splice? #f)))))))
            (register-capability-menu-item 'drracket:special:xml-menus (get-insert-menu))
            (instantiate menu:can-restore-menu-item% ()
              (label (string-constant xml-tool-insert-scheme-splice-box))
              (parent menu)
              (demand-callback demand-callback)
              (callback
               (lambda (menu evt)
                 (insert-snip
                  (lambda () (instantiate scheme-snip% () (splice? #t)))))))
            (register-capability-menu-item 'drracket:special:xml-menus (get-insert-menu)))
          
          (frame:reorder-menus this)))
      
      (drracket:language:register-capability 'drracket:special:xml-menus (flat-contract boolean?) #t)
      
      (drracket:get/extend:extend-unit-frame xml-box-frame-extension))
