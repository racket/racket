#|

todo:

slideshow language:
- handle ranges (just the first letter of an identifier), 
- show/hide menu in the wrong place
- dock/undock the preview window
- editing should make the annotations disappear (need to extend the program mixin)
- move calls to draw-pict over to user's eventspace

pict snip :
- snipclass for running snips outside of drscheme
- need to toggle the picts back to scheme code when
  important things happen (save, execute, etc).
- should save the true pict size when it gets recorded.
- show the true size in the GUI
- when a snip is deleted from inside the pasteboard, remove it from the caches
- check that when a snip is inserted, things revert (?).
   maybe something better should happen?
- test up-to-date? flag
|#

(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "unitsig.ss")
           (lib "contract.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework")
           (lib "mrpict.ss" "texpict")
           (lib "pict-value-snip.ss" "texpict")
           (lib "list.ss")
           "private/pict-box-lib.ss"
           "private/image-snipr.ss")

  (provide tool@
           get-snp/poss
           build-lib-pict-stx)

  (define orig-inspector (current-code-inspector))
  (define orig-lcp (current-library-collection-paths))

  (define-syntax syntax/cert 
    (syntax-rules ()
      [(_ stx tmpl) (let ([stx stx])
		      (syntax-recertify
		       (syntax/loc stx tmpl)
		       stx
		       orig-inspector
		       #f))]))

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define original-output-port (current-output-port))
      (define (oprintf . args) (apply fprintf original-output-port args))
      
      (define sc-show-slideshow-panel (string-constant slideshow-show-slideshow-panel))
      (define sc-hide-slideshow-panel (string-constant slideshow-hide-slideshow-panel))
      (define sc-freeze-picts (string-constant slideshow-freeze-picts))
      (define sc-thaw-picts (string-constant slideshow-thaw-picts))
      (define sc-hide-picts (string-constant slideshow-hide-picts))
      (define sc-show-picts (string-constant slideshow-show-picts))
      (define sc-cannot-show-picts (string-constant slideshow-cannot-show-picts))
      (define sc-insert-pict-box (string-constant slideshow-insert-pict-box))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; pict box
      ;;
      
      
      (define pict-pasteboard%
        (class pasteboard%
          (inherit get-admin)
          
          (define/augment (after-insert snip before x y)
            (let ([admin (get-admin)])
              (when (is-a? admin editor-snip-editor-admin<%>)
                (send (send admin get-snip) inserted-snip)))
            (inner (void) after-insert snip before x y))
          
          (super-new)))
      
      (define pict-snip%
        (class* decorated-editor-snip% (readable-snip<%>)
          (inherit get-editor)

          (define show-picts? #f)
          
          ;; only for use in the copy method and the read snipclass method
          (define/public (set-show-picts b) (set! show-picts? b))
          
          ;; up-to-date? : boolean
          ;; indicates if the recent shapes cache is up to date
          (define up-to-date? #f)
          
          ;; only for use in the copy method and the read snipclass method
          (define/public (set-up-to-date b) (set! up-to-date? b))
          
          ;; bitmap-table : hash-table[snip -o> bitmap]
          ;; maps from the true (Scheme) snip to its current bitmap
          (define bitmap-table (make-hash-table))
          
          ;; only for use in the copy method and the read snipclass method
          (define/public (set-bitmap-table bt) (set! bitmap-table bt))
          
          (define/override (make-editor) (make-object pict-pasteboard%))
          (define/override (get-corner-bitmap) slideshow-bm)
          
          (define/override (copy)
            (let* ([cp (make-object pict-snip%)]
                   [ed (send cp get-editor)])
              (send (get-editor) copy-self-to ed)
              (let ([bt (make-hash-table)])
                (hash-table-for-each bitmap-table (lambda (k v) (hash-table-put! bt (send k copy) v)))
                (send cp set-bitmap-table bt)
                (send cp set-show-picts show-picts?)
                (send cp set-up-to-date up-to-date?)
                cp)))
          
          
          (define/override (get-menu)
            (let ([menu (instantiate popup-menu% () (title #f))])
              (cond
                [show-picts?
                 (make-object checkable-menu-item%
                   sc-hide-picts
                   menu
                   (lambda (x y)
                     (hide-picts)))]
                [up-to-date?
                 (make-object checkable-menu-item%
                   sc-show-picts
                   menu
                   (lambda (x y)
                     (show-picts)))]
                [else
                 (let ([m (make-object menu-item%
                            sc-cannot-show-picts
                            menu
                            (lambda (x y) void))])
                   (send m enable #f))])
              menu))
          
          (define/public (update-bitmap-table sub-snips sub-bitmaps)
            (let ([hidden-table (make-hash-table)])
              (let loop ([snip (send (get-editor) find-first-snip)])
                (cond
                  [snip
                   (when (is-a? snip image-snip/r%)
                     (hash-table-put! hidden-table (send snip get-orig-snip) snip))
                   (loop (send snip next))]
                  [else (void)]))
              (for-each (lambda (snip bitmap) 
                          (hash-table-put! bitmap-table snip bitmap)
                          (let ([showing (hash-table-get hidden-table snip (lambda () #f))])
                            (when showing
                              (send showing set-bitmap bitmap))))
                        sub-snips
                        sub-bitmaps)
              (set! up-to-date? #t)))
          
          (define/private (show-picts)
            (let ([pb (get-editor)])
              (set! show-picts? #t)
              (send pb begin-edit-sequence)
              
              (set! system-insertion? #t)
              (hash-table-for-each 
               bitmap-table
               (lambda (snip bitmap)
                 (let ([bm-snip (make-object image-snip/r% bitmap snip)])
                   (let-values ([(x y) (snip-location pb snip)])
                     (send snip release-from-owner)
                     (send pb insert bm-snip x y)))))
              (set! system-insertion? #f)
              
              (send pb end-edit-sequence)))
          
          (define/private (hide-picts)
            (let ([pb (get-editor)])
              (set! show-picts? #f)
              
              (send pb begin-edit-sequence)
              
              (let ([all-snips (let loop ([snip (send pb find-first-snip)])
                                 (cond
                                   [snip (cons snip (loop (send snip next)))]
                                   [else null]))])
                (set! system-insertion? #t)
                (for-each (lambda (snip)
                            (when (is-a? snip image-snip/r%)
                              (let ([real-snip (send snip get-orig-snip)])
                                (let-values ([(x y) (snip-location pb snip)])
                                  (send snip release-from-owner)
                                  (send pb insert real-snip x y)))))
                          all-snips)
                (set! system-insertion? #f))
              
              (send pb end-edit-sequence)))
          
          ;; called on user thread
          (define/public (read-special file line col pos)
            (let ([ans-chan (make-channel)])
              (parameterize ([current-eventspace drs-eventspace])
                (queue-callback
                 (lambda ()
                   (channel-put ans-chan (get-snp/poss this)))))
              (let ([snp/poss (channel-get ans-chan)])
                (build-lib-pict-stx 
                 (lambda (ids)
                   (with-syntax ([(ids ...) ids]
                                 [this this]
                                 [build-bitmap/check build-bitmap/check]
                                 [drs-eventspace drs-eventspace]
                                 [(subsnips ...) (map snp/pos-snp snp/poss)]
                                 [(bitmap-ids ...) (generate-ids "drawer-id" (map snp/pos-snp snp/poss))])
                     (syntax
                      (let ([bitmap-ids (build-bitmap/check ids (pict-width ids) (pict-height ids) draw-pict pict?)] ...)
                        (parameterize ([current-eventspace drs-eventspace])
                          (queue-callback 
                           (lambda () ;; drs eventspace
                             (send this update-bitmap-table 
                                   (list subsnips ...)
                                   (list bitmap-ids ...)))))))))
                 snp/poss))))
          
          (define/override (write stream-out)
            (send stream-out put (if show-picts? 1 0))
            (send stream-out put (if up-to-date? 1 0))
            (send (get-editor) write-to-file stream-out))
          (define/override (make-snip) (new pict-snip%))
          
          (define system-insertion? #f)
          (define/public (inserted-snip)
            (unless system-insertion?
              (set! up-to-date? #f)
              (when show-picts?
                (hide-picts))))
          
          (inherit show-border set-snipclass)
          (super-new)
          (show-border #t)
          (set-snipclass lib-pict-snipclass)))
      
      (define lib-pict-snipclass%
        (class snip-class%
          (define/override (read stream-in)
            (let* ([snip (new pict-snip%)]
                   [editor (send snip get-editor)]
                   [show-picts? (not (zero? (send stream-in get-exact)))]
                   [up-to-date? (not (zero? (send stream-in get-exact)))])
              (send editor read-from-file stream-in #f)
              (send snip set-up-to-date up-to-date?)
              (send snip set-show-picts show-picts?)
              (let ([bt (make-hash-table)])
                (let loop ([snip (send editor find-first-snip)])
                  (cond
                    [(is-a? snip snip%)
                     (when (is-a? snip image-snip/r%)
                       (let ([orig (send snip get-orig-snip)]
                             [bm (send snip get-bitmap)])
                         (hash-table-put! bt orig bm)))
                     (loop (send snip next))]
                    [else (void)]))
                (send snip set-bitmap-table bt))
              snip))
          (super-new)))
      
      ;; build-bitmap/check : pict number number (pict dc number number -> void) (any -> boolean) -> bitmap
      ;; called on user-thread with a pict that the user made
      (define (build-bitmap/check pict w h draw-pict pict?)
        (unless (pict? pict) 
          (error 'pict-snip "expected a pict to be the result of each embedded snip, got ~e"
                 pict))
        (let* ([bm (make-object bitmap% 
                     (inexact->exact (ceiling w))
                     (inexact->exact (ceiling h)))]
               [bdc (make-object bitmap-dc% bm)])
          (send bdc clear)
          (draw-pict pict bdc 0 0)
          (send bdc set-bitmap #f)
          bm))
      
      (define (set-box/f b v) (when (box? b) (set-box! b v)))
      
      (define slideshow-bm
        (let ([bm (make-object bitmap% (build-path (collection-path "slideshow") "slideshow.bmp"))])
          (and (send bm ok?)
               bm)))
      
      (define drs-eventspace (current-eventspace))
      
      (define (add-special-menu-item menu frame)
        (let* ([find-insertion-point ;; -> (union #f editor<%>)
                ;; returns the editor (if there is one) with the keyboard focus
                (lambda ()
                  (let ([editor (send frame get-edit-target-object)])
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
            (label sc-insert-pict-box)
            (parent menu)
            (demand-callback demand-callback)
            (callback 
             (lambda (menu evt)
               (insert-snip 
                (lambda () (new pict-snip%))))))))
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  drscheme extensions
      ;;
      
      (define-struct p (pict-drawer width height n))
      (define new-p
        (let ([c 0])
          (lambda (pd w h)
            (set! c (+ c 1))
            (make-p pd w h c))))
      
      (define show-picts<%>
        (interface ()
          slideshow:register-pict))
      
      (define show-picts-mixin
        (mixin (color:text<%> editor<%>) (show-picts<%>)
          (inherit get-canvas freeze-colorer)
          
          ;; all-picts-ht : hash-table[(cons text% number) -op> hash-table[pict -o> p]]
          ;; the inner hashtables are really treated as sets, using the pict as
          ;; the equality measure.
          (define all-picts-ht (make-hash-table 'equal))
          
          (define frozen-colorers (make-hash-table))
          
          (define mouse-loc #f)
          (define visible-picts #f)
          
          (define/public (slideshow:clear-picts)
            (set! all-picts-ht (make-hash-table 'equal))
            (hash-table-for-each
             frozen-colorers
             (lambda (k v)
               (send k thaw-colorer)))
            (set! frozen-colorers (make-hash-table)))
          
          (define/public (slideshow:register-pict text offset range pict pict-drawer width height)
            (hash-table-get frozen-colorers
                            text
                            (lambda ()
                              (when (is-a? text color:text<%>)
                                (let ([locked? (send text is-locked?)])
                                  (send text lock #f)
                                  (send text freeze-colorer)
                                  (send text lock locked?))
                                (hash-table-put! frozen-colorers text #t))))
            (let ([locked? (send text is-locked?)])
              (send text lock #f)
              (send text change-style has-info-style offset (+ offset 1) #f)
              (send text lock locked?))
            (let* ([key (cons text offset)]
                   [picts-ht
                    (hash-table-get all-picts-ht 
                                    key
                                    (lambda ()
                                      (let ([new-ht (make-hash-table)])
                                        (hash-table-put! all-picts-ht key new-ht)
                                        new-ht)))])
              ;; store the new pict in the hash-table, unless it is already in there,
              ;; in which case we leave the current one (so we don't get a new number)
              (hash-table-get
               picts-ht
               pict
               (lambda ()
                 (hash-table-put!
                  picts-ht
                  pict
                  (new-p pict-drawer width height))))))
          
          (define/override (on-event evt)
            (cond
              [(send evt leaving?)
               (update-mouse #f #f)
               (super on-event evt)]
              [(or (send evt moving?)
                   (send evt entering?))
               (let-values ([(pos text) (get-pos/text evt)])
                 (update-mouse text pos))
               (super on-event evt)]
              [(send evt button-down? 'right)
               (let-values ([(pos text) (get-pos/text evt)])
                 (if (and pos text)
                     (unless (show-menu evt text pos)
                       (super on-event evt))
                     (super on-event evt)))]
              [else
               (super on-event evt)]))
          
          ;; show-menu : ... -> boolean
          ;; result indicates if a menu was shown
          (define/private (show-menu evt text pos)
            (let ([frame (let ([canvas (get-canvas)])
                           (and canvas
                                (send canvas get-top-level-window)))])
              (and frame
                   (let ([admin (send text get-admin)]
                         [menu (new popup-menu%)]
                         [show? #f])
                     (let* ([frozen-mouse-picts-key (cons text pos)]
                            [picts-ht (hash-table-get all-picts-ht frozen-mouse-picts-key (lambda () #f))])
                       (when picts-ht
                         (let ([picts (get-all-ps-from-ht picts-ht)])
                           (set! show? #t)
                           (new menu-item%
                                (label sc-freeze-picts)
                                (parent menu)
                                (callback
                                 (lambda (x y)
                                   (send frame slideshow:set-permanent-picts picts)))))))
                     (when (send frame slideshow:has-permanent-picts?)
                       (new menu-item%
                            (label sc-thaw-picts)
                            (parent menu)
                            (callback
                             (lambda (x y)
                               (send frame slideshow:set-permanent-picts #f))))
                       (set! show? #t))
                     (and show?
                          (begin
                            (send admin popup-menu 
                                  menu
                                  (send evt get-x)
                                  (send evt get-y))
                            #t))))))
            
          (define/private (update-mouse text pos)
            (let ([new-mouse-loc (and text pos (cons text pos))])
              (unless (equal? new-mouse-loc mouse-loc)
                (set! mouse-loc new-mouse-loc)
                (let ([frame (let ([canvas (get-canvas)])
                               (and canvas
                                    (send canvas get-top-level-window)))])
                  (when frame
                    (send frame slideshow:set-visible-picts
                          (and pos 
                               text 
                               (let ([picts-ht
                                      (hash-table-get all-picts-ht new-mouse-loc (lambda () #f))])
                                 (and picts-ht
                                      (get-all-ps-from-ht picts-ht))))))))))
          
          (define/private (get-all-ps-from-ht picts-ht)
            (let ([ps (hash-table-map picts-ht (lambda (k v) v))])
              (sort ps (lambda (x y) (<= (p-n x) (p-n y))))))

          ;; get-pos/text : event -> (values (union #f text%) (union number #f))
          ;; returns two #fs to indicate the event doesn't correspond to
          ;; a position in an editor, or returns the innermost text
          ;; and position in that text where the event is.
          (define/private (get-pos/text event)
            (let ([event-x (send event get-x)]
                  [event-y (send event get-y)]
                  [on-it? (box #f)])
              (let loop ([editor this])
                (let-values ([(x y) (send editor dc-location-to-editor-location event-x event-y)])
                  (cond
                    [(is-a? editor text%)
                     (let ([pos (send editor find-position x y #f on-it?)])
                       (cond
                         [(not (unbox on-it?)) (values #f #f)]
                         [else
                          (let ([snip (send editor find-snip pos 'after-or-none)])
                            (if (and snip
                                     (is-a? snip editor-snip%))
                                (loop (send snip get-editor))
                                (values pos editor)))]))]
                    [(is-a? editor pasteboard%)
                     (let ([snip (send editor find-snip x y)])
                       (if (and snip
                                (is-a? snip editor-snip%))
                           (loop (send snip get-editor))
                           (values #f #f)))]
                    [else (values #f #f)])))))
          
          (super-new)))
      
      (define has-info-style (make-object style-delta%))
      (send has-info-style set-delta-background "black")
      (send has-info-style set-transparent-text-backing-off #t)
      (send has-info-style set-delta-foreground "hotpink")
      
      (define tab-mixin
        (mixin (drscheme:rep:context<%> drscheme:unit:tab<%>) ()
          (inherit get-defs get-ints)
          (define/augment (clear-annotations)
            (send (get-defs) slideshow:clear-picts)
            (send (get-ints) slideshow:clear-picts)
            (inner (void) clear-annotations))
          (super-new)))
      
      (define unit-frame-mixin
        (mixin (drscheme:unit:frame<%>) ()
          (inherit get-show-menu)
          
          (define slideshow-parent-panel #f)
          (define everything-else-panel #f)
          (define slideshow-panel #f)
          (define slideshow-canvas #f)
          (define slideshow-panel-visible? #f)
          
          (define permanent-picts #f)
          (define visible-picts #f)
          
          (define/public (slideshow:set-visible-picts picts)
            (unless (equal? picts visible-picts)
              (set! visible-picts picts)
              (when slideshow-panel-visible?
                (draw-picts (send slideshow-canvas get-dc)))))
          
          (define/public (slideshow:set-permanent-picts picts)
            (set! permanent-picts picts)
            (if picts
                (send slideshow-canvas 
                      init-auto-scrollbars
                      (inexact->exact (floor (apply max (map p-width picts))))
                      (inexact->exact (floor (apply + (map p-height picts))))
                      0
                      0)
                (send slideshow-canvas init-auto-scrollbars #f #f 0 0)))
          (define/public (slideshow:has-permanent-picts?) permanent-picts)
          
          (define/override (make-root-area-container cls parent)
            (set! slideshow-parent-panel (super make-root-area-container slideshow-dragable% parent))
            (let ([root (make-object cls slideshow-parent-panel)])
              (set! everything-else-panel root)
              root))
          
          (define/override (update-shown)
            (super update-shown)
            (if slideshow-panel-visible?
                (begin
                  (send slideshow-parent-panel begin-container-sequence)
		  (unless slideshow-panel (build-slideshow-panel))
                  (when (is-a? view-menu-item menu-item%)
                    (send view-menu-item set-label sc-hide-slideshow-panel))
		  (send slideshow-parent-panel
                        change-children
                        (lambda (l)
                          (list everything-else-panel slideshow-panel)))
		  (send slideshow-parent-panel end-container-sequence))
                (begin
                  (when (is-a? view-menu-item menu-item%)
                    (send view-menu-item set-label sc-show-slideshow-panel))
                  (send slideshow-parent-panel
                        change-children
                        (lambda (l)
                          (list everything-else-panel))))))
          
          (define/private (build-slideshow-panel)
            (let ([p (preferences:get 'plt:slideshow:panel-percentage)])
              ;; must save the value of the pref before creating slideshow-panel
              ;; so that the callback doesn't clobber it
              
              (set! slideshow-panel (new vertical-panel% (parent slideshow-parent-panel)))
              (set! slideshow-canvas (new canvas%
                                          (style '(hscroll vscroll))
                                          (parent slideshow-panel)
                                          (paint-callback
                                           (lambda (x dc)
                                             (draw-picts dc)))))
              (send slideshow-parent-panel set-percentages (list p (- 1 p)))
              (preferences:set 'plt:slideshow:panel-percentage p)))

          (define/private (draw-picts dc)
            (send dc clear)
            (let ([picts (or permanent-picts visible-picts)])
              (when picts
                (let loop ([picts picts]
                           [y 0])
                  (cond
                    [(null? picts) (void)]
                    [else (let ([pict (car picts)])
                            ((p-pict-drawer pict) dc 0 y)
                            (loop (cdr picts)
                                  (+ y (p-height pict))))])))))
          
          (define/override (add-show-menu-items show-menu)
            (super add-show-menu-items show-menu)
            (set! view-menu-item
                  (new menu-item%
                       (label sc-show-slideshow-panel)
                       (parent (get-show-menu))
                       (callback
                        (lambda (x y)
                          (set! slideshow-panel-visible? (not slideshow-panel-visible?))
                          (update-shown))))))
          
          (define view-menu-item #f)
          
          (super-new)
          
          (inherit get-special-menu register-capability-menu-item)
          (add-special-menu-item (get-special-menu) this)
          (register-capability-menu-item 'drscheme:special:slideshow-menu-item (get-special-menu))))
      
      (define slideshow-dragable%
        (class panel:horizontal-dragable%
          (inherit get-percentages)
          (define/augment (after-percentage-change)
            (let ([percentages (get-percentages)])
              (when (= 2 (length percentages))
                (preferences:set 'plt:slideshow:panel-percentage (car percentages))))
            (inner (void) after-percentage-change))
          (super-new)))

      (define has-info-bkg-color (make-object color% "gray"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; preference defaults
      ;;
      
      ;; size of the drscheme window.
      (preferences:set-default 'plt:slideshow:panel-percentage 3/4 (lambda (x) (and (number? x) (<= 0 x 1))))

      (drscheme:language:register-capability 'drscheme:special:slideshow-menu-item (flat-contract boolean?) #t)
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  communication from user thread to drscheme's repl
      ;;
            
      (define system-eventspace (current-eventspace))
      
      (define (ss-dynamic-require lib id)
	(parameterize ([current-code-inspector orig-inspector]
		       [current-library-collection-paths orig-lcp])
	  (dynamic-require lib id)))

      ;; send-over : any syntax -> void
      ;; thread: (any) user's thread
      (define (send-over v stx)
        (let ([rep (drscheme:rep:current-rep)])
          (when rep
            (let ([pict? (ss-dynamic-require '(lib "mrpict.ss" "texpict") 'pict?)])
              (when (pict? v)
		(let* ([make-pict-drawer (ss-dynamic-require '(lib "mrpict.ss" "texpict") 'make-pict-drawer)]
		       [width ((ss-dynamic-require '(lib "mrpict.ss" "texpict") 'pict-width) v)]
		       [height ((ss-dynamic-require '(lib "mrpict.ss" "texpict") 'pict-height) v)]
		       [pict-drawer (make-pict-drawer v)])
		  (parameterize ([current-eventspace system-eventspace])
		    (queue-callback
		     (lambda ()
		       (add-pict-drawer stx v pict-drawer width height))))))))))
      
      ;; add-pict-drawer : syntax pict-drawer number number -> void
      ;; thread: system eventspace
      (define (add-pict-drawer stx pict pict-drawer width height)
        (let ([src (syntax-source stx)]
              [offset (syntax-position stx)]
              [span (syntax-span stx)])
          (when (and (is-a? src editor<%>)
                     (number? offset)
                     (number? span))
            (let ([top-most (let loop ([src src])
                              (let ([admin (send src get-admin)])
                                (cond
                                  [(not admin) #f]
                                  [(is-a? admin editor-snip-editor-admin<%>)
                                   (let* ([outer-editor-snip (send admin get-snip)]
                                          [es-admin (send outer-editor-snip get-admin)]
                                          [outer-editor (send es-admin get-editor)])
                                     (loop outer-editor))]
                                  [else src])))])
              (when (is-a? top-most show-picts<%>)
                (send top-most slideshow:register-pict src (- offset 1) span pict pict-drawer width height))))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; slideshow lang
      ;;
      
      (define slideshow-mixin
        (mixin (drscheme:language:language<%>) ()
          (define/override (front-end/complete-program input settings teachpack-cache)
            (let ([st (super front-end/complete-program input settings teachpack-cache)])
              (lambda ()
                (let ([sv (st)])
                  (cond
                    [(syntax? sv) (rewrite-syntax sv)]
                    [else sv])))))
          (define/override (front-end/interaction input settings teachpack-cache)
            (let ([st (super front-end/interaction input settings teachpack-cache)])
              (lambda ()
                (let ([sv (st)])
                  (cond
                    [(syntax? sv) (rewrite-syntax sv)]
                    [else sv])))))
          (define/override (get-language-name) "Slideshow")
          (super-new (module '(lib "plt-mred.ss" "lang"))
                     (language-position (list (string-constant experimental-languages)
                                              "Slideshow"))
                     (language-numbers (list 1000 341)))))
      
      (define (rewrite-syntax stx)
        (rewrite-top-level (expand stx)))
      
      (define (rewrite-top-level stx)
        (syntax-case stx (module begin)
          [(module identifier name (#%plain-module-begin module-level-expr ...))
           (with-syntax ([(rewritten-module-level-expr ...) (map rewrite-module-level
                                                                 (syntax->list
                                                                  (syntax (module-level-expr ...))))])
             (syntax/cert stx (module identifier name (#%plain-module-begin rewritten-module-level-expr ...))))]
          [(begin top-level-expr ...)
           (with-syntax ([(rewritten-top-level-expr ...) 
                          (map rewrite-top-level (syntax->list (syntax (top-level-expr ...))))])
             (syntax/cert stx (begin rewritten-top-level-expr ...)))]
          [general-top-level-expr (rewrite-general-top-level stx)]))

      (define (rewrite-module-level stx)
        (syntax-case stx (provide begin)
          [(provide provide-spec ...) stx]
          [(begin module-level-expr ...)
           (with-syntax ([(rewritten-module-level-expr ...)
                          (map rewrite-module-level 
                               (syntax->list (syntax (module-level-expr  ...))))])
             (syntax/cert stx (begin rewritten-module-level-expr ...)))]
          [general-top-level-expr (rewrite-general-top-level stx)]))
      
      (define (rewrite-general-top-level stx)
        (syntax-case stx (define-values define-syntaxes define-values-for-syntax 
			   require require-for-syntax require-for-template)
          [(define-values (variable ...) expr)
           (with-syntax ([rewritten-expr (add-send-over (rewrite-expr (syntax expr)) 
                                                        (syntax expr) 
                                                        (length (syntax->list (syntax (variable ...)))))])
             (syntax/cert stx (define-values (variable ...) rewritten-expr)))]
          [(define-syntaxes (variable ...) expr) stx]
          [(define-values-for-syntax (variable ...) expr) stx]
          [(require require-spec ...) stx]
          [(require-for-syntax require-spec ...) stx]
          [(require-for-template require-spec ...) stx]
          [expr (rewrite-expr stx)]))
      
      (define (rewrite-expr stx)
        (syntax-case stx (lambda case-lambda if begin begin0 let-values letrec-values set! quote quote-syntax 
				 with-continuation-mark #%app #%datum #%top)
          [variable
           (identifier? (syntax variable))
           (add-send-over/var (syntax variable) stx)]
          [(lambda formals expr ...)
           (with-syntax ([(rewritten-expr ...) 
                          (map rewrite-expr (syntax->list (syntax (expr ...))))])
	     (syntax/cert stx (lambda formals rewritten-expr ...)))]
          [(case-lambda (formals expr ...) ...)
           (with-syntax ([((rewritten-expr ...) ...)
                          (map (lambda (exprs) (map rewrite-expr (syntax->list exprs)))
                               (syntax->list (syntax ((expr ...) ...))))])
             (syntax/cert stx (case-lambda (formals rewritten-expr ...) ...)))]
          [(if expr1 expr2)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (rewrite-expr (syntax expr2))])
             (syntax/cert stx (if rewritten-expr1 rewritten-expr2)))]
          [(if expr1 expr2 expr3)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (rewrite-expr (syntax expr2))]
                         [rewritten-expr3 (rewrite-expr (syntax expr3))])
             (syntax/cert stx (if rewritten-expr1 rewritten-expr2 rewritten-expr3)))]
          [(begin expr ... last-expr)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1))
                                                    (syntax->list (syntax (expr ...))))]
                         [rewritten-last-expr (rewrite-expr (syntax last-expr))])
             (syntax/cert stx (begin rewritten-expr ... rewritten-last-expr)))]
          [(begin0 expr ...)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1)) 
                                                    (syntax->list (syntax (expr ...))))])
             (syntax/cert stx (begin0 rewritten-expr ...)))]
          [(let-values (((variable ...) v-expr) ...) expr ...)
           (with-syntax ([(rewritten-expr ...) (map rewrite-expr (syntax->list (syntax (expr ...))))]
                         [(rewritten-v-expr ...) (map rewrite-expr (syntax->list (syntax (v-expr ...))))]
                         [((send-over-vars ...) ...)
                          (map (lambda (vars)
                                 (map (lambda (var) (add-send-over/var var var))
                                      (syntax->list vars)))
                               (syntax->list (syntax ((variable ...) ...))))])
             (syntax/cert stx
			  (let-values (((variable ...) rewritten-v-expr) ...)
			    (begin (void) (begin (void) send-over-vars ...) ...)
			    rewritten-expr ...)))]
          [(letrec-values (((variable ...) v-expr) ...) expr ...)
           (with-syntax ([(rewritten-expr ...) (map rewrite-expr (syntax->list (syntax (expr ...))))]
                         [(rewritten-v-expr ...) (map rewrite-expr (syntax->list (syntax (v-expr ...))))]
                         [((send-over-vars ...) ...)
                          (map (lambda (vars)
                                 (map (lambda (var) (add-send-over/var var var))
                                      (syntax->list vars)))
                               (syntax->list (syntax ((variable ...) ...))))])
             (syntax/cert stx
			  (letrec-values (((variable ...) rewritten-v-expr) ...)
			    (begin (void) (begin (void) send-over-vars ...) ...)
			    rewritten-expr ...)))]
          [(set! variable expr)
           (with-syntax ([rewritten-expr (add-send-over (rewrite-expr (syntax expr)) (syntax expr) 1)])
             (syntax/cert stx (set! variable rewritten-expr)))]
          [(quote datum) stx]
          [(quote-syntax datum) stx]
          [(with-continuation-mark expr1 expr2 expr3)
           (with-syntax ([rewritten-expr1 (add-send-over (rewrite-expr (syntax expr1)) (syntax expr1) 1)]
                         [rewritten-expr2 (add-send-over (rewrite-expr (syntax expr2)) (syntax expr2) 1)]
                         [rewritten-expr3 (rewrite-expr (syntax expr3))])
             (syntax/cert stx (with-continuation-mark rewritten-expr1 rewritten-expr2 rewritten-expr3)))]
          [(#%app expr ...)
           (with-syntax ([(rewritten-expr ...) (map (lambda (x) (add-send-over (rewrite-expr x) x 1)) 
                                                    (syntax->list (syntax (expr ...))))])
             (syntax/cert stx (#%app rewritten-expr ...)))]
          [(#%datum . datum) stx]
          [(#%top . variable) stx]))
      
      (define (add-send-over stx loc-stx values-expected)
        (if (object? (syntax-source loc-stx))
            (with-syntax ([send-over send-over]
                          [stx stx]
                          [loc (datum->syntax-object loc-stx 1 loc-stx)]
                          [(vars ...) (build-vars values-expected)])
              (syntax
               (let-values ([(vars ...) stx])
                 (send-over vars #'loc) ...
                 (values vars ...))))
            stx))
      (define (add-send-over/var stx loc-stx)
        (if (object? (syntax-source loc-stx))
            (with-syntax ([send-over send-over]
                          [stx stx]
                          [loc (datum->syntax-object loc-stx 1 loc-stx)])
              (syntax
               (begin 
                 (send-over stx #'loc)
                 stx)))
            stx))
      (define (build-vars n)
        (cond
          [(zero? n) #'()]
          [else (cons (datum->syntax-object #'here (string->symbol (format "x~a" n)))
                      (build-vars (- n 1)))]))

      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  wire it up
      ;;
      
      (drscheme:get/extend:extend-interactions-text show-picts-mixin)
      (drscheme:get/extend:extend-definitions-text show-picts-mixin)
      (drscheme:get/extend:extend-unit-frame unit-frame-mixin)
      (drscheme:get/extend:extend-tab tab-mixin)
      
      (define (phase1) (void))
      (define (phase2)
        (define slideshow-language%
          (slideshow-mixin
           ((drscheme:language:get-default-mixin)
            (drscheme:language:module-based-language->language-mixin
             (drscheme:language:simple-module-based-language->module-based-language-mixin
              drscheme:language:simple-module-based-language%)))))
        
        (drscheme:language-configuration:add-language
         (new slideshow-language%)))
      
      
      (drscheme:language:add-snip-value
       (lambda (x) ((ss-dynamic-require '(lib "mrpict.ss" "texpict") 'pict?) x))
       (lambda (pict) (new (ss-dynamic-require '(lib "pict-value-snip.ss" "texpict") 'pict-value-snip%) (pict pict))))

        
      (define lib-pict-snipclass (make-object lib-pict-snipclass%))
      (send lib-pict-snipclass set-version 2)
      (send lib-pict-snipclass set-classname (format "~s" '(lib "pict-snipclass.ss" "slideshow")))
      (send (get-the-snip-class-list) add lib-pict-snipclass)

      )))

