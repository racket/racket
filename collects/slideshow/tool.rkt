#|

todo:

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

(module tool racket/base
  (require drscheme/tool
           mred
           mzlib/class
           mzlib/unit
           mzlib/contract
           string-constants
           framework
           texpict/mrpict
           texpict/pict-value-snip
           mzlib/list
           "private/pict-box-lib.rkt"
           "private/image-snipr.rkt")

  (provide tool@
           get-snp/poss
           build-lib-pict-stx)

  (define orig-inspector (variable-reference->module-declaration-inspector
                          (#%variable-reference)))
  (define orig-lcp (current-library-collection-paths))

  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (define original-output-port (current-output-port))
      (define (oprintf . args) (apply fprintf original-output-port args))
      
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
          
          ;; bitmap-table : hash[snip -o> bitmap]
          ;; maps from the true (Scheme) snip to its current bitmap
          (define bitmap-table (make-hasheq))
          
          ;; only for use in the copy method and the read snipclass method
          (define/public (set-bitmap-table bt) (set! bitmap-table bt))
          
          (define/override (make-editor) (make-object pict-pasteboard%))
          (define/override (get-corner-bitmap) slideshow-bm)
          
          (define/override (copy)
            (let* ([cp (make-object pict-snip%)]
                   [ed (send cp get-editor)])
              (send (get-editor) copy-self-to ed)
              (send cp set-show-picts show-picts?)
              ;; initially, share the bitmap table:
              (send cp set-bitmap-table bitmap-table)
              cp))
          
          
          (define/override (get-menu)
            (let ([menu (instantiate popup-menu% () (title #f))])
              (cond
                [show-picts?
                 (make-object checkable-menu-item%
                   sc-hide-picts
                   menu
                   (lambda (x y)
                     (hide-picts)))]
                [(and bitmap-table
                      (positive? (hash-count bitmap-table)))
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
            (let ([hidden-table (make-hasheq)]
                  [position-table (make-hasheq)])
              (let loop ([snip (send (get-editor) find-first-snip)] [pos 0])
                (cond
                  [snip
                   (hash-set! position-table snip pos)
                   (when (is-a? snip image-snip/r%)
                     (hash-set! hidden-table (send snip get-orig-snip) snip))
                   (loop (send snip next) (add1 pos))]
                  [else (void)]))
              (for-each (lambda (snip bitmap) 
                          (hash-set! bitmap-table (hash-ref position-table snip #f) bitmap)
                          (let ([showing (hash-ref hidden-table snip (lambda () #f))])
                            (when showing
                              (send showing set-bitmap bitmap))))
                        sub-snips
                        sub-bitmaps)))
          
          (define/private (show-picts)
            (let ([pb (get-editor)])
              (set! show-picts? #t)
              (send pb begin-edit-sequence)
              
              (set! system-insertion? #t)
              (let ([position-table (make-hasheq)])
                (let loop ([snip (send (get-editor) find-first-snip)] [pos 0])
                  (cond
                   [snip
                    (hash-set! position-table pos snip)
                    (loop (send snip next) (add1 pos))]
                   [else (void)]))
                (hash-for-each 
                 bitmap-table
                 (lambda (pos bitmap)
                   (let ([snip (hash-ref position-table pos #f)])
                     (when snip
                       (let ([bm-snip (make-object image-snip/r% bitmap snip)])
                         (let-values ([(x y) (snip-location pb snip)])
                           (send snip release-from-owner)
                           (send pb insert bm-snip x y))))))))
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
            (send stream-out put 0)
            (send (get-editor) write-to-file stream-out))
          (define/override (make-snip) (new pict-snip%))
          
          (define system-insertion? #f)
          (define/public (inserted-snip)
            (unless system-insertion?
              (set-bitmap-table (make-hasheq))
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
              (send snip set-show-picts show-picts?)
              (let ([bt (make-hasheq)])
                (let loop ([snip (send editor find-first-snip)])
                  (cond
                    [(is-a? snip snip%)
                     (when (is-a? snip image-snip/r%)
                       (let ([orig (send snip get-orig-snip)]
                             [bm (send snip get-bitmap)])
                         (hash-set! bt orig bm)))
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
                     (max 1 (add1 (inexact->exact (ceiling w))))
                     (max 1 (add1 (inexact->exact (ceiling h)))))]
               [bdc (make-object bitmap-dc% bm)])
          (send bdc clear)
          (send bdc set-smoothing 'aligned)
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
      ;;  wire it up
      ;;
      
      (define (phase1) (void))
      (define (phase2) (void))

      (define (pict-box-mixin %)
        (class %
          (inherit get-insert-menu)
          (super-new)
          (add-special-menu-item (get-insert-menu) this)))

      (when (getenv "PLTPICTBOX")
        (drscheme:get/extend:extend-unit-frame pict-box-mixin))
      
      (define orig-namespace (current-namespace))
      
      (define (pict->image-snip p)
        (let* ([pict-width  (dynamic-require 'texpict/mrpict 'pict-width)]
               [pict-height (dynamic-require 'texpict/mrpict 'pict-height)]
               [draw-pict   (dynamic-require 'texpict/mrpict 'draw-pict)]
               [bm (make-object bitmap%
                     (max 1 (add1 (inexact->exact (ceiling (pict-width p)))))
                     (max 1 (add1 (inexact->exact (ceiling (pict-height p))))))]
               [bdc (make-object bitmap-dc% bm)])
          (send bdc clear)
          (send bdc set-smoothing 'aligned)
          (draw-pict p bdc 0 0)
          (send bdc set-bitmap #f)
          (make-object image-snip% bm)))
      
      (drscheme:language:add-snip-value
       ;; Convert to print?
       (lambda (x) 
         ;; if the require fails, then we cannot display the pict.
         ;; this can happen when, for example, there is no mred module
         ;; in the namespace
         (let ([pict? (with-handlers ((exn:fail? (λ (x) #f)))
                        (dynamic-require 'texpict/mrpict 'pict?))])
           (and pict?
                (pict? x))))
       ;; Converter:
       pict->image-snip
       ;; Namespace setup:
       (λ () 
         (with-handlers ((exn:fail? void))
           ;; code running in this thunk cannot fail, or else drscheme gets wedged.
           (dynamic-require 'texpict/mrpict #f))))
        
      (define lib-pict-snipclass (make-object lib-pict-snipclass%))
      (send lib-pict-snipclass set-version 2)
      (send lib-pict-snipclass set-classname (format "~s" '(lib "pict-snipclass.ss" "slideshow")))
      (send (get-the-snip-class-list) add lib-pict-snipclass))))
