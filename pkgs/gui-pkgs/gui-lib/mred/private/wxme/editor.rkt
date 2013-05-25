#lang racket/base
(require racket/class
         (for-syntax racket/base)
         racket/file
         racket/port
         "../syntax.rkt"
         "private.rkt"
         racket/snip/private/snip
         racket/snip/private/private
         racket/snip/private/style
         racket/snip/private/snip-flags
         racket/snip/private/prefs
         "editor-admin.rkt"
         "stream.rkt"
         "undo.rkt"
         "keymap.rkt"
         "editor-data.rkt"
         (only-in "cycle.rkt"
                  printer-dc%
                  text%
                  pasteboard%
                  editor-snip%
                  editor-snip-editor-admin%
                  editor-get-file
                  editor-put-file)
         "wx.rkt")

(provide editor%
         editor<%>
         add-editor-keymap-functions
         ALLOW-X-STYLE-SELECTION?
         copy-style-list
         set-common-copy-region-data!
         cons-common-copy-buffer!
         cons-common-copy-buffer2!
         editor-set-x-selection-mode
         editor-x-selection-allowed
         editor-x-selection-mode?
         editor-x-selection-owner
         detect-wxme-file
         read-editor-version
         read-editor-global-header
         read-editor-global-footer
         write-editor-version
         write-editor-global-header
         write-editor-global-footer
         write-snips-to-file
         get-default-print-size)

;; ----------------------------------------

(define RIDICULOUS-SIZE 2000)
(define ALLOW-X-STYLE-SELECTION? (eq? 'unix (system-type)))

(defclass offscreen% object%
  (define bitmap #f)
  (define dc (make-object bitmap-dc%))
  (define bm-width 0)
  (define bm-height 0)
  (define in-use? #f)
  (define last-used #f)

  (define/public (is-in-use?) in-use?)
  (define/public (set-in-use v) (set! in-use? (and v #t)))
  (define/public (get-bitmap) bitmap)
  (define/public (get-dc) dc)
  (define/public (get-last-used) last-used)
  (define/public (set-last-used v) (set! last-used v))

  (define/public (ready-offscreen width height)
    (if (or #t ; disable on all  platforms
            (width . > . RIDICULOUS-SIZE)
            (height . > . RIDICULOUS-SIZE)
            (eq? (system-type) 'macosx))
        #f
        (if (and (not in-use?)
                 (or (height . > . bm-height)
                     (width . > . bm-width)))
            (let ([oldbm bitmap])
              (set! bm-height (max (add1 (->long height)) bm-height))
              (set! bm-width (max (add1 (->long width)) bm-width))
              (set! bitmap (make-object bitmap% bm-width bm-height))
              (send dc set-bitmap #f)
              (when (send bitmap ok?)
                (send dc set-bitmap bitmap))
              #t)
            #f)))

  (super-new))

(define the-offscreen (new offscreen%))

;; ----------------------------------------

;; 8.5" x 11" paper, 0.5" margin; usually not used
(define PAGE-WIDTH 612)
(define PAGE-HEIGHT 792)

(define (get-printer-orientation)
  (send (current-ps-setup)  get-orientation))

(define (get-default-print-size w h)
  (set-box! w PAGE-WIDTH)
  (set-box! h PAGE-HEIGHT)
  (when (eq? (get-printer-orientation) 'landscape)
    (let ([tmp (unbox h)])
      (set-box! h (unbox w))
      (set-box! w tmp))))

;; ----------------------------------------

(define emacs-style-undo? (and (get-preference* 'GRacket:emacs-undo) #t))
(define (max-undo-value? v) (or (exact-nonnegative-integer? v)
                                (eq? v 'forever)))

(define global-lock (make-semaphore 1))

(defclass editor% object%

  (field [s-offscreen the-offscreen]
         [s-admin #f]
         [s-keymap (new keymap%)]
         [s-own-caret? #f]
         [s-temp-filename? #f]
         [s-user-locked? #f]
         [s-modified? #f]
         [s-noundomode 0])
  (def/public (is-modified?) s-modified?)
  
  (define undomode? #f)
  (define redomode? #f)
  (define interceptmode? #f)
  (define loadoverwritesstyles? #t)

  (field [s-custom-cursor-overrides? #f]
         [s-need-on-display-size? #f])
  (define paste-text-only? #f)

  (define num-parts-modified 0)

  (field [s-caret-snip #f]
         [s-style-list (new style-list%)])
  (define/public (get-focus-snip) s-caret-snip)
  (define/public (get-s-style-list) s-style-list)

  (send s-style-list new-named-style "Standard" (send s-style-list basic-style))
  (define notify-id
    (send s-style-list notify-on-change (lambda (which) (style-has-changed which))))
  
  (field [s-filename #f]) ; last loaded file

  (define max-undos 0)

  (define changes #())
  (define changes-start 0)
  (define changes-end 0)
  (define changes-size 0)

  (define redochanges #())
  (define redochanges-start 0)
  (define redochanges-end 0)
  (define redochanges-size 0)
  
  (define savedchanges #f) ;; for emacs-style undo
  (define intercepted null)

  (field [s-custom-cursor #f]
         [s-inactive-caret-threshold 'show-inactive-caret])

  (define printing #f)
  (define/public (get-printing) printing)

  (define num-extra-headers 0)
  (define seq-lock #f)

  (super-new)

  (define/public (~)
    (send s-style-list forget-notification notify-id)
    (clear-undos))

  (define/public (is-printing?) (and printing #t))

  ;; ----------------------------------------

  (def/public (blink-caret) (void))

  (def/public (size-cache-invalid) (void))
  (def/public (locked-for-read?) #f)
  (def/public (locked-for-write?) #f)
  (def/public (locked-for-flow?) #f)

  (def/public (resized) (void))
  (def/public (recounted) (void))
  (define/public (invalidate-bitmap-cache) (void))
  (def/public (needs-update) (void))
  (def/public (release-snip) (void))

  (def/public (scroll-line-location) (void))
  (def/public (num-scroll-lines) (void))
  (def/public (find-scroll-line) (void))

  ;; ----------------------------------------

  (define/public (on-event event) (void))
  (define/public (on-char event) (void))

  (def/public (on-local-event [mouse-event% event])
    (unless (and s-keymap
                 (or (send s-keymap handle-mouse-event this event)
                     (begin
                       (when (not (send event moving?))
                         (send s-keymap break-sequence))
                       #f)))
      (on-default-event event)))
  
  (def/public (on-local-char [key-event% event])
    (unless (and s-keymap
                 (or (send s-keymap handle-key-event this event)
                     (begin
                       (send s-keymap break-sequence)
                       #f)))
      (on-default-char event)))

  (define/public (on-default-event event) (void))
  (define/public (on-default-char event) (void))
  
  (def/public (on-focus [any? on?]) (void))

  ;; ----------------------------------------

  (def/public (set-admin [(make-or-false editor-admin%) administrator])
    (setting-admin administrator)

    (set! s-admin administrator)
    (when (not s-admin)
      (set! s-own-caret? #f))
    (when s-admin
      (init-new-admin)))

  (def/public (setting-admin [(make-or-false editor-admin%) a]) (void))

  (def/public (init-new-admin) (void))

  (def/public (get-admin) s-admin)

  ;; ----------------------------------------

  (def/public (own-caret [any? ownit?]) (void))

  (def/public (do-own-caret [any? ownit?])
    (let ([ownint? (and ownit? #t)])
      (let ([refresh? (and (not s-caret-snip)
                           (not (eq? s-own-caret? ownit?)))])
        (set! s-own-caret? ownit?)
        (when s-caret-snip
          (send s-caret-snip own-caret ownit?))
        (when (and s-keymap (not ownint?) refresh?)
          (send s-keymap break-sequence))

        (when ALLOW-X-STYLE-SELECTION?
          (cond
           [(and ownit? (not s-caret-snip))
            (set! editor-x-selection-allowed this)]
           [(eq? editor-x-selection-allowed this)
            (set! editor-x-selection-allowed #f)]))

        (when s-admin
          (send s-admin update-cursor))

        refresh?)))

  (def/public (get-dc)
    ;; this can be called by snips to get a DC appropriate for
    ;; sizing text, etc., outside of draws. it isn't the destination 
    ;; for draws, though
    (if s-admin
        (send s-admin get-dc #f #f)
        #f))

  (def/public (get-view-size [(make-or-false box?) w][(make-or-false box?) h])
    (if s-admin
        (send s-admin get-view #f #f w h)
        (begin
          (when w (set-box! w 0.0))
          (when h (set-box! h 0.0)))))

  (define/public (get-snip-location snip x y)
    (when x (set-box! x 0.0))
    (when y (set-box! y 0.0))
    #t)

  (def/public (do-set-caret-owner [(make-or-false snip%) snip] [symbol? dist])
    (let ([same? (eq? snip s-caret-snip)])
      (if (and same?
               (or (not s-admin) (eq? dist 'immediate)))
          #f
          (begin
            (when same?
              (send s-admin grab-caret dist))

            (let ([vis-caret? s-own-caret?])
              (cond
               [(or (not snip)
                    (not (has-flag? (snip->flags snip) HANDLES-EVENTS)))
                
                (let ([old-caret s-caret-snip]
                      [refresh? #f])
                  (set! s-caret-snip #f)
                  (when old-caret
                    (send old-caret own-caret #f)
                    (when vis-caret?
                      (set! refresh? #t)))
                  (when ALLOW-X-STYLE-SELECTION?
                    (set! editor-x-selection-allowed this))
                  (when s-admin
                    (send s-admin update-cursor))
                  refresh?)]
               [(not (get-snip-location snip #f #f)) #f]
               [else
                (let ([had-caret? (and s-own-caret?
                                       (not s-caret-snip))]
                      [old-caret s-caret-snip]
                      [refresh? #f])

                  (set! s-caret-snip snip)

                  (begin-edit-sequence)
                  (cond
                   [old-caret (send old-caret own-caret #f)]
                   [vis-caret? (set! refresh? #t)])
                  (send snip own-caret s-own-caret?)
                  (end-edit-sequence)

                  (when (and s-admin
                             (not (eq? dist 'immediate)))
                    (send s-admin grab-caret dist))

                  (when s-admin
                    (send s-admin update-cursor))

                  refresh?)]))))))

  (define/private (convert-coords admin x y to-local?)
    (let-values ([(lx ly)
                  (if admin
                      (if (admin . is-a? . editor-snip-editor-admin%)
                          (let* ([snip (send admin get-snip)]
                                 [sa (send snip get-admin)])
                            (if sa
                                (let ([mbuf (send sa get-editor)])
                                  (if mbuf 
                                      (let-boxes ([bx 0.0][by 0.0]
                                                  [lx 0.0][ly 0.0]
                                                  [l 0.0][t 0.0][r 0.0][b 0.0])
                                          (begin
                                            (send mbuf local-to-global bx by)
                                            (send mbuf get-snip-location snip lx ly #f)
                                            (send snip get-margin l t r b))
                                        (values (+ lx bx l)
                                                (+ ly by t)))
                                      (values 0.0 0.0)))
                                (values 0.0 0.0)))
                          (let-boxes ([lx 0.0][ly 0.0])
                              (send admin get-dc lx ly)
                            (values (- lx) (- ly))))
                      (values 0.0 0.0))])
      (when x (set-box! x (+ (unbox x) (if to-local? (- lx) lx))))
      (when y (set-box! y (+ (unbox y) (if to-local? (- ly) ly))))))

  (def/public (editor-location-to-dc-location [real? x] [real? y])
    (let-boxes ([x x] [y y])
        (local-to-global x y)
      (values x y)))

  (def/public (dc-location-to-editor-location [real? x] [real? y])
    (let-boxes ([x x] [y y])
        (global-to-local x y)
      (values x y)))

  (def/public (global-to-local [maybe-box? x] [maybe-box? y])
    (convert-coords s-admin x y #t))

  (def/public (local-to-global [maybe-box? x] [maybe-box? y])
    (convert-coords s-admin x y #f))

  (def/public (set-cursor [(make-or-false cursor%) c] [any? [override? #t]])
    (set! s-custom-cursor c)
    (set! s-custom-cursor-overrides? override?)
    (when s-admin
      (send s-admin update-cursor)))

  (def/public (adjust-cursor [mouse-event% event]) (void))

  ;; ----------------------------------------

  (def/public (set-keymap [(make-or-false keymap%) [k #f]])
    (set! s-keymap k))
  (def/public (get-keymap) s-keymap)
  (def/public (get-style-list) s-style-list)

  (def/public (set-style-list [style-list% new-list])
    (send s-style-list forget-notification notify-id)
    (set! notify-id
          (send new-list notify-on-change (lambda (which) (style-has-changed which))))
    (set! s-style-list new-list)
    ;; create "Standard" if it's not there:
    (send s-style-list new-named-style "Standard" (send s-style-list basic-style))
    (void))

  (define/public (style-has-changed which) (void))
  
  (def/public (default-style-name) "Standard")

  (def/public (get-default-style)
    (send s-style-list find-named-style (default-style-name)))

  ;; ----------------------------------------

  (define/public (set-max-width w) (void))
  (define/public (set-min-width v) (void))
  (define/public (get-max-width) 0.0)
  (define/public (get-min-width) 0.0)
  (define/public (set-min-height w) (void))
  (define/public (set-max-height w) (void))
  (define/public (get-min-height) 0.0)
  (define/public (get-max-height) 0.0)

  (define/public (find-first-snip) #f)

  (define/public (get-extent) (void))
  (define/public (get-descent) (void))
  (define/public (get-space) (void))

  (define/public (get-flattened-text) (void))

  ;; ----------------------------------------

  (define/public (clear) (void))
  (define/public (cut ? time) (void))
  (define/public (copy ? time) (void))
  (define/public (paste time) (void))
  (define/public (paste-x-selection time) (void))
  (define/public (kill time) (void))
  (define/public (select-all) (void))
  (define/public (insert snip) (void))
  (define/public (insert-paste-snip snip) (void))
  (define/public (insert-paste-string str) (void))
  (define/public (do-read-insert snip) (void))
  (define/public (set-caret-owner snip focus) (void))
  (define/public (read-from-file mf) #f)

  (def/public (do-edit-operation [symbol? op] [any? [recursive? #t]] [exact-integer? [time 0]])
    (if (and recursive?
             s-caret-snip)
        (send s-caret-snip do-edit-operation op #t time)
        (case op
          [(undo) (undo)]
          [(redo) (redo)]
          [(clear) (clear)]
          [(cut) (cut #f time)]
          [(copy) (copy #f time)]
          [(paste) (paste time)]
          [(kill) (kill time)]
          [(insert-text-box) (insert-box 'text)]
          [(insert-pasteboard-box) (insert-box 'pasteboard)]
          [(insert-image) (insert-image)]
          [(select-all) (select-all)])))

  (def/public (can-do-edit-operation? [symbol? op] [any? [recursive? #t]])
    (if (and recursive?
             s-caret-snip)
        (send s-caret-snip can-do-edit-operation? op #t)
        (cond
         [(and (is-locked?)
               (not (or (eq? op 'copy) (eq? op 'select-all))))
          #f]
         [(and (eq? op 'undo)
               (= changes-start changes-end))
          #f]
         [(and (eq? op 'redo)
               (= redochanges-start redochanges-end))
          #f]
         [else (really-can-edit? op)])))

  (define/public (really-can-edit?) #f)

  (def/public (insert-box [symbol? [type 'text]])
    (let ([snip (on-new-box type)])
      (when snip
        (let ([sname (default-style-name)])

          (begin-edit-sequence)
          (send snip set-s-style (or (send s-style-list find-named-style sname)
                                     (send s-style-list basic-style)))
          (insert snip)
          (set-caret-owner snip)
          (end-edit-sequence)))))

  (def/public (on-new-box [symbol? type])
    (let* ([media (if (eq? type 'text)
                      (new text%)
                      (new pasteboard%))]
           [snip (make-object editor-snip% media)])
      (send media set-keymap s-keymap)  
      (send media set-style-list s-style-list)
      snip))

  (def/public (insert-image [(make-or-false path-string?) [filename #f]]
                            [image-type? [type 'unknown/alpha]]
                            [any? [relative? #f]]
                            [any? [inline-img? #t]])
    (let ([filename (or filename
                        (get-file #f))])
      (when filename
        (let ([snip (on-new-image-snip filename type 
                                       (and relative? #t) 
                                       (and inline-img? #t))])
          (insert snip)))))

  (def/public (on-new-image-snip [path-string? filename]
                                 [image-type? type]
                                 [any? relative?]
                                 [any? inline-img?])
    (make-object image-snip% filename type relative? inline-img?))

  ;; ----------------------------------------

  (def/public (get-snip-data [snip% s]) #f)
  (def/public (set-snip-data [snip% s] [editor-data% v]) (void))
  
  ;; ----------------------------------------

  (def/public (read-header-from-file [editor-stream-in% f] [string? header-name])
    (error 'read-header-from-file "unknown header data: ~s" header-name))
  (def/public (read-footer-from-file [editor-stream-in% f] [string? header-name])
    (error 'read-header-from-file "unknown footer data: ~s" header-name))
  (def/public (write-headers-to-file [editor-stream-out% f]) #t)
  (def/public (write-footers-to-file [editor-stream-out% f]) #t)

  (def/public (begin-write-header-footer-to-file [editor-stream-out% f]
                                                 [string? header-name]
                                                 [box? data-buffer])
    (set-box! data-buffer (send f tell))
    (send f put-fixed 0)
    (send f put-unterminated (string->bytes/utf-8 header-name))
    #t)

  (def/public (end-write-header-footer-to-file [editor-stream-out% f]
                                               [exact-integer? data])
    (let ([end (send f tell)])
      (send f jump-to data)
      (send f put-fixed 0)
      (let ([pos (send f tell)])
        (send f jump-to data)
        (send f put-fixed (- end pos))
        (send f jump-to end)
        (set! num-extra-headers (add1 num-extra-headers))
        #t)))

  (def/public (read-headers-footers [editor-stream-in% f] [any? headers?])
    (let-boxes ([num-headers 0])
        (send f get-fixed num-headers)
      (for/fold ([ok? #t]) ([i (in-range num-headers)] #:when ok?)
        (let-boxes ([len 0])
            (send f get-fixed len)
          (and (send f ok?)
               (if (positive? len)
                   (let ([pos (send f tell)])
                     (send f set-boundary len)
                     (let ([header-name (bytes->string/utf-8 (send f get-unterminated-bytes) #\?)])
                       (and (if headers?
                                (read-header-from-file f header-name)
                                (read-footer-from-file f header-name))
                            (send f ok?)
                            (begin
                              (send f remove-boundary)
                              (let ([len (- len (- (send f tell) pos))])
                                (when (positive? len)
                                  (send f skip len))
                                (send f ok?))))))
                   #t))))))

  (define/public (do-write-headers-footers f headers?)
    (let ([all-start (send f tell)])
      (send f put-fixed 0)
      (set! num-extra-headers 0)

      (and
       (if headers?
           (write-headers-to-file f)
           (write-footers-to-file f))
       (begin
         (when (positive? num-extra-headers)
           (let ([all-end (send f tell)])
             (send f jump-to all-start)
             (send f put-fixed num-extra-headers)
             (send f jump-to all-end))
           #t)))))

  ;; ----------------------------------------

  (def/public (read-snips-from-file [editor-stream-in% f]
                                    [any? overwritestylename?])
    (and (read-headers-footers f #t)
         (let* ([list-id (box 0)]
                [new-list (read-styles-from-file s-style-list f overwritestylename? list-id)])
           (and new-list 
                (begin
                  (unless (eq? new-list s-style-list)
                    (set-style-list new-list))
                  (let-boxes ([num-headers 0])
                      (send f get-fixed num-headers)
                    (and
                     ;; Read headers
                     (for/and ([i (in-range num-headers)])
                       (let ([n (send f get-exact)]
                             [len (send f get-fixed-exact)])
                         (and (send f ok?)
                              (or (zero? len)
                                  (let ([sclass (send (send f get-s-scl) find-by-map-position f n)])
                                    (and
                                     (if sclass
                                         (let ([start (send f tell)])
                                           (send f set-boundary len)
                                           (and (send sclass read-header f)
                                                (send f ok?)
                                                (begin
                                                  (send f do-set-header-flag sclass)
                                                  (let ([rcount (- (send f tell) start)])
                                                    (when (rcount . < . len)
                                                      (error 'read-snips-from-file "underread (caused by file corruption?)"))
                                                    (send f skip (- len rcount)))
                                                  (send f remove-boundary)
                                                  #t)))
                                         (begin (send f skip len) #t))
                                     (send f ok?)))))))
                     ;; Read snips
                     (let-boxes ([num-snips 0])
                         (send f get num-snips)
                       (let ([accum? (this . is-a? . text%)])
                         (let ([accum
                                (for/fold ([accum null]) ([i (in-range num-snips)] #:when accum)
                                  (let ([n (send f get-exact)])
                                    (let ([sclass (if (n . >= . 0)
                                                      (send (send f get-s-scl) find-by-map-position f n)
                                                      #f)]) ; -1 => unknown
                                      (let ([len (if (or (not sclass)
                                                         (not (send sclass get-s-required?)))
                                                     (send f get-fixed-exact)
                                                     -1)])
                                        (and (send f ok?)
                                             (or (and (zero? len) accum)
                                                 (and
                                                  (if sclass
                                                      (let ([start (send f tell)])
                                                        (when (len . >= . 0)
                                                          (send f set-boundary len))
                                                        (let ([style-index (send f get-exact)])
                                                          (let ([snip (send sclass read f)])
                                                            (and 
                                                             snip
                                                             (begin
                                                               (when (has-flag? (snip->flags snip) OWNED)
                                                                 (send snip set-s-flags (remove-flag (snip->flags snip) OWNED)))
                                                               (send snip set-s-style
                                                                     (or
                                                                      (send s-style-list map-index-to-style f style-index (unbox list-id))
                                                                      (send s-style-list basic-style)))
                                                               (let* ([zero-length? (zero? (snip->count snip))]
                                                                      [accum
                                                                       (if zero-length?
                                                                           ;; A 0-length snip is a bug in the input, but 
                                                                           ;;  we continue anyway to recover from bad
                                                                           ;;  files generated by version 4.2.
                                                                           accum
                                                                           (if accum?
                                                                               (cons snip accum)
                                                                               (do-read-insert snip)))])
                                                                 (and
                                                                  accum
                                                                  (let ([data (read-buffer-data f)])
                                                                    (and
                                                                     (send f ok?)
                                                                     (let ([accum
                                                                            (if zero-length?
                                                                                accum
                                                                                (if accum?
                                                                                    (cons (cons (car accum) data) (cdr accum))
                                                                                    (when data
                                                                                      (set-snip-data snip data))))])
                                                                       (and
                                                                        accum
                                                                        (begin
                                                                          (when (len . >= . 0)
                                                                            (let ([rcount (- (send f tell) start)])
                                                                              (when (rcount . < . len)
                                                                                (error 'read-snips-from-file
                                                                                       "underread (caused by file corruption?)"))
                                                                              (send f skip (- len rcount))
                                                                              (send f remove-boundary)))
                                                                          accum))))))))))))
                                                      (begin
                                                        (send f skip len)
                                                        (and (send f ok?)
                                                             accum))))))))))])
                           (and accum
                                (begin
                                  (when accum?
                                    (let ([accum (reverse accum)])
                                      (send this do-read-insert (map car accum))
                                      (for ([p (in-list accum)])
                                        (when (cdr p)
                                          (set-snip-data (car p) (cdr p))))))
                                  
                                  (read-headers-footers f #f)))))))))))))

  ;; ----------------------------------------

  (define/public (insert-port) (void))
  (define/public (insert-file) (void))
  (define/public (save-port) (void))
  (define/public (load-file) (void))
  (define/public (set-filename) (void))
  (define/public (write-to-file) (void))

  (def/public (get-filename [(make-or-false box?) [temp #f]])
    (when temp (set-box! temp s-temp-filename?))
    s-filename)

  (define/private (extract-parent)
    (and s-admin
         ((send s-admin get-s-standard) . > . 0)
         (let ([w (send s-admin do-get-canvas)])
           (send w get-top-level))))

  (define/public (do-begin-print) (void))
  (define/public (print-to-dc) (void))
  (define/public (do-end-print) (void))
  (define/public (do-has-print-page?) (void))

  (define/private (run-printout
                   parent
                   interactive? ; currently ignored
                   fit-to-page? ; ignored
                   begin-doc-proc
                   has-page?-proc
                   print-page-proc
                   end-doc-proc)
    (let ([dc (make-object printer-dc% parent)])
      (send dc start-doc "printing")
      (begin-doc-proc dc)
      (let loop ([i 1])
        (when (has-page?-proc dc i)
          (begin
            (send dc start-page)
            (print-page-proc dc i)
            (send dc end-page)
            (loop (add1 i)))))
      (end-doc-proc)
      (send dc end-doc)))

  (def/public (print [bool? [interactive? #t]]
                     [bool? [fit-to-page? #t]]
                     [(symbol-in standard postscript pdf) [output-mode 'standard]]
                     [any? [parent #f]] ; checked in ../editor.rkt
                     [bool? [force-page-bbox? #t]]
                     [bool? [as-eps? #f]])
    (let ([ps? (or (eq? output-mode 'postscript)
                   (eq? output-mode 'pdf))]
          [parent (or parent
                      (extract-parent))])
      (cond
       [ps?
        (let* ([ps-dc% (if (eq? output-mode 'postscript) post-script-dc% pdf-dc%)]
               [dc (if as-eps?
                       ;; just for size:
                       (new ps-dc% [interactive #f] [output (open-output-nowhere)])
                       ;; actual target:
                       (make-object ps-dc% interactive? parent force-page-bbox? #f))])
          (when (send dc ok?)
            (send dc start-doc "printing buffer")
            (set! printing dc)
            (let ([data (do-begin-print dc fit-to-page?)])
              (let ([new-dc 
                     (if as-eps?
                         ;; now that we know the size, create the actual target:
                         (let ([w (box 0)]
                               [h (box 0)]
                               [sx (box 0)]
                               [sy (box 0)])
                           (get-extent w h)
                           (send (current-ps-setup) get-scaling sx sy)
                           (let ([dc (make-object ps-dc% interactive? parent force-page-bbox?
                                                  #t 
                                                  (* (unbox w) (unbox sx))
                                                  (* (unbox h) (unbox sy)))])
                             (and (send dc ok?)
                                  (send dc start-doc "printing buffer")
                                  (set! printing dc)
                                  dc)))
                         dc)])
                (when new-dc
                  (print-to-dc new-dc (if as-eps? 0 -1))
                  (when as-eps?
                    (send new-dc end-doc)))
                (set! printing #f)
                (do-end-print dc data)
                (send dc end-doc)
                (invalidate-bitmap-cache 0.0 0.0 'end 'end)))))]
       [else
        (let ([data #f])
          (run-printout ;; from wx
           parent
           interactive?
           fit-to-page?
           ;; begin-doc:
           (lambda (dc)
             (set! printing dc)
             (set! data (do-begin-print printing fit-to-page?)))
           ;; has page?:
           (lambda (dc n) (do-has-print-page? dc n))
           ;; print-page:
           (lambda (dc n) (print-to-dc dc n))
           ;; end-doc
           (lambda ()
             (let ([pr printing])
               (set! printing #f)
               (do-end-print printing data))
             (invalidate-bitmap-cache 0.0 0.0 'end 'end))))])))

  (def/public (undo)
    (when (and (not undomode?)
               (not redomode?))
      (set! undomode? #t)
      (perform-undos #f)
      (set! undomode? #f)))

  (def/public (redo)
    (when (and (not undomode?)
               (not redomode?))
      (set! redomode? #t)
      (perform-undos #t)
      (set! redomode? #f)))

  (define/private (do-clear-undos changes start end size)
    (let loop ([i start])
      (unless (= i end)
        (send (vector-ref changes i) cancel)
        (vector-set! changes i #f)
        (loop (modulo (+ i 1) size)))))

  (define/public (add-undo-rec rec)
    (cond
     [interceptmode?
      (send intercepted append rec)]
     [undomode?
      (append-undo rec #t)]
     [(zero? s-noundomode)
      (when (not redomode?)
        (cond
         [emacs-style-undo?
          (when (not (= redochanges-start redochanges-end))
            (let loop ([e redochanges-end])
              (unless (= redochanges-start e)
                (let ([e (modulo (+ e -1 redochanges-size) redochanges-size)])
                  (append-undo (send (vector-ref redochanges e) inverse) #f)
                  (loop e))))
            (let loop ()
              (unless (= redochanges-start redochanges-end)
                (append-undo (vector-ref redochanges redochanges-start) #f)
                (vector-set! redochanges redochanges-start #f)
                (set! redochanges-start (modulo (add1 redochanges-start) redochanges-size))))
            (set! redochanges-start 0)
            (set! redochanges-end 0))]
         [else
          (do-clear-undos redochanges redochanges-start redochanges-end redochanges-size)
          (set! redochanges-start 0)
          (set! redochanges-end 0)]))
      (append-undo rec #f)]
     [else (send rec cancel)]))

  (def/public (add-undo [(make-procedure 0) proc])
    (add-undo-rec (new proc-record% [proc proc])))

  (define/private (append-undo rec redos?)
    (if (or (eq? max-undos 'forever) (positive? max-undos))
        (let-values ([(start end size c) (get-undos redos?)])
          (let-values ([(size c) (if (zero? size)
                                     (let ([size (min 128 (if (eq? max-undos 'forever) 128 max-undos))])
                                       (values size
                                               (make-vector size #f)))
                                     (values size c))])
            (vector-set! c end rec)
            (let ([end (modulo (add1 end) size)])
              (let-values ([(start end size c)
                            (if (= end start)
                                (if (or (eq? max-undos 'forever)
                                        (size . < . max-undos)
                                        emacs-style-undo?)
                                    ;; make more room
                                    (let* ([s (min (* size 2) (if (eq? max-undos 'forever) (* size 2) max-undos))]
                                           [naya (make-vector s #f)])
                                      (for ([j (in-range size)])
                                        (vector-set! naya j (vector-ref c (modulo (+ start j) size))))
                                      (values 0 size s naya))
                                    ;; no room to grow, so drop an undo record
                                    (begin
                                      (send (vector-ref c start) cancel)
                                      (vector-set! c start #f)
                                      (values (modulo (add1 start) size)
                                              end
                                              size
                                              c)))
                                (values start end size c))])
                (put-undos-back redos? start end size c)))))
        (send rec cancel)))

  (define/private (get-undos redos?)
    (if redos?
        (values redochanges-start redochanges-end redochanges-size redochanges)
        (values changes-start changes-end changes-size changes)))

  (define/private (put-undos-back redos? start end size c)
    (if redos?
        (begin
          (set! redochanges-start start)
          (set! redochanges-end end)
          (set! redochanges-size size)
          (set! redochanges c))
        (begin
          (set! changes-start start)
          (set! changes-end end)
          (set! changes-size size)
          (set! changes c))))

  (def/public (begin-edit-sequence) (void))
  (def/public (end-edit-sequence) (void))
  (def/public (in-edit-sequence?) #f)
  (def/public (refresh-delayed?) #f)
  (def/public (locations-computed?) #f)

  (define/private (perform-undos redos?)
    (let ([id #f] [parity #f])
      (let-values ([(start end size c) (get-undos redos?)])
        (begin-edit-sequence)
        (let loop ([end end])
          (unless (= start end)
            (let ([end (modulo (+ end -1 size) size)])
              (let ([rec (vector-ref c end)])
                (vector-set! c end #f)
                (put-undos-back redos? start end size c)
                (when emacs-style-undo?
                  (set! id (send rec get-id))
                  (set! parity (send rec get-parity)))
                (when (send rec undo this)
                  (loop end))))))
        (end-edit-sequence)
        (when (and emacs-style-undo?
                   (not redos?))
          ;; combine all new steps into one undo record, and
          ;; set/generate id
          (let-values ([(start end size c) (get-undos #t)])
            (unless (= start end)
              (let ([cnt (let loop ([e end][cnt 0])
                           (if (= start e)
                               cnt
                               (let ([e (modulo (+ e -1 size) size)])
                                 (if (send (vector-ref c e) is-composite?)
                                     cnt
                                     (loop e (add1 cnt))))))])
                (when (positive? cnt)
                  (let ([cu (new composite-record% [count cnt] [id id] [parity? (not parity)])])
                    (for ([i (in-range cnt)])
                      (let ([e (modulo (+ (- end cnt) i size) size)])
                        (send cu add-undo i (vector-ref c e))
                        (vector-set! c e #f)))
                    (let ([e (modulo (+ (- end cnt) size) size)])
                      (vector-set! c e cu)
                      (set! redochanges-end (modulo (add1 e) size))))))))))))

  (define/public (perform-undo-list changes)
    (begin-edit-sequence)
    (let loop ([changes changes])
      (unless (null? changes)
        (when (send (car changes) undo this)
          (loop (cdr changes)))))
    (end-edit-sequence))

  (define/public (clear-undos)
    (do-clear-undos changes changes-start changes-end changes-size)
    (set! changes-start 0)
    (set! changes-end 0)
    (do-clear-undos redochanges redochanges-start redochanges-end redochanges-size)
    (set! redochanges-start 0)
    (set! redochanges-end 0))

  (def/public (set-max-undo-history [max-undo-value? v])
    (unless (or undomode?
                redomode?
                (eq? v max-undos))
      (when (equal? 0 v)
        (clear-undos)
        (set! changes #f)
        (set! redochanges #f)
        (set! changes-size 0)
        (set! redochanges-size 0))
      ;;  should we bother downsizing if max-undos gets smaller but stays
      ;; non-0?
      (set! max-undos v)))

  (def/public (get-max-undo-history) max-undos)

  (def/public (s-start-intercept)
    (set! interceptmode? #t)
    (set! intercepted null))
  
  (def/public (s-end-intercept)
    (begin0
     intercepted
     (set! interceptmode? #f)
     (set! intercepted null)))
  
  ;; ----------------------------------------

  ;; see top-level functions below, at "copy ring"

  (define/public (copy-ring-next)
    (vector-set! copy-ring-buffer1 copy-ring-pos common-copy-buffer)
    (vector-set! copy-ring-buffer2 copy-ring-pos common-copy-buffer2)
    (vector-set! copy-ring-data copy-ring-pos common-copy-region-data)
    (vector-set! copy-ring-style copy-ring-pos copy-style-list)

    (set! copy-ring-pos (sub1 copy-ring-pos))
    (when (copy-ring-pos . < . 0)
      (set! copy-ring-pos (sub1 copy-ring-max)))

    (set! common-copy-buffer (vector-ref copy-ring-buffer1 copy-ring-pos))
    (set! common-copy-buffer2 (vector-ref copy-ring-buffer2 copy-ring-pos))
    (set! common-copy-region-data (vector-ref copy-ring-data copy-ring-pos))
    (set! copy-style-list (vector-ref copy-ring-style copy-ring-pos)))

  (define/public (begin-copy-buffer)
    (set! copy-depth (add1 copy-depth)))
  (define/public (end-copy-buffer)
    (set! copy-depth (sub1 copy-depth)))

  (define/public (free-old-copies)
    (when copy-style-list
      (if (copy-depth . > . 1)
          ;; delete current "ring" occupant:
          (begin
            (set! common-copy-buffer null)
            (set! common-copy-buffer2 null)
            (set! common-copy-region-data #f)
            (set! copy-style-list #f))

          (begin
            (vector-set! copy-ring-buffer1 copy-ring-pos common-copy-buffer)
            (vector-set! copy-ring-buffer2 copy-ring-pos common-copy-buffer2)
            (vector-set! copy-ring-data copy-ring-pos common-copy-region-data)
            (vector-set! copy-ring-style copy-ring-pos copy-style-list)
            
            (when (copy-ring-max . > . copy-ring-dest)
              ;; no more space: delete current ring occupant:
              (vector-set! copy-ring-buffer1 copy-ring-dest #f)
              (vector-set! copy-ring-buffer2 copy-ring-dest #f)
              (vector-set! copy-ring-data copy-ring-dest #f))

            (set! common-copy-buffer null)
            (set! common-copy-buffer2 null)
            (set! common-copy-region-data #f)
            (set! copy-style-list #f)
            (set! copy-ring-pos copy-ring-dest)

            (set! copy-ring-dest (add1 copy-ring-dest))
            (when (copy-ring-max . < . copy-ring-dest)
              (set! copy-ring-max copy-ring-dest))
            (when (copy-ring-dest . >= . copy-ring-size)
              (set! copy-ring-dest 0))))))
  
  (define/public (install-copy-buffer time sl)
    (set! copy-style-list sl)

    (when (not (= copying-self copy-depth))
      (when (or (not ALLOW-X-STYLE-SELECTION?)
                (not x-clipboard-hack?))
        (send the-clipboard set-clipboard-client the-editor-clipboard-client time))))

  (define/public (do-buffer-paste cb time local?)
    ;; cut and paste to ourself? (same eventspace?)
    (if (or local?
            (and (not paste-text-only?)
                 (send cb same-clipboard-client? the-editor-clipboard-client)
                 (send the-editor-clipboard-client same-eventspace? (current-eventspace))))
        ;; local direct copy:
        (begin
          (set! copy-depth (add1 copy-depth))
          (map (lambda (snip bd)
                 (insert-paste-snip (send snip copy) bd))
               (reverse common-copy-buffer)
               (reverse common-copy-buffer2))
          (set! copy-depth (sub1 copy-depth))
          (when (and common-copy-region-data
                     (this . is-a? . text%))
            (send this paste-region-data common-copy-region-data)))
        ;; general paste:
        (or
         (and (not paste-text-only?)
              (let ([str (send cb get-clipboard-data "WXME" time)])
                (and str
                     (let* ([b (make-object editor-stream-in-bytes-base% str)]
                            [mf (make-object editor-stream-in% b)])
                       (and (read-editor-version mf b #t #f)
                            (begin
                              (when (read-editor-global-header mf)
                                (when (send mf ok?)
                                  (when (read-from-file mf)
                                    (let ([data (read-buffer-data mf)])
                                      (and data
                                           (this . is-a? . text%)
                                           (send this paste-region-data data))))))
                              (read-editor-global-footer mf)
                              #t))))))
         (and (not paste-text-only?)
              (let ([bm (send cb get-clipboard-bitmap time)])
                (and bm
                     (begin
                       (insert-paste-snip (make-object image-snip% bm) #f)
                       #t))))
         (let ([str (send cb get-clipboard-string time)])
           ;; no data => empty string
           (insert-paste-string str)))))

  (def/public (copy-self) (void))

  (def/public (copy-self-to [editor<%> m])
    ;; copy style list
    (send (send m get-s-style-list) copy s-style-list)
    ;; copy all the snips:
    (let ([save-buffer common-copy-buffer]
          [save-buffer2 common-copy-buffer2]
          [save-styles copy-style-list]
          [save-data common-copy-region-data]
          [save-cs copying-self])
      
      (send m begin-edit-sequence)

      (set! common-copy-buffer null)
      (set! common-copy-buffer2 null)
      (set! copy-style-list #f)
      (set! common-copy-region-data #f)
      (set! copying-self (add1 copy-depth))
      
      (cond
       [(this . is-a? . text%)
        (send this copy #t 0 0 (send this last-position))]
       [(this . is-a? . pasteboard%)
        (begin-edit-sequence)
        (let ([unselect
               (let loop ([s (send this find-first-snip)])
                 (if s
                     (if (send this is-selected? s)
                         (loop (snip->next s))
                         (begin
                           (send this add-selected s)
                           (cons s (loop (snip->next s)))))
                     null))])
          (send this copy #t 0)
          (for-each (lambda (s)
                      (send this remove-selected s))
                    unselect))
        (end-edit-sequence)])

      (let ([copy-snips (reverse common-copy-buffer)]
            [copy-snips2 (reverse common-copy-buffer2)])
      
        (set! common-copy-buffer save-buffer)
        (set! common-copy-buffer2 save-buffer2)
        (set! copy-style-list save-styles)
        (set! common-copy-region-data save-data)
        (set! copying-self save-cs)

        (when (this . is-a? . text%)
          (send m do-insert-snips copy-snips 0))

        (for-each (lambda (s bfd)
                    (unless (this . is-a? . text%)
                      (send m insert s #f))
                    (when bfd
                      (send m set-snip-data s bfd)))
                  copy-snips
                  copy-snips2)

        (send m size-cache-invalid)

        (send m set-min-width (get-min-width))
        (send m set-max-width (get-max-width))
        (send m set-min-height (get-min-height))
        (send m set-max-height (get-max-height))

        (let-boxes ([temp? (box #f)]
                    [f (box #f)])
            (set-box! f (get-filename temp?))
          (send m set-filename f temp?))

        (send m set-max-undo-history (get-max-undo-history))
        
        (send m set-keymap (get-keymap))

        (send m set-inactive-caret-threshold (get-inactive-caret-threshold))
        (send m set-load-overwrites-styles (get-load-overwrites-styles))

        (send m end-edit-sequence))))
  
  ;; ----------------------------------------

  (define/public (own-x-selection) (void))

  (define/public (do-own-x-selection on? force?)
    (if on?
        (if (and (not force?)
                 (not (eq? editor-x-selection-allowed this)))
            #f
            (begin
              (when editor-x-selection-owner
                (send editor-x-selection-owner own-x-selection #f #t #f)
                (set! editor-x-selection-owner #f))
              (set! x-selection-copied? #f)
              (send the-x-selection-clipboard set-clipboard-client the-editor-x-clipboard-client 0)
              (set! editor-x-selection-owner this)
              #t))
        (begin
          (when (eq? this editor-x-selection-owner)
            (set! editor-x-selection-owner #f)
            (when (and (not x-selection-copied?)
                       (send the-x-selection-clipboard same-clipboard-client?
                             the-editor-x-clipboard-client))
              (send the-x-selection-clipboard set-clipboard-string "" 0)))
          #t)))

  (define/public (copy-out-x-selection)
    (when (eq? this editor-x-selection-owner)
      (copy-into-selection)
      (set! x-selection-copied? #t)))

  (def/public (get-paste-text-only)
    paste-text-only?)

  (def/public (set-paste-text-only [any? pto?])
    (set! paste-text-only? (and pto? #t)))

  ;; ----------------------------------------

  (def/public (lock [any? lock?])
    (set! s-user-locked? (and lock? #t)))

  (def/public (is-locked?) s-user-locked?)

  (def/public (modified?) s-modified?)

  (def/public (set-modified [any? mod?])
    (let ([mod? (and mod? #t)])
      (unless (eq? mod? s-modified?)
        (set! s-modified? mod?)
        (when mod?
          (set! num-parts-modified 1))

        (when (and (not mod?)
                   (not undomode?))
          ;; get rid of undos that reset the modification state
          (set! num-parts-modified 0)
          (let loop ([i changes-end])
            (unless (= i changes-start)
              (let ([i (modulo (+ i -1 changes-size) changes-size)])
                (send (vector-ref changes i) drop-set-unmodified)
                (loop i))))
          (let loop ([i redochanges-end])
            (unless (= i redochanges-start)
              (let ([i (modulo (+ i -1 redochanges-size) redochanges-size)])
                (send (vector-ref redochanges i) drop-set-unmodified)
                (loop i)))))

        (when s-admin
          (send s-admin modified s-modified?))
        
        (when (and (not mod?) (not undomode?))
          ;; tell all snips that they should now consider themselves unmodified:
          (let loop ([snip (find-first-snip)])
            (when snip
              (send snip set-unmodified)
              (loop (snip->next snip))))))))

  (def/public (on-snip-modified [snip% s] [any? mod?])
    (if (not mod?)
        (when (= num-parts-modified 1)
          (set! num-parts-modified 0)
          (when s-modified?
            (set-modified #f)))
        (if s-modified?
            (set! num-parts-modified (add1 num-parts-modified))
            (set-modified #t))))

  (def/public (get-inactive-caret-threshold)
    s-inactive-caret-threshold)

  (def/public (set-inactive-caret-threshold [(symbol-in no-caret show-inactive-caret show-caret) v])
    (set! s-inactive-caret-threshold v))

  (define/public (scroll-editor-to localx localy w h refresh? bias)
    (if s-admin
        (send s-admin scroll-to localx localy w h refresh? bias)
        #f))

  (def/public (refresh [real? left] [real? top] [nonnegative-real? width] [nonnegative-real? height]
                       [caret-status? show-caret]
                       [(make-or-false color%) bg-color])
    (void))

  (def/public (on-paint [any? pre?] [dc<%> dc]
                        [real? l] [real? t] [real? r] [real? b]
                        [real? dx] [real? dy]
                        [caret-status? show-caret])
    (void))

  (def/public (can-save-file? [path-string? filename]
                              [symbol? format])
    #t)

  (def/public (on-save-file [path-string? filename]
                            [symbol? format])
    (void))

  (def/public (after-save-file [any? ok?])
    (void))

  (def/public (can-load-file? [path-string? filename]
                              [symbol? format])
    #t)

  (def/public (on-load-file [path-string? filename]
                            [symbol? format])
    (void))

  (def/public (after-load-file [any? ok?])
    (void))

  (def/public (on-edit-sequence) (void))

  (def/public (after-edit-sequence) (void))

  (def/public (on-display-size) (void))

  (def/public (on-change) (void))

  (def/public (on-display-size-when-ready)
    (cond
     [(in-edit-sequence?)
      (set! s-need-on-display-size? #t)]
     [(or (not seq-lock)
          (semaphore-try-wait? seq-lock))
      (when seq-lock
        (semaphore-post seq-lock))
      (on-display-size)]
     [else (set! s-need-on-display-size? #t)]))

  (def/public (begin-sequence-lock)
    (call-with-semaphore
     global-lock
     (lambda ()
       (unless seq-lock
         (set! seq-lock (make-semaphore 1)))))
    
    ;; "Try" really should succeed, because multiple refreshes are
    ;; prevented through other flags. Still, we don't want to block if
    ;; someone previously escaped from a repaint.
    (void (semaphore-try-wait? seq-lock)))

  (def/public (end-sequence-lock)
    (semaphore-post seq-lock))

  (def/public (wait-sequence-lock)
    (cond
     [seq-lock
      (sync seq-lock)
      (lambda ()
        (semaphore-post seq-lock))]
     [else void]))

  (def/public (get-file [(make-or-false path-string?) path])
    (editor-get-file "choose a file" (extract-parent) #f path))

  (def/public (put-file [(make-or-false path-string?) dir]
                        [(make-or-false path-string?) suggested-name])
    (editor-put-file "save file as" (extract-parent) dir suggested-name))
  
  (def/public (set-load-overwrites-styles [any? b?])
    (set! loadoverwritesstyles? (and b? #t)))

  (def/public (get-load-overwrites-styles) loadoverwritesstyles?))

(define editor<%> (class->interface editor%))

;; ------------------------------------------------------------

(define/top (add-editor-keymap-functions [keymap% tab])
  (let ([add (lambda (n f)
               (send tab add-function n f))])
    (add "copy-clipboard" (lambda (e event) (send e copy #f (send event get-time-stamp))))
    (add "copy-append-clipboard" (lambda (e event) (send e copy #t (send event get-time-stamp))))
    (add "paste-clipboard" (lambda (e event) (send e paste (send event get-time-stamp))))
    (add "paste-x-selection" (lambda (e event) (send e paste-x-selection (send event get-time-stamp))))
    (add "cut-clipboard"  (lambda (e event) (send e cut #f (send event get-time-stamp))))
    (add "cut-append-clipboard" (lambda (e event) (send e cut #t (send event get-time-stamp))))
    (add "delete-to-end-of-line" (lambda (e event) (send e kill (send event get-time-stamp))))
    (add "undo" (lambda (e event) (send e undo)))
    (add "redo" (lambda (e event) (send e redo)))
    (add "delete-selection" (lambda (e event) (send e clear)))
    (add "clear-selection" (lambda (e event) (send e clear)))
    (add "select-all" (lambda (e event) (send e select-all)))))

;; ------------------------------------------------------------

(define (write-buffer-data f data)
  (let loop ([data data])
    (if data
        (let ([mp (send f do-map-position (send data get-s-dataclass))])
          (send f put mp)
          (let ([req? (send (send data get-s-dataclass) get-s-required?)])
            (let-values ([(data-start data-pos)
                          (if req?
                              (values #f #f)
                              (values (send f tell)
                                      (begin
                                        (send f put-fixed 0)
                                        (send f tell))))])
              (if (not (send data write f))
                  #f
                  (begin
                    (unless req?
                      (let ([data-end (send f tell)])
                        (send f jump-to data-start)
                        (send f put-fixed (- data-end data-pos))
                        (send f jump-to data-end)))
                    (loop (send data get-s-next)))))))
        (begin
          (send f put 0)
          #t))))

(define (write-snips-to-file f style-list snip-list
                             start-snip end-snip
                             extra-data buffer)
  (and 
   (write-styles-to-file style-list f)
   (let ([all-start (send f tell)])
     (send f put-fixed 0)

     (let ([snip-list
            (if snip-list
                (reverse snip-list)
                (let loop ([snip start-snip])
                  (if (and snip
                           (not (eq? snip end-snip)))
                      (cons snip (loop (snip->next snip)))
                      null)))])

       (let ([num-headers
              (let loop ([num-headers 0]
                         [snips snip-list])
                (if (null? snips)
                    num-headers
                    (let ([snip (car snips)])
                      (let ([sclass (snip->snipclass snip)])
                        (unless sclass
                          (error 'write-snips-to-file "snip has no snipclass"))
                        (if (not (send f do-get-header-flag sclass))
                            (begin
                              (send f put (send f do-map-position sclass))
                              (let ([header-start (send f tell)])
                                (send f put-fixed 0)
                                (let ([header-pos (send f tell)])
                                  (if (not (send sclass write-header f))
                                      #f
                                      (begin
                                        (send f do-set-header-flag sclass)
                                        (let ([header-end (send f tell)])
                                          (send f jump-to header-start)
                                          (send f put-fixed (- header-end header-pos))
                                          (send f jump-to header-end)
                                          (if (send f ok?)
                                              (loop (add1 num-headers)
                                                    (cdr snips))
                                              #f)))))))
                            (loop num-headers (cdr snips)))))))])

         (and
          num-headers
          (let ([all-end (send f tell)])
            (send f jump-to all-start)
            (send f put-fixed num-headers)
            (send f jump-to all-end)

            (send f put (length snip-list))
            
            (andmap
             (lambda (snip data)
               (let ([sclass (snip->snipclass snip)])
                 (if sclass
                     (send f put (send f do-map-position sclass))
                     (send f put -1))
                 (let-values ([(snip-start snip-pos)
                               (if (or (not sclass)
                                       (not (send sclass get-s-required?)))
                                   (values (send f tell)
                                           (begin
                                             (send f put-fixed 0)
                                             (send f tell)))
                                   (values #f #f))])
                   (let ([style-index (send style-list style-to-index (snip->style snip))])
                     (when (not style-index)
                       (error 'write-snips-to-file "bad style discovered"))
                     (send f put style-index))
                   (send snip write f)
                   (and (write-buffer-data f data)
                        (begin
                          (when snip-start
                            (let ([snip-end (send f tell)])
                              (send f jump-to snip-start)
                              (send f put-fixed (- snip-end snip-pos))
                              (send f jump-to snip-end)))
                          (send f ok?))))))
             snip-list
             (if extra-data
                 (reverse extra-data)
                 (map (lambda (snip)
                        (send buffer get-snip-data snip))
                      snip-list))))))))))

;; ------------------------------------------------------------

;; Copy and the copy ring: the current clipboard content is stored in
;; common-copy-buffer, etc. to implement the copy ring, then when a
;; copy is started, we moved the wxmb_common-copy-buffer, etc. values
;; into a copy ring. yanking from the ring swaps the values in
;;  wxmb_common-copy-buffer, etc.  and the ring values and adjust the
;; pointer into the ring.

(define copy-depth 0)

(define copy-ring-size 30)
(define copy-ring-pos 0)
(define copy-ring-max 1)
(define copy-ring-dest 1)

(define copy-ring-buffer1 (make-vector copy-ring-size #f))
(define copy-ring-buffer2 (make-vector copy-ring-size #f))

(define copy-ring-style (make-vector copy-ring-size #f))
(define copy-ring-data (make-vector copy-ring-size #f))

(define common-copy-buffer null)
(define common-copy-buffer2 null)
(define copy-style-list #f)
(define common-copy-region-data #f)

(define selection-copy-buffer #f)
(define selection-copy-buffer2 #f)
(define selection-copy-style-list #f)
(define selection-copy-region-data #f)

(define (set-common-copy-region-data! v) (set! common-copy-region-data v))
(define (cons-common-copy-buffer! v) (set! common-copy-buffer (cons v common-copy-buffer)))
(define (cons-common-copy-buffer2! v) (set! common-copy-buffer2 (cons v common-copy-buffer2)))

(define copying-self 0)

(define editor-x-selection-mode? ALLOW-X-STYLE-SELECTION?)
(define editor-x-selection-owner #f)
(define editor-x-selection-allowed #f)
(define x-selection-copied? #f)
(define x-clipboard-hack? #f)

(define (generic-get-data fformat copy-buffer copy-buffer2 copy-styles copy-region-data)
  (cond
   [(equal? fformat "TEXT")
    (string->bytes/utf-8
     (let ([out (open-output-string)])
       (for-each (lambda (snip)
                   (let ([s (send snip get-text 0 (snip->count snip) #t)])
                     (display s out)))
                 (reverse copy-buffer))
       (let ([s (get-output-string out)])
         (cond
          [(eq? 'macosx (system-type))
           ;; change newline to return
           (regexp-replace* #rx"\r" s "\n")]
          [(eq? 'windows (system-type))
           ;; change newline to return-newline:
           (regexp-replace* #rx"\n" s "\r\n")]
          [else s]))))]
   [(equal? fformat "WXME")
    (let* ([b (make-object editor-stream-out-bytes-base%)]
           [mf (make-object editor-stream-out% b)])
      (write-editor-version mf b)
      (write-editor-global-header mf)
      (and (send mf ok?)
           (begin
            (send mf put-fixed 0)
            (and (write-snips-to-file mf copy-styles copy-buffer #f #f copy-buffer2 #f)
                 (begin
                   (send mf put-fixed 0)
                   (write-buffer-data mf copy-region-data))))
           (write-editor-global-footer mf)
           (send b get-bytes)))]
   [else #""]))

(defclass editor-clipboard-client% clipboard-client%
  (inherit add-type)
  (super-new)
  (add-type "TEXT")
  (add-type "WXME")
  (define/override (get-data format)
    (generic-get-data format
                      common-copy-buffer
                      common-copy-buffer2
                      copy-style-list
                      common-copy-region-data))
  (define/override (on-replaced)
    (void)))

(defclass editor-x-clipboard-client% clipboard-client%
  (inherit add-type)
  (super-new)
  (add-type "TEXT")
  (add-type "WXME")
  (define/override (get-data format)
    (cond
     [(and (not x-selection-copied?)
           (not editor-x-selection-owner))
      ""]
     [else
      (when (or (not x-selection-copied?)
                editor-x-selection-owner)
        (copy-into-selection))
      
      ;; if nothing is copied (e.g., do-copy is overriden to not copy anything
      ;; or copies directly to clipboard):
      (if (not selection-copy-style-list)
          (if (send the-x-selection-clipboard same-clipboard-client? this)
              #f
              (send the-x-selection-clipboard get-clipboard-data format 0))
          (generic-get-data format
                            selection-copy-buffer
                            selection-copy-buffer2
                            selection-copy-style-list
                            selection-copy-region-data))]))
  (define/override (on-replaced)
    (if editor-x-selection-owner
        ;; in case this client replaced itself somewhere along the way:
        (when (not (send the-x-selection-clipboard same-clipboard-client? this))
          (let ([b editor-x-selection-owner])
            (set! editor-x-selection-owner #f)
            (set! x-selection-copied? #f)
            (send b own-x-selection #f #t #f)))
        (set! x-selection-copied? #f))))

(define the-editor-clipboard-client
  (new editor-clipboard-client%))
(define the-editor-x-clipboard-client
  (new editor-x-clipboard-client%))

(define/top (editor-set-x-selection-mode [any? on?])
  (when ALLOW-X-STYLE-SELECTION?
    (set! editor-x-selection-mode? (and on? #t))
    (when (and (not on?)
               (send the-x-selection-clipboard same-clipboard-client?
                     the-editor-x-clipboard-client))
      (send the-x-selection-clipboard set-clipboard-string "" 0))))

(define (copy-into-selection)
  ;; copy all the snips:
  (set! x-clipboard-hack? #t)
  
  ;; save normal buffers:
  (let ([save-buffer common-copy-buffer]
        [save-buffer2 common-copy-buffer2]
        [save-styles copy-style-list]
        [save-data common-copy-region-data])
    
    ;; set up new selection buffers, and redirect:
    (set! common-copy-buffer null)
    (set! common-copy-buffer2 null)
    (set! copy-style-list #f)
    (set! common-copy-region-data #f)
    
    (send editor-x-selection-owner copy #f 0)
    
    ;; move "normal" buffers to selection:
    (set! selection-copy-buffer common-copy-buffer)
    (set! selection-copy-buffer2 common-copy-buffer2)
    (set! selection-copy-style-list copy-style-list)
    (set! selection-copy-region-data common-copy-region-data)

    ;; restore normal buffers:
    (set! common-copy-buffer save-buffer)
    (set! common-copy-buffer2 save-buffer2)
    (set! copy-style-list save-styles)
    (set! common-copy-region-data save-data))
  
  (set! x-clipboard-hack? #f))

;; ------------------------------------------------------------

(define (read-buffer-data f)
  (let loop ([data #f])
    (let-boxes ([extra-data-index 0])
        (send f get extra-data-index)
      (if (zero? extra-data-index)
          data
          (let ([dclass (send (send f get-s-bdl) find-by-map-position f extra-data-index)])
            (let ([datalen (if (or (not dclass)
                                   (not (send dclass get-s-required?)))
                               (let-boxes ([datalen 0])
                                   (send f get datalen)
                                 datalen)
                               -1)])
              (if dclass
                  (let ([start (send f tell)])
                    (when (datalen . >= . 0)
                      (send f set-boundary datalen))
                    (let ([newdata (send dclass read f)])
                      (and 
                       newdata
                       (begin
                         (send newdata set-s-next data)
                         (let ([data newdata])
                           (when (datalen . >= . 0)
                             (let ([rcount (- (send f tell) start)])
                               (when (rcount . < . datalen)
                                 (error 'read-buffer-data "underread (caused by file corruption?)"))
                               (send f skip (- datalen rcount)))
                             (send f remove-boundary))
                           (and (send f ok?)
                                (loop data)))))))
                  ;; unknown extra data
                  (begin
                    (send f skip datalen)
                    (and (send f ok?)
                         (loop data))))))))))

;; ------------------------------------------------------------

(define MRED-READER-STR #"#reader(lib\"read.ss\"\"wxme\")")
(define MRED-START-STR #"WXME")
(define MRED-FORMAT-STR #"01")
(define MRED-VERSION-STR #"08")
(define MRED-VERSION-RX #rx"^0[1-8]$")

(define (write-editor-version f b)
  (send b write-bytes MRED-READER-STR)
  (send b write-bytes MRED-START-STR)
  (send b write-bytes MRED-FORMAT-STR)
  (send b write-bytes MRED-VERSION-STR)
  (send b write-bytes #" ## ")
  (not (send b bad?)))

(define MRED-READER+START-STR (bytes-append MRED-READER-STR MRED-START-STR))

(define (detect-wxme-file who f peek?)
  (let* ([l1 (bytes-length MRED-START-STR)]
         [s (if peek?
                (peek-bytes l1 0 f)
                (read-bytes l1 f))])
    (or (equal? s MRED-START-STR)
        (and (equal? s (subbytes MRED-READER-STR 0 l1))
             (let ([s (bytes-append
                       s
                       (let ([v (if peek?                           
                                    (peek-bytes (- (bytes-length MRED-READER+START-STR) l1) l1 f)
                                    (read-bytes (- (bytes-length MRED-READER+START-STR) l1) f))])
                         (if (eof-object? v)
                             ""
                             v)))])
               (equal? s MRED-READER+START-STR))))))

(define (read-editor-version mf b parse-format? [show-errors? #t])
  (and
   (or
    (not parse-format?)
    (let* ([n1 (bytes-length MRED-START-STR)]
           [vbuf (make-vector n1)])
      (let ([n (send b read vbuf)])
        (or (and (= n (vector-length vbuf))
                 (bytes=? MRED-START-STR (string->bytes/latin-1 (list->string (vector->list vbuf)))))
            ;; maybe we have a #reader... prefix?
            (let* ([n2 (bytes-length MRED-READER-STR)]
                   [vbuf2 (make-vector (- n2 n1))])
              (let ([n (send b read vbuf2)])
                (and (= n (- n2 n1))
                     (bytes=? MRED-READER-STR
                              (string->bytes/latin-1 
                               (string-append (list->string (vector->list vbuf))
                                              (list->string (vector->list vbuf2)))))
                     ;; yes, so try reading start again.
                     (let ([n (send b read vbuf)])
                       (and (= n (vector-length vbuf))
                            (bytes=? MRED-START-STR (string->bytes/latin-1 (list->string (vector->list vbuf)))))))))
            (if show-errors?
                (error (method-name 'pasteboard%: 'insert-file) "not a WXME file")
                #f)))))
   (begin
     (let* ([n1 (bytes-length MRED-FORMAT-STR)]
            [vbuf (make-vector n1)])
       (let ([n (send b read vbuf)])
         (send mf set-s-read-format (string->bytes/latin-1 (list->string (vector->list vbuf))))))
     (let* ([n1 (bytes-length MRED-VERSION-STR)]
            [vbuf (make-vector n1)])
       (let ([n (send b read vbuf)])
         (and (= n n1)
              (send mf set-s-read-version (string->bytes/latin-1 (list->string (vector->list vbuf)))))))
     (check-format-and-version mf b show-errors?))))

(define (read-editor-global-header f)
  (send (send f get-s-scl) reset-header-flags f)
  (if (not (send (send f get-s-scl) read f))
      #f
      (begin
        (setup-style-reads-writes f)
        (send (send f get-s-bdl) read f))))

(define (read-editor-global-footer f)
  (done-style-reads-writes f)
  (send (send f get-s-scl) reset-header-flags f)
  #t)

(define (write-editor-global-header f)
  (send f pretty-start)
  (send (send f get-s-scl) reset-header-flags f)
  (if (not (send (send f get-s-scl) write f))
      #f
      (begin
        (setup-style-reads-writes f)
        (send (send f get-s-bdl) write f))))

(define (write-editor-global-footer f)
  (done-style-reads-writes f)
  (send (send f get-s-scl) reset-header-flags f)
  (send f pretty-finish)
  #t)

(define (check-format-and-version s b show-errors?)
  (and
   (or (bytes=? (send s get-s-read-format) MRED-FORMAT-STR)
       (if show-errors?
           (error 'load-file "unknown format number in WXME file format: ~s"
                  (send s get-s-read-format))
           #f))
   (or (regexp-match MRED-VERSION-RX (send s get-s-read-format))
       (if show-errors?
           (error 'load-file "unknown version number in WXME file format")
           #f))
   (if ((send s get-wxme-version) . > . 3)
       ;; need to skip " ## "
       (let* ([v (make-vector 4)]
              [n (send b read v)])
         (or (and (= n 4)
                  (char=? (vector-ref v 0) #\space)
                  (char=? (vector-ref v 1) #\#)
                  (char=? (vector-ref v 2) #\#)
                  (member (vector-ref v 3) '(#\space #\return #\newline)))
             (if show-errors?
                 (error 'load-file "WXME file missing ' ## ' mark")
                 #f)))
       #t)))
