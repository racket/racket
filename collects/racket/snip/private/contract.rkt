#lang racket/base

(require racket/class
         racket/contract/base
         racket/draw
         "snip.rkt"
         "snip-admin.rkt"
         "style.rkt")

(provide (all-defined-out))

;; dummy definitions for contracts
(define-values (cursor% mouse-event% key-event% pasteboard%
                editor-stream-in% editor-stream-out% text% popup-menu%)
  (values object% object% object% object%
          object% object% object% object%))

;; interfaces:
(define (equal<%>/c cls)
  (class/c
   (equal-to? (->m (is-a?/c cls) (-> any/c any/c boolean?) boolean?))
   (equal-hash-code-of (->m (-> any/c exact-integer?) exact-integer?))
   (equal-secondary-hash-code-of (->m (-> any/c exact-integer?) exact-integer?))
   (override
     (equal-to? (->m (is-a?/c cls) (-> any/c any/c boolean?) boolean?))
     (equal-hash-code-of (->m (-> any/c exact-integer?) exact-integer?))
     (equal-secondary-hash-code-of (->m (-> any/c exact-integer?) exact-integer?)))))

(define readable-snip<%>/c
  (class/c
    (read-special
      (->m any/c
           (or/c exact-nonnegative-integer? #f)
           (or/c exact-nonnegative-integer? #f)
           (or/c exact-nonnegative-integer? #f)
           any/c))))

;; contract utilities:
(define font-family/c
  (or/c 'base 'default 'decorative 'roman 'script
        'swiss 'modern 'symbol 'system))

(define font-smoothing/c
  (or/c 'base 'default 'partly-smoothed 'smoothed 'unsmoothed))

(define font-style/c
  (or/c 'base 'normal 'italic 'slant))

(define font-weight/c
  (or/c 'base 'normal 'bold 'light))

(define alignment/c
  (or/c 'base 'top 'center 'bottom))

(define tab-snip-filetype/c
  (or/c 'unknown 'unknown/mask 'unknown/alpha
        'gif 'gif/mask 'gif/alpha
        'jpeg 'png 'png/mask 'png/alpha
        'xbm 'xpm 'bmp 'pict))

(define style-delta%/c
  (class/c
    (collapse (->m (is-a?/c style-delta%) boolean?))
    (copy (->m (is-a?/c style-delta%) void?))
    (equal? (->m (is-a?/c style-delta%) boolean?))
    (get-alignment-off (->m alignment/c))
    (get-alignment-on  (->m alignment/c))
    (get-background-add (->m (is-a?/c add-color<%>)))
    (get-background-mult (->m (is-a?/c mult-color<%>)))
    (get-face (->m (or/c string? false/c)))
    (get-family (->m font-family/c))
    (get-foreground-add  (->m (is-a?/c add-color<%>)))
    (get-foreground-mult (->m (is-a?/c mult-color<%>)))
    (get-size-add (->m byte?))
    (get-size-in-pixels-off (->m boolean?))
    (get-size-in-pixels-on  (->m boolean?))
    (get-size-mult (->m real?))
    (get-smoothing-off (->m font-smoothing/c))
    (get-style-off (->m font-style/c))
    (get-style-on  (->m font-style/c))
    (get-transparent-text-backing-off (->m boolean?))
    (get-transparent-text-backing-on  (->m boolean?))
    (get-underlined-off (->m boolean?))
    (get-underlined-on  (->m boolean?))
    (get-weight-off (->m font-weight/c))
    (get-weight-on  (->m font-weight/c))
    (set-alignment-off (->m alignment/c void?))
    (set-alignment-on  (->m alignment/c void?))
    (set-delta (case->m
                 (-> (is-a?/c style-delta%))
                 (-> (or/c 'change-nothing
                           'change-normal
                           'change-toggle-underline
                           'change-toggle-size-in-pixels
                           'change-normal-color
                           'change-italic
                           'change-bold)
                     (is-a?/c style-delta%))
                 (-> (or/c 'change-family
                           'change-style
                           'change-toggle-style
                           'change-weight
                           'change-toggle-weight
                           'change-smoothing
                           'change-toggle-smoothing
                           'change-alignment
                           'change-size
                           'change-bigger
                           'change-smaller
                           'change-underline
                           'change-size-in-pixels)
                     any/c
                     (is-a?/c style-delta%))))
    (set-delta-background (->m (or/c string? (is-a?/c color%))
                               (is-a?/c style-delta%)))
    (set-delta-face (->*m (string?) (font-family/c) (is-a?/c style-delta%)))
    (set-delta-foreground (->m (or/c string? (is-a?/c color%))
                               (is-a?/c style-delta%)))
    (set-face (->m (or/c string? false/c) void?))
    (set-family (->m font-family/c void?))
    (set-size-add (->m byte? void?))
    (set-size-in-pixels-off (->m any/c void?))
    (set-size-in-pixels-on  (->m any/c void?))
    (set-size-mult (->m real? void?))
    (set-smoothing-off (->m font-smoothing/c void?))
    (set-smoothing-on  (->m font-smoothing/c void?))
    (set-style-off (->m font-style/c void?))
    (set-style-on  (->m font-style/c void?))
    (set-transparent-text-backing-off (->m any/c void?))
    (set-transparent-text-backing-on  (->m any/c void?))
    (set-underlined-off (->m any/c void?))
    (set-underlined-on  (->m any/c void?))
    (set-weight-off (->m font-weight/c void?))
    (set-weight-on  (->m font-weight/c void?))))

(define style-list%/c
  (class/c
    (basic-style (->m (is-a?/c style<%>)))
    (convert (->m (is-a?/c style<%>) (is-a?/c style<%>)))
    (find-named-style (->m string? (or/c (is-a?/c style<%>) false/c)))
    (find-or-create-join-style (->m (is-a?/c style<%>) (is-a?/c style<%>) (is-a?/c style<%>)))
    (find-or-create-style (->m (is-a?/c style<%>) (is-a?/c style-delta%) (is-a?/c style<%>)))
    (forget-notification (->m any/c void?))
    (index-to-style (->m exact-nonnegative-integer? (or/c (is-a?/c style<%>) false/c)))
    (new-named-style (->m string? (is-a?/c style<%>) (is-a?/c style<%>)))
    (notify-on-change (->m (-> (or/c (is-a?/c style<%>) false/c) any) any/c))
    (number (->m exact-nonnegative-integer?))
    (replace-named-style (->m string? (is-a?/c style<%>) (is-a?/c style<%>)))
    (style-to-index (->m (is-a?/c style<%>) (or/c exact-nonnegative-integer? false/c)))))

;; snip% utils
(define snip%-edit-operation/c
  (or/c 'undo 'redo 'clear 'cut
        'copy 'paste 'kill 'select-all
        'insert-text-box 'insert-pasteboard-box 'insert-image))

;; snip% methods
(define snip%-adjust-cursor/c
  (->m (is-a?/c dc<%>)
       real?
       real?
       real?
       real?
       (is-a?/c mouse-event%)
       (or/c (is-a?/c cursor%) false/c)))

(define snip%-blink-caret/c
  (->m (is-a?/c dc<%>) real? real? void?))

(define snip%-can-do-edit-operation/c
  (->*m (snip%-edit-operation/c)
        (any/c)
        boolean?))

(define snip%-copy/c
  (->m (is-a?/c snip%)))

(define snip%-draw/c
  (->m (is-a?/c dc<%>)
       real?
       real?
       real?
       real?
       real?
       real?
       real?
       real?
       (or/c 'no-caret 'show-inactive-caret 'show-caret
             (cons/c exact-nonnegative-integer?
                     exact-nonnegative-integer?))
       void?))

(define snip%-other-equal-to?/c
  (->m (is-a?/c snip%) (-> any/c any/c boolean?) boolean?))

(define snip%-find-scroll-step/c
  (->m real? exact-nonnegative-integer?))

(define snip%-get-admin/c
  (->m (or/c (is-a?/c snip-admin%) false/c)))

(define snip%-get-count/c
  (->m exact-nonnegative-integer?))

(define snip%-get-extent/c
  (->*m ((is-a?/c dc<%>) real? real?)
        ((or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c))
        void?))

(define snip%-get-extent-override/c
  (->m (is-a?/c dc<%>)
       real?
       real?
       (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       void?))

(define snip%-get-flags/c
  (->m (listof symbol?)))

(define snip%-get-num-scroll-steps/c
  (->m exact-nonnegative-integer?))

(define snip%-get-scroll-step-offset/c
  (->m exact-nonnegative-integer?
       (>=/c 0)))

(define snip%-get-snipclass/c
  (->m (or/c #f (is-a?/c snip-class%))))

(define snip%-get-style/c
  (->m (is-a?/c style<%>)))

(define snip%-get-text/c
  (->*m (exact-nonnegative-integer? exact-nonnegative-integer?)
        (any/c)
        string?))

(define snip%-get-text-override/c
  (->m exact-nonnegative-integer?
       exact-nonnegative-integer?
       any/c
       string?))

(define snip%-get-text!/c
  (->m
    (and/c string? (not/c immutable?))
    exact-nonnegative-integer?
    exact-nonnegative-integer?
    exact-nonnegative-integer?
    void?))

(define snip%-is-owned?/c
  (->m boolean?))

(define snip%-match?/c
  (->m (is-a?/c snip%) boolean?))

(define snip%-merge-with/c
  (->m (is-a?/c snip%) (or/c (is-a?/c snip%) false/c)))

(define snip%-next/c
  (->m (or/c (is-a?/c snip%) false/c)))

(define snip%-on-char/c
  (->m (is-a?/c dc<%>)
       real? real? real? real?
       (is-a?/c key-event%)
       void?))

(define snip%-on-event/c
  (->m (is-a?/c dc<%>)
       real? real? real? real?
       (is-a?/c mouse-event%)
       void?))

(define snip%-own-caret/c
  (->m any/c void?))

(define snip%-partial-offset/c
  (->m (is-a?/c dc<%>)
       real? real?
       exact-nonnegative-integer?
       real?))

(define snip%-previous/c
  (->m (or/c (is-a?/c snip%) false/c)))

(define snip%-release-from-owner/c
  (->m boolean?))

(define snip%-resize/c
  (->m (>=/c 0)
       (>=/c 0)
       boolean?))

(define snip%-set-admin/c
  (->m (or/c (is-a?/c snip-admin%) false/c) void?))

(define snip%-set-count/c
  (->m exact-positive-integer? void?))

(define snip%-set-flags/c
  (->m (listof symbol?) void?))

(define snip%-set-snipclass/c
  (->m (is-a?/c snip-class%) void?))

(define snip%-set-style/c
  (->m (is-a?/c style<%>) void?))

(define snip%-set-unmodified/c
  (->m void?))

(define snip%-size-cache-invalid/c
  (->m void?))

(define snip%-split/c
  (->m exact-nonnegative-integer?
       (box/c (is-a?/c snip%))
       (box/c (is-a?/c snip%))
       void?))

(define snip%-write/c
  (->m (is-a?/c editor-stream-out%) void?))

;; snip% class contract
(define snip%/c
  (and/c
    (equal<%>/c snip%)
    (class/c
      (adjust-cursor             snip%-adjust-cursor/c)
      (blink-caret               snip%-blink-caret/c)
      (can-do-edit-operation?    snip%-can-do-edit-operation/c)
      (copy                      snip%-copy/c)
      (draw                      snip%-draw/c)
      (other-equal-to?           snip%-other-equal-to?/c)
      (find-scroll-step          snip%-find-scroll-step/c)
      (get-admin                 snip%-get-admin/c)
      (get-count                 snip%-get-count/c)
      (get-extent                snip%-get-extent/c)
      (get-flags                 snip%-get-flags/c)
      (get-num-scroll-steps      snip%-get-num-scroll-steps/c)
      (get-scroll-step-offset    snip%-get-scroll-step-offset/c)
      (get-snipclass             snip%-get-snipclass/c)
      (get-style                 snip%-get-style/c)
      (get-text                  snip%-get-text/c)
      (get-text!                 snip%-get-text!/c)
      (is-owned?                 snip%-is-owned?/c)
      (match?                    snip%-match?/c)
      (merge-with                snip%-merge-with/c)
      (next                      snip%-next/c)
      (on-char                   snip%-on-char/c)
      (on-event                  snip%-on-event/c)
      (own-caret                 snip%-own-caret/c)
      (partial-offset            snip%-partial-offset/c)
      (previous                  snip%-previous/c)
      (release-from-owner        snip%-release-from-owner/c)
      (resize                    snip%-resize/c)
      (set-admin                 snip%-set-admin/c)
      (set-count                 snip%-set-count/c)
      (set-flags                 snip%-set-flags/c)
      (set-snipclass             snip%-set-snipclass/c)
      (set-style                 snip%-set-style/c)
      (set-unmodified            snip%-set-unmodified/c)
      (size-cache-invalid        snip%-size-cache-invalid/c)
      (split                     snip%-split/c)
      (write                     snip%-write/c)
      (override
        (adjust-cursor           snip%-adjust-cursor/c)
        (draw                    snip%-draw/c)
        (other-equal-to?         snip%-other-equal-to?/c)
        (find-scroll-step        snip%-find-scroll-step/c)
        (get-extent              snip%-get-extent-override/c)
        (get-num-scroll-steps    snip%-get-num-scroll-steps/c)
        (get-scroll-step-offset  snip%-get-scroll-step-offset/c)
        (get-text                snip%-get-text-override/c)
        (get-text!               snip%-get-text!/c)
        (match?                  snip%-match?/c)
        (merge-with              snip%-merge-with/c)
        (on-char                 snip%-on-char/c)
        (on-event                snip%-on-event/c)
        (own-caret               snip%-own-caret/c)
        (partial-offset          snip%-partial-offset/c)
        (release-from-owner      snip%-release-from-owner/c)
        (resize                  snip%-resize/c)
        (set-count               snip%-set-count/c)
        (set-flags               snip%-set-flags/c)
        (set-unmodified          snip%-set-unmodified/c)
        (size-cache-invalid      snip%-size-cache-invalid/c)
        (split                   snip%-split/c)))))


;; snip-class% method contracts
(define snip-class%-get-classname/c
  (->m string?))

(define snip-class%-get-version/c
  (->m exact-integer?))

(define snip-class%-read/c
  (->m (is-a?/c editor-stream-in%)
       (or/c (is-a?/c snip%) false/c)))

(define snip-class%-read-header/c
  (->m (is-a?/c editor-stream-in%) boolean?))

(define snip-class%-reading-version/c
  (->m (is-a?/c editor-stream-in%) exact-integer?))

(define snip-class%-set-classname/c
  (->m string? void?))

(define snip-class%-set-version/c
  (->m exact-integer? void?))

(define snip-class%-write-header/c
  (->m (is-a?/c editor-stream-out%) boolean?))

;; snip-class% class contract
(define snip-class%/c
  (class/c
    (get-classname    snip-class%-get-classname/c)
    (get-version      snip-class%-get-version/c)
    (read             snip-class%-read/c)
    (read-header      snip-class%-read-header/c)
    (reading-version  snip-class%-reading-version/c)
    (set-classname    snip-class%-set-classname/c)
    (set-version      snip-class%-set-version/c)
    (write-header     snip-class%-write-header/c)
    (override
      (read          snip-class%-read/c)
      (read-header   snip-class%-read-header/c)
      (write-header  snip-class%-write-header/c))))

(define string-snip%/c
  (class/c
    (insert (->*m (string? exact-nonnegative-integer?)
                  (exact-nonnegative-integer?)
                  void?))
    (read (->m exact-nonnegative-integer?
               (is-a?/c editor-stream-in%)
               void?))))

(define tab-snip%/c
  (class/c))

(define image-snip%/c
  (class/c
    (equal-hash-code-of (->m (any/c . -> . exact-integer?) exact-integer?))
    (equal-secondary-hash-code-of (->m (any/c . -> . exact-integer?) exact-integer?))
    (get-bitmap (->m (or/c (is-a?/c bitmap%) #f)))
    (get-bitmap-mask (->m (or/c (is-a?/c bitmap%) #f)))
    (get-filename (->*m () ((or/c (box/c any/c) #f)) (or/c path-string? #f)))
    (get-filetype (->m tab-snip-filetype/c))
    (load-file (->*m ((or/c path-string? input-port? #f))
                     (tab-snip-filetype/c any/c any/c)
                     void?))
    (other-equal-to? (->m (is-a?/c image-snip%)
                          (any/c any/c . -> . boolean?)
                          boolean?))
    (resize (->m (>=/c 0)
                 (>=/c 0)
                 boolean?))
    (set-bitmap (->*m ((is-a?/c bitmap%)) ((or/c (is-a?/c bitmap%) #f)) void?))
    (set-offset (->m real? real? void?))))

;; snip-admin% method contracts
(define snip-admin%-get-view/c
  (->*m ((or/c (box/c real?) false/c)
         (or/c (box/c real?) false/c)
         (or/c (box/c (>=/c 0)) false/c)
         (or/c (box/c (>=/c 0)) false/c))
        ((or/c (is-a?/c snip%) false/c))
        void?))

(define snip-admin%-get-view-size/c
  (->m (or/c (box/c (>=/c 0)) false/c)
       (or/c (box/c (>=/c 0)) false/c)
       void?))

(define snip-admin%-modified/c
  (->m (is-a?/c snip%) any/c void?))

(define snip-admin%-needs-update/c
  (->m (is-a?/c snip%)
       real?
       real?
       (>=/c 0)
       (>=/c 0)
       void?))

(define snip-admin%-popup-menu/c
  (->m (is-a?/c popup-menu%)
       (is-a?/c snip%)
       real?
       real?
       boolean?))

(define snip-admin%-recounted/c
  (->m (is-a?/c snip%) any/c void?))

(define snip-admin%-release-snip/c
  (->m (is-a?/c snip%) boolean?))

(define snip-admin%-resized/c
  (->m (is-a?/c snip%) any/c void?))

(define snip-admin%-scroll-to/c
  (->*m ((is-a?/c snip%)
         real?
         real?
         (>=/c 0)
         (>=/c 0)
         any/c)
        ((or/c 'start 'end 'none))
        boolean?))

(define snip-admin%-set-caret-owner/c
  (->m (is-a?/c snip%)
       (or/c 'immediate 'display 'global)
       void?))

(define snip-admin%-update-cursor/c
  (->m void?))

(define snip-admin%-get-line-spacing/c
  (->m (>=/c 0)))

(define snip-admin%-get-selected-text-color/c
  (->m (or/c (is-a?/c color%) #f)))

(define snip-admin%-call-with-busy-cursor/c
  (->m (-> any) any))

(define snip-admin%-get-tabs/c
  (->*m ()
        ((or/c (box/c exact-nonnegative-integer?) false/c)
         (or/c (box/c real?) false/c)
         (or/c (box/c real?) false/c))
        (listof real?)))

(define snip-admin%/c
  (class/c
    (get-dc (->m (or/c (is-a?/c dc<%>) false/c)))
    (get-editor (->m (or/c (is-a?/c text%) (is-a?/c pasteboard%))))
    (get-view snip-admin%-get-view/c)
    (get-view-size snip-admin%-get-view-size/c)
    (modified snip-admin%-modified/c)
    (needs-update snip-admin%-needs-update/c)
    (popup-menu snip-admin%-popup-menu/c)
    (recounted snip-admin%-recounted/c)
    (release-snip snip-admin%-release-snip/c)
    (resized snip-admin%-resized/c)
    (scroll-to snip-admin%-scroll-to/c)
    (set-caret-owner snip-admin%-set-caret-owner/c)
    (update-cursor snip-admin%-update-cursor/c)
    (get-line-spacing snip-admin%-get-line-spacing/c)
    (get-selected-text-color snip-admin%-get-selected-text-color/c)
    (call-with-busy-cursor snip-admin%-call-with-busy-cursor/c)
    (get-tabs snip-admin%-get-tabs/c)
    (override
      (get-view snip-admin%-get-view/c)
      (get-view-size snip-admin%-get-view-size/c)
      (modified snip-admin%-modified/c)
      (needs-update snip-admin%-needs-update/c)
      (popup-menu snip-admin%-popup-menu/c)
      (recounted snip-admin%-recounted/c)
      (release-snip snip-admin%-release-snip/c)
      (resized snip-admin%-resized/c)
      (scroll-to snip-admin%-scroll-to/c)
      (set-caret-owner snip-admin%-set-caret-owner/c)
      (update-cursor snip-admin%-update-cursor/c)
      (get-line-spacing snip-admin%-get-line-spacing/c)
      (get-selected-text-color snip-admin%-get-selected-text-color/c)
      (call-with-busy-cursor snip-admin%-call-with-busy-cursor/c)
      (get-tabs snip-admin%-get-tabs/c))))
