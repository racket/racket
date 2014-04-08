#lang racket/base
(require racket/class
         racket/file file/convertible
         "snip-flags.rkt"
         "load-one.rkt"
         "style.rkt"
         "private.rkt"
         racket/draw/private/syntax
         racket/draw)

(provide snip%
         snip-class%
         string-snip%
         tab-snip%
         image-snip%
         snip-class-list<%>
         get-the-snip-class-list
         
         get-snip-class

         the-snip-class-list ;; parameter
         make-the-snip-class-list
         
         (struct-out snip-class-link)

         snip->admin
         snip->count
         snip->next
         snip->prev
         snip->flags
         snip->line
         snip->loc
         snip->style
         snip->snipclass

         set-snip-admin!
         set-snip-line!
         set-snip-loc!
         set-snip-style!
         set-snip-flags!
         set-snip-count!
         set-snip-prev!
         set-snip-next!

         snip%-get-text

         string-snip-buffer
         string-snip-dtext

         caret-status?

         readable-snip<%>

         image-type?
         int->img-type)

;; these are used only in contracts
;; we don't want the real definitions b/c they require the gui
(define-values (editor-stream-in% editor-stream-out% snip-admin% mouse-event% key-event%)
  (values object% object% object% object% object%))

(define (symbol-list? l)
  (and (list? l) (andmap symbol? l)))
(define (mutable-string? s)
  (and (string? s) (not (immutable? s))))

(define (caret-status? v)
  (or (eq? v 'no-caret)
      (eq? v 'show-inactive-caret)
      (eq? v 'show-caret)
      (and (pair? v)
           (exact-nonnegative-integer? (car v))
           (exact-nonnegative-integer? (cdr v))
           ((car v) . <= . (cdr v)))))

;; ------------------------------------------------------------

(define MAX-WASTE 3)
(define MIN-WASTE-CHECK 24)
(define IMAGE-PIXELS-PER-SCROLL 20.0)
(define IMAGE-VOID-SIZE 20.0)

(define (replace-nuls s)
  (if (for/or ([c (in-string s)]) (or (eq? #\nul c)
                                      (eq? #\page c)))
      (regexp-replace* #rx"\f"
                       (regexp-replace* #rx"\0" s " ")
                       "^L")
      s))

;; ------------------------------------------------------------

(defclass snip-class% object%
  (define classname "wxbad")
  (def/public (set-classname [string? s])
    (set! classname (string->immutable-string s)))
  (def/public (get-classname) classname)
  
  (properties [[exact-nonnegative-integer? version] 0])
  (field [s-required? #f])
  (define/public (get-s-required?) s-required?)

  (super-new)

  (def/public (read-header [editor-stream-in% f]) #t)
  (def/public (write-header [editor-stream-out% f]) #t)
  (def/public (reading-version [editor-stream-in% f])
    (send f do-reading-version this))
  
  (def/public (read [editor-stream-in% f]) (error "should have been overridden")))

;; ------------------------------------------------------------

(defclass* snip% object% (equal<%>)
  ;; For use only by the owning editor:
  (field [s-prev #f]
         [s-next #f]
         [s-line #f]) ; used for line by text%, loc by pastebpard%
  (define/public (set-s-prev p) (set! s-prev p))
  (define/public (set-s-next p) (set! s-next p))
  (define/public (set-s-line l) (set! s-line l))

  (field [s-admin #f]
         [s-count 1]
         [s-flags NO-FLAGS]
         [s-snipclass #f]
         [s-style (send the-style-list basic-style)])
  (define/public (set-s-admin a) (set! s-admin a))
  (define/public (set-s-count v) (set! s-count v))
  (define/public (set-s-flags v) (set! s-flags v))
  (define/public (set-s-snipclass v) (set! s-snipclass v))
  (define/public (set-s-style s) (set! s-style s))

  (def/public (set-snipclass [snip-class% c])
    (set! s-snipclass c))
  (def/public (get-snipclass) s-snipclass)

  (def/public (get-count) s-count)
  (def/public (get-flags) (flags->symbols s-flags))

  (super-new)

  (def/public (next) s-next)
  (def/public (previous) s-prev)
  (def/public (get-admin) s-admin)

  (def/public (set-admin [(make-or-false snip-admin%) a])
    (unless (and (not (eq? a s-admin))
                 (has-flag? s-flags OWNED)
                 (or a 
                     (not (has-flag? s-flags CAN-DISOWN))))
      (set! s-admin a)
      (size-cache-invalid)
      (if (not a)
          (begin
            (set! s-prev #f)
            (set! s-next #f)
            (set! s-line #f))
          (set! s-flags (add-flag OWNED s-flags)))))

  (def/public (set-count [exact-nonnegative-integer? new-count])
    (let ([old-count s-count]
          [new-count (max new-count 1)])
      (set! s-count new-count)
      (when s-admin
        (unless (send s-admin recounted this #t)
          (set! s-count old-count)))))

  (def/public (set-flags [(make-list (symbol-in is-text 
                                                can-append 
                                                invisible 
                                                newline 
                                                hard-newline 
                                                handles-events 
                                                width-depends-on-x 
                                                height-depends-on-y 
                                                width-depends-on-y 
                                                height-depends-on-x
                                                handles-all-mouse-events)) 
                          new-flags])
    (s-set-flags (symbols->flags new-flags)))
  
  (define/public (s-set-flags new-flags)
    (let* (;; make sure that wxSNIP-HARD_NEWLINE implies a wxSNIP-NEWLINE
           [new-flags (if (has-flag? new-flags HARD-NEWLINE)
                          (add-flag new-flags NEWLINE)
                          (remove-flag new-flags NEWLINE))]
           ;; make sure ownership and splitness flags don't change
           [new-flags (copy-flag s-flags new-flags OWNED)]
           [new-flags (copy-flag s-flags new-flags CAN-DISOWN)]
           [new-flags (copy-flag s-flags new-flags CAN-SPLIT)])
      (set! s-flags new-flags)
      (when s-admin
        (send s-admin resized this #t))))

  (def/public (on-event [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [mouse-event% event])
    (void))

  (def/public (adjust-cursor [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [mouse-event% event])
    #f)

  (def/public (on-char [dc<%> dc] [real? x] [real? y] [real? ex] [real? ey] [key-event% event])
    (void))

  (def/public (do-edit-operation [symbol? op] [any? [recur? #t]] [exact-integer? [timestamp 0]])
    (void))

  (def/public (can-do-edit-operation? [symbol? op] [any? [recur? #t]])
    #f)

  (def/public (match? [snip% other])
    (and (eq? s-snipclass (snip->snipclass other))
         (= s-count (get-field s-count other))))
  
  (def/public (own-caret [any? own?])
    (void))

  (def/public (blink-caret [dc<%> dc] [real? ex] [real? ey])
    (void))

  (def/public (size-cache-invalid)
    (void))

  (def/public (get-extent [dc<%> dc] [real? ex] [real? ey] 
                          [maybe-box? [w #f]] [maybe-box? [h #f]] 
                          [maybe-box? [descent #f]] [maybe-box? [space #f]]
                          [maybe-box? [lspace #f]] [maybe-box? [rspace #f]])
    (when w (set-box! w 0.0))
    (when h (set-box! h 0.0))
    (when descent (set-box! descent 0.0))
    (when space (set-box! space 0.0))
    (when lspace (set-box! lspace 0.0))
    (when rspace (set-box! rspace 0.0)))

  (def/public (partial-offset [dc<%> dc] [real? ex] [real? ey] [exact-nonnegative-integer? offset])
    (if (zero? offset)
        0.0
        (let-boxes ([w 0.0])
            (get-extent dc ex ey w #f #f #f #f #f)
          w)))

  (def/public (draw [dc<%> dc] [real? x] [real? y] 
                    [real? left] [real? top] [real? bottom] [real? right] 
                    [real? dx] [real? dy] [caret-status? caret])
    (void))

  (def/public (split [exact-nonnegative-integer? position] [box? first] [box? second])
    (let ([snip (new snip%)])
      (send snip set-s-count position)
      (set! s-count (- s-count position))

      (set-box! first snip)
      (set-box! second this)

      (when (and (not (has-flag? s-flags CAN-SPLIT)) s-admin)
        (send s-admin resized this #t))))

  (def/public (merge-with [snip% other])
    #f)

  (def/public (get-text! [mutable-string? s] [exact-nonnegative-integer? offset] 
                         [exact-integer? num] [exact-nonnegative-integer? dt])
    (unless (num . <= . 0)
      (let ([str (get-text (+ offset dt) num #f)])
        (if (not str)
            (for ([i (in-range num)])
              (string-set! s i #\.))
            (string-copy! s 0 str 0 (min num (string-length str) (string-length s)))))))

  (def/public (get-text [exact-nonnegative-integer? offset] [exact-integer? num] 
                        [any? [flattened? #f]])
    (make-string (min num (max 0 (- s-count (max 0 offset)))) #\.))

  (def/public (set-style [style<%> s])
    (unless (has-flag? s-flags OWNED)
      (set! s-style s)))

  (def/public (get-style)
    s-style)

  (def/public (copy)
    (let ([s (new snip%)])
      (do-copy-to s)
      s))

  (define/public (do-copy-to dest)
    (send dest set-s-count s-count)
    (send dest set-s-flags
          (remove-flag (remove-flag (remove-flag s-flags OWNED)
                                    CAN-DISOWN)
                       CAN-SPLIT))
    (send dest set-s-snipclass s-snipclass)
    (send dest set-s-style s-style))

  (def/public (write [editor-stream-out% f])
    (void))

  (def/public (resize [real? w] [real? h]) 
    #f)

  (def/public (get-num-scroll-steps)
    1)

  (def/public (find-scroll-step [real? n])
    0)

  (def/public (get-scroll-step-offset [exact-integer? n])
    0)

  (def/public (is-owned?)
    (has-flag? s-flags OWNED))

  (def/public (release-from-owner)
    (if (not (has-flag? s-flags OWNED))
        #t
        (if (not s-admin)
            #f
            (if (send s-admin release-snip this)
                (not (has-flag? s-flags OWNED))
                #f))))

  (def/public (set-unmodified)
    (void))
  
  (def/public (equal-to? [snip% that] [any? recur]) 
    (send that other-equal-to? this recur))
  (def/public (other-equal-to? [snip% that] [any? recur]) (eq? this that))
  (define/public (equal-hash-code-of recur) (eq-hash-code this))
  (define/public (equal-secondary-hash-code-of recur) 1))
 
(defclass internal-snip% snip%
  (super-new)
  (def/override (set-count [exact-integer? c])
    ;; reject change
    (void)))
;; ------------------------------------------------------------

(defclass string-snip-class% snip-class%
  (inherit set-classname
           set-version)
  (inherit-field s-required?)

  (super-new)

  (set-classname "wxtext")
  (set-version 3)
  (set! s-required? #t)

  (def/override (read [editor-stream-in% f])
    (s-read (make-object string-snip% 0) f))

  (def/public (s-read [string-snip% snip] [editor-stream-in% f])
    (let ([flags (send f get-exact)])
      (let ([pos (send f tell)])
        (let ([count (send f get-exact)])
          (send f jump-to pos)
          (let ([count (if (count . < . 0)
                           10; this is a failure; we make up something
                           count)])
            (send snip s-read count f))
          (send snip set-s-flags flags)
          snip)))))

;; ------------------------------------------------------------

(defclass string-snip% internal-snip%
  (inherit-field s-style
                 s-count
                 s-flags
                 s-admin
                 s-snipclass)

  (init-rest args)

  (field [str-metric #f] ; a number (in which case height, decsent, and space matches style) or a vector
         [s-dtext 0]
         [s-buffer ""])

  (super-new)
  (set! s-count 0)

  (define/public (set-str-w v) (set! str-metric v))
  (define/public (get-s-dtext) s-dtext)

  (let-values ([(str len)
                (cond
                 ;; handle common case for split, first:
                 [(and (pair? args)
                       (exact-nonnegative-integer? (car args))
                       (null? (cdr args)))
                  (values "" (car args))]
                 [else
                  (case-args
                   args
                   [() (values "" 0)]
                   [([exact-nonnegative-integer? len])
                    (values "" len)]
                   [([string? str])
                    (values str (string-length str))]
                   [([string? str] [exact-nonnegative-integer? len])
                    (values str len)]
                   (init-name 'string-snip%))])])

    (set! s-flags (add-flag (add-flag s-flags IS-TEXT) CAN-APPEND))
    
    (let ([len (if (equal? str "\n")
                   1 ;; string snips that are just created to be newlines don't need extra space
                   (max 8 (* 2 (min len 5000))))])
      (set! s-buffer (make-string len)))
    
    (set! s-snipclass the-string-snip-class)
    
    (unless (equal? str "")
      (insert str (min (string-length str) len) 0)))

  (def/override (size-cache-invalid)
    (set! str-metric #f))

  (define/private (get-text-extent dc count)
    (let ([font (send s-style get-font)])
      (send dc get-text-extent (replace-nuls (substring s-buffer s-dtext (+ s-dtext count)))
            font #f)))

  (def/override (get-extent [dc<%> dc] [real? ex] [real? ey] 
                            [maybe-box? [wo #f]] [maybe-box? [ho #f]]
                            [maybe-box? [dso #f]] [maybe-box? [so #f]]
                            [maybe-box? [ls #f]] [maybe-box? [rs #f]])
    (unless str-metric
      (let ([count s-count])
        (if (or (has-flag? s-flags INVISIBLE)
                (zero? count)
                (and (= count 1) 
                     (or (eq? (string-ref s-buffer s-dtext) #\newline)
                         (eq? (string-ref s-buffer s-dtext) #\tab))))
            (if (and (= count 1) 
                     (eq? (string-ref s-buffer s-dtext) #\tab))
                (set! str-metric (send s-style get-text-width dc))
                (set! str-metric 0.0))
            (let-values ([(w h d s) (get-text-extent dc count)])
              (if (and (= h (send s-style get-text-height dc))
                       (= d (send s-style get-text-descent dc))
                       (= s (send s-style get-text-space dc)))
                  (set! str-metric w)
                  (set! str-metric (vector w h d s)))))))

    (when wo (set-box! wo (if (vector? str-metric)
                              (vector-ref str-metric 0)
                              str-metric)))
    (when (or ho dso so)
      (send s-style reset-text-metrics dc))
    (when ho
      (set-box! ho (if (vector? str-metric)
                       (vector-ref str-metric 1)
                       (style->cached-text-height s-style))))
    (when dso
      (set-box! dso (if (vector? str-metric)
                        (vector-ref str-metric 2)
                        (style->cached-text-descent s-style))))
    (when so
      (set-box! so (if (vector? str-metric)
                       (vector-ref str-metric 3)
                       (style->cached-text-space s-style))))
    (when ls (set-box! ls 0.0))
    (when rs (set-box! rs 0.0)))

  (def/override (partial-offset [dc<%> dc] [real? ex] [real? ey] 
                                [exact-nonnegative-integer? offset])
    (let-values ([(w h d a) (get-text-extent dc (min offset s-count))])
      w))

  (def/override (draw [dc<%> dc] [real? x] [real? y] 
                      [real? left] [real? top] [real? bottom] [real? right] 
                      [real? dx] [real? dy] [caret-status? caret])
    (unless (has-flag? s-flags INVISIBLE)
      (if (and (pair? caret)
               (or (and s-admin (send s-admin get-selected-text-color))
                   (eq? 'solid (send dc get-text-mode))))
          ;; Draw three parts: before selection, selection, after selection
          (let ([before (replace-nuls
                         (substring s-buffer 
                                    s-dtext 
                                    (+ s-dtext (min (car caret) s-count))))]
                [sel (replace-nuls
                      (substring s-buffer 
                                 (+ s-dtext (min (car caret) s-count))
                                 (+ s-dtext (min (cdr caret) s-count))))]
                [after (replace-nuls
                        (substring s-buffer 
                                   (+ s-dtext (min (cdr caret) s-count))
                                   (+ s-dtext s-count)))])
            (let-values ([(bw bh bd ba) (if (string=? before "")
                                            (values 0.0 0.0 0.0 0.0)
                                            (send dc get-text-extent before))]
                         [(sw sh sd sa) (send dc get-text-extent sel)]
                         [(aw ah ad aa) (if (string=? after "")
                                            (values 0.0 0.0 0.0 0.0)
                                            (send dc get-text-extent after))])
              (define (baseline-delta h d)
                (- (max (- bh bd) (- sh sd) (- ah ad)) (- h d)))
              (unless (string=? before "")
                (send dc draw-text before x (+ y (baseline-delta bh bd)) #f))
              (let ([col (send dc get-text-foreground)]
                    [mode (send dc get-text-mode)])
                (when (and s-admin (send s-admin get-selected-text-color))
                  (send dc set-text-foreground (send s-admin get-selected-text-color)))
                (send dc set-text-mode 'transparent)
                (send dc draw-text sel (+ x bw) (+ y (baseline-delta sh sd)) #f)
                (send dc set-text-foreground col)
                (send dc set-text-mode mode)
                (unless (string=? after "")
                  (send dc draw-text after (+ x bw sw) (+ y (baseline-delta ah ad)) #f)))))
          ;; Just draw the string
          (send dc draw-text (replace-nuls (substring s-buffer s-dtext (+ s-dtext s-count))) x y #f))))

  (def/override (split [exact-nonnegative-integer? position] [box? first] [box? second])
    (let ([count s-count])
      (unless (or (position . < . 0)
                  (position . > . count))
        
        (let ([snip (if (and (= 1 position)
                             (equal? (string-ref s-buffer s-dtext) #\newline))
                        ;; making a newline snip, so signal string-snip% this fact by passing "\n"
                        (make-object string-snip% "\n")
                        (make-object string-snip% position))])
          
          (set! str-metric #f)

          (let ([s (string-snip-buffer snip)])
            (unless ((string-length s) . >= . position)
              (set-string-snip-buffer! s (make-string position))))
          
          (string-copy! (string-snip-buffer snip)
                        0
                        s-buffer
                        s-dtext
                        (+ position s-dtext))
          (set-snip-count! snip position)
          (set! s-dtext (+ s-dtext position))

          (let ([count (- count position)])
            (set! s-count count)

            (when ((string-length s-buffer) . > . (max MIN-WASTE-CHECK (* MAX-WASTE (add1 count))))
              (let ([s (make-string count)])
                (string-copy! s 0 s-buffer s-dtext (+ s-dtext count))
                (set! s-dtext 0)
                (set! s-buffer s))))

          (set-box! first snip)
          (set-box! second this)
          
          (when (and s-admin (not (has-flag? s-flags CAN-SPLIT)))
            (send s-admin resized this #t))))))

  (def/override (merge-with [snip% pred])
    (set! str-metric #f)
    (insert-with-offset (string-snip-buffer pred)
                        (snip->count pred)
                        (string-snip-dtext pred)
                        0)
    (when (not (has-flag? s-flags CAN-SPLIT))
      (send s-admin resized this #t))
    this)

  (define/public (insert-with-offset s len delta pos)
    (unless (or (len . <= . 0)
                (pos . < . 0))
      (let ([count s-count])
        (cond
         [((string-length s-buffer) . < . (+ count len))
          (let ([s (make-string (* 2 (+ count len)))])
            (string-copy! s 0 s-buffer s-dtext (+ s-dtext count))
            (set! s-buffer s)
            (set! s-dtext 0))]
         [((+ s-dtext count len) . > . (string-length s-buffer))
          (string-copy! s-buffer 0 s-buffer s-dtext (+ s-dtext count))
          (set! s-dtext 0)])

        (when (pos . < . count)
          (string-copy! s-buffer (+ s-dtext pos len)
                        s-buffer (+ s-dtext pos)
                        (+ s-dtext count)))
        (string-copy! s-buffer
                      (+ s-dtext pos)
                      s
                      delta 
                      (+ delta len))
        (set! s-count (+ count len))
        (set! str-metric #f)
        (when (not (has-flag? s-flags CAN-SPLIT))
          (when s-admin
            (unless (send s-admin recounted this #t)
              (set! s-count count)))))))

  (def/public (insert [string? str] [exact-nonnegative-integer? len] 
                      [exact-nonnegative-integer? [pos 0]])
    (insert-with-offset str len 0 pos))

  (def/override  (get-text! [mutable-string? s] [exact-nonnegative-integer? offset] 
                            [exact-integer? num] [exact-nonnegative-integer? dt])
    (when (positive? num)
      (string-copy! s dt s-buffer (+ s-dtext offset) (+ s-dtext (min (+ offset num) s-count)))))

  (def/override (get-text [exact-nonnegative-integer? offset] [exact-integer? num] 
                          [any? [flat? #f]])
    (let ([num (min num (max 0 (- s-count offset)))])
      (if (num . <= . 0)
          ""
          (let ([s (make-string num)])
            (get-text! s offset num 0)
            s))))

  (def/override (copy)
    (let ([snip (new string-snip%)])
      (do-copy-to snip)
      snip))

  (def/override (do-copy-to [snip% snip])
    (super do-copy-to snip)
    (set-snip-count! snip 0)
    (send snip insert-with-offset s-buffer s-count s-dtext 0))

  (def/override (write [editor-stream-out% f])
    (let* ([write-flags s-flags]
           [write-flags (remove-flag write-flags OWNED)]
           [write-flags (remove-flag write-flags CAN-DISOWN)]
           [write-flags (remove-flag write-flags CAN-SPLIT)])
      (send f put write-flags)
      (let ([bytes (string->bytes/utf-8 s-buffer 0 s-dtext (+ s-dtext s-count))])
        (send f put (bytes-length bytes) bytes))))

  (def/public (read [exact-nonnegative-integer? len]
                    [editor-stream-in% f])
    (s-read len f))

  (define/public (s-read len f)
    (unless (len . < . 0) ; tolerate a 0-length snip, to be filtered out later
      (when ((string-length s-buffer) . < . len)
        (set! s-buffer (make-string (* 2 len))))
      (set! s-dtext 0)
      (let ([rv (send f do-reading-version the-string-snip-class)])
        (cond
         [(not (= rv 2))
          ;; read latin-1 (version < 2) or utf-8 (version > 2)
          (let ([b (make-bytes len)]
                [l2 (box len)])
            (send f get-unterminated-bytes! l2 b)
            (let ([len (unbox l2)]
                  [s (if (rv . < . 2)
                         (bytes->string/latin-1 b #\? 0 len)
                         (bytes->string/utf-8 b #\? 0 len))])
              (string-copy! s-buffer 0 s 0 (string-length s))
              (set! s-count (string-length s))))]
         [else
          ;; version 2 wrote out UTF-32 directly -- bad idea,
          ;; because it uses the machine's endianness.
          (let ([b (make-bytes (* len 4))]
                [l2 (box len)]
                [big? (system-big-endian?)])
            (send f get-unterminated-bytes! len b)
            (let ([len (unbox l2)])
              (for ([i (in-range len)])
                (let ([c (integer-bytes->integer b #f big? (* i 4) (* (add1 i) 4))])
                  (string-set! s-buffer i (char->integer c))))))]))
      (set! str-metric #f))))

(define string-snip-buffer (class-field-accessor string-snip% s-buffer))
(define string-snip-dtext (class-field-accessor string-snip% s-dtext))
(define set-string-snip-buffer! (class-field-mutator string-snip% s-buffer))

;; ------------------------------------------------------------

(defclass tab-snip-class% string-snip-class%
  (inherit set-classname
           set-version
           s-read)
  (inherit-field s-required?)

  (super-new)

  (set-classname "wxtab")
  (set-version 1)
  (set! s-required? #t)

  (def/override (read [editor-stream-in% f])
    (let ([ts (new tab-snip%)])
      (s-read ts f))))

;; ------------------------------------------------------------

(defclass tab-snip% string-snip%
  (inherit-field s-snipclass
                 s-flags
                 s-admin
                 str-metric)
  (inherit set-str-w
           set-s-snipclass
           do-copy-to)

  (super-new)

  (set-s-snipclass the-tab-snip-class)
  (set! s-flags (remove-flag (add-flag s-flags WIDTH-DEPENDS-ON-X)
                             CAN-APPEND))

  (def/override (get-extent [dc<%> dc] [real? ex] [real? ey] 
                            [maybe-box? [wi #f]] [maybe-box? [h #f]] 
                            [maybe-box? [descent #f]] [maybe-box? [space #f]]
                            [maybe-box? [lspace #f]] [maybe-box? [rspace #f]])
    (let* ([old-w str-metric]
           [changed? (not old-w)])
      (super get-extent dc ex ey wi h descent space lspace rspace)

      (when changed?
        ;; w is now width of a space
        (let* ([admin s-admin]
               [media (and admin
                           (send admin get-editor))])
          (let-values ([(n tabs tabspace mult)
                        (let-boxes ([n 0]
                                    [space 0]
                                    [units? #f]
                                    [tabs null])
                            (set-box! tabs (send admin get-tabs n space units?))
                          (values n
                                  tabs ;; a list
                                  space
                                  (if units? 
                                      1 
                                      (if (zero? str-metric)
                                          1.0
                                          str-metric))))])
            (set-str-w
             (let loop ([i 0])
               (if (= i n)
                   (let ([base (if (zero? n)
                                   0
                                   (list-ref tabs (- n 1)))])
                     (let ([tabspace (* tabspace mult)])
                       (+ base (- (->long tabspace)
                                  (modulo (->long (- ex base))
                                          (->long tabspace))))))
                   (let ([v (list-ref tabs i)])
                     (if ((* mult v) . > . ex)
                         (- (* mult v) ex)
                         (loop (add1 i))))))))))

      (when wi (set-box! wi str-metric))))

  (def/override (partial-offset [dc<%> dc] [real? x] [real? y] 
                                [exact-nonnegative-integer? offset])
    (if (zero? offset)
        0.0
        (let-boxes ([w 0.0])
            (get-extent dc x y w #f #f #f #f #f)
          w)))

  (def/override (draw [dc<%> dc] [real? x] [real? y] 
                      [real? left] [real? top] [real? bottom] [real? right] 
                      [real? dx] [real? dy] [caret-status? caret])
    ;; draw nothing
    (void))

  (def/override (copy)
    (let ([snip (new tab-snip%)])
      (do-copy-to snip)
      snip)))

;; ------------------------------------------------------------

(define IMG-MOVE-BUF-SIZE 500)

(define (int->img-type type)
  (case type
    [(#x2) 'bmp]
    [(#x8) 'xbm]
    [(#x200) 'xpm]
    [(#x1000) 'gif]
    [(#x11000) 'gif/mask]
    [(#x4000) 'jpeg]
    [(#x8000) 'png]
    [(#x18000) 'png/mask]
    [(#x12000) 'unknown/mask]
    [else 'unknown]))

(define (img-type->int type)
  (case type
    [(bmp) #x2]
    [(xbm) #x8]
    [(xpm) #x200]
    [(gif) #x1000]
    [(gif/mask) #x11000]
    [(jpeg) #x4000]
    [(png) #x8000]
    [(png/mask) #x18000]
    [(unknown/mask) #x12000]
    [else #x2000]))

(defclass image-snip-class% snip-class%
  (inherit set-classname
           set-version)
  (inherit-field s-required?)

  (super-new)
  
  (set-classname "wximage")
  (set-version 2)
  
  (def/override (read [editor-stream-in% f])
    (let ([scl (get-the-snip-class-list)]
          [can-inline? ((send f do-reading-version this) . > . 1)])
      (let ([filename (send f get-bytes #f)])
        (let-boxes ([type 0]
                    [w 0.0]
                    [h 0.0]
                    [dx 0.0]
                    [dy 0.0]
                    [relative 0])
            (begin
              (send f get type)
              (send f get w)
              (send f get h)
              (send f get dx)
              (send f get dy)
              (send f get relative))

          (let-values ([(loadfile
                         type
                         inlined?
                         backing-scale)
                        (if (and (equal? filename #"")
                                 can-inline?
                                 (positive? type))
                            ;; read inlined image
                            (let-boxes ([len 0])
                                (send f get-fixed len)
                              (if (and (len . > . 0)
                                       (send f ok?))
                                  (let-values ([(in out) (make-pipe)]
                                               [(backing-scale)
                                                (if (= type 4)
                                                    (send f get-inexact)
                                                    1.0)])
                                    (for ([i (in-range len)])
                                      (display (send f get-unterminated-bytes) out))
                                    (close-output-port out)
                                    (values in
                                            'unknown/alpha
                                            #t
                                            backing-scale))
                                  (values filename
                                          (int->img-type type)
                                          #f
                                          1.0)))
                            (values filename
                                    (int->img-type type)
                                    #f
                                    1.0))])
            ;; the call to create an image-snip% object
            ;; here should match the way that super-make-object
            ;; is called in wxme/image.rkt
            (let ([snip (make-object image-snip% 
                                     (if (equal? loadfile #"")
                                         #f
                                         (if (bytes? loadfile)
                                             (bytes->path loadfile)
                                             loadfile))
                                     type
                                     (positive? relative) 
                                     inlined?
                                     backing-scale)])
              (send snip resize w h)
              (send snip set-offset dx dy)

              snip)))))))

;; ------------------------------------------------------------

;; old implementation prevented bitmap modifications while installed 
;; in an image snip
(define (marked-as-selected? bm) #f)
(define (mark-as-selected bm) (void))
(define (unmark-as-selected bm) (void))

(define black-color (make-object color% 0 0 0))

(define image-type?
  (symbol-in unknown unknown/mask unknown/alpha
             gif gif/mask gif/alpha
             jpeg png png/mask png/alpha
             xbm xpm bmp pict))

(define png-convertible<%>
  (interface* ()
              ([prop:convertible
                (lambda (img format default)
                  (cond
                    [(or (eq? format 'png-bytes)
                         (and (eq? format 'png@2x-bytes)
                              (= 2 (send (send img get-bitmap) get-backing-scale))))
                     (let ([s (open-output-bytes)])
                       (send (send img get-bitmap) save-file s 'png
                             #:unscaled? (eq? format 'png@2x-bytes))
                       (get-output-bytes s))]
                    [else default]))])))

(defclass* image-snip% internal-snip% (png-convertible<%>)
  (inherit-field s-admin
                 s-flags)
  (inherit set-snipclass)

  (init-rest args)

  (define filename #f)
  (define filetype 0) ; file != #f => type of file, otherwise loaded 1 => XBM and 2 => XPM
  (define bm #f)
  (define mask #f)
  (define is-relative-path? #f)

  (define w 0.0)
  (define h 0.0)
  (define vieww -1.0)
  (define viewh -1.0)
  (define viewdx 0.0)
  (define viewdy 0.0)
  (define contents-changed? #f)

  (super-new)

  (set-snipclass the-image-snip-class)

  (case-args
   args
   [([bitmap% bm] [(make-or-false bitmap%) [mask #f]])
    (set-bitmap bm mask)]
   [([(make-or-false (make-alts path-string? input-port?)) [name #f]]
     [image-type? [kind 'unknown]]
     [bool? [relative-path? #f]]
     [bool? [inline? #t]]
     [positive-real? [backing-scale 1.0]])
    (load-file name kind relative-path? inline? backing-scale)]
   (init-name 'bitmap%))

  (define/private (size-cache-invalid)
    (set! contents-changed? #t))

  (def/override (get-extent [dc<%> dc] [real? ex] [real? ey] 
                            [maybe-box? [wi #f]] [maybe-box? [hi #f]] 
                            [maybe-box? [descent #f]] [maybe-box? [space #f]]
                            [maybe-box? [lspace #f]] [maybe-box? [rspace #f]])
    (when contents-changed?
     (let-values ([(_w _h)
                   (if (and bm (send bm ok?))
                       (values
                        (if (vieww . < . 0)
                            (send bm get-width)
                            vieww)
                        (if (viewh . < . 0)
                            (send bm get-height)
                            viewh))
                       (values 0 0))])
       (set! w (if (zero? _w)
                   IMAGE-VOID-SIZE
                   _w))
       (set! h (if (zero? _h)
                   IMAGE-VOID-SIZE
                   _h))))
    (when wi (set-box! wi w))
    (when hi (set-box! hi h))
    (when descent
      (if (or (not bm)
              (not (send bm ok?)))
          (set-box! descent 1.0)
          (set-box! descent 0.0)))
    (when space (set-box! space 0.0))
    (when lspace (set-box! lspace 0.0))
    (when rspace (set-box! rspace 0.0)))

  (def/override (draw [dc<%> dc] [real? x] [real? y] 
                      [real? left] [real? top] [real? bottom] [real? right] 
                      [real? dx] [real? dy] [caret-status? caret])
    (if (or (not bm)
            (not (send bm ok?)))
        (begin
          (send dc draw-rectangle
                (+ x 1) (+ y 1)
                (- w 2) (- h 2))
          (send dc draw-line
                (+ x 1) (+ y 1)
                (+ x w -2) (+ y h -2))
          (send dc draw-line
                (+ x 1) (+ y h -2)
                (+ x w -2) (+ y 1)))
        (let ([msk (or mask
                       (let ([mask (send bm get-loaded-mask)])
                         (and mask
                              (send mask ok?)
                              (= w (send mask get-width))
                              (= h (send mask get-height))
                              mask)))]
              [alpha (send dc get-alpha)])
          (when (pair? caret)
            (send dc set-alpha (* 0.5 alpha)))
          (send dc draw-bitmap-section bm x y 0 0 w h
                'solid black-color msk)
          (when (pair? caret)
            (send dc set-alpha alpha)))))

  (def/override (copy)
    (let ([s (new image-snip%)])
      (do-copy-to s)
      s))

  (def/override (write [editor-stream-out% f])
    (send f put (if (path? filename)
                    (path->bytes filename)
                    #""))
    (let ([write-mode
           (if filename
               (begin
                 (send f put (img-type->int filetype))
                 #f)
               (cond
                [(not bm) (send f put 0) #f]
                [(= (send bm get-depth) 1)
                 (send f put 1)
                 'bm]
                [(= 1 (send bm get-backing-scale))
                 (send f put 2)
                 'pm]
                [else
                 (send f put 4)
                 'scaled-pm]))])
      (send f put vieww)
      (send f put viewh)
      (send f put viewdx)
      (send f put viewdy)
      (send f put (if is-relative-path? 1 0))
      
      (when write-mode
        ;; inline the image
        (let ([lenpos (send f tell)])
          (send f put-fixed 0)

          (when (eq? write-mode 'scaled-pm)
            (send f put (send bm get-backing-scale)))

          (let ([num-lines
                 (let-values ([(in out) (make-pipe)])
                   (send bm save-file out 'png #:unscaled? #t)
                   (close-output-port out)
                   (let loop ([numlines 0])
                     (let ([s (read-bytes IMG-MOVE-BUF-SIZE in)])
                       (if (eof-object? s)
                           numlines
                           (begin
                             (send f put-unterminated s)
                             (loop (add1 numlines)))))))])
            
            (let ([end (send f tell)])
              (send f jump-to lenpos)
              (send f put-fixed num-lines)
              (send f jump-to end)))))))
  
  (def/public (load-file [(make-or-false (make-alts path-string? input-port?)) [name #f]]
                         [image-type? [kind 'unknown]]
                         [bool? [rel-path? #f]]
                         [bool? [inline? #t]]
                         [positive-real? [backing-scale 1.0]])
    (do-set-bitmap #f #f #f)
    
    (let* ([rel-path? (and rel-path?
                           (path-string? name)
                           (relative-path? name))]
           [name (if rel-path?
                     name
                     (and name 
                          (if (path-string? name)
                              (path->complete-path name)
                              name)))])
      (set! s-flags
            (if rel-path?
                (add-flag s-flags USES-BUFFER-PATH)
                (remove-flag s-flags USES-BUFFER-PATH)))

      (let ([orig-name name]
            [name (and name
                       (path-string? name)
                       (if (string? name)
                           (string->path name)
                           name))])
        (unless inline?
          (set! filename name)
          (set! filetype kind))

        (when orig-name
          (let ([fullpath (if (input-port? orig-name)
                              orig-name
                              (if rel-path?
                                  (path->complete-path
                                   name
                                   (or (and s-admin
                                            (let ([e (send s-admin get-editor)])
                                              (and e
                                                   (let ([fn (send e get-filename)])
                                                     (and fn
                                                          (let-values ([(base name dir?) (split-path fn)])
                                                            (and (path? base)
                                                                 (path->complete-path base))))))))
                                       (current-directory)))
                                  name))])
            (let ([nbm (if s-admin
                           (send s-admin call-with-busy-cursor
                                 (lambda ()
                                   (make-object bitmap% fullpath kind)))
                           (make-object bitmap% fullpath kind #f #f backing-scale))])
              (when (send nbm ok?)
                (do-set-bitmap nbm #f #f))))))
      ;; for refresh:
      (set-bitmap bm mask)))

  (define/override (do-copy-to d)
    (if (d . is-a? . snip%)
        ((send d do-copy-to #f)
         filename
         filetype
         is-relative-path?
         vieww
         viewh
         viewdx
         viewdy
         bm mask)
        (lambda (-filename -filetype -relative-path?
                           -vieww -viewh -viewdx -viewdy
                           -bm -mask)
          (set! filename -filename)
          (set! filetype -filetype)
          (set! is-relative-path? -relative-path?)
          (set! vieww -vieww)
          (set! viewh -viewh)
          (set! viewdx -viewdx)
          (set! viewdy -viewdy)
          (set! bm -bm)
          (set! mask -mask)
          (mark-as-selected bm)
          (mark-as-selected mask))))

  (def/public (get-filename [maybe-box? [rel? #f]])
    (when rel?
      (set-box! rel? (and filename is-relative-path?)))
    filename)
  
  (def/public (get-filetype)
    (if filename
        filetype
        'unknown))

  (def/public (set-bitmap [(make-or-false bitmap%) map]
                          [(make-or-false bitmap%) [msk #f]])
    (do-set-bitmap map msk #t))

  (define/private (do-set-bitmap map msk refresh?)
    (unless (or (marked-as-selected? map)
                (marked-as-selected? msk))
      (unmark-as-selected bm)
      (unmark-as-selected mask)

      (set! bm #f)
      (set! mask #f)

      (let ([map (and map (send map ok?) map)]
            [msk (and msk (send msk ok?) msk)])
        (set! bm map)
        (set! mask msk)

        (mark-as-selected bm)
        (mark-as-selected msk))

      (when refresh?
        (set! contents-changed? #t)
        (when s-admin 
          (send s-admin resized this #t)))))

  (def/public (get-bitmap)
    bm)

  (def/public (get-bitmap-mask)
    mask)

  (def/override (other-equal-to? [image-snip% other] [any? recur])
    (let* ([bm (send this get-bitmap)]
           [bm2 (send other get-bitmap)])
      (and
       bm (send bm ok?)
       bm2 (send bm ok?)
       (= (send bm get-depth) (send bm2 get-depth))
       (let ([w (send bm get-width)]
             [h (send bm get-height)])
         (and
          (= w (send bm2 get-width))
          (= h (send bm2 get-height))
          (let ([s1 (make-bytes (* w h 4))]
                [s2 (make-bytes (* w h 4))])
            (send bm get-argb-pixels 0 0 w h s1 #f)
            (send bm2 get-argb-pixels 0 0 w h s2 #f)
            (let ([mask (send this get-bitmap-mask)])
              (when (and mask
                         (send mask ok?)
                         (= w (send mask get-width))
                         (= h (send mask get-height)))
                (send mask get-argb-pixels 0 0 w h s1 #t)))
            (let ([mask2 (send other get-bitmap-mask)])
              (when (and mask2
                         (send mask2 ok?)
                         (= w (send mask2 get-width))
                         (= h (send mask2 get-height)))
                (send mask2 get-argb-pixels 0 0 w h s2 #t)))
            (equal? s1 s2)))))))

  (define/private (do-hash-code hash-code)
    (if (and bm 
             (send bm ok?))
        (let ([w (send bm get-width)]
              [h (send bm get-height)])
          (let ([s1 (make-bytes (* w h 4))])
            (send bm get-argb-pixels 0 0 w h s1 #f)
            (when (and mask
                       (send mask ok?)
                       (= w (send mask get-width))
                       (= h (send mask get-height)))
              (send mask get-argb-pixels 0 0 w h s1 #t))
            (hash-code s1)))
        0))

  (def/override (equal-hash-code-of [any? recur])
    (do-hash-code equal-hash-code))
  (def/override (equal-secondary-hash-code-of [any? recur])
    (do-hash-code equal-secondary-hash-code))

  (def/public (set-offset [real? x] [real? y])
    (set! viewdx x)
    (set! viewdy y)
    (set! contents-changed? #t)
    (when s-admin
      (send s-admin needs-update this 0 0 w h)))

  (def/override (resize [real? w] [real? h])
    (set! vieww w)
    (set! viewh h)
    (set! contents-changed? #t)
    (when s-admin
      (send s-admin resized this #t))
    #t)

  (def/override (get-num-scroll-steps)
    (max (->long (/ h IMAGE-PIXELS-PER-SCROLL))
         1))
  
  (def/override (find-scroll-step [real? y])
    (->long (/ y IMAGE-PIXELS-PER-SCROLL)))

  (def/override (get-scroll-step-offset [exact-integer? i])
    (* i IMAGE-PIXELS-PER-SCROLL))

  (def/override (set-admin [(make-or-false snip-admin%) a])
    (when (not (eq? a s-admin))
      (super set-admin a))
    (when (and s-admin is-relative-path? filename)
      (load-file filename filetype #t))))


;; ------------------------------------------------------------

(defclass snip-class-list% object%
  (define ht (make-hash))
  (define pos-ht (make-hash))
  (define rev-pos-ht (make-hash))

  (super-new)

  (def/public (find [string? name])
    (let ([c (hash-ref ht name #f)])
      (or c
          (let ([c (get-snip-class name)])
            (when c (add c))
            c))))
  
  (def/public (find-position [snip-class% c])
    (hash-ref pos-ht c -1))
  
  (def/public (add [snip-class% c])
    (let ([name (send c get-classname)])
      (let ([old (hash-ref ht name #f)])
        (hash-set! ht name c)
        (let ([n (if old
                     (hash-ref pos-ht old)
                     (hash-count pos-ht))])
          (when old (hash-remove! pos-ht old))
          (hash-set! pos-ht c n)
          (hash-set! rev-pos-ht n c)))))
    
  (def/public (number) (hash-count ht))

  (def/public (nth [exact-nonnegative-integer? n]) (hash-ref rev-pos-ht n #f)))

(define snip-class-list<%> (class->interface snip-class-list%))

;; ------------------------------------------------------------

(define the-string-snip-class (new string-snip-class%))
(define the-tab-snip-class (new tab-snip-class%))
(define the-image-snip-class (new image-snip-class%))

(define-struct snip-class-link ([c #:mutable] [name #:mutable] [header-flag #:mutable] map-position reading-version))

(defclass standard-snip-class-list% snip-class-list%
  (inherit add
           number
           nth
           find)
  (super-new)

  (add the-string-snip-class)
  (add the-tab-snip-class)
  ;(add the-editor-snip-class)
  (add the-image-snip-class)

  (define/public (reset-header-flags s)
    (send s set-sl null)
    (send s set-dl null))

  (def/public (write [editor-stream-out% f])
    (let ([n (number)])
      (send f put n)
      (for ([i (in-range n)])
        (let ([c (nth i)])
          (send f put (string->bytes/utf-8 (send c get-classname)))
          (send f put (send c get-version))
          (send f put (if (send c get-s-required?) 1 0))

          (send f add-sl (make-snip-class-link c
                                               #f
                                               0
                                               i
                                               0)))))
    #t)

  (def/public (read [editor-stream-in% f])
    (let-boxes ([count 0])
        (send f get count)
      (for/and ([i (in-range count)])
        (let ([s (send f get-bytes)])
          (let-boxes ([version 0]
                      [required 0])
              (begin
                (send f get version)
                (send f get required))
            (and (send f ok?)
                 (send f add-sl (make-snip-class-link
                                 #f
                                 (bytes->string/utf-8 s #\?)
                                 0
                                 i
                                 version))
                 #t))))))

  (define/public (find-by-map-position f n)
    (ormap (lambda (s)
             (and (= n (snip-class-link-map-position s))
                  (begin
                    (when (snip-class-link-name s)
                      (let ([c (find (snip-class-link-name s))])
                        (cond
                          [(not c)
                           (log-error (format "unknown snip class: ~e (version: ~e)" 
                                              (snip-class-link-name s)
                                              (snip-class-link-reading-version s)))]
                          [((send c get-version) . < . (snip-class-link-reading-version s))
                           ;; unknown class/version;
                           ;; since we #f out sl->name, error is only shown once
                           (log-error (format "unknown snip class: ~e; found version: ~e, need at least version ~e" 
                                              (snip-class-link-name s)
                                              (send c get-version)
                                              (snip-class-link-reading-version s)))]
                          [else
                           ;; no prolems
                           (void)])
                        (set-snip-class-link-name! s #f)
                        (set-snip-class-link-c! s c)))
                    (snip-class-link-c s))))
           (send f get-sl))))

(define (make-the-snip-class-list)
  (new standard-snip-class-list%))

(define the-snip-class-list (make-parameter (make-the-snip-class-list)))

(define (get-the-snip-class-list)
  (the-snip-class-list))

;; ------------------------------------------------------------

(define snip->admin (class-field-accessor snip% s-admin))
(define snip->count (class-field-accessor snip% s-count))
(define snip->next (class-field-accessor snip% s-next))
(define snip->prev (class-field-accessor snip% s-prev))
(define snip->flags (class-field-accessor snip% s-flags))
(define snip->line (class-field-accessor snip% s-line))
(define snip->loc (class-field-accessor snip% s-line))
(define snip->style (class-field-accessor snip% s-style))
(define snip->snipclass (class-field-accessor snip% s-snipclass))

(define set-snip-admin! (class-field-mutator snip% s-admin))
(define set-snip-line! (class-field-mutator snip% s-line))
(define set-snip-loc! (class-field-mutator snip% s-line))
(define set-snip-style! (class-field-mutator snip% s-style))
(define set-snip-flags! (class-field-mutator snip% s-flags))
(define set-snip-count! (class-field-mutator snip% s-count))
(define set-snip-prev! (class-field-mutator snip% s-prev))
(define set-snip-next! (class-field-mutator snip% s-next))

(define snip%-get-text (generic snip% get-text))

;; install the getters:
(define get-snip-class
 (lambda (name)
   (load-one name 'snip-class snip-class%)))

(define readable-snip<%>
  (interface ()
    read-special))
