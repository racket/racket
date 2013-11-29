#lang racket/base
(require racket/class
         "../syntax.rkt"
         "wx.rkt")

(provide keymap%
         map-command-as-meta-key)

(define map-command-as-meta? #f)

(define/top (map-command-as-meta-key [bool? v])
  (set! map-command-as-meta? v))

(define (as-meta-key k)
  (case (system-type)
    [(macosx) (if map-command-as-meta?
                  k
                  #f)]
    [else k]))

(define (as-cmd-key k)
  (case (system-type)
    [(macosx) k]
    [else #f]))

(define keylist
  #hash(("leftbutton" . mouse-left)
        ("rightbutton" . mouse-right)
        ("middlebutton" . mouse-middle)
        ("leftbuttondouble" . mouse-left-double)
        ("rightbuttondouble" . mouse-right-double)
        ("middlebuttondouble" . mouse-middle-double)
        ("leftbuttontriple" . mouse-left-triple)
        ("rightbuttontriple" . mouse-right-triple)
        ("middlebuttontriple" . mouse-middle-triple)
        ("leftbuttonseq" . mouse-left)
        ("rightbuttonseq" . mouse-right)
        ("middlebuttonseq" . mouse-middle)
        ("wheelup" . wheel-up)
        ("wheeldown" . wheel-down)
        ("wheelleft" . wheel-left)
        ("wheelright" . wheel-right)
        ("esc" . escape) 
        ("delete" . #\rubout)
        ("del" . #\rubout)
        ("insert" . insert)
        ("ins" . insert)
        ("add" . add)
        ("subtract" . subtract)
        ("multiply" . multiply)
        ("divide" . divide)
        ("backspace" . #\backspace)
        ("back" . #\backspace)
        ("return" . #\return)
        ("enter" . #\return)
        ("tab" . #\tab)
        ("space" . #\space)
        ("right" . right)
        ("left" . left)
        ("up" . up)
        ("down" . down)
        ("home" . home)
        ("end" . end)
        ("pageup" . prior)
        ("pagedown" . next)
        ("semicolon" . #\;)
        ("colon" . #\:)
        ("numpad0" . numpad0)
        ("numpad1" . numpad1)
        ("numpad2" . numpad2)
        ("numpad3" . numpad3)
        ("numpad4" . numpad4)
        ("numpad5" . numpad5)
        ("numpad6" . numpad6)
        ("numpad7" . numpad7)
        ("numpad8" . numpad8)
        ("numpad9" . numpad9)
        ("numpadenter" . #\u3)
        ("f1" . f1)
        ("f2" . f2)
        ("f3" . f3)
        ("f4" . f4)
        ("f5" . f5)
        ("f6" . f6)
        ("f7" . f7)
        ("f8" . f8)
        ("f9" . f9)
        ("f10" . f10)
        ("f11" . f11)
        ("f12" . f12)
        ("f13" . f13)
        ("f14" . f14)
        ("f15" . f15)
        ("f16" . f16)
        ("f17" . f17)
        ("f18" . f18)
        ("f19" . f19)
        ("f20" . f20)
        ("f21" . f21)
        ("f22" . f22)
        ("f23" . f23)
        ("f24" . f24)))
(define rev-keylist
  (make-immutable-hash
   (hash-map keylist (lambda (k v) (cons v k)))))

(define-struct kmfunc (name f))

(define-struct key (code

                    shift-on?
                    shift-off?
                    ctrl-on?
                    ctrl-off?
                    alt-on?
                    alt-off?
                    meta-on?
                    meta-off?
                    cmd-on?
                    cmd-off?
                    caps-on?
                    caps-off?

                    score

                    check-other?
                    fullset?

                    [fname #:mutable]

                    isprefix?
                    seqprefix))

(define-local-member-name 
  chain-handle-key-event
  get-best-score
  chain-handle-mouse-event
  get-best-mouse-score
  cycle-check)

(defclass keymap% object%
  
  (super-new)

  (define functions (make-hash))
  (define keys (make-hash))

  (define prefix #f)
  (define prefixed? #f)

  (define active-mouse-function #f)

  (define grab-key-function #f)
  (define grab-mouse-function #f)
  (define on-break #f)

  (define chain-to null)

  (define last-time 0)
  (define last-x 0)
  (define last-y 0)
  (define click-count 0)
  (define last-code #f)
  (define last-button #f)

  (define double-interval (get-double-click-threshold))

  (def/public (reset)
    (set! prefix #f)
    (set! prefixed? #f)

    (for-each (lambda (c)
                (send c reset))
              chain-to))

  (def/public (break-sequence)
    (set! prefix #f)

    (when on-break
      (let ([f on-break])
        (set! on-break #f)
        (f)))
    
    (for-each (lambda (c)
                (send c break-sequence))
              chain-to))

  (def/public (set-break-sequence-callback [(make-procedure 0) f])
    (let ([old on-break])
      (set! on-break f)
      (when old (old))))

  (define/private (find-key code other-code alt-code other-alt-code caps-code
                            shift? ctrl? alt? meta? cmd? caps?
                            prefix)
    (for*/fold ([best-key #f]
                [best-score -1])
        ([findk (in-list (list code other-code alt-code other-alt-code caps-code))]
         [key (in-list (hash-ref keys findk null))])
      (if (and (or (eqv? (key-code key) code)
                   (and (key-check-other? key)
                        (or (eqv? (key-code key) other-code)
                            (eqv? (key-code key) alt-code)
                            (eqv? (key-code key) other-alt-code)
                            (eqv? (key-code key) caps-code))))
               (or (and (key-shift-on? key) shift?)
                   (and (key-shift-off? key) (not shift?))
                   (and (not (key-shift-on? key)) (not (key-shift-off? key))))
               (or (and (key-ctrl-on? key) ctrl?)
                   (and (key-ctrl-off? key) (not ctrl?))
                   (and (not (key-ctrl-on? key)) (not (key-ctrl-off? key))))
               (or (and (key-alt-on? key) alt?)
                   (and (key-alt-off? key) (not alt?))
                   (and (not (key-alt-on? key)) (not (key-alt-off? key))))
               (or (and (key-meta-on? key) meta?)
                   (and (key-meta-off? key) (not meta?))
                   (and (not (key-meta-on? key)) (not (key-meta-off? key))))
               (or (and (key-cmd-on? key) cmd?)
                   (and (key-cmd-off? key) (not cmd?))
                   (and (not (key-cmd-on? key)) (not (key-cmd-off? key))))
               (or (and (key-caps-on? key) caps?)
                   (and (key-caps-off? key) (not caps?))
                   (and (not (key-caps-on? key)) (not (key-caps-off? key))))
               (eq? (key-seqprefix key) prefix))
          (let ([score (+ (key-score key)
                          (if (eqv? (key-code key) code)
                              0
                              (if (eqv? (key-code key) other-alt-code)
                                  -4
                                  -2)))])
            (if (score . > . best-score)
                (values key score)
                (values best-key best-score)))
          (values best-key best-score))))

  (define/private (do-map-function code shift ctrl alt meta cmd caps check-other?
                                   fname prev isprefix? fullset?)
    ;; look for existing key mapping:
    (let ([key
           (ormap (lambda (key)
                    (and (eqv? (key-code key) code)
                         (eq? (key-shift-on? key) (shift . > . 0))
                         (eq? (key-shift-off? key) (shift . < . 0))
                         (eq? (key-ctrl-on? key) (ctrl . > . 0))
                         (eq? (key-ctrl-off? key) (ctrl . < . 0))
                         (eq? (key-alt-on? key) (alt . > . 0))
                         (eq? (key-alt-off? key) (alt . < . 0))
                         (eq? (key-meta-on? key) (meta . > . 0))
                         (eq? (key-meta-off? key) (meta . < . 0))
                         (eq? (key-cmd-on? key) (cmd . > . 0))
                         (eq? (key-cmd-off? key) (cmd . < . 0))
                         (eq? (key-caps-on? key) (caps . > . 0))
                         (eq? (key-caps-off? key) (caps . < . 0))
                         (eq? (key-check-other? key) check-other?)
                         (eq? (key-seqprefix key) prev)
                         key))
                  (hash-ref keys code null))])

      (if key
          ;; Found existing
          (if (not (eq? isprefix? (key-isprefix? key)))
              ;; prefix vs no-prefix mismatch:
              (let ([s
                     (string-append
                      (if (meta . > . 0) "m:" "")
                      (if (meta . < . 0) "~m:" "")
                      (if (cmd . > . 0) "d:" "")
                      (if (cmd . < . 0) "~d:" "")
                      (if (alt . > . 0) "a:" "")
                      (if (alt . < . 0) "~a:" "")
                      (if (ctrl . > . 0) "c:" "")
                      (if (ctrl . < . 0) "~c:" "")
                      (if (shift . > . 0) "s:" "")
                      (if (shift . < . 0) "~s:" "")
                      (if (caps . > . 0) "l:" "")
                      (if (caps . < . 0) "~l:" "")
                      (or (hash-ref rev-keylist code)
                          (format "~c" code)))])
                (error (method-name 'keymap% 'map-function)
                       "~s is already mapped as a ~aprefix key"
                       s (if isprefix? "non-" "")))
              (begin
                (set-key-fname! key (string->immutable-string fname))
                key))
          ;; Create new
          (let ([newkey (make-key
                         code
                         (shift . > . 0) (shift . < . 0)
                         (ctrl . > . 0) (ctrl . < . 0)
                         (alt . > . 0) (alt . < . 0)
                         (meta . > . 0) (meta . < . 0)
                         (cmd . > . 0) (cmd . < . 0)
                         (caps . > . 0) (caps . < . 0)
                         (+ (if (shift . > . 0) 1 0)
                            (if (shift . < . 0) 5 0)
                            (if (ctrl . > . 0) 1 0)
                            (if (ctrl . < . 0) 5 0)
                            (if (alt . > . 0) 1 0)
                            (if (alt . < . 0) 5 0)
                            (if (meta . > . 0) 1 0)
                            (if (meta . < . 0) 5 0)
                            (if (cmd . > . 0) 1 0)
                            (if (cmd . < . 0) 5 0)
                            (if (caps . > . 0) 1 0)
                            (if (caps . < . 0) 5 0)
                            (if check-other? 6 30))
                         check-other?
                         fullset?
                         (string->immutable-string fname)
                         isprefix?
                         prev)])
            (hash-set! keys code (cons newkey (hash-ref keys code null)))
            newkey))))

  (define/private (get-code str)
    (let ([code (hash-ref keylist (string-downcase str) #f)])
      (if code
          (values code (member str '("leftbuttonseq"
                                     "middlebuttonseq"
                                     "rightbuttonseq")))
          (if (= 1 (string-length str))
              (values (string-ref str 0)
                      #f)
              (values #f #f)))))

  (def/public (map-function [string? keys]
                            [string? fname])
    (if (string=? keys "")
        (error (method-name 'keymap% 'map-function)
               "bad key string: ~e"
               keys)
        (let loop ([seq (regexp-split #rx";" keys)]
                   [prev-key #f])
          (let ([str (car seq)])
            (define (bad-string msg)
              (error (method-name 'keymap% 'map-function)
                     "bad keymap string: ~e~a: ~a"
                     str
                     (if (equal? str keys)
                         ""
                         (format " within ~e" keys))
                     msg))
            (let-values ([(str default-off?)
                          (if (regexp-match? #rx"^:" str)
                              (values (substring str 1) #t)
                              (values str #f))])
              (let sloop ([str str]
                          [downs null]
                          [ups null]
                          [others? #f])
                (cond
                 [(regexp-match? #rx"^[?]:" str)
                  (sloop (substring str 2) downs ups #t)]
                 [(regexp-match? #rx"^~[SsCcAaMmDdLl]:" str)
                  (let ([c (char-downcase (string-ref str 1))])
                    (if (memv c downs)
                        (bad-string (format "inconsistent ~a: modifier state" c))
                        (sloop (substring str 3) downs (cons c ups) others?)))]
                 [(regexp-match? #rx"^[SsCcAaMmDdLl]:" str)
                  (let ([c (char-downcase (string-ref str 0))])
                    (if (memv c ups)
                        (bad-string (format "inconsistent ~a: modifier state" c))
                        (sloop (substring str 2) (cons c downs) ups others?)))]
                 [else
                  (let-values ([(code fullset?) (get-code str)])
                    (if (not code)
                        (bad-string "unrecognized key name")
                        (let-values ([(downs code)
                                      (if (and (char? code)
                                               ((char->integer code) . > . 0)
                                               ((char->integer code) . < . 127)
                                               (char-alphabetic? code))
                                          (cond
                                           [(memq #\s downs)
                                            (if (or (and (eq? (system-type) 'macosx)
                                                         (not (memq #\m downs))
                                                         (not (memq #\d downs)))
                                                    (and (eq? (system-type) 'windows)
                                                         (or (not (memq #\c downs))
                                                             (memq #\m downs))))
                                                (values downs (char-upcase code))
                                                (values downs code))]
                                           [(char-upper-case? code)
                                            (values (cons #\s downs) code)]
                                           [else
                                            (values downs code)])
                                          (values downs code))])
                          (let ([newkey
                                 (let ([modval (lambda (c [default-off? default-off?])
                                                 (cond
                                                  [(memq c downs) 1]
                                                  [(memq c ups) -1]
                                                  [else (if default-off? -1 0)]))])
                                   (do-map-function code
                                                    (modval #\s)
                                                    (modval #\c)
                                                    (modval #\a)
                                                    (modval #\m)
                                                    (modval #\d)
                                                    (modval #\l #f)
                                                    others?
                                                    fname
                                                    prev-key
                                                    (not (null? (cdr seq)))
                                                    fullset?))])
                            (if (null? (cdr seq))
                                (void)
                                (loop (cdr seq) newkey))))))])))))))

  (define/private (handle-event code other-code alt-code other-alt-code caps-code
                                shift? ctrl? alt? meta? cmd? caps?
                                score)
    (let-values ([(key found-score)
                  (find-key code other-code alt-code other-alt-code caps-code
                            shift? ctrl? alt? meta? cmd? caps? prefix)])
      (set! prefix #f)

      (if (and key (found-score . >= . score))
          (if (key-isprefix? key)
              (begin
                (set! prefix key)
                (values #t #f #f))
              (values #t
                      (key-fname key)
                      (key-fullset? key)))
          (values #f #f #f))))

  (define/public (get-best-score code other-code alt-code other-alt-code caps-code
                                 shift? ctrl? alt? meta? cmd? caps?)
    (let-values ([(key score)
                  (find-key code other-code alt-code other-alt-code caps-code
                            shift? ctrl? alt? meta? cmd? caps? prefix)])
      (for/fold ([s (if key score -1)])
          ([c (in-list chain-to)])
        (max s
             (send c get-best-score code other-code alt-code other-alt-code caps-code
                   shift? ctrl? alt? meta? cmd? caps?)))))

  (def/public (set-grab-key-function [(make-procedure 4) grab])
    (set! grab-key-function grab))
  
  (def/public (remove-grab-key-function)
    (set! grab-key-function #f))

  (def/public (handle-key-event [any? obj] [key-event% event])
    (let ([code (send event get-key-code)])
      (or (eq? code 'shift)
          (eq? code 'rshift)
          (eq? code 'control)
          (eq? code 'rcontrol)
          (eq? code 'release)
          (let ([score (get-best-score 
                        code
                        (send event get-other-shift-key-code)
                        (send event get-other-altgr-key-code)
                        (send event get-other-shift-altgr-key-code)
                        (send event get-other-caps-key-code)
                        (send event get-shift-down)
                        (send event get-control-down)
                        (send event get-alt-down)
                        (as-meta-key (send event get-meta-down))
                        (as-cmd-key (send event get-meta-down))
                        (send event get-caps-down))])
            (let ([was-prefixed? prefixed?])
              
              (let* ([r (chain-handle-key-event obj event #f prefixed? score)]
                     [r (if (and (zero? r)
                                 was-prefixed?)
                            (begin
                              (reset)
                              ;; try again without prefix:
                              (chain-handle-key-event obj event #f #f score))
                            r)])
                (when (r . >= . 0)
                  (reset))
                (not (zero? r))))))))

  (define/private (other-handle-key-event obj event grab try-prefixed? score)
    (for/fold ([r 0])
        ([c (in-list chain-to)]
         #:when (r . <= . 0))
      (let ([r2 (send c chain-handle-key-event obj event grab try-prefixed? score)])
        (if (r2 . > . 0)
            (begin
              (reset)
              r2)
            (if (r2 . < . 0)
                r2
                r)))))

  (define/public (chain-handle-key-event obj event grab only-prefixed? score)
    ;; results: 0 = no match, 1 = match, -1 = matched prefix
    (set! last-time (send event get-time-stamp))
    (set! last-button #f)
    (let ([grab (or grab-key-function
                    grab)])
      (if (and only-prefixed? (not prefixed?))
          0
          (let ([sub-result (other-handle-key-event obj event grab only-prefixed? score)])
            (if (sub-result . > . 0)
                sub-result
                (let-values ([(h? fname fullset?)
                              (handle-event (send event get-key-code)
                                            (send event get-other-shift-key-code)
                                            (send event get-other-altgr-key-code)
                                            (send event get-other-shift-altgr-key-code)
                                            (send event get-other-caps-key-code)
                                            (send event get-shift-down)
                                            (send event get-control-down)
                                            (send event get-alt-down)
                                            (as-meta-key (send event get-meta-down))
                                            (as-cmd-key (send event get-meta-down))
                                            (send event get-caps-down)
                                            score)])
                  (if h?
                      (if fname
                          (begin
                            (reset)
                            (if (and grab
                                     (grab fname this obj event))
                                1
                                (if (call-function fname obj event)
                                    1
                                    0)))
                          (if prefix
                              (begin
                                (set! prefixed? #t)
                                -1)
                              ;; shouldn't get here
                              0))
                      (let ([result
                             (if (sub-result . < . 0)
                                 (begin
                                   (set! prefixed? #t)
                                   -1)
                                 0)])
                        (if (and (zero? result)
                                 grab-key-function
                                 (grab-key-function #f this obj event))
                            1
                            result)))))))))

  (def/public (set-grab-mouse-function [(make-procedure 4) grab])
    (set! grab-mouse-function grab))

  (def/public (remove-grab-mouse-function)
    (set! grab-mouse-function #f))

  (define/private (adjust-button-code code click-count)
    (case click-count
      [(0) code]
      [(1) (case code
             [(mouse-right) 'mouse-right-double]
             [(mouse-left) 'mouse-left-double]
             [(mouse-middle) 'mouse-middle-double])]
      [else (case code
              [(mouse-right) 'mouse-right-triple]
              [(mouse-left) 'mouse-left-triple]
              [(mouse-middle) 'mouse-middle-triple])]))

  (def/public (handle-mouse-event [any? obj][mouse-event% event])
    (let ([score (get-best-mouse-score event)])
      (not (zero? (chain-handle-mouse-event obj event #f 0 score)))))

  (define/public (get-best-mouse-score event)
    (cond
     [(not (send event button-down?))
      (if active-mouse-function
          100
          (or (ormap (lambda (c)
                       (and (not (zero? (send c get-best-mouse-score event)))
                            100))
                     chain-to)
              -1))]
     [else
      (let ([code (cond
                   [(send event get-right-down) 'mouse-right]
                   [(send event get-left-down) 'mouse-left]
                   [(send event get-middle-down) 'mouse-middle]
                   [else #f])])
        (if (not code)
            -1
            (let ([code
                   (if (and (eq? code last-button)
                            (= (send event get-x) last-x)
                            (= (send event get-y) last-y)
                            ((abs (- (send event get-time-stamp) last-time)) . < . double-interval))
                       (adjust-button-code code click-count)
                       code)])
              (get-best-score code #f #f #f #f
                              (send event get-shift-down)
                              (send event get-control-down)
                              (send event get-alt-down)
                              (as-meta-key (send event get-meta-down))
                              (as-cmd-key (send event get-meta-down))
                              (send event get-caps-down)))))]))

  (define/private (other-handle-mouse-event obj event grab try-state score)
    (for/fold ([result 0])
        ([c (in-list chain-to)]
         #:when (result . <= . 0))
      (let ([r (send c chain-handle-mouse-event obj event grab try-state score)])
        (cond
         [(r . > . 0)
          (reset)
          r]
         [(zero? r) result]
         [else r]))))

  (define/public (chain-handle-mouse-event obj event grab try-state score)
    (let ([grab (or grab-mouse-function grab)])
      (define (step1)
        (cond
         [(and (not prefix)
               (try-state . >= . 0))
          (let ([r (other-handle-mouse-event obj event grab 1 score)])
            (cond
             [(r . > . 0) r]
             [(try-state . > . 0) r]
             [else (step2 -1)]))]
         [(and prefix (try-state . < . 0))
          (other-handle-mouse-event obj event grab -1 score)]
         [else (step2 try-state)]))
      (define (step2 try-state)
        (cond
         [(not (send event button-down?))
          (when (and (not (send event dragging?))
                     (not (send event button-up?)))
            ;; we must have missed the button-up
            (set! active-mouse-function #f))
          (if (not active-mouse-function)
              (other-handle-mouse-event obj event grab -1 score)
              (let ([v (if (and grab 
                                (grab active-mouse-function this obj event))
                           1
                           (if (call-function active-mouse-function obj event)
                               1
                               0))])
                (when (send event button-up?)
                  (set! active-mouse-function #f))
                v))]
         [else
          (let ([code (cond
                       [(send event get-right-down) 'mouse-right]
                       [(send event get-left-down) 'mouse-left]
                       [(send event get-middle-down) 'mouse-middle]
                       [else #f])])
            (if (not code)
                0 ;; FIXME: should we call grab here?
                (let ([orig-code code]
                      [code
                       (if (and (eq? code last-button)
                                (= (send event get-x) last-x)
                                (= (send event get-y) last-y))
                           (if ((abs (- (send event get-time-stamp) last-time)) . < . double-interval)
                               (begin0
                                (adjust-button-code code click-count)
                                (set! click-count (add1 click-count)))
                               (begin
                                 (set! click-count 1)
                                 code))
                           (begin
                             (set! last-button code)
                             (set! click-count 1)
                             code))])
                  (set! last-time (send event get-time-stamp))
                  (set! last-x (send event get-x))
                  (set! last-y (send event get-y))

                  (let loop ([code code])
                    (let-values ([(h? fname fullset?) (handle-event code
                                                                    #f #f #f #f
                                                                    (send event get-shift-down)
                                                                    (send event get-control-down)
                                                                    (send event get-alt-down)
                                                                    (as-meta-key (send event get-meta-down))
                                                                    (as-cmd-key (send event get-meta-down))
                                                                    (send event get-caps-down)
                                                                    score)])
                      (cond
                       [(and h? fname)
                        (reset)
                        (when fullset?
                          (set! active-mouse-function fname))
                        (cond
                         [(and grab (grab fname this obj event)) 1]
                         [(call-function fname obj event) 1]
                         [else 0])]
                       [h?
                        (let ([r (other-handle-mouse-event obj event grab try-state score)])
                          (if (r . > . 0)
                              r
                              -1))]
                       [else
                        (set! last-code code)
                        (if (not (eqv? last-code orig-code))
                            (loop orig-code)
                            (let ([result (other-handle-mouse-event obj event grab try-state score)])
                              (if (and (zero? result)
                                       grab-mouse-function
                                       (grab-mouse-function #f this obj event))
                                  1
                                  result)))]))))))]))
      (step1)))

  (def/public (add-function [string? name] [(make-procedure 2) f])
    (hash-set! functions
               (string->immutable-string name)
               f))

  (def/public (call-function [string? name] [any? obj] [event% event] [any? [try-chained? #f]])
    (let ([f (hash-ref functions name #f)])
      (cond
       [f 
        (f obj event)
        #t]
       [try-chained?
        (ormap (lambda (c)
                 (send c call-function name obj event #t))
               chain-to)]
       [else
        (error 'keymap "no function ~e" name)])))
  
  (def/public (is-function-added? [string? name])
    (and (hash-ref functions name #f) #t))

  (def/public (get-double-click-interval)
    double-interval)

  (def/public (set-double-click-interval [exact-positive-integer? d])
    (set! double-interval d))

  (define/public (cycle-check km)
    (ormap (lambda (c)
             (or (eq? km c)
                 (send c cycle-check km)))
           chain-to))

  (def/public (chain-to-keymap [keymap% km] [any? prefix?])
    (unless (or (eq? km this)
                (cycle-check km)
                (send km cycle-check this))
      (set! chain-to (if prefix?
                         (cons km chain-to)
                         (append chain-to (list km))))))

  (def/public (remove-chained-keymap [keymap% km])
    (set! chain-to (remq km chain-to))))
