(module play-game racket
  (require "gui.rkt"
           "admin.rkt"
           "board.rkt"
           #;"moves.rkt"
           "die.rkt"
           racket/gui)
  
  (provide play-game)
  
  (define animation-frame%
    (class* frame% (game-observer<%>)
      (init-field eventspace)
      (super-new)
      (define boards (list (new-board)))
      (define dice '())
      (define colors '())
      (define index 0)
      (define main-hp (new horizontal-panel% (parent this)))
      (define left-vp (new vertical-panel% (parent main-hp) (stretchable-width #f)))
      (define blue-name (new name-message% (parent left-vp) (up? #f)))
      (define unused1 (new vertical-panel% (parent left-vp)))
      (define yellow-name (new name-message% (parent left-vp) (up? #t)))
      (define canvas (new board-canvas% (parent main-hp)))
      (define right-vp (new vertical-panel% (parent main-hp) (stretchable-width #f)))
      (define red-name (new name-message% (parent right-vp) (up? #f)))
      (define unused2 (new vertical-panel% (parent right-vp)))
      (define green-name (new name-message% (parent right-vp) (up? #t)))
      
      (define info-panel (new horizontal-panel% 
                              (parent this)
                              (alignment '(center center))
                              (stretchable-height #f)))
      (define dice-msg (new message% (parent info-panel) (label "")))
      (define die1 (new die% (parent info-panel)))
      (define die2 (new die% (parent info-panel)))
      (send dice-msg stretchable-width #t)
      (define bp (new horizontal-panel% 
                      (parent this)
                      (stretchable-height #f)))
      (define next (new button% 
                        (label "next")
                        (callback
                         (lambda (x y) 
                           (set! index (modulo (+ index 1) (length boards)))
                           (update)))
                        (parent bp)))
      (define prev (new button% 
                        (label "prev")
                        (callback
                         (lambda (x y) 
                           (set! index (modulo (- index 1) (length boards)))
                           (update)))
                        (parent bp)))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  observer methods (not called on the eventspace main thread)
      ;;
      
      (define/public (introduce color name)
        (queue-callback/sync
         (lambda ()
           (case color
             [(red) (send red-name set-text name)]
             [(blue) (send blue-name set-text name)]
             [(green) (send green-name set-text name)]
             [(yellow) (send yellow-name set-text name)]))))
      (define/public (game-over winner color) 
        (queue-callback/sync
         (lambda ()
           (new message% 
                (parent this) 
                (label (format "Winner ~a (~a)" winner color))))))
      (define/public (taking-turn color roll)
        (queue-callback/sync
         (lambda ()
           (set! colors (append colors (list color)))
           (set! dice (append dice (list roll))))))
      (define/public (took-turn color board)
        (queue-callback/sync
         (lambda ()
           (set! boards (append boards (list board)))
           (when (= index (- (length boards) 2))
             (set! index (+ index 1))
             (update)))))
      
      (define/private (update)
        (send canvas set-board (list-ref boards index))
        (cond
          [(< index (length dice))
           (send dice-msg set-label (format "Turn ~a (~a)" index (list-ref colors index)))
           (send die1 set-digit (car (list-ref dice index)))
           (send die2 set-digit (cadr (list-ref dice index)))]
          [else
           (send dice-msg set-label (format "Turn ~a" index))
           (send die1 set-digit #f)
           (send die2 set-digit #f)]))
  
      (define/private (queue-callback/sync t)
        (parameterize ([current-eventspace eventspace])
          (let ([s (make-semaphore 0)])
            (queue-callback
             (lambda ()
               (t)
               (semaphore-post s)))
            (semaphore-wait s))))))
  
  (define name-message%
    (class canvas%
      (init-field up?)
      (inherit min-width min-height get-dc)
      (define txt "")
      (define/public (set-text l) 
        (set! txt l)
        (update-min-size)
        (on-paint))
      (define/override (on-paint)
        (let ([dc (get-dc)])
          (let-values ([(w h _1 _2) (send dc get-text-extent txt)])
            (if up?
                (send dc draw-text txt 0 w #f 0 (/ pi 2))
                (send dc draw-text txt h 0 #f 0 (- (/ pi 2)))))))
      (define/private (update-min-size)
        (let ([dc (get-dc)])
          (let-values ([(w h _1 _2) (send dc get-text-extent txt)])
            (min-width (ceiling (inexact->exact h)))
            (min-height (ceiling (inexact->exact w))))))
      (super-new 
       (stretchable-width #f)
       (stretchable-height #f))))
  
  (define (play-game players)
    (define game (new game%))
    (define esp (make-eventspace))
    (define af (parameterize ([current-eventspace esp])
                 (new animation-frame% (label "Parcheesi") (eventspace esp))))
    (send game set-observer af)
    (for-each (lambda (player) (send game register player)) players)
    (send af show #t)
    (send game start))

  #;
  (begin
    (require "best-player.rkt") 
    (play-game (list (new first-player%) 
                     (new last-player%) 
                     (new first-player%) 
                     (new last-player%)))))
