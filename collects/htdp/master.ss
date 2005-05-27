#cs(module master mzscheme
  (provide master)
  
  (require "error.ss"
           (lib "class.ss")
           (lib "class100.ss")
           (lib "mred.ss" "mred")
           (lib "prim.ss" "lang")
           (lib "etc.ss"))
  
  (define-higher-order-primitive master master/proc (compare-guess))
  
  #| ---------------------------------------------------------------------------
  The Basic Constants |#
  
  (define TITLE "TeachScheme Color Guessing")
  
  (define WELCOME "Welcome to the TeachScheme Color-Guessing Game") 
  
  (define COLORS
    (list 'black 'white 'red 'blue 'green 'gold 'pink 'orange 'purple 'navy))
  
  (define COL# (length COLORS))
  
  (define GUESSES# 2) 
  
  (define BUT-SIZE 30)
  (define WIDTH (* COL# BUT-SIZE))
  (define HIGHT BUT-SIZE)
  
  (define STOPS 
    (list 'PerfectGuess 'perfect_guess 'perfect! 'perfect 'Perfect 'perfekt 'Perfekt))
  
  (define TRUMPET
    (make-object bitmap% (build-path (collection-path "icons") "trumpet.xbm") 'xbm))
  
  #|
  cd ~.../plt/collects/icons 
  cp where/ever/trumpet.xbm .
  cvs update -A 
  cvs add -kb trumpet.xbm 
  cvs commit -m "added trumpet image"
  cvs tag -F exp trumpet.xbm
  cvs update -r exp
  |#
  
  #| ------------------------------------------------------------------------
  The Layout: (computed as a function of constants)
  
  ------------------------------------------------------------------
  |       
  |   CB1  CB2   CB3   CB4   .......   CB*   CB*   CB*   CB*   CB* 
  | 
  |          CB-GUESS1    ...    CB-GUESS*
  | 
  |                     ONE-MESSAGE
  ------------------------------------------------------------------
  
  Two horizontal panels: 
  the first one with all the colors (as buttons)
  the second is a sequence of colored buttons 
  |#
  
  (define frame (make-object frame% TITLE #f WIDTH HIGHT))
  
  (define verti (make-object vertical-panel% frame))
  
  (define panel (make-object horizontal-panel% verti))
  
  (define guess-panels
    (let ((p (make-object horizontal-panel% verti)))
      (build-list GUESSES# (lambda (i) (make-object horizontal-panel% p)))))
  
  (for-each (lambda (p) (send p set-alignment 'center 'center)) guess-panels)
  
  (define message-panel (make-object horizontal-panel% verti))
  (send message-panel set-alignment 'center 'center)
  
  (define message #f)
  (define (add-message!)
    (send message-panel change-children (lambda (x) null))
    (set! message (make-object message% WELCOME message-panel)))
  (define (add-winner!)
    (send message-panel change-children (lambda (x) null))
    (make-object message% TRUMPET message-panel)
    (make-object button% "New Game?" message-panel new-game))
  
  #| ------------------------------------------------------------------------
  Some additional functionality |#
  
  (define colored-button%
    (class100 button% (color:str parent call-back [_width BUT-SIZE] [_height BUT-SIZE])
      (private-field (width _width)
                     (height _height))
      (private
        (make-colored-bm
         (lambda (color:str)
           (let* ([bm (make-object bitmap% width height)]
                  [dc (make-object bitmap-dc% bm)])
             (send dc set-brush (make-object brush% color:str 'solid))
             (send dc draw-rectangle 0 0 width height)
             (send dc set-bitmap #f)
             bm))))
      (public
        (change-color
         (lambda (color:str)
           (send this set-label (make-colored-bm color:str))))) 
      (sequence
        (super-init (make-colored-bm color:str) parent call-back))
      ))
  
  
  (define (make-color-button color:sym)
    (let ((color:str (symbol->string color:sym)))
      (letrec ((this
                (lambda (x y)
                  (let* ((guess-button (pop!)))
                    (send guess-button change-color color:str)
                    (add-a-guess! color:sym)
                    (if (pair? guesses)
                        (send message set-label "Another guess, please!")
                        (let ((response (check-now!)))
                          (initialize-guesses)
                          (send message set-label (symbol->string response))
                          (when (memq response STOPS) (add-winner!))))))))
        (make-object colored-button% color:str panel this))))
  
  ;; master : (color-symbol color-symbol color-symbol color-symbol -> symbol) -> ???
  (define (master/proc cg)
    (check-proc 'master cg 4 'first 'arguments)
    (set! check-guess cg)
    (send frame show #t))
  
  #| ------------------------------------------------------------------------
  Setting up the buttons |#
  
  (for-each make-color-button COLORS)
  
  (define guess-buttons
    (map (lambda (p) (make-object colored-button% "gray" p void)) guess-panels))
  
  ;; ------------------------------------------------------------------------
  ;; State of Game
  
  (define choices null)
  (define (new-game . x)
    (add-message!)
    (set! choices
          (build-list GUESSES# (lambda (i) (list-ref COLORS (random COL#))))))
  (new-game)
  
  (define guesses null)
  (define (initialize-guesses)
    (set! guesses guess-buttons))
  (define (pop!)
    (when (null? guesses) (error 'TeachMind "can't happen"))
    (let ((g (car guesses)))
      (set! guesses (cdr guesses))
      g))
  (initialize-guesses)
  
  (define guessed-colors null)
  (define (add-a-guess! color:sym)
    (set! guessed-colors (cons color:sym guessed-colors)))
  (define (check-now!)
    (begin0
      (if (= GUESSES# 2)
          (apply check-guess (append choices (reverse! guessed-colors)))
          (check-guess choices (reverse! guessed-colors)))
      (set! guessed-colors null)))
  
  ;; ------------------------------------------------------------------------
  ;; Student Contribution
  
  (define check-guess #f))
