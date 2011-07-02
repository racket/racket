#lang mzscheme

(require games/cards mred mzlib/class mzlib/list mzlib/file mzlib/unit
         "../show-scribbling.rkt")

(define (list-first-n l n)
  (if (zero? n)
    null
    (cons (car l) (list-first-n (cdr l) (sub1 n)))))
(define (vector-copy v)
  (list->vector (vector->list v)))

(provide game@)
(define game@ (unit (import) (export)

(define t (make-table "Spider" 11 6))

(define num-suits (get-preference 'spider:num-suits (lambda () 2)))

(define (make-spider-deck)
  (let-values ([(suits copies)
                (case num-suits
                  [(1) (values '(spades) 4)]
                  [(2) (values '(spades hearts) 2)]
                  [(4) (values '(spades hearts clubs diamonds) 1)])])
    (let ([l (filter (lambda (c) (memq (send c get-suit) suits)) (make-deck))])
      (let loop ([n (* 2 copies)])
        (if (zero? n)
          null
          (append (map (lambda (c) (send c copy)) l) (loop (sub1 n))))))))

(define deck (make-spider-deck))

(define draw-pile deck)

(define CARD-WIDTH (send (car deck) card-width))
(define CARD-HEIGHT (send (car deck) card-height))

(define dx (quotient CARD-WIDTH 11))
(define dy dx)

(define stacks (make-vector 10 null))
(define dones (make-vector 8 null))

(define done-count 0)

(define old-states null)

(define-struct state (draw-pile stacks dones done-count face-down?s))

(define mb (make-object menu-bar% t))

(define file-menu (make-object menu% "&File" mb))

(new menu-item%
     [label "&Reset Game..."]
     [parent file-menu]
     [callback
      (lambda (i e)
        (when (eq? 'yes (message-box "Reset Game"
                                     "Are you sure you want to reset the game?"
                                     t
                                     '(yes-no)))
          (reset-game!)))])

(new separator-menu-item% [parent file-menu])

(new menu-item%
     [label "&Close"]
     [parent file-menu]
     [shortcut #\W]
     [callback (lambda (i e) (send t show #f))])

(define edit-menu (make-object menu% "&Edit" mb))

(define undo
  (new menu-item% 
       [label "&Undo"]
       [parent edit-menu]
       [shortcut #\Z]
       [callback (lambda (i e) (pop-state!))]))

(new separator-menu-item%  [parent edit-menu])

(new menu-item% 
     [label "&Options..."]
     [parent edit-menu]
     [callback (lambda (i e)
                 (define d
                   (new dialog%
                        [label "Spider Options"]
                        [parent t]
                        [stretchable-width #f]
                        [stretchable-height #f]))
                 (define suits
                   (new radio-box%
                        [label #f]
                        [parent (new group-box-panel%
                                     [parent d]
                                     [label "Number of Suits"]
                                     [stretchable-width #f]
                                     [stretchable-height #f])]
                        [choices '("1 (easiest)" "2" "4 (hardest)")]))
                 (define bottom-panel
                   (new horizontal-panel%
                        [parent d]
                        [alignment '(right center)]
                        [stretchable-height #f]))
                 (new button%
                      [parent bottom-panel]
                      [label "&Cancel"]
                      [callback (lambda (b e) (send d show #f))])
                 (new button%
                      [parent bottom-panel]
                      [label "&Ok"]
                      [style '(border)]
                      [callback (lambda (b e)
                                  (let ([n (expt 2 (send suits get-selection))])
                                    (if (not (= n num-suits))
                                      (when (eq? 'yes 
                                                 (message-box "Warning"
                                                              "Reset the game for new suit count?"
                                                              d
                                                              '(yes-no)))
                                        (set! num-suits n)
                                        (put-preferences '(spider:num-suits) (list n))
                                        (send d show #f)
                                        (reset-game!))
                                      (send d show #f))))])
                 (send suits set-selection (case num-suits [(1) 0][(2) 1][(4) 2]))
                 (send d center)
                 (send d show #t))])

      (define help (show-scribbling '(lib "games/scribblings/games.scrbl") 
                                    "spider"))
      (new menu-item%
	   [label "&Rules"]
	   [parent (make-object menu% "&Help" mb)]
	   [callback (lambda (i e) (help))])

(define (push-state!)
  (when (null? old-states)
    (send undo enable #t))
  (set! old-states
        (cons (make-state draw-pile
                          (vector-copy stacks)
                          (vector-copy dones)
                          done-count
                          (map (lambda (c) (send c face-down?)) deck))
              old-states)))

(define (pop-state!)
  (let ([state (car old-states)])
    (send t begin-card-sequence)
    (set! old-states (cdr old-states))
    (set! draw-pile (state-draw-pile state))
    (set! stacks (state-stacks state))
    (set! dones (state-dones state))
    (set! done-count (state-done-count state))
    (for-each (lambda (c fd?)
                (send c user-can-move #f)
                (unless (eq? (send c face-down?) fd?) (send c flip)))
              deck (state-face-down?s state))
    (send t move-cards draw-pile dx dy)
    (send t stack-cards draw-pile)
    (let loop ([i 0])
      (unless (= i (vector-length stacks))
        (send t stack-cards (vector-ref stacks i))
        (loop (add1 i))))
    (let loop ([i 0])
      (unless (= i (vector-length dones)) (move-dones i) (loop (add1 i))))
    (shift-stacks)
    (when (null? old-states) (send undo enable #f))
    (send t end-card-sequence)))

(define (find-stack find)
  (let loop ([i 0])
    (if (= i (vector-length stacks))
      #f
      (let ([l (vector-ref stacks i)])
        (if (and (pair? l) (memq find l))
          i
          (loop (add1 i)))))))

(define (remove-from-stack! cards)
  (let* ([i (find-stack (car cards))]
         [l (vector-ref stacks i)])
    (vector-set! stacks i (list-tail l (length cards)))))

(define (stacked-cards card)
  (let ([i (find-stack card)])
    (if i
      (reverse (let loop ([l (vector-ref stacks i)])
                 (cond [(not (send (car l) user-can-move)) null]
                       [(eq? (car l) card) (list card)]
                       [else (cons (car l) (loop (cdr l)))])))
      #f)))

(define (drag-ok? cards i)
  (let ([c (car cards)]
        [l (vector-ref stacks i)])
    (and l
         (or (null? l)
             (= (send (car l) get-value)
                (add1 (send c get-value)))))))

(let loop ([i 0])
  (unless (= i (vector-length stacks))
    null
    (let ([r (make-region (+ dx (* i (+ CARD-WIDTH dx)))
                          (+ dy CARD-HEIGHT dy)
                          CARD-WIDTH
                          (- (* CARD-HEIGHT 5) dy dy)
                          #f
                          (lambda (cards)
                            (when (drag-ok? cards i)
                              (move-to-stack cards i))))])
      (set-region-interactive-callback! 
       r
       (lambda (on? cards)
         (let ([ok? (and on? (drag-ok? cards i))])
           (for-each (lambda (c) (send c snap-back-after-move (not ok?)))
                     cards)
           (let ([l (vector-ref stacks i)])
             (unless (null? l) (send (car l) dim ok?))))))
      (send t add-region r)
      (loop (add1 i)))))


(define (move-to-stack cards i)
  (unselect)
  (let ([l (vector-ref stacks i)])
    (unless (null? l) (send (car l) dim #f)))
  (push-state!)
  (remove-from-stack! cards)
  (vector-set! stacks i (append (reverse cards) (vector-ref stacks i)))
  (for-each (lambda (c) (send c snap-back-after-move #t)) cards)
  (shift-stacks))

(define selected null)

(define (select cards)
  (unselect)
  (set! selected cards)
  (for-each (lambda (c) (send c dim #t)) selected))

(define (unselect)
  (for-each (lambda (c) (send c dim #f)) selected)
  (set! selected null))

(define (move-dones i)
  (send t move-cards (vector-ref dones i)
        (- (* 10 CARD-WIDTH) dx (* i (+ CARD-WIDTH dx)))
        dy))

(define (draw push?)
  (when push? (push-state!))
  (let ([drawn-cards
         (let loop ([i 0])
           (if (or (= i (vector-length stacks)) (null? draw-pile))
             null
             (if (vector-ref stacks i)
               (let ([a (car draw-pile)])
                 (vector-set! stacks i (cons a (vector-ref stacks i)))
                 (send a flip)
                 (set! draw-pile (cdr draw-pile))
                 (cons a (loop (add1 i))))
               (loop (add1 i)))))])
    (send t card-to-front (car drawn-cards))
    (send t stack-cards drawn-cards))
  (shift-stacks))

(define (check-complete)
  (let loop ([i 0])
    (unless (= i (vector-length stacks))
      (let ([l (vector-ref stacks i)])
        (when (and (pair? l) (= 1 (send (car l) get-value)))
          (let ([suit (send (car l) get-suit)])
            (let loop ([j 2][a (list (car l))][l (cdr l)])
              (cond
                [(= j 14)
                 ;; Complete set - move 13 cards to a done pile
                 (vector-set! dones done-count a)
                 (move-dones done-count)
                 (set! done-count (add1 done-count))
                 (for-each (lambda (c) (send c user-can-move #f)) a)
                 (vector-set! stacks i l)]
                [(and (pair? l)
                      (= j (send (car l) get-value))
                      (equal? suit (send (car l) get-suit)))
                 (loop (add1 j) (cons (car l) a) (cdr l))]
                [else (void)])))))
      (loop (add1 i)))))

(define (shift-stacks)
  (unselect)
  (check-complete)
  (let ([cards (apply append (map reverse (vector->list stacks)))]
        [deltas (list->vector
                 (let loop ([i 0])
                   (if (= i (vector-length stacks))
                     null
                     (append
                      (let* ([l (vector-ref stacks i)]
                             [ddy (min (quotient CARD-HEIGHT 3)
                                       (quotient (- (* CARD-HEIGHT 4)
                                                    dy dy dy)
                                                 (max 1 (sub1 (length l)))))])
                        (let loop ([l l][dy 0])
                          (if (null? l)
                            null
                            (cons (list (* i (+ CARD-WIDTH dx)) dy)
                                  (loop (cdr l) (+ dy ddy))))))
                      (loop (add1 i))))))])
    (send t move-cards cards dx (+ CARD-HEIGHT dy dy)
          (lambda (i) (apply values (vector-ref deltas i))))

    (let loop ([i 0])
      (unless (= i (vector-length stacks))
        (let ([l (vector-ref stacks i)])
          (when (pair? l)
            (when (send (car l) face-down?) (send t flip-card (car l)))
            (send (car l) user-can-move #t)
            (let loop ([l (cdr l)][prev (car l)])
              (unless (null? l)
                (if (and (not (send (car l) face-down?))
                         (equal? (send prev get-suit)
                                 (send (car l) get-suit))
                         (= (add1 (send prev get-value))
                            (send (car l) get-value)))
                  (begin (send (car l) user-can-move #t)
                         (loop (cdr l) (car l)))
                  (for-each (lambda (c) (send c user-can-move #f))
                            l))))))
        (loop (add1 i))))))

(send t set-double-click-action void)

(send t set-single-click-action
      (lambda (c)
        (cond
          [(and (pair? draw-pile)
                (eq? c (car draw-pile)))
           (if (ormap null? (vector->list stacks)) (bell) (draw #t))]
          [(and (pair? selected) (eq? c (car selected)))
           (unselect)]
          [(and (pair? selected)
                (let ([i (find-stack c)])
                  (and i
                       (not (equal? i (find-stack (car selected))))
                       (drag-ok? selected i)
                       i)))
           => (lambda (i)
                (send t card-to-front (car (last-pair selected)))
                (send t stack-cards (reverse selected))
                (move-to-stack selected i))]
          [(stacked-cards c) => (lambda (cards) (select cards))])))

;; Add a region for each stack to receive clicks when
;;  the stack is empty:
(let loop ([i 0])
  (unless (= i (vector-length stacks))
    (send t add-region (make-button-region
                        (+ dx (* i (+ CARD-WIDTH dx)))
                        (+ dy CARD-HEIGHT dy)
                        CARD-WIDTH CARD-HEIGHT
                        #f
                        (lambda ()
                          (when (and (null? (vector-ref stacks i))
                                     (pair? selected))
                            (move-to-stack selected i)))))
    (loop (add1 i))))

(send t set-button-action 'left 'drag-raise/above)
(send t set-button-action 'middle 'drag-raise/above)
(send t set-button-action 'right 'drag-raise/above)

(define (reset-game!)
  (send t remove-cards deck)
  (set! deck (make-spider-deck))
  (send t add-cards deck dx dy)
  (send t begin-card-sequence)
  (unselect)
  (send undo enable #f)
  (set! draw-pile (shuffle-list deck 7))
  (for-each (lambda (c)
              (unless (send c face-down?) (send c flip))
              (send c user-can-flip #f)
              (send c user-can-move #f)
              (send c snap-back-after-move #t))
            draw-pile)
  (set! stacks (make-vector 10 null))
  (set! dones (make-vector 8 null))
  (set! done-count 0)
  (set! old-states null)
  (send t stack-cards draw-pile)
  (let loop ([i 0])
    (unless (= i (vector-length stacks))
      (let ([n (if (< i 4) 5 4)])
        (vector-set! stacks i (list-first-n draw-pile n))
        (set! draw-pile (list-tail draw-pile n)))
      (loop (add1 i))))
  (draw #f)
  (send t end-card-sequence))
(reset-game!)
(send t show #t)

))
