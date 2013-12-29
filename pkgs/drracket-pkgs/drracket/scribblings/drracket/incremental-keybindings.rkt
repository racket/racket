#lang s-exp framework/keybinding-lang

(require drracket/tool-lib)

(module test racket/base)

(keybinding "c:c;c:e" (lambda (ed evt) (send-toplevel-form ed #f)))
(keybinding "c:c;c:r" (lambda (ed evt) (send-selection ed #f)))
(keybinding "c:c;~c:m:e" (lambda (ed evt) (send-toplevel-form ed #t)))
(keybinding "c:c;~c:m:r" (lambda (ed evt) (send-selection ed #t)))

(define/contract (send-toplevel-form defs shift-focus?)
  (-> any/c boolean? any)
  (when (is-a? defs drracket:unit:definitions-text<%>)
    (define sp (send defs get-start-position))
    (when (= sp (send defs get-end-position))
      (cond
        [(send defs find-up-sexp sp)
         ;; we are inside some top-level expression;
         ;; find the enclosing expression
         (let loop ([pos sp])
           (define next-up (send defs find-up-sexp pos)) 
           (cond
             [next-up (loop next-up)]
             [else
              (send-range-to-repl defs 
                                  pos
                                  (send defs get-forward-sexp pos)
                                  shift-focus?)]))]
        [else
         ;; we are at the top-level
         (define fw (send defs get-forward-sexp sp))
         (define bw (send defs get-backward-sexp sp))
         (cond
           [(and (not fw) (not bw)) 
            ;; no expressions in the file, give up
            (void)]
           [(not fw) 
            ;; no expression after the insertion point;
            ;; send the one before it
            (send-range-to-repl defs 
                                bw
                                (send defs get-forward-sexp bw)
                                shift-focus?)]
           [else 
            ;; send the expression after the insertion point
            (send-range-to-repl defs 
                                (send defs get-backward-sexp fw)
                                fw
                                shift-focus?)])]))))
              
(define/contract (send-selection defs shift-focus?)
  (-> any/c boolean? any)
  (when (is-a? defs drracket:unit:definitions-text<%>)
    (send-range-to-repl defs
                        (send defs get-start-position) 
                        (send defs get-end-position) 
                        shift-focus?)))

(define/contract (send-range-to-repl defs start end shift-focus?)
  (->i ([defs (is-a?/c drracket:unit:definitions-text<%>)]
        [start exact-positive-integer?]
        [end (start) (and/c exact-positive-integer? (>=/c start))]
        [shift-focus? boolean?])
       any)
  (unless (= start end) ;; don't send empty regions
    (define ints (send (send defs get-tab) get-ints))
    (define frame (send (send defs get-tab) get-frame))
    ;; copy the expression over to the interactions window
    (send defs move/copy-to-edit 
          ints start end
          (send ints last-position)
          #:try-to-move? #f)
    
    ;; erase any trailing whitespace
    (let loop ()
      (define last-pos (- (send ints last-position) 1))
      (when (last-pos . > . 0)
        (define last-char (send ints get-character last-pos))
        (when (char-whitespace? last-char)
          (send ints delete last-pos (+ last-pos 1))
          (loop))))
    
    ;; put back a single newline
    (send ints insert
          "\n"
          (send ints last-position)
          (send ints last-position))
    
    ;; make sure the interactions is visible 
    ;; and run the submitted expression
    (send frame ensure-rep-shown ints)
    (when shift-focus? (send (send ints get-canvas) focus))
    (send ints do-submission)))
