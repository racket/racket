;;; EARLEY -- Earley's parser, written by Marc Feeley.

; $Id: earley.sch,v 1.2 1999/07/12 18:05:19 lth Exp $
; 990708 / lth -- changed 'main' to 'earley-benchmark'.
; 100404 / Vincent St-Amour -- got rid of one-armed ifs
;
; (make-parser grammar lexer) is used to create a parser from the grammar
; description `grammar' and the lexer function `lexer'.
;
; A grammar is a list of definitions.  Each definition defines a non-terminal
; by a set of rules.  Thus a definition has the form: (nt rule1 rule2...).
; A given non-terminal can only be defined once.  The first non-terminal
; defined is the grammar's goal.  Each rule is a possibly empty list of
; non-terminals.  Thus a rule has the form: (nt1 nt2...).  A non-terminal
; can be any scheme value.  Note that all grammar symbols are treated as
; non-terminals.  This is fine though because the lexer will be outputing
; non-terminals.
;
; The lexer defines what a token is and the mapping between tokens and
; the grammar's non-terminals.  It is a function of one argument, the input,
; that returns the list of tokens corresponding to the input.  Each token is
; represented by a list.  The first element is some `user-defined' information
; associated with the token and the rest represents the token's class(es) (as a
; list of non-terminals that this token corresponds to).
;
; The result of `make-parser' is a function that parses the single input it
; is given into the grammar's goal.  The result is a `parse' which can be
; manipulated with the procedures: `parse->parsed?', `parse->trees'
; and `parse->nb-trees' (see below).
;
; Let's assume that we want a parser for the grammar
;
;  S -> x = E
;  E -> E + E | V
;  V -> V y |
;
; and that the input to the parser is a string of characters.  Also, assume we
; would like to map the characters `x', `y', `+' and `=' into the corresponding
; non-terminals in the grammar.  Such a parser could be created with
;
; (make-parser
;   '(
;      (s (x = e))
;      (e (e + e) (v))
;      (v (v y) ())
;    )
;   (lambda (str)
;     (map (lambda (char)
;            (list char ; user-info = the character itself
;                  (case char
;                    ((#\x) 'x)
;                    ((#\y) 'y)
;                    ((#\+) '+)
;                    ((#\=) '=)
;                    (else (fatal-error "lexer error")))))
;          (string->list str)))
; )
;
; An alternative definition (that does not check for lexical errors) is
;
; (make-parser
;   '(
;      (s (#\x #\= e))
;      (e (e #\+ e) (v))
;      (v (v #\y) ())
;    )
;   (lambda (str) (map (lambda (char) (list char char)) (string->list str)))
; )
;
; To help with the rest of the discussion, here are a few definitions:
;
; An input pointer (for an input of `n' tokens) is a value between 0 and `n'.
; It indicates a point between two input tokens (0 = beginning, `n' = end).
; For example, if `n' = 4, there are 5 input pointers:
;
;   input                   token1     token2     token3     token4
;   input pointers       0          1          2          3          4
;
; A configuration indicates the extent to which a given rule is parsed (this
; is the common `dot notation').  For simplicity, a configuration is
; represented as an integer, with successive configurations in the same
; rule associated with successive integers.  It is assumed that the grammar
; has been extended with rules to aid scanning.  These rules are of the
; form `nt ->', and there is one such rule for every non-terminal.  Note
; that these rules are special because they only apply when the corresponding
; non-terminal is returned by the lexer.
;
; A configuration set is a configuration grouped with the set of input pointers
; representing where the head non-terminal of the configuration was predicted.
;
; Here are the rules and configurations for the grammar given above:
;
;  S -> .         \
;       0          |
;  x -> .          |
;       1          |
;  = -> .          |
;       2          |
;  E -> .          |
;       3           > special rules (for scanning)
;  + -> .          |
;       4          |
;  V -> .          |
;       5          |
;  y -> .          |
;       6         /
;  S -> .  x  .  =  .  E  .
;       7     8     9     10
;  E -> .  E  .  +  .  E  .
;       11    12    13    14
;  E -> .  V  .
;       15    16
;  V -> .  V  .  y  .
;       17    18    19
;  V -> .
;       20
;
; Starters of the non-terminal `nt' are configurations that are leftmost
; in a non-special rule for `nt'.  Enders of the non-terminal `nt' are
; configurations that are rightmost in any rule for `nt'.  Predictors of the
; non-terminal `nt' are configurations that are directly to the left of `nt'
; in any rule.
;
; For the grammar given above,
;
;   Starters of V   = (17 20)
;   Enders of V     = (5 19 20)
;   Predictors of V = (15 17)

(define (make-parser grammar lexer)

  (define (non-terminals grammar) ; return vector of non-terminals in grammar

    (define (add-nt nt nts)
      (if (member nt nts) nts (cons nt nts))) ; use equal? for equality tests

    (let def-loop ((defs grammar) (nts '()))
      (if (pair? defs)
        (let* ((def (car defs))
               (head (car def)))
          (let rule-loop ((rules (cdr def))
                          (nts (add-nt head nts)))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nts nts))
                  (if (pair? l)
                    (let ((nt (car l)))
                      (loop (cdr l) (add-nt nt nts)))
                    (rule-loop (cdr rules) nts))))
              (def-loop (cdr defs) nts))))
        (list->vector (reverse nts))))) ; goal non-terminal must be at index 0

  (define (ind nt nts) ; return index of non-terminal `nt' in `nts'
    (let loop ((i (- (vector-length nts) 1)))
      (if (>= i 0)
        (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
        #f)))

  (define (nb-configurations grammar) ; return nb of configurations in grammar
    (let def-loop ((defs grammar) (nb-confs 0))
      (if (pair? defs)
        (let ((def (car defs)))
          (let rule-loop ((rules (cdr def)) (nb-confs nb-confs))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nb-confs nb-confs))
                  (if (pair? l)
                    (loop (cdr l) (+ nb-confs 1))
                    (rule-loop (cdr rules) (+ nb-confs 1)))))
              (def-loop (cdr defs) nb-confs))))
      nb-confs)))

; First, associate a numeric identifier to every non-terminal in the
; grammar (with the goal non-terminal associated with 0).
;
; So, for the grammar given above we get:
;
; s -> 0   x -> 1   = -> 4   e ->3    + -> 4   v -> 5   y -> 6

  (let* ((nts (non-terminals grammar))          ; id map = list of non-terms
         (nb-nts (vector-length nts))           ; the number of non-terms
         (nb-confs (+ (nb-configurations grammar) nb-nts)) ; the nb of confs
         (starters (make-vector nb-nts '()))    ; starters for every non-term
         (enders (make-vector nb-nts '()))      ; enders for every non-term
         (predictors (make-vector nb-nts '()))  ; predictors for every non-term
         (steps (make-vector nb-confs #f))      ; what to do in a given conf
         (names (make-vector nb-confs #f)))     ; name of rules

    (define (setup-tables grammar nts starters enders predictors steps names)

      (define (add-conf conf nt nts class)
        (let ((i (ind nt nts)))
          (vector-set! class i (cons conf (vector-ref class i)))))

      (let ((nb-nts (vector-length nts)))

        (let nt-loop ((i (- nb-nts 1)))
          (if (>= i 0)
            (begin
              (vector-set! steps i (- i nb-nts))
              (vector-set! names i (list (vector-ref nts i) 0))
              (vector-set! enders i (list i))
              (nt-loop (- i 1)))
            #f))

        (let def-loop ((defs grammar) (conf (vector-length nts)))
          (if (pair? defs)
            (let* ((def (car defs))
                   (head (car def)))
              (let rule-loop ((rules (cdr def)) (conf conf) (rule-num 1))
                (if (pair? rules)
                  (let ((rule (car rules)))
                    (vector-set! names conf (list head rule-num))
                    (add-conf conf head nts starters)
                    (let loop ((l rule) (conf conf))
                      (if (pair? l)
                        (let ((nt (car l)))
                          (vector-set! steps conf (ind nt nts))
                          (add-conf conf nt nts predictors)
                          (loop (cdr l) (+ conf 1)))
                        (begin
                          (vector-set! steps conf (- (ind head nts) nb-nts))
                          (add-conf conf head nts enders)
                          (rule-loop (cdr rules) (+ conf 1) (+ rule-num 1))))))
                  (def-loop (cdr defs) conf))))
            #f))))

; Now, for each non-terminal, compute the starters, enders and predictors and
; the names and steps tables.

    (setup-tables grammar nts starters enders predictors steps names)

; Build the parser description

    (let ((parser-descr (vector lexer
                                nts
                                starters
                                enders
                                predictors
                                steps
                                names)))
      (lambda (input)

        (define (ind nt nts) ; return index of non-terminal `nt' in `nts'
          (let loop ((i (- (vector-length nts) 1)))
            (if (>= i 0)
              (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
              #f)))

        (define (comp-tok tok nts) ; transform token to parsing format
          (let loop ((l1 (cdr tok)) (l2 '()))
            (if (pair? l1)
              (let ((i (ind (car l1) nts)))
                (if i
                  (loop (cdr l1) (cons i l2))
                  (loop (cdr l1) l2)))
              (cons (car tok) (reverse l2)))))

        (define (input->tokens input lexer nts)
          (list->vector (map (lambda (tok) (comp-tok tok nts)) (lexer input))))

        (define (make-states nb-toks nb-confs)
          (let ((states (make-vector (+ nb-toks 1) #f)))
            (let loop ((i nb-toks))
              (if (>= i 0)
                (let ((v (make-vector (+ nb-confs 1) #f)))
                  (vector-set! v 0 -1)
                  (vector-set! states i v)
                  (loop (- i 1)))
                states))))

        (define (conf-set-get state conf)
          (vector-ref state (+ conf 1)))

        (define (conf-set-get* state state-num conf)
          (let ((conf-set (conf-set-get state conf)))
            (if conf-set
              conf-set
              (let ((conf-set (make-vector (+ state-num 6) #f)))
                (vector-set! conf-set 1 -3) ; old elems tail (points to head)
                (vector-set! conf-set 2 -1) ; old elems head
                (vector-set! conf-set 3 -1) ; new elems tail (points to head)
                (vector-set! conf-set 4 -1) ; new elems head
                (vector-set! state (+ conf 1) conf-set)
                conf-set))))

        (define (conf-set-merge-new! conf-set)
          (vector-set! conf-set
            (+ (vector-ref conf-set 1) 5)
            (vector-ref conf-set 4))
          (vector-set! conf-set 1 (vector-ref conf-set 3))
          (vector-set! conf-set 3 -1)
          (vector-set! conf-set 4 -1))

        (define (conf-set-head conf-set)
          (vector-ref conf-set 2))

        (define (conf-set-next conf-set i)
          (vector-ref conf-set (+ i 5)))

        (define (conf-set-member? state conf i)
          (let ((conf-set (vector-ref state (+ conf 1))))
            (if conf-set
              (conf-set-next conf-set i)
              #f)))

        (define (conf-set-adjoin state conf-set conf i)
          (let ((tail (vector-ref conf-set 3))) ; put new element at tail
            (vector-set! conf-set (+ i 5) -1)
            (vector-set! conf-set (+ tail 5) i)
            (vector-set! conf-set 3 i)
            (if (< tail 0)
              (begin
                (vector-set! conf-set 0 (vector-ref state 0))
                (vector-set! state 0 conf))
              #f)))

        (define (conf-set-adjoin* states state-num l i)
          (let ((state (vector-ref states state-num)))
            (let loop ((l1 l))
              (if (pair? l1)
                (let* ((conf (car l1))
                       (conf-set (conf-set-get* state state-num conf)))
                  (if (not (conf-set-next conf-set i))
                    (begin
                      (conf-set-adjoin state conf-set conf i)
                      (loop (cdr l1)))
                    (loop (cdr l1))))
                #f))))

        (define (conf-set-adjoin** states states* state-num conf i)
          (let ((state (vector-ref states state-num)))
            (if (conf-set-member? state conf i)
              (let* ((state* (vector-ref states* state-num))
                     (conf-set* (conf-set-get* state* state-num conf)))
                (if (not (conf-set-next conf-set* i))
                  (conf-set-adjoin state* conf-set* conf i)
                  #f)
                #t)
              #f)))

        (define (conf-set-union state conf-set conf other-set)
          (let loop ((i (conf-set-head other-set)))
            (if (>= i 0)
              (if (not (conf-set-next conf-set i))
                (begin
                  (conf-set-adjoin state conf-set conf i)
                  (loop (conf-set-next other-set i)))
                (loop (conf-set-next other-set i)))
              #f)))

        (define (forw states state-num starters enders predictors steps nts)

          (define (predict state state-num conf-set conf nt starters enders)

            ; add configurations which start the non-terminal `nt' to the
            ; right of the dot

            (let loop1 ((l (vector-ref starters nt)))
              (if (pair? l)
                (let* ((starter (car l))
                       (starter-set (conf-set-get* state state-num starter)))
                  (if (not (conf-set-next starter-set state-num))
                    (begin
                      (conf-set-adjoin state starter-set starter state-num)
                      (loop1 (cdr l)))
                    (loop1 (cdr l))))
                #f))

            ; check for possible completion of the non-terminal `nt' to the
            ; right of the dot

            (let loop2 ((l (vector-ref enders nt)))
              (if (pair? l)
                (let ((ender (car l)))
                  (if (conf-set-member? state ender state-num)
                    (let* ((next (+ conf 1))
                           (next-set (conf-set-get* state state-num next)))
                      (conf-set-union state next-set next conf-set)
                      (loop2 (cdr l)))
                    (loop2 (cdr l))))
                #f)))

          (define (reduce states state state-num conf-set head preds)

            ; a non-terminal is now completed so check for reductions that
            ; are now possible at the configurations `preds'

            (let loop1 ((l preds))
              (if (pair? l)
                (let ((pred (car l)))
                  (let loop2 ((i head))
                    (if (>= i 0)
                      (let ((pred-set (conf-set-get (vector-ref states i) pred)))
                        (if pred-set
                          (let* ((next (+ pred 1))
                                 (next-set (conf-set-get* state state-num next)))
                            (conf-set-union state next-set next pred-set))
                          #f)
                        (loop2 (conf-set-next conf-set i)))
                      (loop1 (cdr l)))))
                #f)))

          (let ((state (vector-ref states state-num))
                (nb-nts (vector-length nts)))
            (let loop ()
              (let ((conf (vector-ref state 0)))
                (if (>= conf 0)
                  (let* ((step (vector-ref steps conf))
                         (conf-set (vector-ref state (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (if (>= step 0)
                      (predict state state-num conf-set conf step starters enders)
                      (let ((preds (vector-ref predictors (+ step nb-nts))))
                        (reduce states state state-num conf-set head preds)))
                    (loop))
                  #f)))))

        (define (forward starters enders predictors steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (states (make-states nb-toks nb-confs))
                 (goal-starters (vector-ref starters 0)))
            (conf-set-adjoin* states 0 goal-starters 0) ; predict goal
            (forw states 0 starters enders predictors steps nts)
            (let loop ((i 0))
              (if (< i nb-toks)
                (let ((tok-nts (cdr (vector-ref toks i))))
                  (conf-set-adjoin* states (+ i 1) tok-nts i) ; scan token
                  (forw states (+ i 1) starters enders predictors steps nts)
                  (loop (+ i 1)))
                #f))
            states))

        (define (produce conf i j enders steps toks states states* nb-nts)
          (let ((prev (- conf 1)))
            (if (and (>= conf nb-nts) (>= (vector-ref steps prev) 0))
              (let loop1 ((l (vector-ref enders (vector-ref steps prev))))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)))
                        (if (>= k 0)
                          (begin
                            (and (>= k i)
                                 (conf-set-adjoin** states states* k prev i)
                                 (conf-set-adjoin** states states* j ender k))
                            (loop2 (conf-set-next ender-set k)))
                          (loop1 (cdr l))))
                      (loop1 (cdr l))))
                  #f))
              #f)))

        (define (back states states* state-num enders steps nb-nts toks)
          (let ((state* (vector-ref states* state-num)))
            (let loop1 ()
              (let ((conf (vector-ref state* 0)))
                (if (>= conf 0)
                  (let* ((conf-set (vector-ref state* (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state* 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (let loop2 ((i head))
                      (if (>= i 0)
                        (begin
                          (produce conf i state-num enders steps
                                   toks states states* nb-nts)
                          (loop2 (conf-set-next conf-set i)))
                        (loop1))))
                  #f)))))

        (define (backward states enders steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (nb-nts (vector-length nts))
                 (states* (make-states nb-toks nb-confs))
                 (goal-enders (vector-ref enders 0)))
            (let loop1 ((l goal-enders))
              (if (pair? l)
                (let ((conf (car l)))
                  (conf-set-adjoin** states states* nb-toks conf 0)
                  (loop1 (cdr l)))
                #f))
            (let loop2 ((i nb-toks))
              (if (>= i 0)
                (begin
                  (back states states* i enders steps nb-nts toks)
                  (loop2 (- i 1)))
                #f))
            states*))

        (define (parsed? nt i j nts enders states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        #t
                        (loop (cdr l))))
                    #f)))
              #f)))

        (define (deriv-trees conf i j enders steps names toks states nb-nts)
          (let ((name (vector-ref names conf)))

            (if name ; `conf' is at the start of a rule (either special or not)
              (if (< conf nb-nts)
                (list (list name (car (vector-ref toks i))))
                (list (list name)))

              (let ((prev (- conf 1)))
                (let loop1 ((l1 (vector-ref enders (vector-ref steps prev)))
                            (l2 '()))
                  (if (pair? l1)
                    (let* ((ender (car l1))
                           (ender-set (conf-set-get (vector-ref states j)
                                                    ender)))
                      (if ender-set
                        (let loop2 ((k (conf-set-head ender-set)) (l2 l2))
                          (if (>= k 0)
                            (if (and (>= k i)
                                     (conf-set-member? (vector-ref states k)
                                                       prev i))
                              (let ((prev-trees
                                      (deriv-trees prev i k enders steps names
                                                   toks states nb-nts))
                                    (ender-trees
                                      (deriv-trees ender k j enders steps names
                                                   toks states nb-nts)))
                                (let loop3 ((l3 ender-trees) (l2 l2))
                                  (if (pair? l3)
                                    (let ((ender-tree (list (car l3))))
                                      (let loop4 ((l4 prev-trees) (l2 l2))
                                        (if (pair? l4)
                                          (loop4 (cdr l4)
                                                 (cons (append (car l4)
                                                               ender-tree)
                                                       l2))
                                          (loop3 (cdr l3) l2))))
                                    (loop2 (conf-set-next ender-set k) l2))))
                              (loop2 (conf-set-next ender-set k) l2))
                            (loop1 (cdr l1) l2)))
                        (loop1 (cdr l1) l2)))
                    l2))))))

        (define (deriv-trees* nt i j nts enders steps names toks states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (trees '()))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (append (deriv-trees conf i j enders steps names
                                                   toks states nb-nts)
                                      trees))
                        (loop (cdr l) trees)))
                    trees)))
              #f)))

        (define (nb-deriv-trees conf i j enders steps toks states nb-nts)
          (let ((prev (- conf 1)))
            (if (or (< conf nb-nts) (< (vector-ref steps prev) 0))
              1
              (let loop1 ((l (vector-ref enders (vector-ref steps prev)))
                          (n 0))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)) (n n))
                        (if (>= k 0)
                          (if (and (>= k i)
                                   (conf-set-member? (vector-ref states k)
                                                     prev i))
                            (let ((nb-prev-trees
                                    (nb-deriv-trees prev i k enders steps
                                                    toks states nb-nts))
                                  (nb-ender-trees
                                    (nb-deriv-trees ender k j enders steps
                                                    toks states nb-nts)))
                              (loop2 (conf-set-next ender-set k)
                                     (+ n (* nb-prev-trees nb-ender-trees))))
                            (loop2 (conf-set-next ender-set k) n))
                          (loop1 (cdr l) n)))
                      (loop1 (cdr l) n)))
                  n)))))

        (define (nb-deriv-trees* nt i j nts enders steps toks states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (nb-trees 0))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (+ (nb-deriv-trees conf i j enders steps
                                                 toks states nb-nts)
                                 nb-trees))
                        (loop (cdr l) nb-trees)))
                    nb-trees)))
              #f)))

        (let* ((lexer      (vector-ref parser-descr 0))
               (nts        (vector-ref parser-descr 1))
               (starters   (vector-ref parser-descr 2))
               (enders     (vector-ref parser-descr 3))
               (predictors (vector-ref parser-descr 4))
               (steps      (vector-ref parser-descr 5))
               (names      (vector-ref parser-descr 6))
               (toks       (input->tokens input lexer nts)))

          (vector nts
                  starters
                  enders
                  predictors
                  steps
                  names
                  toks
                  (backward (forward starters enders predictors steps nts toks)
                            enders steps nts toks)
                  parsed?
                  deriv-trees*
                  nb-deriv-trees*))))))

(define (parse->parsed? parse nt i j)
  (let* ((nts     (vector-ref parse 0))
         (enders  (vector-ref parse 2))
         (states  (vector-ref parse 7))
         (parsed? (vector-ref parse 8)))
    (parsed? nt i j nts enders states)))

(define (parse->trees parse nt i j)
  (let* ((nts          (vector-ref parse 0))
         (enders       (vector-ref parse 2))
         (steps        (vector-ref parse 4))
         (names        (vector-ref parse 5))
         (toks         (vector-ref parse 6))
         (states       (vector-ref parse 7))
         (deriv-trees* (vector-ref parse 9)))
    (deriv-trees* nt i j nts enders steps names toks states)))

(define (parse->nb-trees parse nt i j)
  (let* ((nts             (vector-ref parse 0))
         (enders          (vector-ref parse 2))
         (steps           (vector-ref parse 4))
         (toks            (vector-ref parse 6))
         (states          (vector-ref parse 7))
         (nb-deriv-trees* (vector-ref parse 10)))
    (nb-deriv-trees* nt i j nts enders steps toks states)))

(define (test k)
  (let ((p (make-parser '( (s (a) (s s)) )
                        (lambda (l) (map (lambda (x) (list x x)) l)))))
    (let ((x (p (vector->list (make-vector k 'a)))))
      (length (parse->trees x 's 0 k)))))

(time (test 14))
