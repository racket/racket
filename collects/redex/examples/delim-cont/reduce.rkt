#lang racket

(require redex/reduction-semantics
         "grammar.rkt"
         "meta.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reductions:

(define :->
  (reduction-relation
   grammar
   
   ;; beta
   (~~> ((λ (x_1 ..._1) e_1) v_1 ..._1)
        (subst* (x_1 ...) (v_1 ...) e_1)
        "beta")
   
   ;; arithmetic
   (~~> (+ n_1 n_2)
        ,(+ (term n_1) (term n_2))
        "+")
   (~~> (zero? 0)
        #t
        "zero?")
   (~~> (zero? v_1)
        #f
        (side-condition (not (equal? 0 (term v_1))))
        "non-zero")
   
   ;; lists
   (~~> (cons v_1 (list v_2 ...))
        (list v_1 v_2 ...)
        "cons")
   (~~> (first (list v_1 v_2 ...))
        v_1
        "first")
   (~~> (rest (list v_1 v_2 ...))
        (list v_2 ...)
        "rest")
   
   ;; printing
   (--> (<> s_1 [o_1 ...] (in-hole E_1 (print o_2)))
        (<> s_1 [o_1 ... o_2] (in-hole E_1 #f))
        "print")
   
   ;; if
   (~~> (if #t e_1 e_2)
        e_1
        "ift")
   (~~> (if #f e_1 e_2)
        e_2
        "iff")
   
   ;; begin
   (~~> (begin v e_1)
        e_1
        "begin-v")
   
   ;; set! and lookup
   (--> (<> ([x_1 v_1] ... [x_2 v_2] [x_3 v_3] ...) [o_1 ...] (in-hole E_1 (set! x_2 v_4)))
        (<> ([x_1 v_1] ... [x_2 v_4] [x_3 v_3] ...) [o_1 ...] (in-hole E_1 #f))
        "assign")
   (--> (<> ([x_1 v_1] ... [x_2 v_2] [x_3 v_3] ...) [o_1 ...] (in-hole E_1 x_2))
        (<> ([x_1 v_1] ... [x_2 v_2] [x_3 v_3] ...) [o_1 ...] (in-hole E_1 v_2))
        "lookup")
   
   ;; prompt
   ;;  When we get a value, drop the prompt.
   (~~> (% v_1 v_2 v_3)
        v_2
        "prompt-v")
   
   ;; call/cc
   ;;  Capture; the context E_2 must not include a prompt with the same tag,
   ;;  and we don't want immediate marks.
   (~~> (% v_2 (in-hole E_2 (wcm w_1 (call/cc v_1 v_2))) v_3)
        (% v_2 (in-hole E_2 (wcm w_1 (v_1 (cont v_2 E_2)))) v_3)
        (side-condition (term (noMatch E_2 E (% v_2 E v))))
        "call/cc")
   ;;  Invoke a continuation when there are no dw pre or post thunks to run (i.e.,
   ;;  no dw thunks in the unshared parts of the current and target continuations).
   ;;  D_2/D_6 is shared between the captured and current continuations; we make sure
   ;;  that W_3 and E_4 don't share.
   (~~> (% v_1 (in-hole D_2 (in-hole W_3 ((cont v_1 (in-hole D_6 (in-hole W_4 hole))) v_2))) v_3)
        (% v_1 (in-hole D_6 (in-hole W_4 v_2)) v_3)
        (side-condition (term (noMatch (in-hole D_2 W_3) E (% v_1 E v))))
        (side-condition (term (sameDWs D_2 D_6)))
        (side-condition (term (noShared W_3 W_4)))
        "cont")
   ;;  Invoke a continuation where there is a dw post thunk to run:
   ;;   - D_2/D_6 is the shared prefix of the current and captured continuation.
   ;;     (We make sure that E_3[(dw x_1 e_1 W_5 e_2)] and E_4 don't share.)
   ;;   - Keep D_2[E_3], replacing the relevant `dw' to run the post thunk
   ;;     and then resume the continuation jump.
   ;;  The second step means replacing (dw x e_1 W_5[((cont ...) v)] e_2)
   ;;  with (begin e_2 ((cont ...) v))).
   (~~> (% v_2 (in-hole D_2 (in-hole E_3 (dw x_1 e_1 (in-hole W_5 ((cont v_2 (in-hole D_6 (hide-hole E_4))) v_1)) e_2))) v_3)
        (% v_2 (in-hole D_2 (in-hole E_3 (begin e_2 ((cont v_2 (in-hole D_6 E_4)) v_1)))) v_3)
        (side-condition (term (noMatch (in-hole D_2 E_3) E (% v_2 E v))))
        (side-condition (term (sameDWs D_2 D_6)))
        (side-condition (term (noMatch W_5 E (% v_2 E v))))
        (side-condition (term (noShared (in-hole E_3 (dw x_1 e_1 W_5 e_2)) E_4)))
        "cont-post")
   ;;  Invoke a continuation when there are only dw pre thunks to run (i.e.,
   ;;  no dw thunks in the unshared part of the current continuation).
   ;;  D_2/D_6 is shared between the captured and current continuations; we
   ;;  make sure that W_3 and W_4[(dw ...)] don't share.
   ;;  We do one pre thunk at a time, just in case the pre thunk arranges for
   ;;  the relevant prompt to disappear. To do just one pre thunk, we
   ;;  create `(begin e_1 (dw x_1 e_1 ((cont ...) v) e_2))', which runs the pre
   ;;  thunk and then tries again to invoke the continuation --- but inside a
   ;;  `dw' for the already-run pre-thunk, so that it's treated as shared and not
   ;;  run again.
   (~~> (% v_1 (in-hole D_2 (in-hole W_3 ((cont v_1 (name k (in-hole D_6 (in-hole W_4 (dw x_1 e_1 (hide-hole E_5) e_2))))) v_2))) v_3)
        (% v_1 (in-hole D_6 (in-hole W_4 (begin e_1 (dw x_1 e_1 ((cont v_1 k) v_2) e_2)))) v_3)
        (side-condition (term (noMatch (in-hole D_2 W_3) E (% v_1 E v))))
        (side-condition (term (sameDWs D_2 D_6)))
        (side-condition (term (noShared W_3 (in-hole W_4 (dw x_1 e_1 E_5 e_2)))))
        "cont-pre")
   
   ;; abort
   ;;  Like continuation invocation, the case without dw post thunks:
   (~~> (% v_1 (in-hole W_2 (abort v_1 v_2)) v_3)
        (v_3 v_2)
        (side-condition (term (noMatch W_2 E (% v_1 E v))))
        "abort")
   ;;  And the case with a dw post thunk --- simpler than invoking a
   ;;  continuation, because we don't have to compute shared parts:
   (~~> (dw x_1 e_1 (in-hole W_2 (abort v_1 v_2)) e_2)
        (begin e_2 (abort v_1 v_2))
        (side-condition (term (noMatch W_2 E (% v_1 E v))))
        "abort-post")
   
   ;; composable continuation
   ;;  Capture up to the relevant prompt, not including immediate marks:
   (~~> (% v_2 (in-hole E_2 (wcm w_1 (call/comp v_1 v_2))) v_3)
        (% v_2 (in-hole E_2 (wcm w_1 (v_1 (comp E_2)))) v_3)
        (side-condition (term (noMatch E_2 E (% v_2 E v))))
        "call/comp")
   ;;  On invocation, we want to splice leading `wcm's with any marks
   ;;  at the invocation context. We do that by convertings the leading
   ;;  `wcm's back to `call/cm', so they get spliced as usual
   ;;  for evaluating `call/cm' (see below). Meanwhile, we need to
   ;;  handle the case that there are dw pre thunks to run on the way in
   ;;  (which is a little simpler than for non-composable continuations,
   ;;  since we don't have to worry about sharing w.r.t. a prompt).
   (~~> ((comp (in-hole W_1 hole)) v_1)
        (expose-wcm (in-hole W_1 v_1))
        "comp")
   (~~> ((comp (in-hole W_1 (dw x_1 e_1 (hide-hole E_2) e_2))) v_1)
        (expose-wcm (in-hole W_1 (begin e_1 (dw x_1 e_1 ((comp E_2) v_1) e_2))))
        "comp-pre")
   
   ;; continuation marks
   ;;  Introduce a `wcm' when needed for certain primitives:
   (-+> (in-hole E_1 (u_1 v_1 ...))
        (in-hole E_1 (wcm () (u_1 v_1 ...)))
        (side-condition (term (noMatch E_1 E (wcm w hole))))
        "wcm-intro")
   ;;  When we get a value, discard marks:
   (~~> (wcm w v_1)
        v_1
        "wcm-v")
   
   ;;  When `call/cm' uses the same key as a wrapping
   ;;  mark, then replace the old value.
   (~~> (wcm ((v_1 v_2) ... (v_3 v_4) (v_5 v_6) ...) 
             (call/cm v_3 v_7 (λ () e_1)))
        (wcm ((v_1 v_2) ... (v_3 v_7) (v_5 v_6) ...) e_1)
        "wcm-set")
   ;;  When `call/cm' uses a different key than any wrapping
   ;;  mark, then add a new mark.
   (~~> (wcm ((v_1 v_2) ...) (call/cm v_3 v_4 (λ () e_1)))
        (wcm ((v_1 v_2) ... (v_3 v_4)) e_1)
        (side-condition (term (notIn v_3 (v_1 ...))))
        "wcm-add")
   ;;  To get the current mark value for mark key, search
   ;;  the current context (using `get-marks'), using only
   ;;  the part of the continuation up to a prompt with the
   ;;  given tag.
   (~~> (% v_2 (in-hole E_2 (current-marks v_1 v_2)) v_3)
        (% v_2 (in-hole E_2 (get-marks E_2 v_1 (list))) v_3)
        (side-condition (term (noMatch E_2 E (% v_2 E v))))
        "marks")
   
   ;; dynamic-wind
   ;;  Evaluate a `dynamic-wind' function by generating a new `dw'
   ;;  wrapper. The wrapper uses a newly allocated (globally unique)
   ;;  tag variable. Also, introduce a `begin' with the pre-thunk
   ;;  body --- which, crucially, is put *outside* the generated `dw'.
   (~~> (dynamic-wind (λ () e_1) (λ () e_2) (λ () e_3))
        (begin e_1 (dw x_1 e_1 e_2 e_3))
        (fresh x_1)
        "dw")
   ;;  When we get a result from the dw, evaluate the post thnk
   ;;  (outside the `dw') and then continue returning the result.
   (~~> (dw x e_1 v_1 e_3)
        (begin e_3 v_1)
        "dw-v")
   
   with
   ;; -+> is evaluation independent of the store and output:
   [(--> (<> s_1 [o_1 ...] from) (<> s_1 [o_1 ...] to)) (-+> from to)]
   ;; ~~> is evaluation in any E:
   [(-+> (in-hole E_1 from)
         (in-hole E_1 to))
    (~~> from to)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :->)
