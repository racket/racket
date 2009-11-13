
#lang scheme/base
(require "../gentest-framework.ss")
(provide proto:macros)

(define-tests proto:macros "Macros"
  [#:suite 
   "Macros"
   (test "id"
         (id 'a)
         [#:steps (macro 'a)]
         #:no-hidden-steps)
   (test "Tid"
         (Tid 'a)
         [#:steps (macro 'a)]
         #:same-hidden-steps)
   (test "pre-id"
         (pre-id 'a)
         [#:steps (macro (id 'a))
                  (macro 'a)]
         #:no-hidden-steps)
   (test "myor (base)"
         (myor 'a)
         [#:steps (macro 'a)]
         #:no-hidden-steps)
   (test "myor (recursive 1)" 
         (myor 'a 'b)
         [#:steps (macro (let ((t 'a)) (if t t (myor 'b))))
                  (macro (let-values (((t) 'a)) (if t t (myor 'b))))
                  (rename-let-values (let-values (((t) 'a)) (if t t (myor 'b))))
                  (macro (let-values (((t) 'a)) (if t t 'b)))]
         #:no-hidden-steps)

   (test "leid with id"
         (leid (id 'a))
         [#:steps (macro 'a (leid (id 'a)))
                  (macro (#%expression 'a))]
         #:no-hidden-steps)
   (test "leid with Tid"
         (leid (Tid 'a))
         [#:steps (macro 'a (leid (Tid 'a)))
                  (macro (#%expression 'a))]
         [#:hidden-steps (macro (leid 'a))])]

  (test "lift"
        (lift 'a)
        [#:steps (local-lift (#rx"^lifted") (lift 'a))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression
                                        (#%top . #rx"^lifted"))))]
        #:no-hidden-steps)
  (test "lift with id"
        (lift (id 'a))
        [#:steps (local-lift (#rx"^lifted") (lift (id 'a)))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        #:no-hidden-steps)

  (test "lift with Tid"
        (lift (Tid 'a))
        [#:steps (local-lift (#rx"^lifted") (lift (Tid 'a)))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        ;; Don't show lifts, but do find (Tid 'a), show in orig ctx
        [#:hidden-steps (macro (lift 'a))])

  (test "Tlift"
        (Tlift 'a)
        [#:steps (local-lift (#rx"^lifted") (Tlift 'a))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression (#%top . #rx"^lifted"))))]
        [#:hidden-steps (local-lift (#rx"^lifted") (Tlift 'a))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with id"
        (Tlift (id 'a))
        [#:steps (local-lift (#rx"^lifted") (Tlift (id 'a)))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        [#:hidden-steps (local-lift (#rx"^lifted") (Tlift (id 'a)))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with Tid"
        (Tlift (Tid 'a))
        [#:steps (local-lift (#rx"^lifted") (Tlift (Tid 'a)))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        [#:steps (local-lift (#rx"^lifted") (Tlift (Tid 'a)))
                 (macro (#%expression #rx"^lifted"))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression #rx"^lifted")))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression #rx"^lifted")))])

  [#:suite "set! macros"
           (test "set! (macro)"
                 (set! the-current-output-port 'a)
                 [#:steps
                  (macro (#%plain-app current-output-port 'a))]
                 #:no-hidden-steps)]
  
  )
