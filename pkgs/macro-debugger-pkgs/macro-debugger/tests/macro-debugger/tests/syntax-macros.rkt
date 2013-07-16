#lang racket/base
(require "../gentest-framework.rkt")
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
        [#:steps (remark local-lift 'a (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression
                                        (#%top . #rx"^lifted"))))]
        #:no-hidden-steps)
  (test "lift with id"
        (lift (id 'a))
        [#:steps (remark local-lift (id 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        #:no-hidden-steps)

  (test "lift with Tid"
        (lift (Tid 'a))
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        ;; FIXME:
        ;;  maybe don't show lifts, but do find (Tid 'a), show in orig ctx
        ;;  but maybe not a good idea
        #| 
        [#:hidden-steps (macro (lift 'a))]
        |#)

  (test "Tlift"
        (Tlift 'a)
        [#:steps (remark local-lift 'a (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                       (#%expression (#%top . #rx"^lifted"))))]
        [#:hidden-steps (remark local-lift 'a (#rx"^lifted"))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") 'a)
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with id"
        (Tlift (id 'a))
        [#:steps (remark local-lift (id 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        [#:hidden-steps (remark local-lift (id 'a) (#rx"^lifted"))
                        (macro (#%expression #rx"^lifted"))
                        (capture-lifts (begin (define-values (#rx"^lifted") (id 'a))
                                              (#%expression #rx"^lifted")))])

  (test "Tlift with Tid"
        (Tlift (Tid 'a))
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
                 (macro (#%expression #rx"^lifted"))
                 (tag-top (#%expression (#%top . #rx"^lifted")))
                 (capture-lifts (begin (define-values (#rx"^lifted") (Tid 'a))
                                       (#%expression (#%top . #rx"^lifted"))))
                 (macro (begin (define-values (#rx"^lifted") 'a)
                               (#%expression (#%top . #rx"^lifted"))))]
        [#:steps (remark local-lift (Tid 'a) (#rx"^lifted"))
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
