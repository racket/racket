#lang racket/base

(require (for-syntax racket/base))

(provide call/prompt call/comp abort/cc

         abort

         fcontrol %

         control prompt control-at prompt-at
         ;; `-at' variations expect a prompt tag

         shift reset shift-at reset-at

         control0 prompt0 control0-at prompt0-at
         shift0 reset0 shift0-at reset0-at

         spawn

         splitter

         new-prompt set cupto)

;; ----------------------------------------

(define call/prompt call-with-continuation-prompt)
(define call/comp call-with-composable-continuation)
(define abort/cc abort-current-continuation)

;; ----------------------------------------

(define (abort . vals)
  (abort-current-continuation
   (default-continuation-prompt-tag)
   (lambda () (apply values vals))))

;; ----------------------------------------
;; Sitaram, PLDI'93
;;  The `%' here is compable with Sitaram & Felleisen, LSC'90,
;;  since we make the handler optional.

(define (fcontrol f #:tag [prompt-tag (default-continuation-prompt-tag)])
  (call-with-composable-continuation
   (lambda (k)
     (abort-current-continuation
      prompt-tag
      f
      k))))

(define-syntax %
  (syntax-rules ()
    [(_ expr handler #:tag prompt-tag)
     (call-with-continuation-prompt
      (lambda () expr)
      prompt-tag
      handler)]
    [(_ expr handler)
     (call-with-continuation-prompt
      (lambda () expr)
      (default-continuation-prompt-tag)
      handler)]
    [(_ expr)
     (call-with-continuation-prompt
      (lambda () expr))]))

;; ----------------------------------------
;; Predecessors of Sitaram, PLDI'93
;;  Felleisen, Wand, Friedman, & Duba, LFP'88
;;  Instead of `#', we use `prompt' as in Felleisen, POPL'88
;;   (where `control' is called `F')
;;  See also Sitaram and Felleisen, LSC'90

;; Helpder function: abort-current-continuation/keep-prompt is
;; like abort-current-continuation, but it always leaves the
;; prompt in place, independent of the prompt's handler.
;; This is possible via call/cc (i.e., it must be possible
;; to abort and keep a prompt, because call/cc needs it).
(define (abort-current-continuation/keep-prompt tag thunk)
  ((call-with-continuation-prompt
    (lambda ()
      ((call-with-current-continuation
        (lambda (k) (lambda () k))
        tag)))
    tag)
   thunk))

;; call-with-control, parameterized over whether to keep the
;; prompt (if the prompt's handler gives us the option of
;; removing it). The generated function is the same
;; as fcontrol when `abort-cc' is `abort-current-continuation'.
(define (make-call-with-control abort-cc)
  ;; Uses call/cc to always keep the enclosing prompt.
  (letrec ([call-with-control
            (case-lambda
             [(f) (call-with-control f (default-continuation-prompt-tag))]
             [(f tag) (call-with-composable-continuation
                       (lambda (k)
                         (abort-cc
                          tag
                          (lambda ()
                            (f k))))
                       tag)])])
    call-with-control))

(define call-with-control
  (make-call-with-control abort-current-continuation/keep-prompt))

(define-syntax define-control-macros
  (syntax-rules ()
    [(_ control control-at call-with-control)
     (begin
       (define-syntax (control stx)
         (syntax-case stx ()
           [(control id expr0 expr (... ...))
            (identifier? #'id)
            #'(call-with-control (lambda (id) expr0 expr (... ...)))]))
       (define-syntax (control-at stx)
         (syntax-case stx ()
           [(control-at tag id expr0 expr (... ...))
            (identifier? #'id)
            #'(call-with-control (lambda (id) expr0 expr (... ...)) tag)])))]))

(define-control-macros control control-at call-with-control)

(define-syntax define-prompt-macros
  (syntax-rules ()
    [(_ prompt prompt-at call-with-prompt)
     (begin
       (define-syntax prompt
         (syntax-rules ()
           [(prompt expr0 expr (... ...))
            (call-with-prompt (lambda () expr0 expr (... ...)))]))
       (define-syntax prompt-at
         (syntax-rules ()
           [(prompt-at tag expr0 expr (... ...))
            (call-with-prompt (lambda () expr0 expr (... ...)) tag)])))]))

(define-prompt-macros prompt prompt-at call-with-continuation-prompt)

;; ----------------------------------------
;; Danvy & Filinski, LFP'90

;; call-with-shift, parameterized over whether to keep the prompt
;; (if the prompt's handler gives us the option of removing it),
;; and whether the new one is removable:
(define (make-call-with-shift abort-cc inserted-handler)
  (letrec ([call-with-shift
            (case-lambda
             [(f) (call-with-shift f (default-continuation-prompt-tag))]
             [(f tag)
              (call-with-composable-continuation
               (lambda (k)
                 (abort-cc
                  tag
                  (lambda ()
                    (f (lambda vals
                         (call-with-continuation-prompt
                          (lambda ()
                            (apply k vals))
                          tag
                          inserted-handler))))))
               tag)])])
    call-with-shift))

(define call-with-shift
  (make-call-with-shift abort-current-continuation/keep-prompt #f))

(define-control-macros shift shift-at call-with-shift)

(define-prompt-macros reset reset-at call-with-continuation-prompt)

;; ----------------------------------------
;; Shan, SCHEME'04
;; Kiselyov, Indiana CS TR-611, 2005
;;
;;  The `control0' and `shift0' here are closer to Kiselyov, in that
;;  `control0' and `shift0' only behave as in Shan when paired with
;;  `prompt0' or `reset0' (which are two names for the same thing).
;;  When paired with `prompt' or `reset' (again, the same thing),
;;  they act like `control' and `shift'.
;;
;;  This difference is intentional. The programmer that inserts a
;;  prompt should choose whether the current continuation is visible
;;  or not. Note, also, that `control' and `shift' work whether
;;  they're paired with `prompt'/`reset' or `prompt0'/`reset0'.

(define call-with-control0
  ;; Uses abort-current-continuation, so that the prompt
  ;;  is removed --- if the prompt is willing to be removed.
  (make-call-with-control abort-current-continuation))

(define call-with-shift0
  ;; Uses abort-current-continuation, so that the prompt
  ;;  is removed --- if the prompt is willing to be removed.
  ;; The prompt installed with the captured continuation is
  ;;  itself willing to be removed.
  (make-call-with-shift abort-current-continuation (lambda (thunk) (thunk))))

(define-control-macros control0 control0-at call-with-control0)

(define-control-macros shift0 shift0-at call-with-shift0)

(define call-with-prompt0
  (case-lambda
   [(thunk) (call-with-prompt0 thunk (default-continuation-prompt-tag))]
   [(thunk tag)
    (call-with-continuation-prompt thunk tag (lambda (thunk) (thunk)))]))

(define-prompt-macros prompt0 prompt0-at call-with-prompt0)

(define-prompt-macros reset0 reset0-at call-with-prompt0)

;; ----------------------------------------
;; Hieb & Dybvig, PPOPP'90

(define (spawn f)
  (let ([p (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (lambda ()
       (f (lambda (f)
            (call-with-composable-continuation
             (lambda (k)
               (abort-current-continuation
                p
                (lambda ()
                  (f (lambda vals
                       (call-with-continuation-prompt
                        (lambda ()
                          (apply k vals))
                        p
                        (lambda (thunk) (thunk))))))))
             p))))
     p
     (lambda (thunk) (thunk)))))

;; ----------------------------------------
;; Queinnec & Serpette, POPL'91

(define (splitter receiver)
  (let ([p (make-continuation-prompt-tag)])
    (call-with-continuation-prompt
     (lambda ()
       (receiver (lambda (thunk)
                   (abort-current-continuation
                    p
                    thunk))
                 (lambda (proc)
                   (call-with-composable-continuation
                    proc
                    p))))
     p
     (lambda (thunk) (thunk)))))

;; ----------------------------------------
;; Gunter, Remy, & Rieke, FPLCA'95
;;  Unfortunately, the "prompt"s in Gunter et al. are what
;;  we call "prompt tags". In our terminology, a "prompt"
;;  is a tagged instance in a continuation.

(define (new-prompt) (make-continuation-prompt-tag))

(define-syntax set (make-rename-transformer #'prompt0-at))

(define-syntax cupto (make-rename-transformer #'control0-at))

;; ----------------------------------------

