(module mz mzscheme
  (require (lib "struct.ss" "scribble")
           (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble"))

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble")))

  (define (*exnraise s)
    (make-element #f (list s " exception is raised")))
  (define-syntax exnraise
    (syntax-rules ()
      [(_ s) (*exnraise (scheme s))]))
  (define-syntax Exn
    (syntax-rules ()
      [(_ s) (scheme s)]))
  (provide exnraise Exn))
