#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type ours_t* void_t*)
(define-ffi2-type mine_t* ours_t*)

(define-ffi2-type ours_t** (array ours_t* *))
(define-ffi2-type mine_t** (array mine_t* *))

(define-ffi2-type also_ours_t* ours_t*
  #:tag #f)

(let ()
  (define p (ffi2-malloc #:manual 100))
  (check-true (void_t*? p))
  (check-false (void_t*/gcable? p))
  (check-false (ours_t*? p))
  (check-false (mine_t*? p))
  (ffi2-free p))

(let ()
  (define p (ffi2-malloc 100))
  (check-true (void_t*? p))
  (check-true (void_t*/gcable? p))
  (check-false (ours_t*? p))
  (check-false (mine_t*? p)))

(let ()
  (define op (ffi2-malloc #:manual 100 #:as ours_t*))
  (check-true (void_t*? op))
  (check-false (void_t*/gcable? op))
  (check-true (ours_t*? op))
  (check-true (also_ours_t*? op))
  (check-false (mine_t*? op))
  (ffi2-free op))

(let ()
  (define op (ffi2-malloc 100 #:bytes #:as ours_t*))
  (check-true (void_t*? op))
  (check-true (void_t*/gcable? op))
  (check-true (ours_t*? op))
  (check-false (mine_t*? op)))

(let ()
  (define mp (ffi2-malloc #:manual 100 #:as mine_t*))
  (check-true (void_t*? mp))
  (check-false (void_t*/gcable? mp))
  (check-true (ours_t*? mp))
  (check-true (mine_t*? mp))
  (check-false (ours_t**? mp))
  (check-false (mine_t**? mp))
  (ffi2-free mp))

(let ()
  (define mp (ffi2-malloc 100 #:as mine_t*))
  (check-true (void_t*? mp))
  (check-true (ours_t*? mp))
  (check-true (mine_t*? mp)))

(let ()
  (define pp (ffi2-malloc ours_t*))
  (check-false (ours_t*? pp))
  (check-false (mine_t*? pp))
  (check-true (ours_t**? pp))
  (check-false (mine_t**? pp)))

(let ()
  (define pp (ffi2-malloc mine_t*))
  (check-false (ours_t*? pp))
  (check-false (mine_t*? pp))
  (check-false (ours_t**? pp))
  (check-true (mine_t**? pp)))
