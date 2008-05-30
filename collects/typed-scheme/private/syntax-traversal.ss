#lang scheme/base

(require mzlib/trace)

;; from Eli

(provide (all-defined-out))

;; -------------------- utilities

(define (pull-from-syntax stx . locs)
  (let loop ([stx stx] [locs locs])
    (if (null? locs)
      stx
      (loop (list-ref (syntax->list stx) (car locs)) (cdr locs)))))

(define (syntax-loc stx) (list (syntax-position stx) (syntax-span stx)))

;; -------------------- the real stuff

;; Look for `lookfor' in `enclosing', return chain of syntaxes from
;; the innermost out of only syntaxes with the given src, returns #f
;; if it can't find it.
(define (enclosing-syntaxes-with-source enclosing lookfor src)
  (let loop ([r '()] [stx enclosing])
    (let* ([r (if (and (syntax? stx) (eq? src (syntax-source stx)))
                (cons stx r)
                r)]
           [loop (lambda (stx) (loop r stx))])
      (if (eq? stx lookfor)
        r
        (let ([stx (if (syntax? stx) (syntax-e stx) stx)])
          (and (pair? stx)
               (or (loop (car stx)) (loop (cdr stx)))))))))



;; Look for (the outermost) syntax in `orig' that has the same
;; location as `lookfor' which is coming from the expanded `orig',
;; given in `expanded'.
(define (look-for-in-orig orig expanded lookfor)
  (define src (syntax-source orig))
  ;(printf "orig : ~a~n" orig)
  ;(printf "expanded : ~a~n" expanded)
  ;(printf "lookfor : ~a~n" lookfor)
  ;(printf "src : ~a~n" src)
  ;; we just might get a lookfor that is already in the original
   (let ([enclosing (enclosing-syntaxes-with-source expanded lookfor src)]
         [syntax-locs (make-hash)])
     ;; find all syntax locations in original code
     (let loop ([stx orig])
       (when (syntax? stx) (hash-set! syntax-locs (syntax-loc stx) stx))
       (let ([stx (if (syntax? stx) (syntax-e stx) stx)])
         (when (pair? stx) (loop (car stx)) (loop (cdr stx)))))
     (or
      ;; we just might get a lookfor that is already in the original
      (and (eq? src (syntax-source lookfor))
           (hash-ref syntax-locs (syntax-loc lookfor) #f)
           #;(printf "chose branch one: ~a~n" (hash-ref syntax-locs (syntax-loc lookfor) #f)))
           
      ;; look for some enclosing expression
      (and enclosing
           (begin0
             (ormap (lambda (enc) (hash-ref syntax-locs (syntax-loc enc) #f))
                    enclosing)
             #;(printf "chose branch two ~a~n" enclosing))))))

;(trace look-for-in-orig)

