#lang racket/base
(require racket/cmdline
	 racket/path)

(define same-up? #f)
(define exec? #f)
(define no-trailing-sep? #f)

(define-values (orig-p extras)
  (command-line
   #:once-each
   [("--same-up") "Leave path alone if it starts \"..\""
    (set! same-up? #t)]
   [("--exec") "Find executable path"
    (set! exec? #t)]
   [("--no-trailing-sep") "Avoid a trailing \"/\""
    (set! no-trailing-sep? #t)]
   #:args
   (path . extra)
   (values path extra)))

(cond
 [(and same-up?
       (eq? (car (explode-path orig-p)) 'up))
  (display orig-p)]
 [else
  (define p
    (if exec?
	(let ([p orig-p])
	  (if (path-element? (string->path p))
	      (or (find-executable-path p)
		  p)
	      p))
	orig-p))

  (define (fix-trailing p)
    (if no-trailing-sep?
        (let-values ([(base name dir?) (split-path p)])
          (build-path base name))
        p))
  
  (display (fix-trailing (simplify-path (path->complete-path p))))])
  
;; In case there are extra arguments to an executable, preserve them
(for ([e (in-list extras)])
  (display " ")
  (display e))

(newline)
