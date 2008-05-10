#lang scheme/base
(require (for-syntax scheme/base)
         scheme/match
         scheme/contract
         "deriv.ss"
         "deriv-util.ss"
         "reductions-config.ss"
         "stx-util.ss")

(provide current-unvisited-lifts
         current-unhidden-lifts

         current-hiding-warning-handler

         handle-hiding-failure
         warn

         DEBUG-LIFTS
         
         add-unhidden-lift
         extract/remove-unvisited-lift)

;; Parameters

;; current-hiding-warning-handler : (parameter-of (symbol any -> void))
(define current-hiding-warning-handler
  (make-parameter
   (lambda (tag args) (printf "hiding warning: ~a\n" tag))))

;; current-unvisited-lifts : (paramter-of Derivation)
;; The derivs for the lifts yet to be seen in the processing 
;; of the first part of the current lift-deriv.
(define current-unvisited-lifts (make-parameter null))

;; current-unhidden-lifts : (parameter-of Derivation)
;; The derivs for those lifts that occur within unhidden macros.
;; Derivs are moved from the current-unvisited-lifts to this list.
(define current-unhidden-lifts (make-parameter null))

;; Helper

(define-syntax DEBUG-LIFTS
  (syntax-rules ()
    [(DEBUG-LIFTS . b)
     (void)]
    #;
    [(DEBUG-LIFTS . b)
     (begin . b)]))

;; Operations

;; add-unhidden-lift : Derivation -> void
(define (add-unhidden-lift d)
  (when d
    (current-unhidden-lifts
     (cons d (current-unhidden-lifts)))))

;; extract/remove-unvisted-lift : identifier -> Derivation
(define (extract/remove-unvisited-lift id)
  (define (get-defined-id d)
    (match d
      [(Wrap deriv (e1 e2))
       (with-syntax ([(?define-values (?id) ?expr) e1])
         #'?id)]))
  ;; The Wrong Way
  (let ([unvisited (current-unvisited-lifts)])
    (if (null? unvisited)
        (begin (DEBUG-LIFTS
                (printf "hide:extract/remove-unvisited-lift: out of lifts!"))
               #f)
        (let ([lift (car unvisited)])
          (DEBUG-LIFTS
           (printf "extracting lift: ~s left\n" (length (cdr unvisited))))
          (current-unvisited-lifts (cdr unvisited))
          lift)))
  ;; The Right Way
  ;; FIXME: Doesn't work inside of modules. Why not?
  #;
  (let loop ([lifts (current-unvisited-lifts)]
             [prefix null])
    (cond [(null? lifts)
           (DEBUG-LIFTS
            (fprintf (current-error-port)
                     "hide:extract/remove-unvisited-lift: can't find lift for ~s~n"
                     id))
           (raise (make localactions))]
          [(bound-identifier=? id (get-defined-id (car lifts)))
           (let ([lift (car lifts)])
             (current-unvisited-lifts
              (let loop ([prefix prefix] [lifts (cdr lifts)])
                (if (null? prefix)
                    lifts
                    (loop (cdr prefix) (cons (car prefix) lifts)))))
             lift)]
          [else
           (loop (cdr lifts) (cons (car lifts) prefix))])))

;; Warnings

(define (warn tag . args)
  ((current-hiding-warning-handler) tag args))

(define (handle-hiding-failure d failure)
  (match failure
    [(struct nonlinearity (term paths))
     (warn 'nonlinearity term paths d)]
    [(struct localactions ())
     (warn 'localactions d)]
    [(struct hidden-lift-site ())
     (warn 'hidden-lift-site d)]))

