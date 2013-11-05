#lang scheme/base
(require slideshow/base
         pict
         scheme/list
         scheme/math)

(provide play play-n
         fade-pict
         slide-pict
         fade-around-pict
         sequence-animations
         reverse-animations
         animate-slide
         fast-start
         fast-end
         fast-edges
         fast-middle
         split-phase)

(define (fail-gracefully t)
  (with-handlers ([exn:fail? (lambda (x) (values 0 0))])
    (t)))

(define single-pict (lambda (p) (if (list? p) (last p) p)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation player

;; Create a slide sequence where `mid' takes a number from 0.0 to 1.0.
;; The 0.0 slide will wit until you advance, but the remaining ones will
;; time out automatically to create the animation.
(define (play #:title [title #f]
              #:name [name title]
              #:layout [layout 'auto] 
              #:steps [N 10]
              #:delay [secs 0.05]
              #:skip-first? [skip-first? #f]
              mid)
  (unless skip-first?
    (slide #:title (if (procedure? title) (title 0) title) 
           #:name (if (procedure? name) (name 0) name)
           #:layout layout 
           (mid 0)))
  (if condense?
      (skip-slides N)
      (map (lambda (n)
             (slide #:title (if (procedure? title) (title n) title)
                    #:name (if (procedure? name) (name n) name)
                    #:layout layout 
                    #:timeout secs
                    (mid n)))
           (let ([cnt N])
             (let loop ([n cnt])
               (if (zero? n)
                   null
                   (cons (/ (- cnt -1 n) 1.0 cnt)
                         (loop (sub1 n)))))))))

;; Create a sequences of N `play' sequences, where `mid' takes
;; N arguments, each a number between 0.0 and 1.0. Initially, all
;; arguments will be 0.0. The first argument goes from 0.0 to 1.0
;; for the first `play' sequence, and then it stays at 1.0 while
;; the second goes from 0.0 to 1.0 for the second sequence, etc.
(define (play-n #:title [title #f] 
                #:name [name title] 
                #:layout [layout 'auto]
                #:steps [N 10]
                #:delay [secs 0.05]
                #:skip-last? [skip-last? #f]
                #:skip-first? [skip-first? #f]
                mid)
  (let ([n (procedure-arity mid)])
    (let loop ([post (vector->list (make-vector n))]
               [pre null]
               [skip? skip-first?]
               [Ns N])
      (if (null? post)
          (unless skip-last?
            (slide #:title (if (procedure? title) (apply title pre) title)
                   #:name (if (procedure? name) (apply name pre) name)
                   #:layout layout
                   (apply mid pre)))
          (begin
            (play #:title (if (procedure? title)
                              (lambda (n)
                                (apply title (append pre (list n) (cdr post))))
                              title)
                  #:name (if (procedure? name)
                             (lambda (n)
                               (apply name (append pre (list n) (cdr post))))
                             name)
                  #:layout layout 
                  #:steps (if (pair? Ns) (car Ns) Ns)
                  #:delay secs
                  #:skip-first? skip?
                  (lambda (n)
                    (apply mid (append pre (list n) (cdr post)))))
            (loop (cdr post) (cons 1.0 pre) #f (if (pair? Ns) (cdr Ns) Ns)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Like `slide', supports 'next and 'alts, but produces as a
;; function of N number arguments (for N stages)
(define (animate-slide . content)
  (let ([n (let loop ([content content])
             (cond
               [(null? content) 1]
               [(eq? (car content) 'next)
                (add1 (loop (cdr content)))]
               [(eq? (car content) 'alts)
                (+ (apply + (map (lambda (alt)
                                   (loop alt))
                                 (cadr content)))
                   (sub1 (loop (cddr content))))]
               [else (loop (cdr content))]))])
    (procedure-reduce-arity
     (lambda ns
       (let loop ([content content]
                  [ns (cons 1.0 ns)]
                  [k (lambda (p ns) (or p (blank)))])
         (cond
           [(null? content) (k #f ns)]
           [(eq? 'next (car content))
            (loop (cdr content)
                  (cdr ns)
                  k)]
           [(eq? 'alts (car content))
            (let aloop ([l (cadr content)]
                        [p (blank)]
                        [ns ns])
              (if (null? l)
                  (loop (cddr content)
                        ns 
                        (lambda (p2 ns)
                          (k (if p2 (vc-append gap-size p p2) p) ns)))
                  (loop (car l) 
                        ns
                        (lambda (p2 ns2)
                          (aloop (cdr l)
                                 (cellophane
                                  (if p2
                                      (ct-superimpose
                                       p
                                       p2)
                                      p)
                                  (if (null? (cdr l))
                                      1.0
                                      (- 1.0 (min 1.0 (* 2 (cadr ns2))))))
                                 (if (null? (cdr l))
                                     ns2
                                     (let ([ns (cdr ns2)])
                                       (cons (max 0.0 (* 2 (- (car ns) 0.5)))
                                             (cdr ns)))))))))]
           [else (vc-append
                  gap-size
                  (let ([p (cellophane (car content) (car ns))])
                    (loop (cdr content) ns
                          (lambda (p2 ns)
                            (k (if p2 (vc-append gap-size p p2) p) ns)))))])))
     (sub1 n))))



