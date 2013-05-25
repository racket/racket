#lang racket
(require slideshow
         redex
         "list-machine.rkt"
         "list-machine-typing.rkt")

(define (with-rewriters t)
  (define rewrite-lookup
    (match-lambda
      [(list op id r v a cp)
       (list "" r "(" v ") = " a)]))
  (define set-rewrite
    (match-lambda
      [(list op id r1 v a r2 cp)
       (list "" r1 "[" v ":=" a "] = " r2)]))
  (define subset-rewrite
    (match-lambda
      [(list op id left right cp)
       (list "" left " ⊂ " right "")]))
  (define (turn which subscr)
    (hbl-append (text which (default-style) (default-font-size))
                (text subscr (cons 'subscript (default-style)) (default-font-size))
                (text " " (default-style) (default-font-size))))
  (with-compound-rewriters
   (['var-lookup rewrite-lookup]
    ['var-set set-rewrite]
    ['program-lookup rewrite-lookup]
    [':lookup rewrite-lookup]
    [':set set-rewrite]
    ['check-program
     (match-lambda 
       [(list op id p Π cp)
        (list (turn "⊨" "prog") p " : " Π)])]
    ['check-blocks
     (match-lambda
       [(list op ip  Π p cp)
        (list "" Π (turn "⊢" "blocks") p)])]
    ['check-block
     (match-lambda
       [(list op id Π Γ ι cp)
        (list "" Π ";" Γ (turn "⊢" "blocks") ι)])]
    ['check-instr
     (match-lambda
       [(list op id Π Γ1 ι Γ2 cp)
        (list "" Π (turn "⊢" "instr") Γ1 "{" ι "}" Γ2)])]
    ['l-⊂ subset-rewrite]
    ['Γ-⊂ subset-rewrite]
    ['⊔ 
     (match-lambda
       [(list op id τ1 τ2 τ3 cp)
        (list "" τ1 "⊔" τ2 " = " τ3)])]
    ['dom
     (λ (lws)
       (define arg (list-ref lws 2))
       (list "dom(" arg ")"))])
   (t)))

(define (scale-to-fit/m p)
  (scale p (min (/ (- 1024 margin margin) (pict-width p))
                (/ (- 768 margin margin) (pict-height p)))))

(slide 
 (with-rewriters
  (λ ()
    (scale-to-fit/m
     (ht-append
      40
      (language->pict list-machine #:nts '(a p ι))
      (reduction-relation->pict red))))))

(slide
 (with-rewriters
  (λ ()
    (scale-to-fit/m
     (judgment-form->pict check-instr)))))

(slide
 (with-rewriters
  (λ ()
    (scale-to-fit/m
     (vc-append
      40
      (judgment-form->pict check-block)
      (judgment-form->pict check-blocks)
      (judgment-form->pict check-program))))))
