
(module deriv-show mzscheme
  (require (lib "mrpict.ss" "texpict")
           (lib "utils.ss" "texpict")
           (lib "match.ss")
           (lib "pretty.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  (require "../model/deriv.ss")

  (define code-size 12)

  (define (draw deriv)
    (define (redraw canvas dc)
      (draw-pict the-pict dc 10 10))
    (define f (new frame% (label "Derivation") (width 200) (height 200)))
    (define c (new canvas%
                   (parent f)
                   (style '(hscroll vscroll))
                   (paint-callback redraw)))
    (define _ (dc-for-text-size (new bitmap-dc%)))
    (define the-pict (show deriv))
    (send c init-auto-scrollbars 1000 1000 0.0 0.0)
    (send f show #t))
  
  ;; code : syntax -> pict
  (define (code stx)
    (let ([out (open-output-string)])
      (print (syntax-object->datum stx) out)
      (text (get-output-string out) null code-size)))
  
  ;; show : Derivation -> pict
  (define (show deriv)
    (match deriv
      [($ pderiv e1 e2 p)
       (VJ (show-prule p)
           (J e1 e2))]
      [($ mderiv e1 e2 mrule next)
       (let ([top (A (show-mrule mrule) (show next))]
             [bottom (J e1 e2)])
         (vc-append 5
                    top
                    (hline (max (pict-width top) (pict-width bottom)) 1)
                    bottom))]))

  (define (VJ top bottom)
    (color-frame
     (if top
         (vc-append 5
                    top
                    (hline (max (pict-width top) (pict-width bottom)) 1)
                    bottom)
         (vc-append (hline (pict-width bottom) 1)
                    bottom))
     "gray"))
  
  (define (show-mrule rule)
    (match rule
      [($ mrule e1 e2 _ _ resolves locals)
       (hb-append (code e1) (text " -> " null code-size) (code e2))]))
  
  (define (J e1 e2)
    (colorize (hb-append (code e1) (text " => " null code-size) (code e2)) "blue"))
  (define (A . args)
    (apply hb-append 10 args))
  
  (define (show-prule pr)
    (match pr
      [($ p:define-values e1 e2 rs rhs)
       (show rhs)]
      [($ p:define-syntaxes e1 e2 rs rhs)
       (show rhs)]
      [($ p:if e1 e2 rs full? test then else)
       (if full?
           (A (show test) (show then) (show else))
           (A (show test) (show then)))]
      [($ p:wcm e1 e2 rs key value body)
       (A (show key) (show value) (show body))]
      [($ p:set! _ _ _ _ rhs)
       (show rhs)]
      [($ p:set!-macro _ _ _ inner)
       (show inner)]
      [($ p:begin _ _ _ lderiv)
       (show-lderiv lderiv)]
      [($ p:begin0 _ _ _ deriv0 lderiv)
       (A (show deriv0) (show-lderiv lderiv))]
      [($ p:#%app _ _ _ lderiv)
       (show-lderiv lderiv)]
      [($ p:lambda _ _ _ renames body)
       (show-bderiv body)]
      ;; case-lambda
      ;; let-values
      ;; let*-values
      ;; letrec-values
      ;; letrec-syntaxes+values
      [($ prule e1 e2 rs)
       #f #;(text "" null code-size)]))
  
  (define (show-lderiv ld)
    (match ld
      [($ lderiv es1 es2 derivs)
       (vc-append 5
                  (apply A (map show derivs))
                  (J es1 es2))]))
  
  (define (show-bderiv bd)
    (colorize (text "block" null code-size) "red"))
  
  )
