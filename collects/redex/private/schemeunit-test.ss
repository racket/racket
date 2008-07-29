(module schemeunit-test mzscheme
  (require "../schemeunit.ss"
           (all-except "../reduction-semantics.ss" check)
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2))
           (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  
  (define-language lang
    (e number (+ e e) (choose e e))
    (ec hole (+ e ec) (+ ec e))
    (v number true false))
  
  (define reductions
    (reduction-relation
     lang
     (--> (in-hole ec_1 (+ number_1 number_2))
          (in-hole ec_1 ,(+ (term number_1) (term number_2))))
     (--> (in-hole ec_1 (choose e_1 e_2))
          (in-hole ec_1 e_1))
     (--> (in-hole ec_1 (choose e_1 e_2))
          (in-hole ec_1 e_2))))
  
  (define tests-passed 0)
  
  (define (try-it check in out key/vals)
    (let ([sp (open-output-string)])
      (parameterize ([current-output-port sp])
        (test/text-ui (test-case "X" (check reductions in out))))
      (let ([s (get-output-string sp)])
        (for-each 
         (λ (key/val)
           (let* ([key (car key/val)]
                  [val (cadr key/val)]
                  [m (regexp-match (format "\n~a: ([^\n]*)\n" key) s)])
             (unless m 
               (error 'try-it "didn't find key ~s in ~s" key s))
             (unless (if (regexp? val)
                         (regexp-match val (cadr m))
                         (equal? val (cadr m)))
               (error 'try-in "didn't match key ~s, expected ~s got ~s" key val (cadr m)))))
         key/vals)))
    (set! tests-passed (+ tests-passed 1)))

  (try-it check-reduces
          '(choose 1 2)
          1 
          '((multiple-results "(2 1)")))
  
  (try-it check-reduces
          '(+ 1 2) 
          1
          '((expected "1")
            (actual "3")))
  
  (try-it check-reduces/multiple
          '(+ (choose 3 4) 1) 
          '(4 6)
          '((expecteds "(4 6)")
            (actuals #rx"[(][45] [54][)]")))
  
  (printf "schemeunit-tests: all ~a tests passed.\n" tests-passed))
