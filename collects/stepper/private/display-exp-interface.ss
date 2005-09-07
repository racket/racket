(module display-exp-interface mzscheme
  
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "contract.ss")
           "my-macros.ss"
           "highlight-placeholder.ss")
  
  (provide exp-without-holes?
           exp-with-holes?)
  
  ; an exp-with-holes is either:
  ; - a pair of exp-with-holes's,
  ; - null,
  ; - a symbol, or
  ; - the highlight-placeholder
  
  (define exp-without-holes-base-case? (union symbol? number? string? null? (lambda (v) (is-a? v snip%))))
  
  (define exp-without-holes?
    (union  exp-without-holes-base-case?
           (and/c pair? (cons/c (lx ((flat-contract-predicate exp-without-holes?) _))
                                (lx ((flat-contract-predicate exp-without-holes?) _))))))
  
  (define exp-with-holes-base-case? 
    (union exp-without-holes-base-case?
          (lx (eq? _ highlight-placeholder))))
  
  (define exp-with-holes?
    (union exp-with-holes-base-case?
          (and/c pair? (cons/c (lx ((flat-contract-predicate exp-with-holes?) _)) 
                               (lx ((flat-contract-predicate exp-with-holes?) _))))))
  
)