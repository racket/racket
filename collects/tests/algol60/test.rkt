#lang at-exp racket/base
(require algol60/algol60
         rackunit
         (for-syntax racket/base))

(define-syntax (capture-output stx)
  (syntax-case stx ()
    [(_ exp)
     (with-handlers ((exn:fail?
                      (λ (exn)
                        #`(list 'expand
                                #,(exn-message exn)))))
       (define expanded (local-expand #'exp 'expression #f))
       #`(let ([op (open-output-string)]
               [ep (open-output-string)])
           (let/ec k
             (parameterize ([current-output-port op]
                            [current-error-port ep]
                            [error-escape-handler (λ () (k (void)))])
               #,expanded))
           (list 'run
                 (get-output-string op)
                 (get-output-string ep))))]))

(check-equal?
 (capture-output
  @literal-algol{
begin
  printsln (`hello world')
end
})
 '(run "hello world\n" ""))

(check-pred
 (λ (x) (and (eq? (list-ref x 0) 'expand)
             (regexp-match #rx"parse error near BEGIN"
                           (list-ref x 1))))
 (capture-output
  @literal-algol{
begin
}))


(check-pred
 (λ (x) (and (eq? (list-ref x 0) 'expand)
             (regexp-match #rx"parse error near PROCEDURE"
                           (list-ref x 1))))
 (capture-output
  @literal-algol{
procedure Absmax(a) Size:(n, m) Result:(y) Subscripts:(i, k);
   value n, m; array a; integer n, m, i, k; real y;
begin integer p, q;
   y := 0; i := k := 1;
   for p:=1 step 1 until n do
   for q:=1 step 1 until m do
       if abs(a[p, q]) > y then
           begin y := abs(a[p, q]);
           i := p; k := q
           end
end Absmax}))
