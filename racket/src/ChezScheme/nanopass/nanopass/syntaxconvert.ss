;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (nanopass syntaxconvert)
  (export convert-pattern)
  (import (rnrs) (nanopass helpers)) 

  (define convert-pattern
    ; accepts pattern & keys
    ; returns syntax-dispatch pattern & ids
    (lambda (pattern)
      (define cvt*
        (lambda (p* n flds lvls maybes)
          (if (null? p*)
              (values '() flds lvls maybes)
              (let-values ([(y flds lvls maybes) (cvt* (cdr p*) n flds lvls maybes)])
                (let-values ([(x flds lvls maybes) (cvt (car p*) n flds lvls maybes)])
                  (values (cons x y) flds lvls maybes))))))
      (define cvt
        (lambda (p n flds lvls maybes)
          (if (identifier? p)
              (values 'any (cons p flds) (cons n lvls) (cons #f maybes))
              (syntax-case p ()
                [(x dots)
                 (ellipsis? (syntax dots))
                 (let-values ([(p flds lvls maybes) (cvt (syntax x) (fx+ n 1) flds lvls maybes)])
                   (values (if (eq? p 'any) 'each-any (vector 'each p)) flds lvls maybes))]
                [(x dots y ... . z) 
                 (ellipsis? (syntax dots))
                 (let-values ([(z flds lvls maybes) (cvt (syntax z) n flds lvls maybes)])
                   (let-values ([(y flds lvls maybes) (cvt* (syntax (y ...)) n flds lvls maybes)])
                     (let-values ([(x flds lvls maybes) (cvt (syntax x) (fx+ n 1) flds lvls maybes)])
                       (values `#(each+ ,x ,(reverse y) ,z) flds lvls maybes))))]
                [(maybe x)
                 (and (identifier? #'x) (eq? (datum maybe) 'maybe))
                 (values 'any (cons #'x flds) (cons n lvls) (cons #t maybes))]
                [(x . y)
                 (let-values ([(y flds lvls maybes) (cvt (syntax y) n flds lvls maybes)])
                   (let-values ([(x flds lvls maybes) (cvt (syntax x) n flds lvls maybes)])
                     (values (cons x y) flds lvls maybes)))]
                [() (values '() flds lvls maybes)]
                [oth (syntax-violation 'cvt "unable to find match" #'oth)]))))
      (cvt pattern 0 '() '() '()))))
  

