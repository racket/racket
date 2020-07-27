;;; Copyright (c) 2000-2015 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(library (tests unit-test-helpers-implementation)
  (export with-output-to-string display-condition)
  (import (ikarus))
  
  (define display-condition
    (case-lambda
      [(c) (display-condition c (current-output-port))]
      [(c op)
       (display
         (format "~a~a~a~a~a"
           (if (warning? c) "Warning" "Exception")
           (if (who-condition? c) (format " in ~s" (condition-who c)) "")
           (if (message-condition? c) (format ": ~a" (condition-message c)) "")
           (if (irritants-condition? c) (format " with irritants ~s" (condition-irritants c)) "")
           (if (syntax-violation? c)
               (if (syntax-violation-subform c)
                   (format "~s in ~s" (syntax-violation-subform c) (syntax-violation-form c))
                   (format "~s" (syntax-violation-form c)))
               ""))
         op)])))
