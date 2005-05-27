(module formula mzscheme

  (provide (all-defined))

  (define-struct formula (name dependencies) (make-inspector))
  (define-struct (xl-number formula) (val) (make-inspector))
  (define-struct (cell-ref formula) () (make-inspector))
  (define-struct (named-cell-ref cell-ref) (cell-name) (make-inspector))
  (define-struct (binary-op formula) (op arg1 arg2) (make-inspector))
  (define-struct (boolean-op formula) (op arg1 arg2) (make-inspector))
  (define-struct (unary-op formula) (op arg) (make-inspector))
  (define-struct (tbl-top formula) (input-cell) (make-inspector))
  (define-struct (tbl-left formula) (input-cell) (make-inspector))
  (define-struct (application formula) (fun args) (make-inspector)))
