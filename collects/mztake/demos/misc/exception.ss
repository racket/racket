(module exception mzscheme
  (thread (lambda () (raise 'exn:oops-made-a-mztake!))))