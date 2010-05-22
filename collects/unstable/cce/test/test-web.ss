#lang scheme

(require "checks.ss"
         "../web.ss")

(provide web-suite)

(define web-suite
  (test-suite "web.ss"
    (test-suite "css?"
      (test-true "CSS" (css? '((foo (a b) (c d)) (bar (w x) (y z)))))
      (test-false "not CSS" (css? '(a b c d))))
    (test-suite "css/c"
      (test-ok "CSS" (with/c css/c '((foo (a b) (c d)) (bar (w x) (y z)))))
      (test-bad "not CSS" (with/c css/c '(a b c d))))
    (test-suite "xexpr/c"
      (test-ok "XExpr" (with/c xexpr/c '(a ([href "url"]) "somewhere")))
      (test-bad "not XExpr" (with/c xexpr/c '(a ("href" url) . "nowhere"))))
    (test-suite "write-css")
    (test-suite "write-xexpr")
    (test-suite "create-stylesheet")
    (test-suite "create-webpage")))
