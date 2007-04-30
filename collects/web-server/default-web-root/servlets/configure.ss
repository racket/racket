; DO NOT DELETE THIS SERVLET,
; unless you never want to reconfigure the Web server again.
; The servlet accepts requests only from the *same machine* as the Web server
; for security purposes.
(module configure mzscheme
  (require (lib "configure.ss" "web-server" "private"))
  (provide (all-from (lib "configure.ss" "web-server" "private"))))