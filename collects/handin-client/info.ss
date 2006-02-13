(module info (lib "infotab.ss" "setup")
  ;; Modify these definitions to customize the tool.
  ;; Also replace the "icon.png" and "server-cert.pem" files.
  ;; Instead of uncommenting the definition of server:port, you
  ;; can set the PLT_HANDIN_SERVER_PORT environment variable.
  (define name "Course")
  (define collection "handin-client")
  ;(define server:port "localhost:7979")

  ;; The following are optional. Uncomment and fill in
  ;; the values to add a menu item under "Help" to open
  ;; the specified web page (using the user's chosen web
  ;; browser.)
  ;(define web-menu-name "Course Homepage")
  ;(define web-address "http://www.university.edu/course/")

  (define tools      `(("client-gui.ss")))
  (define tool-names `(,name))
  (define tool-icons `(("icon.png" ,collection)))

  (define requires '(("mred") ("openssl")))

  ;; Auto-updater section (see handin-server/doc.txt for details)
  ;(define enable-auto-update #t) ; enable auto-update?
  ;(define version-filename "handin-version")
  ;(define package-filename "handin.plt")

  ;; Multi-file submission section (see handin-server/doc.txt for details)
  ;(define enable-multifile-handin #t) ; enable multi-file?
  ;(define selection-mode 'extended) ; mode for file choose, usually 'extended
  ;(define selection-default ; suffixes to auto-choose (string or string-list)
  ;  '("*.scm;*.ss" "*.scm;*.ss;*.txt"))

  )
