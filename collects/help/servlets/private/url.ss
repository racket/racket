(module url mzscheme
  (require "../../private/internal-hp.ss")

  (provide (all-defined))

  (define url-helpdesk-root
    (format "http://~a:~a/servlets/" internal-host (internal-port)))

  (define url-helpdesk-home         (string-append url-helpdesk-root "home.ss"))
  (define url-helpdesk-results      (string-append url-helpdesk-root "results.ss"))
  (define url-helpdesk-master-index (string-append url-helpdesk-root "master-index.ss"))


  (define (url-home-subpage subpage-str)
    (string-append url-helpdesk-home "?subpage=" subpage-str))

  (define (version-major)
    ; TODO: Fix this
    (cond [(regexp-match #px"^(\\d+).*$" (version))
           => cadr]
          [else "352"]))

  (define (url-manual-on-doc-server manual)
    (format "http://download.plt-scheme.org/doc/~a/html/~a/"
            (version-major) manual))

  (define (url-static doc manual path)
    (format "~astatic.ss/~a/~a/~a"
            url-helpdesk-root doc manual path))

  (define url-external-announcement-list-archive     "http://list.cs.brown.edu/pipermail/plt-announce/")
  (define url-external-discussion-list-archive       "http://list.cs.brown.edu/pipermail/plt-scheme/")
  (define url-external-discussion-list-archive-old   "http://www.cs.utah.edu/plt/mailarch/")
  (define url-external-mailing-list-subscription     "http://www.plt-scheme.org/maillist/")
  (define url-external-mrflow                        "http://www.plt-scheme.org/software/mrflow/")
  (define url-external-mrspidey                      "http://www.plt-scheme.org/software/mrspidey/")
  (define url-external-mysterx                       "http://www.plt-scheme.org/software/mysterx/")
  (define url-external-mzcom                         "http://www.plt-scheme.org/software/mzcom/")
  (define url-external-send-bug-report               "http://bugs.plt-scheme.org/")
  (define url-external-tour-of-drscheme              "http://www.plt-scheme.org/software/drscheme/tour/")
  (define url-external-planet                        "http://planet.plt-scheme.org/")
  (define url-external-srpersist                     "http://www.plt-scheme.org/software/srpersist/")

  (define url-helpdesk-acknowledge          (url-home-subpage "acknowledge"))
  (define url-helpdesk-batch                (url-home-subpage "batch"))
  (define url-helpdesk-books                (url-home-subpage "books"))
  (define url-helpdesk-cgi                  (url-home-subpage "cgi"))
  (define url-helpdesk-databases            (url-home-subpage "databases"))
  (define url-helpdesk-documentation        (url-home-subpage "documentation"))
  (define url-helpdesk-drscheme             (url-home-subpage "drscheme"))
  (define url-helpdesk-drscheme-faq         (url-static "doc1" "drscheme" "drscheme-Z-H-5.html#node_chap_5"))
  (define url-helpdesk-drscheme-manual      (url-static "doc1" "drscheme" "index.htm"))
  (define url-helpdesk-faq                  (url-home-subpage "faq"))
  (define url-helpdesk-graphics             (url-home-subpage "graphics"))
  (define url-helpdesk-help                 (url-home-subpage "help"))
  (define url-helpdesk-how-to-search        (url-home-subpage "how-to-search"))
  (define url-helpdesk-interface-essentials (url-static "doc1" "drscheme" "drscheme-Z-H-2.html#node_chap_2"))
  (define url-helpdesk-known-bugs           (url-home-subpage "known-bugs"))
  (define url-helpdesk-languages            (url-home-subpage "languages"))
  (define url-helpdesk-libraries            (url-home-subpage "libraries"))
  (define url-helpdesk-license              (url-home-subpage "license"))
  (define url-helpdesk-manuals              (url-home-subpage "manuals"))
  (define url-helpdesk-mailing-lists        (url-home-subpage "mailing-lists"))
  (define url-helpdesk-mzlib                (url-static "doc1" "mzlib" "mzlib.html"))
  (define url-helpdesk-patches              (url-home-subpage "patches"))
  (define url-helpdesk-program-design       (url-home-subpage "program-design"))
  (define url-helpdesk-release              (url-home-subpage "release"))
  (define url-helpdesk-release-notes        (url-home-subpage "release-notes"))
  (define url-helpdesk-script               (url-home-subpage "script"))
  (define url-helpdesk-search               (url-home-subpage "search"))
  (define url-helpdesk-software             (url-home-subpage "software"))
  (define url-helpdesk-srpersist            (url-home-subpage "srpersist"))
  (define url-helpdesk-stand-alone          (url-home-subpage "stand-alone"))
  (define url-helpdesk-system               (url-home-subpage "system"))
  (define url-helpdesk-teachpacks           (url-home-subpage "teachpacks"))
  (define url-helpdesk-teachscheme          (url-home-subpage "teachscheme"))
  (define url-helpdesk-teachpacks-for-htdp  (url-static "doc1" "teachpack" "index.html#HtDP"))
  (define url-helpdesk-teachpacks-for-htdc  (url-static "doc1" "teachpack-htdc" "index.html#HtDC"))
  (define url-helpdesk-teach-yourself       (url-static "doc1" "t-y-scheme" "index.htm"))
  (define url-helpdesk-tour                 (url-home-subpage "tour"))
  (define url-helpdesk-why-drscheme         (url-home-subpage "why-drscheme"))

  )
