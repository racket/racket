(module internal-hp mzscheme
  (require (lib "dirs.ss" "setup")
           (lib "config.ss" "planet"))
  (provide internal-port 
           is-internal-host? internal-host
	   collects-hosts collects-dirs
	   doc-hosts doc-dirs
           planet-host)

  ;; Hostnames defined here should not exist as real machines

  ;; The general idea is that there's one virtual host for
  ;;  every filesystem tree that we need to access.
  ;; The "get-help-url.ss" library provides a function to
  ;;  convert a path into a suitable URL (i.e., a URL using
  ;;  the right virtual host).
  ;; The "gui.ss" library performs a bit of extra URL
  ;;  processing at the last minute, sometimes switching
  ;;  a URL for a manual to a different host. (That's needed
  ;;  when cross-manual references are implemented as relative
  ;;  URLs.)

  (define internal-host "helpdesk-internal.localhost")
  (define internal-port 8000)

  (define (is-internal-host? str)
    (member str all-internal-hosts))
  
  (define (generate-hosts prefix dirs)
    (let loop ([dirs dirs][n 0])
      (if (null? dirs)
	  null
	  (cons (format "~a~a.~a" prefix n internal-host)
		(loop (cdr dirs) (add1 n))))))

  (define planet-host (format "planet.~a" internal-host))
  
  (define collects-dirs
    (get-collects-search-dirs))
  (define collects-hosts
    (generate-hosts "collects" collects-dirs))

  (define doc-dirs
    (get-doc-search-dirs))
  (define doc-hosts
    (generate-hosts "doc" doc-dirs))
  
  (define all-internal-hosts 
    (append (list internal-host planet-host)
            collects-hosts
            doc-hosts)))
