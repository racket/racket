((port 80)
 (max-waiting 40)
 (initial-connection-timeout 30)
 (default-host-table
   (host-table
     (default-indices "index.html" "index.htm")
     (log-format parenthesized-default)
     (messages
       (servlet-message
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/servlet-error.html")
       (authentication-message
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/forbidden.html")
       (servlets-refreshed
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/servlet-refresh.html")
       (passwords-refreshed
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/passwords-refresh.html")
       (file-not-found-message
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/not-found.html")
       (protocol-message
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/protocol-error.html")
       (collect-garbage
         "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./conf/collect-garbage.html"))
     (timeouts
       (default-servlet-timeout 30)
       (password-connection-timeout 300)
       (servlet-connection-timeout 86400)
       (file-per-byte-connection-timeout 1/20)
       (file-base-connection-timeout 30))
     (paths
      (configuration-root "conf")
      (host-root
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/.")
      (log-file-path
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./log")
      (file-root
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./htdocs")
      (servlet-root
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./.")
      (mime-types
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./mime.types")
      (password-authentication
        "/Users/mflatt/proj/plt/collects/web-server/default-web-root/./passwords"))))
 (virtual-host-table))
