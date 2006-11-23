(module maillist mzscheme
  (require "../private/headelts.ss"
           (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head ,hd-css ,@hd-links (title "Mailing Lists"))
      (body
       (a ([name "mail"] [value "mailing lists"]))
       (h1 "Mailing Lists")
       "PLT maintains two English-language mailing lists: one for"
       " announcements, the other for discussion.  There is a discussion list"
       " in Spanish."
       (p)
       (hr)
       (p)
       (b "Announcements List") (br)
       "The announcement-only list is designed for people who need to track"
       " releases and patches.  The list is moderated.  There are a handful"
       " of postings a year."
       (p)
       "To subscribe to " (tt "plt-announce@list.cs.brown.edu") ", visit the "
       "Web page "
       (blockquote
        (a ([href "http://list.cs.brown.edu/mailman/listinfo/plt-announce/"]
            [target "_top"])
           "http://list.cs.brown.edu/mailman/listinfo/plt-announce/"))
       " or send email to "
       (blockquote
        (a ([href "mailto:plt-announce-request@list.cs.brown.edu"])
           "plt-announce-request@list.cs.brown.edu"))
       " with the word `help' in the subject or body of the message."
       " You'll get back a message with instructions."
       (p)
       (hr)
       (p)
       (b "Discussion List") (br)
       "If you have problems with installation, or questions about "
       "using PLT Scheme, send mail to the list "
       (blockquote
        (a ([href "mailto:plt-scheme@list.cs.brown.edu"])
           "plt-scheme@list.cs.brown.edu"))
       (p)
       "Only subscribers can post to the list."
       " To subscribe, visit the Web page "
       (blockquote
        (a ([href "http://list.cs.brown.edu/mailman/listinfo/plt-scheme/"]
            [target "_top"])
           "http://list.cs.brown.edu/mailman/listinfo/plt-scheme/"))
       " or send email to "
       (blockquote
        (a ((href "mailto:plt-scheme-request@list.cs.brown.edu"))
           "plt-scheme-request@list.cs.brown.edu"))
       " with the word `help' in the subject or body of the message. "
       "You'll get back a message with instructions."
       (p)
       (hr)
       (p)
       (a ([name "mail-es"] [value "Spanish mailing lists"]))
       (a ([name "mail-es2"] [value "Lista de Correo"]))
       (b "Lista de Correo") (br)
       "Si tienes problemas con la instalación o preguntas sobre el uso"
       " de PLT Scheme, envía un mensaje a la lista "
       (blockquote
        (a ([href "mailto:plt-scheme-es@list.cs.brown.edu"])
           "plt-scheme-es@list.cs.brown.edu"))
       "Para reducir la recepción de mensajes no deseados (SPAM), "
       "hemos adoptado la política de que sólo los suscriptores a la lista "
       "pueden enviar mensajes. Para suscribirte, visita la página de Web "
       (blockquote
        (a ([href "http://list.cs.brown.edu/mailman/listinfo/plt-scheme-es/"]
            [target "_top"])
           "http://list.cs.brown.edu/mailman/listinfo/plt-scheme-es/"))
       " o envía un mensaje a "
       (blockquote
        (a ([href "mailto:plt-scheme-es-request@list.cs.brown.edu"])
           "plt-scheme-es-request@list.cs.brown.edu"))
       " con la palabra `help' en el asunto o en el cuerpo de tu mensaje. "
       "Recibirás un mensaje de regreso con instrucciones."))))
