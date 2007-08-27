(module cgi mzscheme
  (require "../../private/headelts.ss")

  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)

  (define (start initial-request)
    (with-errors-to-browser
     send/finish
     (lambda ()
       `(html
         (head ,hd-css
               ,@hd-links
               (title  "How to write CGI scripts"))
         (body
          (h1  "How to write CGI scripts")
          (a ([name "cgi"] (value "CGI scripts")))
          "Type " (tt  "CGI") " in the " (b "Search for") " "
          "field in Help Desk and click on the "
          (b (tt "Search")) " button to get information "
          "on CGI-related functions."
          (p)
          "A CGI script is merely a program with funny inputs and "
          "outputs.  Input comes either from an environment variable "
          "or through the standard input port, in a special format. "
          "Output consists of a MIME header followed by the content.  "
          "Everything in-between is pure program."
          (p)
          "MzScheme comes with a CGI library that is designed to "
          "make it easy to write such scripts.  In the mini-tutorial "
          "below, we'll walk you through the "
          "construction of such a script.  If you have questions or "
          "comments, send email to "
          (a ((href "mailto:sk@plt-scheme.org")) "sk@plt-scheme.org") "."
          (p)
          (hr)
          (p)
          "Let's write a simple \"finger server\" in MzScheme. "
          "The front-end will be a Web form that accepts a username. "
          "The form should supply a username in the field `name'. "
          "The CGI script fingers that user."
          (p)
          "First, make sure you have MzScheme installed on the host "
          "where your Web server is located."
          (p)
          "A CGI script must be an executable.  Each OS has different "
          "ways of launching an application.  Under Unix, it's "
          "probably easiest to make them simple shell scripts.  "
          "Therefore, place the following magic incantation at the "
          "top of your script:"
          (p)
          (pre " #!/bin/sh" (br)
               " string=? ; exec /usr/local/bin/mzscheme -r $0 \"$@\"")
          (p)
          "Make sure the path to MzScheme is specified correctly."
          (p)
          "Now we're in Scheme-land.  First, let's load the Scheme "
          "CGI library and define where `finger' resides."
          (p)
          (pre 
           " (require (lib \"cgi.ss\" \"net\"))" (br)
           " (define finger-program \"/usr/bin/finger\")")
          (p)
          "Next we must get the names bound by the form, and "
          "extract the username field."
          (p)
          (pre 
           " (let ((bindings (get-bindings)))" (br)
           "   (let ((name (extract-binding/single 'name bindings)))")
          (p)
          "We use extract-binding/single to make sure only one name "
          "field was bound.  (You can bind the same field multiple "
          "times using check-boxes.  This is just one kind of "
          "error-checking; a robust CGI script will do more."
          (p)
          "Next we invoke the finger program using `process*'.  "
          "If no username was specified, we just run finger on the host."
          (p)
          (pre
           " (let ((results (if (string=? name \"\"))" (br)
           "   (process* finger-program)" (br)
           "   (process* finger-program name))))")
          (p)
          "The `process*' function returns a list of several values. "
          "The first of these is the output port.  Let's pull this "
          "out and name it."
          (p)
          (pre
           " (let ((proc->self (car results)))")
          (p)
          "Now we extract the output of running finger into a "
          "list of strings."
          (p)
          (pre
           " (let ((strings (let loop " (br)
           "   (let ((l (read-line proc->self)))" (br)
           "     (if (eof-object? l)" (br)
           "        null" (br)
           "        (cons l (loop))))))))")
          (p)
          "All that's left is to print this out to the user.  "
          "We use the `generate-html-output' procedure to do that, "
          "which takes care of generating the appropriate MIME header "
          "(as required of CGI scripts). "
          "Note that the <pre> tag of HTML doesn't prevent its "
          "contents from being processed.  To avoid this "
          "(i.e., to generate truly verbatim output), "
          "we use `string->html', which knows about HTML quoting "
          "conventions."
          (p)
          (pre
           " (generate-html-output \"Finger Gateway Output\"" (br)
           "   (append " (br)
           "    '(\"<pre>\")" (br)
           "    (map string->html strings)" (br)
           "    '(\"</pre>\"))))))))")
          (p)
          "That's all!  This program will work irrespective of "
          "whether the form uses a GET or POST method to send its "
          "data over, which gives designers additional flexibility "
          "(GET provides a weak form of persistence, while "
          "POST is more robust and better suited to large volumes of "
          "data)."
          (p)
          "Here's the entire program, once again:"
          (p)
          (pre
           " #!/bin/sh" (br)
           " string=? ; exec /usr/local/bin/mzscheme -r $0 \"$@\"" (br)
           "" (br)
           " (require (lib \"cgi.ss\" \"net\"))" (br)
           " (define finger-program \"/usr/bin/finger\")" (br)
           "" (br)
           " (let ((bindings (get-bindings)))" (br)
           "   (let ((name (extract-binding/single 'name bindings)))" (br)
           "    (let ((results (if (string=? name "")" (br)
           "      (process* finger-program)" (br)
           "     (process* finger-program name))))" (br)
           "     (let ((proc->self (car results)))" (br)
           "       (let ((strings (let loop " (br)
           "                        (let ((l (read-line proc->self)))" (br)
           "                          (if (eof-object? l)" (br)
           "                            null" (br)
           "                            (cons l (loop)))))))" (br)
           "         (generate-html-output \"Finger Gateway Output\"" (br)
           "         (append" (br)
           "           '(\"<pre>\")" (br)
           "           (map string->html strings)" (br)
           "           '(\"</pre>\"))))))))")))))))
