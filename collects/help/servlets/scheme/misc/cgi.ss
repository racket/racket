(module cgi mzscheme
  (require "../../private/headelts.ss")
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE  "How to write CGI scripts"))
      (BODY 
       (H1  "How to write CGI scripts")
       (A ((NAME "cgi") (VALUE "CGI scripts")))
       "Type " (TT  "CGI") " in the " (B  "Search for") " "
       "field in Help Desk and click on the "
       (B  (TT  "Search")) " button to get information "
       "on CGI-related functions."
       (P)
       "A CGI script is merely a program with funny inputs and "
       "outputs.  Input comes either from an environment variable "
       "or through the standard input port, in a special format. "
       "Output consists of a MIME header followed by the content.  "
       "Everything in-between is pure program."
       (P)
       "MzScheme comes with a CGI library that is designed to "
       "make it easy to write such scripts.  In the mini-tutorial "
       "below, we'll walk you through the "
       "construction of such a script.  If you have questions or "
       "comments, send email to "
       (A ((HREF "mailto:sk@plt-scheme.org"))
          "sk@plt-scheme.org") "."
          (P)
          (HR)
          (P)
          "Let's write a simple \"finger server\" in MzScheme. "
          "The front-end will be a Web form that accepts a username. "
          "The form should supply a username in the field `name'. "
          "The CGI script fingers that user."
          (P)
          "First, make sure you have MzScheme installed on the host "
          "where your Web server is located."
          (P)
          "A CGI script must be an executable.  Each OS has different "
          "ways of launching an application.  Under Unix, it's "
          "probably easiest to make them simple shell scripts.  "
          "Therefore, place the following magic incantation at the "
          "top of your script:"
          (P)
          (PRE 
           " #!/bin/sh" (BR)
           " string=? ; exec /usr/local/bin/mzscheme -r $0 \"$@\"")
          (P)
          "Make sure the path to MzScheme is specified correctly."
          (P)
          "Now we're in Scheme-land.  First, let's load the Scheme "
          "CGI library and define where `finger' resides."
          (P)
          (PRE 
           " (require (lib \"cgi.ss\" \"net\"))" (BR)
           " (define finger-program \"/usr/bin/finger\")")
          (P)
          "Next we must get the names bound by the form, and "
          "extract the username field."
          (P)
          (PRE 
           " (let ((bindings (get-bindings)))" (BR)
           "   (let ((name (extract-binding/single 'name bindings)))")
          (P)
          "We use extract-binding/single to make sure only one name "
          "field was bound.  (You can bind the same field multiple "
          "times using check-boxes.  This is just one kind of "
          "error-checking; a robust CGI script will do more."
          (P)
          "Next we invoke the finger program using `process*'.  "
          "If no username was specified, we just run finger on the host."
          (P)
          (PRE 
           " (let ((results (if (string=? name \"\"))" (BR)
           "   (process* finger-program)" (BR)
           "   (process* finger-program name))))")
          (P)
          "The `process*' function returns a list of several values. "
          "The first of these is the output port.  Let's pull this "
          "out and name it."
          (P)
          (PRE 
           " (let ((proc->self (car results)))")
          (P)
          "Now we extract the output of running finger into a "
          "list of strings."
          (P)
          (PRE 
           " (let ((strings (let loop " (BR)
           "   (let ((l (read-line proc->self)))" (BR)
           "     (if (eof-object? l)" (BR)
           "        null" (BR)
           "        (cons l (loop))))))))")
          (P)
          "All that's left is to print this out to the user.  "
          "We use the `generate-html-output' procedure to do that, "
          "which takes care of generating the appropriate MIME header "
          "(as required of CGI scripts). "
          "Note that the <PRE> tag of HTML doesn't prevent its "
          "contents from being processed.  To avoid this "
          "(i.e., to generate truly verbatim output), "
          "we use `string->html', which knows about HTML quoting "
          "conventions."
          (P)
          (PRE 
           " (generate-html-output \"Finger Gateway Output\"" (BR)
           "   (append " (BR)
           "    '(\"<PRE>\")" (BR)
           "    (map string->html strings)" (BR)
           "    '(\"</PRE>\"))))))))")
          (P)
          "That's all!  This program will work irrespective of "
          "whether the form uses a GET or POST method to send its "
          "data over, which gives designers additional flexibility "
          "(GET provides a weak form of persistence, while "
          "POST is more robust and better suited to large volumes of "
          "data)."
          (P)
          "Here's the entire program, once again:"
          (P)
          (PRE 
           " #!/bin/sh" (BR)
           " string=? ; exec /usr/local/bin/mzscheme -r $0 "$@"" (BR)
           "" (BR)
           " (require (lib \"cgi.ss\" \"net\"))" (BR)
           " (define finger-program \"/usr/bin/finger\")" (BR)
           ""  (BR)
           " (let ((bindings (get-bindings)))" (BR)
           "   (let ((name (extract-binding/single 'name bindings)))" (BR)
           "    (let ((results (if (string=? name "")" (BR)
           "      (process* finger-program)" (BR)
           "     (process* finger-program name))))" (BR)
           "     (let ((proc->self (car results)))" (BR)
           "       (let ((strings (let loop " (BR)
           "                        (let ((l (read-line proc->self)))" (BR)
           "                          (if (eof-object? l)" (BR)
           "                            null" (BR)
           "                            (cons l (loop)))))))" (BR)
           "         (generate-html-output \"Finger Gateway Output\"" (BR)
           "         (append" (BR)
           "           '(\"<PRE>\")" (BR)
           "           (map string->html strings)" (BR)
           "           '(\"</PRE>\"))))))))")))))