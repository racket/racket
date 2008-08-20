#lang web-server/template
@(require "module-test.ss")
<html>
  <head><title>@$title </title></head>
  <body>
    <table>
      @in[client $clients]{
      <tr>
        <td>@(client-surname client), @(client-fname client)
        <td><a href="mailto:@(client-email client)">@(client-email client)</a></td>
      </tr>
      }
    </table>
  </body>
</html>

@; Example:
@;{
  (template #:title "Title"
            #:clients
            (list (make-client "First1" "Last1" "email1")
                  (make-client "First2" "Last2" "email2")
                  (make-client "First3" "Last3" "email3")
                  (make-client "First4" "Last4" "email4")
                  (make-client "First5" "Last5" "email5")))
  }