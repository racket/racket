#lang web-server/template
@(title clients client-surname client-firstname client-email)
<html>
  <head><title>@|title|</title></head>
  <body>
    <table>
      @for[([c clients])]{
      <tr>
        <td>@(client-surname c), @(client-firstname c)
        <td><a href="mailto:@(client-email c)">@(client-email c)</a></td>
      </tr>
      }
    </table>
  </body>
</html>

@; Example:
@; (template "Title"
@;           `(["First1" "Last1" "email1"]
@;             ["First2" "Last2" "email2"]
@;             ["First3" "Last3" "email3"]
@;             ["First4" "Last4" "email4"]
@;             ["First5" "Last5" "email5"])
@;           second first third)