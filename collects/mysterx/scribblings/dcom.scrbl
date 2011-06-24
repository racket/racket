#lang scribble/doc
@(require "common.rkt" scribble/bnf)

@title[#:tag "remote"]{Remote COM servers (DCOM)}

For the MysterX procedures @racket[cocreate-instance-from-coclass] and
@racket[cocreate-instance-from-progid], the optional @racket[_where]
argument can be @racket['remote].  In that case, the server instance
is run at the location given by the Registry key

@centerline{@tt{HKEY_CLASSES_ROOT\AppID\@nonterm{CLSID}\RemoteServerName}}

where @nonterm{CLSID} is the CLSID of the application.  This key may
be set using the @exec{dcomcnfg} utility.  From @exec{dcomcnfg}, pick
the application to be run on the @onscreen{Applications} tab, then
click on the @onscreen{Properties} button.  On the @onscreen{Location}
tab, choose @onscreen{Run application on the following computer}, and
enter the machine name.

In order to run a COM remote server, the registry on the client
machine must contain an entry at

@centerline{@tt{HKEY_CLASSES_ROOT\CLSID\@nonterm{CLSID}}}

where @nonterm{CLSID} is the CLSID for the server.  The server
application itself need not be installed on the client machine.

There are a number of configuration issues relating to DCOM, which
MysterX uses to invoke remote COM servers.  The Web page

@centerline{@link["http://www.distribucon.com/dcom95.aspx"]{http://www.distribucon.com/dcom95.html}}

discusses how to setup client and server machines for DCOM.
