#lang scribble/doc
@(require "common.ss")

@title{Client Customization}

@itemize[
@item{Rename (or make a copy of) the @filepath{handin-client}
collection directory.  The new name should describe your class
uniquely.  For example, @filepath{uu-cpsc2010} is a good name for CPSC
2010 at the University of Utah.}

@item{Edit the first three definitions of @filepath{info.ss} in your
  renamed client collection:
  @itemize[
  @item{For @scheme[name], choose a name for the handin tool as it
    will appear in DrRacket's interface (e.g., the @onscreen{XXX} for
    the @onscreen{Manage XXX Handin Account...}  menu item).  Again,
    make the name specific to the course, in case a student installs
    multiple handin tools.  Do not use @onscreen{Handin} as the last
    part of the name, since @onscreen{Handin} is always added for
    button and menu names.}

  @item{Uncomment the definitions of @scheme[tools],
    @scheme[tool-names], and @scheme[tool-icons].  (But leave the
    latter field's definition as @filepath{icon.png}.)}

  @item{For @scheme[server:port], uncomment the line, and use the
    hostname and port where the server will be running to accept
    handin submissions.}]

  Optionally uncomment and edit the next two definitions,
  @scheme[web-menu-name] and @scheme[web-address], to add an item to
  the @onscreen{Help} menu that opens a (course-specific) web page.}

@item{Replace @filepath{icon.png} in your renamed directory with a new
  32x32 icon.  This icon is displayed on startup with DrRacket's
  splash screen, and it is included at half size on the
  @onscreen{Handin} button.  A school logo is typically useful, as it
  provides a recognizably local visual cue.  If students might use
  multiple installed handin tools, then make sure to vary the icon
  according to the course.}

@item{Replace @filepath{server-cert.pem} in your renamed directory
  with a server certificate.  The file @filepath{server-cert.pem} in
  @filepath{handin-client} collection is ok for testing, but the point
  of this certificate is to make handins secure, so you should
  generate a new (self-certifying) certificate and keep its key
  private.  (See @secref{server-setup}.)}

@item{To create an installable package, run
  @commandline{mzc --collection-plt <name>.plt <name>} where @tt{<name>}
  is the name that you chose for your directory (i.e., whatever you
  changed @filepath{handin-client} to).

  This directory should exist in your @filepath{collects} directory:
  this can be done by making sure that the copy is in the same place the
  original client directory was (see @secref{wheres-the-collection}) or
  by specifying a value for the @envvar{PLTCOLLECTS} environment
  variable.  For example, if your customized collection directory is
  called @filepath{cs1} and it is located at
  @filepath{/home/joe/intro/cs1}, then you can run
  @commandline{PLTCOLLECTS=/home/joe/intro: mzc --collection-plt cs1.plt cs1}
  (Don't forget the colon at the end of the @envvar{PLTCOLLECTS} value;
  it is important!)

  Note that if you create an updated copy of the client package (that
  is, students already have an older version installed), then you should
  use the @DFlag{replace} to indicate that the package should replace
  existing files instead of throwing an error.}

@item{Distribute @filepath{<name>.plt} to students for installation
  into their copies of DrRacket.  The students need not have access to
  the DrRacket installation directory; the tool will be installed on
  the filesystem in the student's personal space.  If you want to
  install it once on a shared installation, use setup-plt with the
  @DFlag{all-users} flag.}

]
