#lang scribble/doc
@(require "common.ss")

@title{Handin-Server and Client}

The @filepath{handin-server} directory contains a server to be run by a
course instructor for accepting homework assignments and reporting on
submitted assignments.

The @filepath{handin-client} directory contains a client to be
customized then re-distributed to students in the course.  The
customized client will embed a particular hostname and port where the
server is running, as well as a server certificate.

With a customized client, students simply install a @filepath{.plt}
file---so there's no futzing with configuration dialogs and
certificates.  A student can install any number of clients at once
(assuming that the clients are properly customized, as described
below).

The result, on the student's side, is a @onscreen{Handin} button in
DrRacket's toolbar.  Clicking the @onscreen{Handin} button allows the
student to type a password and upload the current content of the
definitions and interactions window to the course instructor's server.
The @onscreen{File} menu is also extended with a @onscreen{Manage...}
menu item for managing a handin account (i.e., changing the password
and other information, or creating a new account if the instructor
configures the server to allow new accounts).  Students can submit
joint work by submitting with a concatenation of usernames separated
by a ``@tt{+}''.

On the instructor's side, the handin server can be configured to check
the student's submission before accepting it.

The handin process uses SSL, so it is effectively as secure as the
server and each user's password.
