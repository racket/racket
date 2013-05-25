#lang racket/signature

(struct communicator (sender receiver server port))
connect-to-server connect-to-server* disconnect-from-server
authenticate-user open-news-group
head-of-message body-of-message
newnews-since generic-message-command
make-desired-header extract-desired-headers

(struct nntp ())
(struct unexpected-response (code text))
(struct bad-status-line (line))
(struct premature-close (communicator))
(struct bad-newsgroup-line (line))
(struct non-existent-group (group))
(struct article-not-in-group (article))
(struct no-group-selected ())
(struct article-not-found (article))
(struct authentication-rejected ())
