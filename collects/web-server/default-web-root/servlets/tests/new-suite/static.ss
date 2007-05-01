(module static mzscheme
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (list
     #"text/html"
     "<html><head></head><body>Foo</body><html>")))
