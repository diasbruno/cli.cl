;;;; cli.cl.asd

(asdf:defsystem #:cli.cl
  :description "Command-line parser and runner."
  :author "Bruno Dias"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:just-getopt-parser)
  :components ((:file "package")
               (:file "cli.cl")))
