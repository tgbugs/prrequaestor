(in-package :cl-user)
(defpackage :prcl-asd
  (:use #:cl #:asdf))
(in-package :prcl-asd)
(defsystem :prcl
  :version "0.0.1"
  :author "Tom Gillespie <tgbugs@gmail.com>"
  :license "MIT"
  :description "TODO"
  :depends-on ("local-time"
               "dexador"
               ;;"cl-git" ; libgit2/libssh2 is missing key functionality
               "cl-json"
               "quri"
               "parse-args")
  :serial t
  :components ((:file "prcl")))
