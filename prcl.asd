(in-package :cl-user)
(defpackage :prcl-asd
  (:use #:cl #:asdf))
(in-package :prcl-asd)
(defsystem :prcl
  :version "0.0.1"
  :author "Tom Gillespie <tgbugs@gmail.com>"
  :description "TODO"
  :depends-on ("local-time"
               "dexador"
               "cl-json"
               "quri"
               "parse-args")
  :serial t
  :components (;(:file "prcl-package")
               (:file "prcl")))

#+() ; we don't really need this system because we don't have a separate entrypoint file right now
(defsystem :prcl/entrypoint
  ;; this extra system is critical for getting prcl
  ;; itself loaded for the build image, otherwise
  ;; it will just fizzle
  :depends-on (:prcl))
