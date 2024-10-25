#!/usr/bin/env -S sbcl --script
;; -*- mode: Common-Lisp -*-

#| ; -*- mode: Common-Lisp -*-
sbcl --load "${0}" --eval "(main)" --quit --end-toplevel-options "${@:1}"; exit
|#

#-asdf
(load #p"/usr/share/common-lisp/source/asdf/build/asdf.lisp") ; FIXME gentoo specific

#-uiop
(asdf:load-asd #p"/usr/share/common-lisp/systems/uiop.asd") ; FIXME gentoo specific

#-quicklisp
(let ((quicklisp-init (merge-pathnames "code/lisp/quicklisp/setup.lisp" ; FIXME abstract path
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#-dexador
(asdf:load-system :dexador :verbose nil)
#-dexador
(push :dexador *features*)
#-quicklisp-https-always
(asdf:load-system :quicklisp-https-always :verbose nil)
#-quicklisp-https-always
(push :quicklisp-https-always *features*)

;; XXX all commands in this must be run at the top level
;; so that namespaces can be resolved one form at a time
;; you cannot define a function that both loads and references a namespace
;; the compiler will barf
;; this is one of the issues with having an interning reader

(in-package :cl-user)

(defun build ()
  ;(asdf:load-system 'parse-args) ; doesn't work
  (pushnew :dumped-image *features*)
  #+(and not ready yet)
  (push (uiop:truenamize #p"~/git/NOFORK/cl-git/") ql:*local-project-directories*) ; need version 2.0.0 ; FIXME abstract path
  (push (uiop:truenamize #p"~/git/git-share/") ql:*local-project-directories*) ; neede for parse args
  (push (uiop:truenamize #p"~/git/prrequaestor/") ql:*local-project-directories*) ; FIXME abstract path
  ;; can't quickload prcl itself it seems, have to load :prcl/entrypoint which depends on prcl? no? < FALSE
  ;; the issue was that we were doing this inside a function
  (ql:quickload :prcl)
  (pop ql:*local-project-directories*)
  (pop ql:*local-project-directories*)
  (ensure-directories-exist "~/git/prrequaestor/bin/") ; FIXME abstract path
  )

(build)
(fmakunbound 'build) ; remove from the image before slad
;; slad MUST be called after build because prcl package must exist when slad is called
(save-lisp-and-die
 "bin/prcl" ; FIXME running this from another directory will break things
 :toplevel #'prcl:main :executable t :compression t) ; set :compression 22 for max zstd compression
