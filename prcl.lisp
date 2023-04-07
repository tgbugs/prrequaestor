;; -*- mode: Common-Lisp -*-

;;; usage

;; there are at least three ways to use this file

;; run it in emacs via slime (you may need to run the `ql:quickload's first)

;; run it from cli via
;; sbcl --script prcl.lisp --specs path/to/sync-specs.lisp --fun test

;; or slad and run the executable directly via prcl-build.lisp
;; ./prcl-build.lisp && bin/prcl --specs path/to/sync-specs.lisp --fun test

;;; configuration

;; add an entry in ~/.netrc for the build user e.g.
;; machine github.com login github-user password ghp_etcetcetc
;; prcl then automatically adds the following to .git/config
;; pushurl https://github-user@github.com/github-user/repo.git
;; this is a stop-gap until some more sensible solution appears

;; FIXME detect stale automated branches and error if the upstream branch has changed or something
;; FIXME add new file if not exists fails in some cases?
;; FIXME need separate working trees/copies of repos per sync to avoid concurrent syncs using the same repo
;; TODO deal with prov churn issues, possibly by moving prov triples to a central file and pointing to it
;; TODO see if we can use cl-git (answer: not quickly, and not completely if with ssh)

(in-package :cl-user)
; #+() #+() #+()
;(ql:quickload 'cl-ppcre)
;(ql:quickload 'github-api-cl)
;(ql:quickload 'drakma)
;(drakma:get-content-type) ; fewer deps than dex but missing features

#-(or swank dumped-image) (pushnew :debug *features*) ; enable debug if running sbcl --script basically

#-asdf
(load #p"/usr/share/common-lisp/source/asdf/build/asdf.lisp") ; FIXME gentoo specific

#-uiop
(asdf:load-asd #p"/usr/share/common-lisp/systems/uiop.asd") ; FIXME gentoo specific

#-quicklisp
(let ((quicklisp-init (merge-pathnames "code/lisp/quicklisp/setup.lisp" ; FIXME abstract path
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

#-dumped-image (ql:quickload 'local-time)
#-dumped-image (ql:quickload 'dexador)
;;#-dumped-image (ql:quickload 'cl-git)
#-dumped-image (ql:quickload 'cl-json)
#-dumped-image (ql:quickload 'quri)
#-dumped-image (asdf:initialize-source-registry
                '(:source-registry (:tree #p"~/git/git-share/") ; FIXME abstract path
                  :inherit-configuration))
#-dumped-image (asdf:load-system :parse-args)

(defpackage :prcl
  (:use :cl)
  (:export
   #:main

   #:defrepo
   #:defsync

   #:*repo-working-dir*
   #:*build-id*
   #:*build-dir*
   #:*oa-secrets*
   #:*subprocess-environment*

   #:run-command
   #:run-command-check
   #:executable-find
           ))

(defpackage :prcl-sync ; XXX FIXME I want to be able to call (export 'symbol) and
  (:use #:cl #:prcl) ; XXX dangerzone ... but maybe protected by export above? or no?
  (:export #:sync-test
           #:sync-test-2
           ))

(in-package :prcl)
(defvar *pr-branch-prefix-default* "automated-")
(defvar *pr-branch-prefix* nil)
(defvar *push-pr* nil)
(defvar *repos* (make-hash-table)) ; repos by name
(defvar *oa-secrets* "~/ni/dev/secrets.sxpr") ; FIXME abstract path
(defvar *auth-sources* nil)
(defvar *authinfo-order* '(:host :port :user :secret))
(defvar *repo-working-dir*)
;(export *repo-working-dir* 'prcl)
(defvar *build-dir* (uiop:ensure-directory-pathname (merge-pathnames "prm-repos" (uiop:default-temporary-directory))))
(defvar *sync-specs* (and
                      (uiop:file-exists-p "sync-specs.lisp")
                      (merge-pathnames "sync-specs.lisp" *default-pathname-defaults*)))
(defvar *current-sync* nil)
(defvar *github-token* nil) ; may not want this
(defvar *build-id*)
(defvar *current-git-output-port* nil)
(defvar *current-repo* nil
  "default should always be nil, this is let-bound by `with-repo'")
(defvar *repo-just-cloned* nil
  "default should always be nil, this is let-bound by `with-repo'")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defvar *do-not-clone* nil
    "if set do not clone during `with-repo' setup"))

(defvar *forge-user*) ; FIXME refactor usage to pass as keyword to any function that uses it
(defvar *forge-user-default* "tgbugs-build") ; XXX until we can get some cl-orthauth version up
(defvar *forge-fork* nil
  "if cannot push to target repo create a fork, only needed once per repo usually")
(defvar *auto-sync-fork* nil
  "whether to automatically sync forks with upstream"
  )
(defvar *no-auth* nil)
(defvar *debug*)
#+swank ; make testing and development easier
(setf *debug* nil)
(defvar *git-raise-error* nil)
(defvar *sepstr* (make-string 70 :initial-element #\-))

(defun git-config-forge-user-push-remote (&key ((:forge-user forge-user) *forge-user*))
  (list "prrequaestor" forge-user "push" "remote"))

(defun executables () ; stupidly inefficient
  (loop with path = (uiop:getenv "PATH")
        for p in (uiop:split-string path :separator ":")
        for dir = (probe-file p)
        when (uiop:directory-exists-p dir)
          append (uiop:directory-files dir)))

(defun executable-find (name)
  (find name (executables)
        :test #'equalp
        :key #'pathname-name))

(setf *auth-sources*
      '(
        "~/ni/dev/.authinfo" ; FIXME abstract path
        #+()
        "~/git/orthauth/test/configs/authinfo-1"
        ))

#+()
(let ((test '(:a 1 :b 2)))
  (getf test :a)
  (setf (getf test :a) 3)
  (getf test :a)
  )

(defun alists-to-nested-plist (alists order)
  (let (rout (rorder (reverse order)))
    (loop
      for plist in
                (loop for alist in alists ; convert to plist and then merge
                      collect (let ((out (cdr (assoc (car rorder) alist))))
                                (loop for k in (cdr rorder)
                                      when (cdr (assoc k alist))
                                        do (setf out
                                                 (cons (intern (cdr (assoc k alist)) 'keyword)
                                                       (list out))))
                                out))
      do (loop for k in plist by #'cddr
               for v in (cdr plist) by #'cddr
               do (if (getf rout k)
                      (setf (getf rout k) (append (getf rout k) v))
                      (setf (getf rout k) v))))
    rout))

(defun oa-norm-authinfo (k)
  (cond
    ((eq k :machine) :host)
    ((eq k :login) :user)
    ((eq k :account) :user)
    ((eq k :protocol) :port)
    ((eq k :password) :secret)
    (t k)))

(defun oa-netrc-as-alist (path-string)
  (with-open-file (stream path-string)
    (loop for line = (read-line stream nil)
          while line
          unless (uiop:string-prefix-p "default" line)
            collect (let ((spl (uiop:split-string line)))
                      (loop for a in spl by #'cddr
                            for b in (cdr spl) by #'cddr
                            ;; FIXME we could skip the whole alist step here ...
                            collect (cons (oa-norm-authinfo
                                           (intern (string-upcase a) 'keyword))
                                          b))))))

(defun get-path (nested-plist names)
  (let ((blob nested-plist))
    (loop for k in names
          unless (getf blob k) do (error "unknown path")
            do (setf blob (getf blob k)))
    blob))

(defun oa-authinfo-get (&rest names)
  (let ((alists (oa-netrc-as-alist
                 (car *auth-sources*)
                 )
                ))
    (let ((nested-plist (alists-to-nested-plist alists *authinfo-order*)))
      (get-path nested-plist names))))


#+()
(oa-authinfo-get :|api.github.com| :|unknown|)

#+()
(defparameter *api-docs* (github-api-doc:read-api-json))
#+()
(defparameter *api-doc* (github-api-doc:make-api-doc-from-json *api-docs* "repositories" "repositories" "List repositories for a user"))

;;(gethash "")

#+()
(loop for key being the hash-keys of *api-docs* collect key)




#+()
(let ((token (oa-authinfo-get :|api.github.com| :|tgbugs^forge|)))
  (defparameter *client-with-token* (make-instance 'github-client:api-client :token token)))

#+()
(github-client:github-api-call
 *client-with-token*
 *api-doc*)

(defun make-build-id (&optional time)
  (local-time:format-timestring nil (or time (local-time:now))
                                :format
                                (append
                                 local-time:+iso-8601-date-format+
                                 '(#\T (:hour 2) (:min 2) #\Z))
                                :timezone local-time:+utc-zone+))

(defun flatten-tree (tree)
  "adapted from elisp"
  (let (elems)
    (loop while (consp tree)
          do (let ((elem (pop tree)))
               (loop
                 while (consp elem)
                 do (push (cdr elem) tree)
                 do (setq elem (car elem)))
               (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(defstruct repo
  (id nil)
  (local nil)
  (remote nil)
  ;(local-remote-name nil) ; superseeded by the git config value ??
  (forge nil)
  ;(forge-api nil) ; via function
  (owner nil)
  (name nil)
  (pull-requests nil)
  ;;(forge-user-repo nil) ; self if have push else other, superseded by (push nil)
  (push nil) ; that would be the repo that the current api user can push to
  )

(defun repo-forge-user-token-remote (repo &key ((:forge-user forge-user) *forge-user*))
  (cond
    ((eq (repo-forge repo) 'github)
     (quri:render-uri
      (quri:make-uri
       :scheme "https"
       :userinfo forge-user
       :host "github.com"
       :path (concatenate 'string "/" (repo-owner repo) "/" (repo-name repo) ".git"))))
    (t (error (format nil "don't know forge for ~a" (repo-remote repo))))))

(defun make-repo-from-working-dir (&key id
                                     ((:working-dir working-dir) *repo-working-dir*)
                                     ((:remote-name remote-name) "origin"))
  (let* ((*repo-working-dir* working-dir)
         (dwt (uiop:ensure-directory-pathname working-dir))
         (dir-name (car (reverse (pathname-directory dwt))))
         (remote (car (split-string-nl (run-git "remote" "get-url" remote-name))))
         (repo
           (make-repo
            :id (or id (intern (string-upcase dir-name)))
            :remote remote
            :local working-dir)))
    (repo-remote-fill-values repo)
    repo))

(defun git-forge-user-repo-push (repo)
  (let ((remote-push-name
          (car (apply #'git-config (git-config-forge-user-push-remote)))))
    (and
     remote-push-name
     (if (string= remote-push-name "origin")
         repo
         (make-repo-from-working-dir
          :id (intern (concatenate 'string (symbol-name (repo-id repo)) "-" (string-upcase *forge-user*)))
          :remote-name remote-push-name)))))

#-dumped-image
(defun test-gfrurp ()
  (let* ((*repo-working-dir* #p"/tmp/test-prcl-build-dir/test-github-api/")
         (repo (make-repo-from-working-dir)))
    (git-forge-user-repo-push repo)
    ))

(defun repo-local-path (repo)
  (let ((out
          (or (repo-local repo)
              (uiop:ensure-directory-pathname (merge-pathnames (repo-name repo) *build-dir*)))))
    ;#+debug
    (format t "rlp out: ~s ~s~%" *build-dir* out)
    out
    ))

(defun repo-local-validate (local)
  ;; TODO check that it is an absolute path
  local)

(defun defrepo (id remote &key local)
  ;; TODO somewhere in here we need to check, detect, warn, and
  ;; possibly change if the remote provided does not match the config
  (let ((repo (make-repo :id id :remote remote)))
    (when local
      (setf (repo-local repo) (repo-local-validate local)))
    (repo-remote-fill-values repo)
    (setf (gethash (repo-id repo) *repos*) repo)))

(defun forge-latest-automated-pr-branch (repo &key prefix)
  ;; FIXME make sure the pull request is open!
  ;; FIXME this will pretty much only work for github
  (let* ((prefix (or prefix *pr-branch-prefix* *pr-branch-prefix-default*))
         (repo-push (repo-push repo))
         (push-owner (and repo-push (repo-owner repo-push))) ; avoid type error on nil struct
         (expect-label-prefix (concatenate 'string push-owner ":" prefix))
         out)
    #+debug
    (format t "prefix: ~a ~a ~a~%" prefix *pr-branch-prefix* *pr-branch-prefix-default*)
    ;; concat the owner of the push repo to the prefix
    (loop for pr in (repo-pull-requests repo)
          ;;(assoc :title pr)
          ;;(assoc :base pr)
          ;; TODO sort by date
          until (let* ((head (cdr (assoc :head pr)))
                       (label (cdr (assoc :label head)))
                       (ref (cdr (assoc :ref head)))
                       ;;(repo (cdr (assoc :repo head)))
                       ;;(owner (cdr (assoc :owner head)))
                       )
                  #+debug
                  (format *standard-output* "pr ref: ~s~%" ref)
                  (if (uiop:string-prefix-p expect-label-prefix label)
                      (progn
                        (format *standard-output* "ref ok match: ~s~%" label)
                        (setf out ref))
                      (format *standard-output* "ref no match: ~s~%" label))))
    out))

(defun git-latest-automated-branch (&key prefix remote)
  "a real remote branch, may or may not have a pull request"
  #+debug
  (format t "glab: ~a ~a ~a~%" prefix remote (git-list-remote-branch-names :remote remote))
  (car
   (sort
    (loop
      for branch in (git-list-remote-branch-names :remote remote)
      when (search prefix branch)
        collect branch)
    #'string>)))

(defun get-json (uri &key auth)
  (multiple-value-bind (body status response-headers response-uri stream)
      (dex:get uri
               :verbose *debug*
               :headers (github-api-headers :auth auth))
    (declare (ignore response-headers))
    (declare (ignore response-uri))
    (declare (ignore stream))
    #+debug
    (format t "get-json status: ~a~%" status)
    (if (eq status 200)
        #+(and would have to define constructors for the return object for this to work correctly)
        (cl-json:with-decoder-simple-clos-semantics (cl-json:decode-json-from-string body))
        (cl-json:decode-json-from-string body)
        nil)))

(defun forge-get-repo-prs (repo)
  (let ((uri
          (quri:make-uri
           :scheme "https"
           :host "api.github.com"
           :path (concatenate 'string "/repos/" (repo-owner repo) "/" (repo-name repo) "/pulls")
           )))
    #+debug
    (format t "uri: ~s" uri)
    (get-json uri)))

(defun github-api-headers (&key auth)
  (cons '("Accept" . "application/vnd.github+json")
        (when auth
          (unless *github-token*
            (error "no auth token has been set"))
          (list
           (cons "Authorization" (concatenate 'string "token " *github-token*))))))

(define-condition continue-from-http-error (condition) ())
(defun forge-get-repo-push (repo &key fork)
  (let* ((ok-to-fork (or fork *forge-fork*))
         (push
           (or
            (repo-push repo) ; if the value is already set return it !??!?! is that the right hack here ???
            (git-forge-user-repo-push repo) ; see if this is already configured locally
            ;; (let (fork) (and (forge-repo-exists fork))) ; XXX we can't do this here because we need to check collaborators first
            (restart-case
                (handler-bind
                    ((dexador.error:http-request-failed
                       (lambda (condition)
                         (let ((status (dexador.error:response-status condition)))
                           (cond ((= status 403) ; must have push acces to view repository collaborators
                                  (invoke-restart 'continue-from-http-error status))
                                 ;; 404 means private and we have no access at all and should never get here
                                 (t
                                  (signal condition)))))))
                  (and ; if we don't get an error checking collaborators then repo is push
                   (let ((uri-collaborators
                           (quri:make-uri
                            :scheme "https"
                            :host "api.github.com"
                            :path (concatenate
                                   'string
                                   "/repos/" (repo-owner repo)
                                   "/" (repo-name repo) "/collaborators"))))
                     (get-json uri-collaborators :auth t))
                   repo))
              (continue-from-http-error (&optional status)
                (unless (or (not status) (= status 403))
                  (error (format nil "unhandled status ~a" status)))
                (let ((fork (repo-fork-repo repo :user *forge-user*)))
                  (unless (forge-repo-exists fork)
                    (if ok-to-fork
                        (forge-fork-repo repo :fork fork :block t)
                        (error (format nil "no push access to ~a as ~a and not ok to fork"
                                       (repo-name repo) *forge-user*))))
                  (setf (repo-push fork) fork)
                  (let ((fork-remote-name (repo-owner fork)))
                    (unless (apply #'git-config (git-config-forge-user-push-remote))
                      (setf (apply #'git-config (git-config-forge-user-push-remote))
                            fork-remote-name))
                    (unless (git-remote-exists fork-remote-name)
                      (git-remote-add fork-remote-name (repo-remote fork))))
                  (repo-forge-fill-values fork)
                  fork)
                )))))
    (when (and
           (not (equal repo push))
           (not (git-config "remote" (repo-owner push) "pushurl"))
           #+() ; dumb, just wrap with *git-raise-error* nil
           (restart-case
               (handler-bind
                   ((error
                      (lambda (condition)
                        (declare (ignore condition))
                        (invoke-restart 'continue-from-git-error))))
                 (run-git "remote" "get-url" "--push" (repo-owner push))) ; FIXME conflating branch name here
             (continue-from-git-error ()
               nil)))
        (run-git "remote" ; must set token push url, also will encounter issues with ssh
                 "set-url" "--add" "--push" (repo-owner push) ; FIXME conflating branch name here
                 (repo-forge-user-token-remote push)))
    push))

(defun repo-forge-api (repo)
  (cond
    ((eq (repo-forge repo) 'github)
     (quri:make-uri
      :scheme "https"
      :host "api.github.com"
      :path
      (concatenate
       'string "/repos/" (repo-owner repo) "/" (repo-name repo))))
    (t (error (format nil "don't know forge for ~a" (repo-remote repo))))))

(defun forge-repo-exists (repo)
  ; https://api.github.com/repos/tgbugs-build/test-github-api
  (restart-case
      (handler-bind
          ((dexador.error:http-request-failed
             (lambda (condition)
               (invoke-restart 'continue-from-http-error (dexador.error:response-status condition)))))
        (dex:head
         (repo-forge-api repo)
         :headers (github-api-headers :auth t)))
    (continue-from-http-error (&optional status)
      (declare (ignore status))
      nil)))

(defun forge-fork-repo (repo &key fork block)
  (cond
    ((eq (repo-forge repo) 'github)
     (github-fork-repo repo :fork fork :block block))
    (t (error (format nil "don't know forge for ~a" (repo-remote repo))))))

(defun repo-forge-fill-values (repo)
  (let ((pull-requests (forge-get-repo-prs repo))
        (push (and (not *no-auth*) ; if we are in no-auth mode we can't check this
                   (forge-get-repo-push repo))))
    (setf (repo-pull-requests repo) pull-requests)
    (setf (repo-push repo) push)))

(defun repo-remote-fill-values (repo)
  (let ((github-pos (search "github.com" (repo-remote repo))))
    (cond (github-pos
           (setf (repo-forge repo) 'github)
           (let* ((r (reverse (uiop:split-string (subseq (repo-remote repo) (+ github-pos 10))
                                                 :separator '(#\/ #\:))))
                  (name-ext (car r))
                  (name (car (uiop:split-string name-ext :separator '(#\.))))
                  (owner (cadr r)))
             (setf (repo-owner repo) owner)
             ;; cannot set repo-local until *build-dir* is known, otherwise it will always be /tmp/repo-name
             (setf (repo-name repo) name)))
          (t (error (format nil "don't know forge for ~a" (repo-remote repo)))))))

(defun remote-from-forge-user-name (forge user name &key ((:uri-type uri-type) 'git))
  (cond
    ((eq forge 'github)
     (cond
       ((eq uri-type 'git) (concatenate 'string "git@github.com:" user "/" name ".git"))
       (t (error (format nil"uri-type not implemented ~a" uri-type)))))
    (t (error (format nil "don't know forge ~a" forge)))))

(defun repo-fork-repo (repo &key user)
  (make-repo
   :id (intern (concatenate 'string (symbol-name (repo-id repo)) "-" (string-upcase user)))
   ;;:local-remote-name user ; TODO may be set by user in the future if organization ; XXX not clear we need this, get the push remote from git config mostly
   :forge (repo-forge repo) ; someday cross forge pulls ... hah right ?! who would enable such a thing?
   :owner user
   :name (repo-name repo) ; TODO set by new name if/when we let people provide it
   :remote (remote-from-forge-user-name (repo-forge repo) user (repo-name repo))))

#-dumped-image
(defun test ()

  (let ((owner "tgbugs") (name "pyontutils"))
    (format nil "https://api.github.com/repos/~s/~s/pulls" owner name))

  (let ((test-repo
          (defrepo 'pyontutils
            "https://github.com/tgbugs/pyontutils.git")))
    (let ((out (repo-forge-fill-values test-repo)))
      out)))

#-dumped-image
(defvar *var* nil)
#-dumped-image
(defvar *rp* nil)
#-dumped-image
(defun test-1.5 ()
  (progn
    (setf *var* (test))
    (loop for (k . v) in (car *var*) collect k)

    (assoc :title (car *var*))
    (forge-latest-automated-pr-branch (gethash 'pyontutils *repos*))

    (defrepo 'apinatomy-models
      "https://github.com/open-physiology/apinatomy-models.git")
    (repo-forge-fill-values (gethash 'apinatomy-models *repos*))
    (forge-latest-automated-pr-branch
     (gethash 'apinatomy-models *repos*))
    (repo-push (gethash 'apinatomy-models *repos*))

    #+()
    (progn
      (setf *pr-branch-prefix* "auto-test-")
      (setf *repo-working-dir* #p"/tmp/test-2-prcl-build-dir/test-github-api/")
      (setf *forge-user* "tgbugs-build")
      (setf *github-token* (ensure-token-exists)))
    (setf (gethash 'test-github-api *repos*) nil)
    (defrepo 'test-github-api
      "https://github.com/tgbugs/test-github-api.git")
    (repo-forge-fill-values (gethash 'test-github-api *repos*))
    (car (reverse (repo-pull-requests (gethash 'test-github-api *repos*))))
    (forge-latest-automated-pr-branch
     (gethash 'test-github-api *repos*))
    (setf *rp* (forge-get-repo-push (gethash 'test-github-api *repos*)))
    ))

(defun github-fork-repo (repo &key fork block)
  (let* ((uri-path
           (concatenate
            'string
            "/repos/" (repo-owner repo) "/" (repo-name repo) "/forks"))
         (uri
           (quri:make-uri
            :scheme "https"
            :host "api.github.com"
            :path uri-path
            ))
         (resp
           (dex:post
            uri
            :headers (github-api-headers :auth t)
            )))
    (format t "github-fork-repo-resp: ~a~%" resp)
    (when block
      ; exponential backoff on the retry until it is up
      ; with a supremum retry time of 64 seconds
      (or (loop
            for time in '(0 1 2 4 8 16 32)
            do (format t "no fork waiting for ~s~%" time)
            do (sleep time)
            when (forge-repo-exists fork)
              return t)
          (loop
            for i upto 4
            do (format t "no fork waiting for ~s~%" 64)
            do (sleep 64)
            when (forge-repo-exists fork)
              return t)
          (error "we've hit the 5 minute limit that github suggests to wait and still nothing")))))

(defun github-merge-upstream (repo branch)
  ;; FIXME we should probably do this locally and not on the remote
  ;; FIXME need to get the repo-push for this and repo in the index usually _is_ the upstream repo
  (let* ((uri-path
           (concatenate
            'string
            "/repos/" (repo-owner repo) "/" (repo-name repo) "/merge-upstream"))
         (uri
           (quri:make-uri
            :scheme "https"
            :host "api.github.com"
            :path uri-path))
         (content
           (cl-json:encode-json-to-string
            `(("branch" . ,branch))))
         (resp
           (dex:post
            uri
            :headers (github-api-headers :auth t)
            :content content
            )))
    resp))

(defun github-create-pull-request (title body &key source target)
  (let* ((source (or source (git-get-upstream-branch (git-get-current-branch))))
         ;; XXX warning, ensure we push creating a new branch
         (target (or target (git-get-upstream-branch (git-master-branch))))
         (base-repo *current-repo*)
         (head-repo (repo-push *current-repo*))
         (uri-path (concatenate 'string "/repos/"
                                (repo-owner base-repo) "/"
                                (repo-name base-repo) "/pulls"))
         (uri (quri:make-uri
               :scheme "https"
               :host "api.github.com"
               :path uri-path))
         (base (split-branch-name target))
         (head (split-branch-name source))
         (base-remote (car base))
         (base-branch (cdr base))
         (head-remote (car head))
         (head-branch (cdr head)))
    #+debug
    (format t "debug gcpr: ~a ~a ~a ~a ~a ~a ~%"
            source target
            base-remote base-branch head-remote head-branch)
    ;; head source
    ;; base target
    (let ((resp
            (dex:post
             uri
             :verbose *debug*
             :headers
             `(("Accept" . "application/vnd.github.shadow-cat-preview+json") ; Support draft pull-requests.
               ("Authorization" . ,(concatenate 'string "token " *github-token*)))
             :content
             (cl-json:encode-json-to-string
              `(("title" . ,title)
                ("body" . ,body)
                ("base" . ,base-branch)
                ("head" . ,(if (string= head-remote base-remote)
                               head-branch
                               (concatenate 'string (repo-owner head-repo) ":" head-branch)))
                ("draft" . t)
                ("maintainer_can_modify" . t))))))
      #+debug
      (format t "request: ~s~%" resp)
      resp)))

#-dumped-image
(defun test-create-pull-request ()
  (defrepo 'test-github-api "git@github.com:tgbugs/test-github-api.git")
  (let ((repo-id  'test-github-api)
        (*github-token* (ensure-token-exists)))
    (with-repo
      ;'what
      ;(repo-forge-fill-values *current-repo*)
        (list
         (split-branch-name "origin/patch-1")
         (split-branch-name "patch-1")
         (split-branch-name "master")
         (branch-local-name "origin/patch-1")
         (branch-local-name "origin/no-local-branch")
         (branch-local-name "nosuchremote/some-branch") ; should be nil and is atm
         (branch-local-name nil) ; correctly return nil
         )
      #+() ; this 400 errors because there is not a real branch to create a pull request from
      (github-create-pull-request "test pull requestion creation from prcl" "hello there!")
      )
    )
  )

#+(and this won't work outside of with-repo)
(test-create-pull-request)

(defun run-git (&rest args)
  ;; apparently this already has knowledge of the repo in question ...
  ;; so need to check in on `*default-pathname-defaults*'
  (let ((*default-pathname-defaults* *repo-working-dir*))
    (apply #'call-git args)))

#-dumped-image
(defun test-setf-getf ()
  (let ((sigh '(1 2 3 :foo poop))) (setf (getf sigh :foo) nil))
  (let ((sigh '(1 2 3 4 :foo poop))) (setf (getf sigh :foo) nil) sigh)
  #+(and not allowed at all the replacas are coming for us)
  (let ((sigh '(1 2 3 :foo poop 10 11 12))) ; sigh
    ;;(getf sigh :foo)
    (setf (cadr (member :foo sigh)) nil)
    sigh
    ))

(defun cull-keyword (args key)
  "not efficient, but works"
  (or (and (member key args)
           (remove-if (lambda (x) (or (not x) (eq x key)))
                      (let ((l (copy-list args)))
                        (setf (cadr (member key l)) nil) ; this does mutate
                        l)))
      args))

(defvar *subprocess-environment* nil
  "A special variable that works like elisp PROCESS-ENVIRONMENT
except that it only works for code that uses WITH-COMMAND-PROCESS
(such as RUN-COMMAND). The default value should never be changed
from nil.")

(defmacro with-command-process (&body body)
  `(let* ((out (make-string-output-stream))
          (process (sb-ext:run-program
                    command (loop for e in (flatten-tree args)
                                  collect (if (pathnamep e) (namestring e) e))
                    :environment (or *subprocess-environment* (sb-ext:posix-environ))
                    :directory *default-pathname-defaults*
                    :output out
                    :error *error-output*
                    :search t)))
     ,@body))

(defun run-command-check (command &rest args)
  (with-command-process
      (when (not (= 0 (sb-ext:process-exit-code process)))
        (error (format nil "command ~s failed with status ~s~%"
                       (cons command args)
                       (sb-ext:process-exit-code process))))
    (values (get-output-stream-string out) process)))

(defun run-command (command &rest args)
  (with-command-process
    (values (get-output-stream-string out) process)))

(defun run-command-old (command &rest args &key raise-error &allow-other-keys) ; causes style warnings so don't use ...
  ;; too many issues with &allow-other-keys and style warnings
  (let ((args (cull-keyword args :raise-error)))
    (with-command-process
        (when (and raise-error (not (= 0 (sb-ext:process-exit-code process))))
          (error (format nil "command ~s failed with status ~s~%"
                         (cons command args)
                         (sb-ext:process-exit-code process))))
      (values (get-output-stream-string out) process))))

(defun call-git (&rest args)
  #+debug
  (format t "wat: ~s ~s ~s~%" args *default-pathname-defaults* *repo-working-dir*)
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program
                   "git" (loop for e in (flatten-tree args)
                               collect (if (pathnamep e) (namestring e) e))
                   :directory *default-pathname-defaults*
                   :output out
                   :error *error-output*
                   :search t))
         (output-string (get-output-stream-string out))) ; reminder: `get-output-stream-string' is destructive
    #+debug
    (when *current-git-output-port*
      (format *current-git-output-port* "git output for git ~a:~%~a" args output-string))
    (when (and *git-raise-error* (not (= 0 (sb-ext:process-exit-code process))))
      (error (format nil "git ~s failed with status ~s~%"
                     args
                     (sb-ext:process-exit-code process))))
    (values output-string process)))

(defun git-master-branch ()
  ; TODO
  "master")

(defun split-string-nl (string)
  (remove-if
   (lambda (s) (string= s ""))
   (uiop:split-string
    string
    :separator '(#\Newline))))

(defun split-branch-name (branch)
  (cond
    ((member branch (git-list-local-branch-names) :test #'string=)
     (cons "." branch))
    ((find #\/ branch)
     (or (loop for remote in (git-list-remotes)
               when
               (uiop:string-prefix-p
                (concatenate 'string remote "/")
                branch)
               return (cons remote (cadr (uiop:split-string branch :separator '(#\/)))))))))

(defun branch-local-name (branch)
  (format t "bln: ~a~%" branch)
  (let* ((bn (split-branch-name branch))
         (branch (cdr bn)))
    branch))

#+()
(let ((foo "~/git/sigh.foo")
      (poop "~/git/sigh.poop/"))
  (list
   (uiop:ensure-pathname foo)
   (uiop:ensure-pathname poop)
   (pathname-directory poop)

   ;(file-namestring (uiop:ensure-directory-pathname foo))

   "--"
   (uiop:ensure-directory-pathname foo)
   (uiop:directory-pathname-p foo)
   (pathname-directory foo)
   (directory foo)
   (directory-namestring foo)
   :file-namestring (file-namestring foo)
   (uiop:pathname-parent-directory-pathname foo)
   "--"
   (uiop:ensure-directory-pathname poop)
   (uiop:directory-pathname-p poop)
   (pathname-directory poop)
   (directory poop)
   (directory-namestring poop)
   :file-namestring (file-namestring poop)
   (uiop:pathname-parent-directory-pathname poop)
   ))

(defun git-clone (repo-url worktree)
  (let* ((dwt (uiop:ensure-directory-pathname worktree))
         (parent (uiop:pathname-parent-directory-pathname dwt))
         (dir-name (car (reverse (pathname-directory dwt)))) ; XXX i cannot fooing believe how fooing insanely broken this poop is
         )
    #+() ; this does not work due to libgit2/libssh2 credentials issues, we still need git git for clone
    (cl-git:clone-repository repo-url worktree)
    ;; FIXME probably need to ensure parent exists at some point?
    (call-git "-C" parent "clone" repo-url dir-name)))

(defun git-fetch (&optional remote)
  (run-git "fetch" remote))

(defun git-status (&rest paths)
  (run-git "status" "--" paths))

;;(define-condition continue-from-git-error (condition) ()) ; not needed, *git-raise-error* nil better
(defun git-stash-checkout-reset (&optional branch)
  (let* ((branch (or branch (git-master-branch)))
         (is-set-remote (car (git-config "branch" branch "remote")))
         (upstream-remote
           (or is-set-remote
               (git-push-remote) ; fail over to the master remote i.e. usually origin
               ;; XXX there is probably a correct way to find the possible remotes for an ambiguous branch
               ;; but for now just go with whatever master is mapped to
               (car (git-config "branch" (git-master-branch) "remote"))))
         (upstream-branch (concatenate 'string upstream-remote "/" branch)))
    (when (or (git-diff) (git-ls-others))
      (git-stash-both))
    (run-git "checkout" branch)
    (or
     (let (*git-raise-error*) (git-ref-abbrev (concatenate 'string branch "@{upstream}")))
     (run-git "branch" (concatenate 'string "--set-upstream-to=" upstream-branch) branch))
    #+() ; duh
    (restart-case
        (handler-bind
            ((error
               (lambda (condition)
                 (declare (ignore condition))
                 (invoke-restart 'continue-from-git-error))))
          (git-ref-abbrev (concatenate 'string branch "@{upstream}")))
      (continue-from-git-error ()
        ;; FIXME > asuming error code
        (run-git "branch" (concatenate 'string "--set-upstream-to=" upstream-branch)
                 branch)))
    (run-git "reset" "--hard" upstream-branch)))

(defun git-stash-checkout-reset-pull (&optional branch)
  (git-stash-checkout-reset branch)
  (run-git "pull"))

(defun git-stash-both ()
  ;; the magit version of this is hideously complicated
  (run-git "stash" "--include-untracked"))

(defun git-diff (&optional pattern)
  (let ((pattern (or pattern ".")))
    (split-string-nl (run-git "diff" "--name-only" "--" pattern))))

(defun git-ls-others ()
  (split-string-nl
   (run-git "ls-files" "--others" "--exclude-standard")))

(defun git-list-remotes ()
  (split-string-nl
   (run-git "remote")))

(defun git-list-local-branch-names ()
  (split-string-nl (run-git "for-each-ref" "--format" "%(refname:short)" "refs/heads")))

(defun git-list-remote-branch-names (&key remote)
  (split-string-nl (run-git "for-each-ref" "--format" "%(refname:short)" (concatenate 'string "refs/remotes" (and remote "/") remote))))

(defun git-log-p-n-1 ()
  (run-git "log" "-p" "-n" "1" "--color=always" "--word-diff" "--patience"))

(defun git-checkout-create (branch &key force)
  ;; XXX check this one
  (run-git "checkout" (if force "-B" "-b") branch))

(defun git-add (&rest paths)
  (run-git "add" "--" paths))

(defun git-commit (message)
  (run-git "commit" "-m" message))

(defun git-config (&rest key-elems)
  (let ((*git-raise-error* nil) ; config returns 1 if key not found which isn't an error in this case
        (key (format nil "~{~A~^.~}" key-elems)))
    (split-string-nl
     (run-git "config" key))))

(defun (setf git-config) (value &rest key-elems)
  (run-git "config" (format nil "~{~A~^.~}" key-elems) value)
  value)

(defun git-ref-abbrev (refname)
  (car (split-string-nl (run-git "rev-parse" "--abbrev-ref" refname))))

(defun git-get-current-branch ()
  (git-ref-abbrev "HEAD"))

(defun git-primary-remote ()
  (let ((remotes (git-list-remotes)))
    (car
     (remove-if-not
      (lambda (s) (member s remotes :test #'string=))
      (remove-duplicates
       (remove-if-not
        (lambda (s) s)
        (cons (git-config "magit.primaryRemote") '("upstream" "origin"))))))))

(defun git-push-remote (&key ((:forge-user forge-user) *forge-user*))
  (car (apply #'git-config (git-config-forge-user-push-remote :forge-user forge-user))))

(defun git-remote-add (remote-name remote-uri)
  (run-git "remote" "add" remote-name remote-uri))

(defun git-remote-exists (remote-name)
  (member remote-name (git-list-remotes) :test #'string=))

(defun ensure-token-exists ()
  ;; TODO derive from somewhere/something like forge
  (let ((host "api.github.com")
        (user *forge-user*))
    (unless user
      (error "*forge-user* has not been set"))
    (oa-authinfo-get
     (intern host 'keyword)
     (intern (concatenate 'string user "^" "prrequaestor") 'keyword))))

(defun git-push (&key dry-run)
  ;; FIXME reminder that there may be distinct push and pull urls
  (let* ((branch (git-get-current-branch))
         (is-set-remote (car (git-config "branch" branch "remote")))
         (remote (or is-set-remote  ; XXX TODO other remote ??? or did pushurl fix it?
                     (git-push-remote)
                     (and dry-run (car (git-config "branch" (git-master-branch) "remote"))))))
    (if is-set-remote
        (run-git "push" (and dry-run "--dry-run"))
        (progn
          (ensure-token-exists) ; before we proceed to push make sure we have access to create the pull request
          ;; we mostly use ssh access for git operations, but we need the token for api access
          ;; all the robots already have ssh keys that we can use for this stuff (mostly)
          ;; though having the access token might simplify operations? at least for github?
          (unless remote
            (error "No remote for ~a. This would go badly." branch))
          (run-git "push" (and dry-run "--dry-run") "-v" "--set-upstream" remote branch)))))

(defun git-get-upstream-branch (&optional branch)
  (let* ((branch (or branch (git-get-current-branch)))
         (upstream (git-ref-abbrev (concatenate 'string branch "@{upstream}"))))
    upstream))

(defun git-reset-to-upstream ()
  (let* ((branch (git-get-current-branch))
         (upstream-branch (git-get-upstream-branch branch)))
    (run-git "reset" "--hard" upstream-branch)))

#-dumped-image
(defun test2 ()
  (let ((*repo-working-dir*
          #p"~/git/pyontutils/" ; fooing trailing slash on paths
          ))
    (format t "~s~%"
            (list
             ;;(git-status)
             ;;(git-diff)
             ;;(git-log-p-n-1)
             (git-get-current-branch)
             (git-config "branch" "master" "remote")
             (git-list-remotes)
             (git-config "magit.primaryRemote")
             (git-primary-remote)
             (ensure-token-exists)
             (git-get-upstream-branch)
             (git-list-local-branch-names)
             (git-list-remote-branch-names)
             #+()
             (git-ls-others)))))

(defmacro with-repo (&rest body)
    #+()
    (&body body &key ((:no-clone do-not-clone)) &allow-other-keys)
  ;; XXX cannot have :no-clone as a &key because then body can only have an even number of arguments
  ;; cl reeeally not good a mixing rest and kwargs
  ;; amusingly this doesn't quire repo-id because repo-id is already present in defsync ...
  ;; I understand why this is an easier way to ensure that we have the repo we want present
  `(let* ((repo (gethash repo-id *repos*))
          (_ (unless repo (error (format nil "foo: ~s~%" repo-id))))
          #+debug
          (__ (format t "repo-struct: ~s~%" repo))
          (*repo-working-dir* (repo-local-path repo))
          just-cloned)
     (declare (ignore _) #+debug (ignore __))
     (unless (uiop:directory-exists-p *repo-working-dir*)
       ,(if *do-not-clone*
            '(error (format nil "not cloned and :no-clone is set, could clone to ~s~%" *repo-working-dir*))
            '(progn
              (git-clone (repo-remote repo) *repo-working-dir*) ; this must clone first
              ;; FIXME repo-forge-fill-values needs to be called in here so we can get the configuration
              ;; correct before anyone tries to run the block ???? OR with-repo is really with-repo
              ;; and we need to sort all the push stuff out before we get here, and it is always
              ;; (repo-push repo) that we pass into with-repo ? this is tricky because
              ;; with-repo is technically the local repo that can have multiple remotes ...
              ;;(can-push-p (repo-remote repo))
              ;; TODO test push rights
              (setf just-cloned t))))
     (let ((*default-pathname-defaults* *repo-working-dir*)
           (*current-repo* repo)
           (*repo-just-cloned* just-cloned)
           ;; TODO figure out if there is a way we can skip actually needing a token when testing/internal
           ;; also ideally we want to move toward being able to use tokens to push so we can dispense with
           ;; needing ssh config, so I guess we provide a flag that will check for the token but not
           ;; do the remote checks? or what ... hrm I think we need one that allows ensure-token-exists
           ;; to return nil as well as one that disables authed forge checks, this mostly doesn't matter
           ;; after the initial setup of a repo and fork as long as there is no push happening
           (*github-token*
             (unless *no-auth*
               (format t "checking for api token ...~%")
               (prog1
                   (ensure-token-exists)
                 (format t "token found ...~%")))))
       (repo-forge-fill-values repo)
       (git-fetch (car (apply #'git-config (git-config-forge-user-push-remote))))
       ,@body)))

(defun file-name-with-extension (path extension)
  (concatenate 'string (uiop:split-name-type path) "." extension))

(defun defsync
    (&key
       ((:repo repo-id))
       ((:fn command-fun))
       old-branch
       new-branch
       ((:prefix branch-prefix-string))
       ;;(:branch old-branch)
       ;;(:branch new-branch)
       ((:diff diff-pattern) ".")
       ((:get-add-files get-add-files) #'identity)
       ((:title pr-title))
       ((:body pr-body)))
    (unless repo-id (error ":repo-id is a required argument"))
    (unless command-fun (error ":fn is a required argument"))
    (with-repo ; repo-id ; XXX possibly bad design but we don't need repo-id because cl is not hygenic ...
      ; :no-clone t
      (let* ((source-branch (or old-branch (git-master-branch)))
             (_ (git-stash-checkout-reset source-branch)) ; if not (git-master-branch) won't exist will error
             (branch-prefix-string (or *pr-branch-prefix* branch-prefix-string *pr-branch-prefix-default*))
             (pr-already-exists (forge-latest-automated-pr-branch
                                 *current-repo*
                                 :prefix branch-prefix-string))
             (latest-remote-auto-branch (branch-local-name
                                         (git-latest-automated-branch
                                          :prefix branch-prefix-string
                                          :remote (car (split-branch-name
                                                        (git-get-upstream-branch source-branch))))))
             (parent-branch-or-existing (or pr-already-exists latest-remote-auto-branch source-branch))
             (target-branch (or new-branch (concatenate 'string branch-prefix-string *build-id*))))
        (declare (ignore _))
        (format t "pr-already-exists: ~s~%" pr-already-exists)
        (format t "latest-remote-auto-branch: ~s~%" latest-remote-auto-branch)
        (if *repo-just-cloned*
            (when (or pr-already-exists latest-remote-auto-branch)
              (run-git "checkout" parent-branch-or-existing))
            (progn
              (format t "stash and pull ...~%")
              (git-stash-checkout-reset-pull parent-branch-or-existing)))
        (git-reset-to-upstream) ; reminder that this fails if we didn't set upstream beforehand
        (format t "running command ... ~a~%" command-fun)
        (funcall command-fun)
        ;; FIXME the ordering of the operations seems a bit suspect here?
        ;; or maybe not? shouldn't we check out the target branch first
        ;; for safety? or no? we want the repo in the state of the upstream
        ;; branch, not the existing pr-branch probably?
        (let* ((files-changed (git-diff diff-pattern))
               ;; FIXME need to be able to add untracked files becuase files-changed is empty for new files
               (files-to-add (and files-changed (funcall get-add-files files-changed)))) ; don't trust callers to not provide a constant function like we did in test-2
          (format t ":changed ~a :add ~a~%" files-changed files-to-add)
          (if files-to-add
              (progn
                (format t "adding ...~%    ~{~a~^~%    ~}~%" files-to-add) ; last ~% avoids wierd error print
                (unless (or pr-already-exists latest-remote-auto-branch)
                  ;; checkout first so failsafe on branch
                  ;; XXX hit up the reflog if something goes wrong
                  ;; XXX yet another reason to never use this on working repos
                  (git-checkout-create target-branch :force t))
                (git-add files-to-add)
                (git-commit (format nil "~a~%~%~a" pr-title (or pr-body "")))
                (format t "~a~%~a~%~a~%" *sepstr* (git-log-p-n-1) *sepstr*)
                (if *push-pr*
                    (progn
                      (format t "pushing ...~%")
                      ;; FIXME amusingly the changes I just finished implementing
                      ;; are a problem because the API user might not, or rather
                      ;; will amost surely not match the ssh user, so when we go
                      ;; to push with a git@github.com:user/repo.git uri it will fail (lol)
                      (git-push)
                      (unless pr-already-exists
                        (format t "creating pull request ...~%")
                        (github-create-pull-request
                         pr-title
                         pr-body
                         ;; always target upstream source even if there is a branch on upstream
                         ;; that matches the auto pull request branch name that shouldn't happen
                         ;; but if it does as it has during testing, ignore the other auto branch
                         :source (git-get-upstream-branch parent-branch-or-existing)
                         :target (git-get-upstream-branch source-branch) ; FIXME EXTREME NAMING CONFUSION  YEAH
                         ;; source-branch up there means that it will be the target for the pull request
                         ;; because it is the source we modified and thus want to pull request back to
                         ;; perspective is everything ... so really need to different names
                         )
                        ;; TODO print link to pull request so if run by human they can click
                        )
                      ;; note that we only run checkout back to master when
                      ;; running for real when in pretend mode 99% of the time
                      ;; the user will want the repo on the new branch
                      (format t "checking out ~a branch ...~%" (git-master-branch))
                      ;; TODO might need to stash again?
                      (run-git "checkout" (git-master-branch)))
                    (progn
                      (format t "dry-run push ...~%")
                      (git-push :dry-run t)))
                nil)
              (format t "no files changed ...~%"))))))

(defun build ()
  ;; FIXME not quite what we want because run-command-check writes to a string not stdout
  (let* ((command "sbcl")
         (args '("--script" "./prcl-build.lisp"))
         (process (sb-ext:run-program
                   command args
                   :environment (or *subprocess-environment* (sb-ext:posix-environ))
                   :directory *default-pathname-defaults*
                   :output *standard-output*
                   :error *error-output*
                   :search t)))
    (when (not (= 0 (sb-ext:process-exit-code process)))
      (error (format nil "command ~s failed with status ~s~%"
                     (cons command args)
                     (sb-ext:process-exit-code process))))
    process))

(defun main (&key fun)
  (parse-args:cli-gen
   (((:current-build-id (make-build-id)) *build-id*) ; set `current-build-id' from cli if needed
    ((:push-pr) *push-pr*)
    ((:resume) resume) ; start from results of a pretend run ; TODO implement this
    ((:forge-fork) *forge-fork*)
    ((:forge-user *forge-user-default*) *forge-user*)
    ((:no-auth) *no-auth*) ; only needed if testing before a fork has been created and .git/config updated
    ;; paths
    ((:build-dir *build-dir*) cli-build-dir)
    ((:auth-source nil) auth-source) ; pass an additional path to add to `auth-sources'
    ((:secrets *oa-secrets*) *oa-secrets*)
    ;; debug
    ((:debug) *debug*)
    ;; dynamic sync
    ((:specs *sync-specs*) *sync-specs*) ; path to elisp file with sync defs
    ((:fun fun) cli-current-sync)
    ;; pull request info
    ((:pr-branch-prefix nil) pr-branch-prefix) ; override defsync :prefix
    )
   (declare (ignore parse-args::cases parse-args::returns parse-args::parsed))
   (when *debug*
     (format *standard-output* "argv: ~A~%" sb-ext:*posix-argv*)
     (format *standard-output* "~S~%"
             (list
              :bid *build-id*
              :ppr *push-pr*
              :res resume ;|resume|
              :deb *debug* ;|debug|
              :bd cli-build-dir
              :as auth-source
              :oas *oa-secrets*
              :ss *sync-specs*
              :ccs cli-current-sync
              :bpr pr-branch-prefix
              )))
   (let ((*auth-sources* (or (and auth-source (cons auth-source *auth-sources*)) *auth-sources*))
         (*build-dir* (uiop:ensure-directory-pathname cli-build-dir)) ; SIGH
         (*current-git-output-port* (when *debug* *standard-output*))
         (*pr-branch-prefix* pr-branch-prefix)
         (*git-raise-error* t)
         *current-sync*)
     (ensure-directories-exist *build-dir*)
     (if (or (and *sync-specs* cli-current-sync) (uiop:string-prefix-p "test" cli-current-sync))
         (let ((fun (intern (string-upcase (concatenate 'string "sync-" cli-current-sync)) 'prcl-sync)))
           (when *sync-specs*
             (in-package :prcl-sync)
             (load *sync-specs*)
             (in-package :prcl)
             ) ; no package safety ...
           (unless (fboundp fun)
             (error (format nil "No sync function named: ~s~%" fun)))
           (setf *current-sync* (intern cli-current-sync))
           (funcall fun))
         (when cli-current-sync
           (format t "unknown sync function ~a~%" cli-current-sync)
           ;; TODO set exit value
           (uiop:quit 1)))
     (when *current-sync*
       (format *standard-output* "~async completed for ~a ~a~%"
               (if *push-pr* "" "pretend ")
               *current-sync* *build-id*)))))

(in-package :prcl-sync)

(defrepo 'idlib "git@github.com:tgbugs/idlib.git")
(defun sync-test ()
  (format *standard-output* "WHAEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE~%")
  (defsync
      :repo 'idlib
      :fn (lambda () (format t "hello word from inside the thunk~%"))
    ))

(defrepo 'test-github-api "https://github.com/tgbugs/test-github-api.git")
(defun sync-test-2 ()
  #+debug
  (format *standard-output* "HAWYEE ??????~%")
  (let ((*random-state* (make-random-state t)) ; per sbcl manual `*random-state*' inits to same value at start
        ; note also that this works because `*random-state*' is not initialized with a default value
        ; which explains some of the issues I've had with dynamic variables in other parts of the file
        (new-file "some-file.org"))
    (defsync
        :repo 'test-github-api
      :fn (lambda ()
            (let ((new-file-path (merge-pathnames new-file *repo-working-dir*)))
              #+(or debug)
              (format t "ffs: ~s~%" new-file-path)
              (with-open-file
                  (stream new-file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
                (format stream "* A new heading~%~a~%"
                        (loop
                          with len = 16
                          with s = (make-string len)
                          for i below len
                          do (setf (aref s i) (code-char (+ 48 (random 10))))
                          finally (return  s))))))
      :get-add-files (lambda (paths) (declare (ignore paths)) (list new-file))
      :title (format nil "automated changes for test-2 sync process at ~a" *build-id*)
      :body "some automated changes"
      )))

(export 'sync-test :prcl-sync)
(export 'sync-test-2 :prcl-sync)

(in-package :prcl)
#+debug (format t "features: ~S~%" *features*)
#+debug (format t "raw-cli-args: ~S~%" (uiop:raw-command-line-arguments))
#-(or swank dumped-image) ; this is a hack that only sort of works
(main)

;; test
;; (prcl:main :fun "test")
;; bin/prcl --fun test --debug --resume --build-dir /tmp/test-prcl-build-dir
;; bin/prcl --fun test --debug --resume --build-dir /tmp/test-prcl-build-dir --specs ~/ni/sparc/sync-specs.lisp
;; bin/prcl --fun test-2 --debug --resume --build-dir /tmp/test-prcl-build-dir --specs ~/ni/sparc/sync-specs.lisp --pr-branch-prefix auto-test-
;; bin/prcl --fun test-2 --debug --resume --build-dir /tmp/test-2-prcl-build-dir --specs ~/ni/sparc/sync-specs.lisp --pr-branch-prefix auto-test-
