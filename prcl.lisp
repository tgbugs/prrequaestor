;; -*- mode: Common-Lisp -*-

;; there are at least three ways to use this file

;; run it in emacs via slime (you may need to run the `ql:quickload's first)

;; run it from cli via
;; sbcl --script prcl.lisp --specs path/to/sync-specs.lisp --fun test

;; or slad and run the executable directly via prcl-build.lisp
;; ./prcl-build.lisp && bin/prcl --specs path/to/sync-specs.lisp --fun test

;; FIXME detect stale automated branches and error if the upstream branch has changed or something
;; FIXME add new file if not exists fails in some cases?
;; FIXME need separate working trees/copies of repos per sync to avoid concurrent syncs using the same repo
;; TODO fork to do the git operations as a user without push rights?
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
(defvar *repo-working-dir* nil)
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

(defvar *git-raise-error* nil)
(defvar *sepstr* (make-string 70 :initial-element #\-))

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
  (forge nil)
  ;(forge-api nil)
  (owner nil)
  (name nil)
  (pull-requests nil)
  )

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
  (let ((repo (make-repo :id id :remote remote)))
    (when local
      (setf (repo-local repo) (repo-local-validate local)))
    (repo-remote-fill-values repo)
    (setf (gethash (repo-id repo) *repos*) repo)))

(defun get-latest-automated-pr-branch (repo &key prefix)
  ;; FIXME make sure the pull request is open!
  (let ((prefix (or prefix *pr-branch-prefix* *pr-branch-prefix-default*))
        out)
    #+debug
    (format t "prefix: ~a ~a ~a~%" prefix *pr-branch-prefix* *pr-branch-prefix-default*)
    (loop for pr in (repo-pull-requests repo)
          ;;(assoc :title pr)
          ;;(assoc :base pr)
          ;; TODO sort by date
          until (let ((ref (cdr
                            (assoc :ref (cdr (assoc :head pr))))))
                  #+debug
                  (format *standard-output* "pr ref: ~s~%" ref)
                  (if (uiop:string-prefix-p prefix ref)
                      (progn
                        (format *standard-output* "ref ok match: ~s~%" ref)
                        (setf out ref))
                      (format *standard-output* "ref no match: ~s~%" ref))))
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

(defun get-json (uri)
  (multiple-value-bind (body status response-headers response-uri stream)
    (dex:get uri)
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

(defun repo-forge-fill-values (repo)
  (let ((pull-requests (forge-get-repo-prs repo)))
    (setf (repo-pull-requests repo) pull-requests)))

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
             (setf (repo-name repo) name))))))

(defun test ()

  (let ((owner "tgbugs") (name "pyontutils"))
    (format nil "https://api.github.com/repos/~s/~s/pulls" owner name))

  (let ((test-repo
          (defrepo 'pyontutils
            "https://github.com/tgbugs/pyontutils.git")))
    (let ((out (repo-forge-fill-values test-repo)))
      out)))

(defvar *var* nil)
(defun test-1.5 ()
  (progn
    (setf *var* (test))
    (loop for (k . v) in (car *var*) collect k)

    (assoc :title (car *var*))
    (get-latest-automated-pr-branch (gethash 'pyontutils *repos*))

    (defrepo 'apinatomy-models
      "https://github.com/open-physiology/apinatomy-models.git")
    (repo-forge-fill-values (gethash 'apinatomy-models *repos*))
    (get-latest-automated-pr-branch
     (gethash 'apinatomy-models *repos*))

    (defrepo 'test-github-api
      "https://github.com/tgbugs/test-github-api.git")
    (repo-forge-fill-values (gethash 'test-github-api *repos*))
    (get-latest-automated-pr-branch
     (gethash 'test-github-api *repos*))

    ))

(defun github-create-pull-request (title body &key source target)
  (let* ((source (or source (git-get-upstream-branch (git-get-current-branch))))
         ;; XXX warning, ensure we push creating a new branch
         (target (or target (git-get-upstream-branch (git-master-branch))))

         (head-repo *current-repo*)
         (uri-path (concatenate 'string "/repos/"
                                (repo-owner head-repo) "/"
                                (repo-name head-repo) "/pulls"))
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
    ;; head source
    ;; base target
    (let ((resp
            (dex:post
             uri
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
      resp)))

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
                   :search t)))
    #+debug
    (when *current-git-output-port*
      (format *current-git-output-port* "git output for git ~a:~%~a" args (get-output-stream-string out)))
    (when (and *git-raise-error* (not (= 0 (sb-ext:process-exit-code process))))
      (error (format nil "git ~s failed with status ~s~%"
                     args
                     (sb-ext:process-exit-code process))))
    (values (get-output-stream-string out) process)))

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
   (file-namestring foo)
   (uiop:pathname-parent-directory-pathname foo)
   "--"
   (uiop:ensure-directory-pathname poop)
   (uiop:directory-pathname-p poop)
   (pathname-directory poop)
   (directory poop)
   (directory-namestring poop)
   (file-namestring poop)
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

(defun git-status (&rest paths)
  (run-git "status" "--" paths))

(defun git-stash-checkout (&optional branch)
  (let ((branch (or branch (git-master-branch))))
    (when (or (git-diff) (git-ls-others))
      (git-stash-both))
    (run-git "checkout" branch)))

(defun git-stash-checkout-pull (&optional branch)
  (git-stash-checkout branch)
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

(defun git-checkout-create (branch)
  ;; XXX check this one
  (run-git "checkout" "-b" branch))

(defun git-add (&rest paths)
  (run-git "add" "--" paths))

(defun git-commit (message)
  (run-git "commit" "-m" message))

(defun git-config (&rest key-elems)
  (let ((*git-raise-error* nil) ; config returns 1 if key not found which isn't an error in this case
        (key (format nil "~{~A~^.~}" key-elems)))
    (split-string-nl
     (run-git "config" key))))

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

(defun ensure-token-exists ()
  ;; TODO derive from somewhere/something like forge
  (let ((host "api.github.com")
        (user "tgbugs"))
    (oa-authinfo-get
     (intern host 'keyword)
     (intern (concatenate 'string user "^" "forge") 'keyword))))

(defun git-push ()
  (let* ((branch (git-get-current-branch))
         (is-set-remote (git-config "branch" branch "remote"))
         (remote (or is-set-remote (git-primary-remote))))
    (if is-set-remote
        (run-git "push")
        (progn
          (ensure-token-exists) ; before we proceed to push make sure we have access to create the pull request
          ;; we mostly use ssh access for git operations, but we need the token for api access
          ;; all the robots already have ssh keys that we can use for this stuff (mostly)
          ;; though having the access token might simplify operations? at least for github?
          (run-git "push" "-v" "--set-upstream" remote branch)))))

(defun git-get-upstream-branch (&optional branch)
  (let* ((branch (or branch (git-get-current-branch)))
         (upstream (git-ref-abbrev (concatenate 'string branch "@{upstream}"))))
    upstream))

(defun git-reset-to-upstream ()
  (let* ((branch (git-get-current-branch))
         (upstream-branch (git-get-upstream-branch branch)))
    (run-git "reset" "--hard" upstream-branch)))

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
          (__ (format t "repo-struct: ~s~%" repo))
          (*repo-working-dir* (repo-local-path repo))
          just-cloned)
     (declare (ignore _) (ignore __))
     (unless (uiop:directory-exists-p *repo-working-dir*)
       ,(if *do-not-clone*
            '(error (format nil "not cloned and :no-clone is set, could clone to ~s~%" *repo-working-dir*))
            '(progn
              (git-clone (repo-remote repo) *repo-working-dir*)
              (setf just-cloned t))))
     (let ((*default-pathname-defaults* *repo-working-dir*)
           (*current-repo* repo)
           (*repo-just-cloned* just-cloned))
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
      (repo-forge-fill-values *current-repo*)
      (let* ((source-branch (or old-branch (git-master-branch)))
             (_ (git-stash-checkout source-branch)) ; if not (git-master-branch) won't exist will error
             (branch-prefix-string (or *pr-branch-prefix* branch-prefix-string *pr-branch-prefix-default*))
             (pr-already-exists (get-latest-automated-pr-branch
                                 *current-repo*
                                 :prefix branch-prefix-string))
             (latest-remote-auto-branch (branch-local-name
                                         (git-latest-automated-branch
                                          :prefix branch-prefix-string
                                          :remote (car (split-branch-name
                                                        (git-get-upstream-branch source-branch))))))
             (parent-branch-or-existing (or pr-already-exists latest-remote-auto-branch source-branch))
             (target-branch (or new-branch (concatenate 'string branch-prefix-string *build-id*))))
        (format t "pr-already-exists: ~s~%" pr-already-exists)
        (format t "latest-remote-auto-branch: ~s~%" latest-remote-auto-branch)
        (if *repo-just-cloned*
            (when (or pr-already-exists latest-remote-auto-branch)
              (run-git "checkout" parent-branch-or-existing))
            (progn
              (format t "stash and pull ...~%")
              (git-stash-checkout-pull parent-branch-or-existing)))
        (git-reset-to-upstream)
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
                  (git-checkout-create target-branch)) ; checkout first so failsafe on branch
                (git-add files-to-add)
                (git-commit (format nil "~a~%~%~a" pr-title (or pr-body "")))
                (format t "~a~%~a~%~a~%" *sepstr* (git-log-p-n-1) *sepstr*)
                (unless pr-already-exists
                  (format t "checking for api token ...~%")
                  (setf *github-token* (ensure-token-exists)))
                (format t "token found ...~%")
                (when *push-pr*
                  (format t "pushing ...~%")
                  (git-push)
                  (unless pr-already-exists
                    (format t "creating pull request ...~%")
                    (github-create-pull-request
                     pr-title
                     pr-body
                     :target (git-get-upstream-branch parent-branch-or-existing))
                    ;; TODO print link to pull request so if run by human they can click
                    )
                  ;; note that we only run checkout back to master when
                  ;; running for real when in pretend mode 99% of the time
                  ;; the user will want the repo on the new branch
                  (format t "checking out ~a branch ...~%" (git-master-branch))
                  ;; TODO might need to stash again?
                  (run-git "checkout" (git-master-branch)))
                nil)
              (format t "no files changed ...~%"))))))

(defun main (&key fun)
  (parse-args:cli-gen
   (((:current-build-id (make-build-id)) *build-id*) ; set `current-build-id' from cli if needed
    ((:push-pr) *push-pr*)
    ((:resume) resume) ; start from results of a pretend run ; TODO implement this
    ;; paths
    ((:build-dir *build-dir*) cli-build-dir)
    ((:auth-source nil) auth-source) ; pass an additional path to add to `auth-sources'
    ((:secrets *oa-secrets*) *oa-secrets*)
    ;; debug
    ((:debug) debug)
    ;; dynamic sync
    ((:specs *sync-specs*) *sync-specs*) ; path to elisp file with sync defs
    ((:fun fun) cli-current-sync)
    ;; pull request info
    ((:pr-branch-prefix nil) pr-branch-prefix) ; override defsync :prefix
    )
   (declare (ignore parse-args::cases parse-args::returns parse-args::parsed))
   (when debug
     (format *standard-output* "argv: ~A~%" sb-ext:*posix-argv*)
     (format *standard-output* "~S~%"
             (list
              :bid *build-id*
              :ppr *push-pr*
              :res resume ;|resume|
              :deb debug ;|debug|
              :bd cli-build-dir
              :as auth-source
              :oas *oa-secrets*
              :ss *sync-specs*
              :ccs cli-current-sync
              :bpr pr-branch-prefix
              )))
   (let ((*auth-sources* (or (and auth-source (cons auth-source *auth-sources*)) *auth-sources*))
         (*build-dir* (uiop:ensure-directory-pathname cli-build-dir)) ; SIGH
         (*current-git-output-port* (when debug *standard-output*))
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
