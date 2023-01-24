:;exec emacs -batch -no-site-file -file   "$0" -eval "(progn (delete-region 1 2) (eval-buffer))" -- "$@" # -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;     @emacs -batch -no-site-file -file "%~f0" -eval "(progn (delete-region 1 2) (eval-buffer))" -- %*   & goto :EOF
;;; prrequaestor.el --- run commands create pull requests from the diff

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/prrequaestor
;; Keywords: git
;; Package-Requires: ((emacs "27.1") magit forge)

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Use case:

;; let p be a process that modifies the contents of a git repo
;; prrequestor converts the changes induced by p on the repo into a
;; single commit and then makes a pull request for visibility

;; Configuration:

;; this script needs push acces to the remote repositories that
;; it operates on, the best way to achieve this is by adding an
;; entry for the remote host in ~/.ssh/config

;; config vars can be passed via the command line
;; Persistent values pointing to secrets paths can be set in
;; e.g. /tmp/prm-build/.emacs.d/init.el via
;; (setq oa-secrets "/path/to/secrets.sxpr")
;; (setq auth-sources (cons "/path/to/.authinfo" auth-sources))

;; Examples:

;; ./prrequaestor.el --fun test
;; ./prrequaestor.el --fun test --debug
;; ./prrequaestor.el --user-init-file ~/.emacs.d/prm-init.el --fun test --debug
;; ./prrequaestor.el --auth-source path/.authinfo --secrets path/secrets.sxpr --fun test --push-pr 
;; ./prrequaestor.el --specs path/to/specs.el --fun some-other-sync

;; Old Examples:

;; ./prrequaestor.el sync test
;; ./prrequaestor.el sync test --debug
;; ./prrequaestor.el --user-init-file ~/.emacs.d/prm-init.el sync test --debug
;; ./prrequaestor.el --auth-source path/.authinfo --secrets path/secrets.sxpr sync test --push-pr 

;; Extension:

;; Using `abstracted-git-transformer' to define a new sync function.

;; (defun sync-name ()
;;   ":name"
;;   (abstracted-git-transformer
;;    my-repo                          ; repository
;;    "master"                         ; existing branch
;;    (concat "automated-" build-id)   ; default value for new branch
;;    (lambda () body ...)             ; our process p, a git transformer, can also be a #'function-name
;;    "."                              ; git path expansion to match for changes
;;    (lambda (paths) other-paths)     ; function from list of files matched above to a list of files to add
;;    "commit and pull request title"
;;    "commit and pull request body"))

;; If you define sync-name in sync-specs.el it can be run via ./prrequaestor.el --sync-specs sync-specs.el --sync-fun name

;; Old extension
;; Adding a new sync function as a command line argument that can be run via ./prrequaestor.el --name
;; 1. In the syncs section define a new function sync-name using the pattern above.
;; 2. In the defrepos section add my-repo if it does not exist.
;; 3. In `main' add (:name) as a new entry in the `ow-cli-gen' bind-keywords spec
;; 4. In `main' add (name (setq current-sync 'name) (sync-name)) to the cond block

;;; Code:

(require 'cl-lib)

;; TODO see if we can remove forge since it is big and EmacsSQL takes forever to compile

;; TODO make it possible to register commands so that we can import
;; the core file and then reuse the machinery

(defvar prm-build-dir (expand-file-name "prm-repos" (temporary-file-directory)))
(defvar prm-sync-specs (and (file-exists-p "sync-specs.el")
                            (expand-file-name "sync-specs.el" default-directory)))
(defvar prm--repos (make-hash-table :test #'eq))
(defvar prm--push-pr nil)
(defvar just-cloned nil)
(defvar current-build-id nil)

;;; begin setup

;; see if --user-emacs-directory or --build-dir was passed in argv
;; reval's caching behavior means that it needs ued to be set
;; there is no way to defer that and have fetch-once cache behavior
;; so we peak at `argv' to see if we can set an alternate ued

(defun cli-opt (opt)
  (let ((index (cl-position opt argv :test #'string=)))
    (and index (elt argv (1+ index)))))

(when noninteractive
  (let ((prm-build-dir (or (cli-opt "--build-dir") prm-build-dir)))
    (setq package-user-dir nil ; sticks to ~/.emacs.d, must be cleared
          user-emacs-directory
          (or (cli-opt "--user-emacs-directory")
              (and prm-build-dir
                   (expand-file-name ".emacs.d/" prm-build-dir))))))

;; minimal reval

(defun setup-reval ()
  (unless (featurep 'reval)
    (defvar reval-cache-directory (concat user-emacs-directory "reval/cache/"))
    ;; FIXME we need a deferred write mechanism so that we can reval before
    ;; having a known `user-emacs-directory' and then write once we know
    (defun reval-minimal (cypher checksum path-or-url &rest alternates)
      "Simplified and compact implementation of reval."
      (let* (done (o url-handler-mode) (csn (symbol-name checksum))
             (cache-path (concat reval-cache-directory (substring csn 0 2) "/" csn
                                 "-" (file-name-nondirectory path-or-url))))
        (url-handler-mode)
        (unwind-protect
            (cl-loop
             for path-or-url in (cons cache-path (cons path-or-url alternates)) do
             (when (file-exists-p path-or-url)
               (let* ((buffer (find-file-noselect path-or-url))
                      (buffer-checksum (intern (secure-hash cypher buffer))))
                 (if (eq buffer-checksum checksum)
                     (progn
                       (unless (string= path-or-url cache-path)
                         (let ((parent-path (file-name-directory cache-path))
                               make-backup-files)
                           (unless (file-directory-p parent-path)
                             (make-directory parent-path t))
                           (with-current-buffer buffer
                             (write-file cache-path))))
                       (eval-buffer buffer)
                       (setq done t))
                   (kill-buffer buffer) ; kill so cannot accidentally evaled
                   (error "reval: checksum mismatch! %s" path-or-url))))
             until done) 
          (unless o
            (url-handler-mode 0)))))
    (defalias 'reval #'reval-minimal)
    (reval 'sha256 '3620321396c967395913ff19ce507555acb92335b0545e4bd05ec0e673a0b33b 
           "https://raw.githubusercontent.com/tgbugs/orgstrap/300b1d5518af53d76d950097bcbcd7046cfa2285/reval.el"))

  (let ((ghost "https://raw.githubusercontent.com/tgbugs/orgstrap/"))
    (unless (featurep 'ow)
      (reval 'sha256 'b3b26172d8e54abd0d3c4503d1b5438e0fc29d05fb6275a6da17d4d1b628a38a
             (concat ghost "f4df67e94926f9d389f4a456a9cbf721c9b22b89" "/ow.el")))))

(defun setup ()
  ;; useful for more persistent sourcing of variables
  ;; must load this before `forge' otherwise fail due to missing authinfo
  (when (file-exists-p user-init-file)
    (load user-init-file))
  ;; don't compile packages since it is faster to run interpreted
  ;; than to wait for compile unless we pre-build an image
  (require 'package)
  (defun package--compile (p) (ignore p))

  (ow-enable-use-package)
  (ow-use-packages
   magit
   forge)

  ' ; failed attempt to silence warnings
  (package-delete (cadr (assoc 'bind-key package-alist)))

  (require 'forge-repo))

;;; end setup

(defun make-build-id (&optional time)
  (format-time-string "%Y-%m-%dT%H%MZ" (or time (current-time)) '(0 "UTC")))

(defun run-with-current-build-id (f)
  (let ((current-build-id (make-build-id)))
    (funcall f)))

' ; unused
(defun git-url-basename (repo-url)
  "in git/builtin/clone.c::cmd_clone if dir does not come from argv then it is determined by calling git/dir.c::git_url_basename on repo_name, is_bundle, option_bare, this replicates some portion of that functionality ... the actual code that does this is rather convoluted"
  ;; use `magit-clone--url-to-name' which hopefully is consistent?
  ;; have to strip trailing slashes to match git behavior
  (magit-clone--url-to-name (replace-regexp-in-string "[/ \t]+$" "" repo-url)))

(defmacro defrepo (name remote &optional type local)
  (let ((type (or type 'git)))
    `(puthash ',name (list ,remote ,local) prm--repos)))

(defmacro with-repo (defrepo-name &optional no-clone &rest body)
  (declare (indent defun))
  ;; FIXME replace defrepo-name -> remote-url

  ;; if local exists got to it out
  ;; if it does not and :clone is set then automatically clone it
  ;; all of this is overly complex to deal with the fact that you
  ;; need a local copy for everything else to work as expected and
  ;; it is really easy to mess up the cloning logic
  (let* ((do-not-clone (eq no-clone :no-clone))
         (body (if do-not-clone body (when no-clone (cons no-clone body)))))
    `(let* ((remote-local (gethash ',defrepo-name prm--repos))
            (remote (car remote-local))
            (local (cadr remote-local))
            (repo (forge-get-repository remote remote 'create)) ; XXX this additional call needed to trigger?
            (worktree
             (or (and local (expand-file-name local prm-build-dir))
                 (oref repo worktree)
                 (and (oref repo name)
                      (expand-file-name
                       (oref repo name)
                       prm-build-dir))
                 (error "oops")))
            just-cloned)
       (unless (file-directory-p worktree)
         ,(if do-not-clone
              '(error "not cloned and no-clone is set, could clone to %s" worktree)
            ;; TODO set the mapping here or later?
            '(progn
               (git--clone (oref repo remote) worktree)
               (setq just-cloned t))))
       (let ((default-directory worktree))
         (with-temp-buffer ; prevent accidental command leaks
           ,@body)))))

(defun git--clone (repo-url worktree)
  ;; for internal use, makes some fairly strong assumptions
  ;; about worktree
  ;; have to use `magit-git' because we don't have a repo yet
  (let* ((dwt (directory-file-name worktree))
         (parent (file-name-directory dwt))
         (dir-name (file-name-nondirectory dwt)))
    (magit-git "-C" parent "clone" repo-url dir-name)
    (with-current-buffer (magit-process-buffer)
      (message "%s" (buffer-string)))))

(defun git-stash-checkout-pull (&optional branch)
  (let ((branch (or branch (magit-main-branch))))
    (when (or (git-diff) (git-ls-others))
      (condition-case nil
          (magit-stash-both "stash before pull" 'include-untracked)
        (user-error nil)))
    ;; XXX for some reason here magit decides to call save buffers or
    ;; something and for whatever reason that invokes `map-y-or-n-p'
    ;(magit-checkout branch) ; XXX `magit-checkout' does not do what we want ???
    ;;(ow-run-command "git" "checkout" branch)
    (magit-run-git "checkout" branch)
    (magit-run-git "pull")))

(defun git-diff (&optional pattern)
  (let ((pattern (or pattern ".")))
    (split-string
     ;;(ow-run-command "git" "diff" "--name-only" "--" pattern) ; FIXME has extra output
     (with-output-to-string
       (with-current-buffer standard-output
         (call-process "git" nil t nil "diff" "--name-only" "--" pattern)))
     "\n" t)))

(defun git-ls-others ()
  (split-string
   ;;(ow-run-command "git" "diff" "--name-only" "--" pattern) ; FIXME has extra output
   (with-output-to-string
     (with-current-buffer standard-output
       (call-process "git" nil t nil "ls-files" "--others" "--exclude-standard")))
   "\n" t))

(defun git-log-p-n-1 ()
  (with-output-to-string
    (with-current-buffer standard-output
      (call-process "git" nil t nil "log" "-p" "-n" "1" "--color=always"))))

(defun git-checkout-create (branch)
  (magit-branch-spinoff branch))

(defun git-add (&rest paths)
  (magit-run-git "add" "--" paths))

(defun git-commit (message)
  (magit-run-git "commit" "-m" message))

(defun git-push ()
  ;; we have to set the merge = value in the git config and
  ;; it seems that the remote will then set itself accordingly ?
  (let* ((branch (magit-get-current-branch))
         (no-set-remote (magit-get "branch" branch "remote"))
         (remote (or no-set-remote 
                     (magit-primary-remote))))
    (if no-set-remote
        ;; can't use `magit-push' in batch mode due to
        ;; issue with `switch-to-buffer-other-window'
        (magit-run-git "push")
      (run-hooks 'magit-credential-hook)
      (magit-run-git "push" "-v" "--set-upstream" remote branch))))

(defun git-reset-to-upstream ()
  (let* ((branch (magit-get-current-branch))
         (upstream-branch (magit-get-upstream-branch branch)))
    (magit-run-git "reset" "--hard" upstream-branch)))

(defun get-latest-automated-pr-branch (repo)
  ;; see `forge-list-pullreqs'
  (let ((prs
         (forge-sql [:select $i1
                     :from pullreq
                     :where (and (= repository $s2)
                                 (= state $s3)
                                 ;; string match to avoid other prs
                                 (like head-ref $s4))
                     :order-by (desc created)]
                    ;; additional fields of interest draft-p
                    ;; [base-ref base-repo head-ref head-repo created updated closed merged state]
                    [head-ref]
                    (oref repo id)
                    'open
                    "automated-%")))
    (message "existing-pull-requests: %s" prs)
    (caar prs)))

(defun github-create-pull-request (title body &optional source target)
  ;; adapted from `forge--submit-create-pullreq'
  ;; `magit-list-remote-branch-names'
  (let ((source (or source (magit-get-upstream-branch (magit-get-current-branch))))
        ;; XXX warning, ensure we push creating a new branch
        (target (or target (magit-get-upstream-branch (magit-main-branch))))
        (head-repo (forge-get-repository 'stub))
        (url-mime-accept-string
         ;; Support draft pull-requests.
         "application/vnd.github.shadow-cat-preview+json"))

    ;; head source
    ;; base target
    ;; see `ghub-request' for full details
    (pcase-let* ((`(,base-remote . ,base-branch)
                  (magit-split-branch-name target))
                 (`(,head-remote . ,head-branch)
                  (magit-split-branch-name source)))

      (ghub-post
       (forge--format-resource head-repo "/repos/:owner/:repo/pulls")
       `((title . , title)
         (body  . , body)
         (base  . ,base-branch)
         (head  . ,(if (equal head-remote base-remote)
                       head-branch
                     (concat (oref head-repo owner) ":" head-branch)))
         (draft . t) ; automated pull requests should always be drafts
         (maintainer_can_modify . t))
       :auth 'forge))))

(defconst sepstr (make-string 70 ?\-))

(cl-defun defsync (&key name &key old-branch &key new-branch
                   &key command-thunk &key diff-pattern
                   &key get-add-files &key pr-title &key pr-body)
  ;; we might not even need the thunk here as long as
  ;; defsync is called inside another function instead of
  ;; at top level?
  (unless (boundp 'something-or-other)
    (user-error "don't call this beofre `something-or-other' is bound")
    )
  '
  (lambda ()
    (with-repo name
               (list
                name
                old-branch
                new-branch
                command-thunk
                diff-pattern
                get-add-files)))
  (with-repo name
             (list
              name
              old-branch
              new-branch
              command-thunk
              diff-pattern
              get-add-files))
  )

(defun sync-test-alt ()
  (funcall
   (defsync
    :name 'test-github-api
    :command-thunk (lambda () 'lol)
    :diff-pattern "*"
    :get-add-files (lambda (paths) paths)
    :pr-title "lololol"
    :pr-body "hahahahah")))

' ; probably not this one
(defalias 'sync-test-hrm
  (defsync
   :name 'test-github-api
   :command-thunk (lambda () 'lol)
   :diff-pattern "*"
   :get-add-files (lambda (paths) paths)
   :pr-title "lololol"
   :pr-body "hahahahah"))

'
(sync-test-hrm)

(defun sync-test-thrice ()
  "third time's the charm"
  (defsync
   :name 'test-github-api
   :command-thunk (lambda () 'lol)
   :diff-pattern "*"
   :get-add-files (lambda (paths) paths)
   :pr-title "lololol"
   :pr-body "hahahahah"))

' ; don't call at top level because things aren't usually configured
(sync-test-thrice)

(defmacro abstracted-git-transformer
    ;; XXX FIXME pretty sure this doesn't actually need to be a macro?
    ;; because all the input arguments are strings thunks, or functions?
    ;; XXX FALSE, we need it to be a macro because we need to assign them
    ;; XXX DOUBLY FALSE, have the defsync function return the function
    ;; to call DUH
    (defrepo-name
     old-branch
     new-branch
     ; TODO optional source branch
     command-thunk ; or funciton name or function or something
     diff-pattern
     get-add-files
     pr-title
     pr-body)
  "Transform DEFREPO-NAME by calling COMMAND-THUNK on it.

This macro should be used inside function definitions not at the top level.

The repo starts checked out on `magit-main-branch' and if there
are any changes to files matching DIFF-PATTERN then a commit is
created with PR-TITLE and PR-BODY that includes all files
returned by the function GET-ADD-FILES and made to a NEW-BRANCH
if an existing open automated pull request does not exist. If
there is an existing automated pull request, then that branch
is used as both the parent and source branch.

If the --push-pr option aka `prm--push-pr' is non-nil then a pull
request is created against the upstream master branch.

Function signatures:
command-thunk () -> any
get-add-files (list string ...) -> (list string ...)"
  `(with-repo ,defrepo-name
     (let (done)
       ;; FIXME this can hang forever somehow ??!?
       ;; calling `ow-run-command' beforehand is what triggers the issue !??!? WHAT THE FOO !??!?!
       ;; XXX new variant requires this to be run and then funcall with ow-run-command below fill freeze
       ;(url-copy-file "http://example.org" "/tmp/sigh-wat-sigh.html" t) ;
       ;; XXX it is url-copy-file or something, in concert with one of the counless fooing things
       ;; inside `forge-get-repository' which could be some network call or the sqlite poop who the foo knows
       (forge--pull repo nil (lambda () (setq done t)))
       ;;(message "%S" repo)
       (while (not done) (sleep-for 0.01)
              ;;(message "%S\n%S" (process-list) (all-threads))
              ))
     (let* ((pr-already-exists (get-latest-automated-pr-branch repo))
            (parent-branch (or pr-already-exists ,old-branch (magit-main-branch))))
       ;; (message "parent-branch: %s" parent-branch)
       (if just-cloned
           (when pr-already-exists
             (magit-run-git "checkout" parent-branch))
         (message "stash and pull ..." )
         (git-stash-checkout-pull parent-branch))
       ;; we have to reset to the upstream remote tracking branch otherwise
       ;; local commits when running the pretend workflow will persist
       (git-reset-to-upstream)
       (message "running command ...\n    %s" ,(string-replace "\n" "\n    " (pp-to-string command-thunk)))
       (funcall ,command-thunk)
       (message "never")
       (let* ((files-changed (git-diff ,diff-pattern))
              (files-to-add (funcall ,get-add-files files-changed)))
         (message ":changed %s :add %s" files-changed files-to-add)
         (when files-to-add
           (message "adding ...\n    %s" (string-join files-to-add "\n    "))
           (unless pr-already-exists
             (git-checkout-create ,new-branch)) ; checkout first so failsafe on branch
           (git-add files-to-add)
           (git-commit (concat ,pr-title "\n\n" ,pr-body)) ; FIXME errors need to be fatal here?
           (message "%s\n%s\n%s" sepstr (git-log-p-n-1) sepstr)
           (unless pr-already-exists
             (message "checking for api token ...")
             (ghub--token (oref repo apihost) (ghub--username repo) 'forge))
           (message "token found ...")
           (when prm--push-pr
             (message "pushing ...")
             (git-push)
             (unless pr-already-exists
               (message "creating pull request ...")
               (github-create-pull-request ,pr-title ,pr-body nil (magit-get-upstream-branch parent-branch))
               ;; TODO print link to pull request so if run by human they can click
               )
             ;; note that we only run checkout back to master when
             ;; running for real when in pretend mode 99% of the time
             ;; the user will want the repo on the new branch
             (message "checking out %s branch ..." (magit-main-branch))
             ;; TODO might need to stash again?
             (magit-run-git "checkout" (magit-main-branch))))))))

;;; syncs -- sync specifications

(defun sync-test ()
  ":test"
  (let ((new-file (concat current-build-id ".org")))
    (abstracted-git-transformer
     test-github-api
     nil
     (concat "automated-" current-build-id)
     (lambda ()
       (with-current-buffer (find-file-noselect new-file)
         (insert "* A new heading\n")
         (save-buffer)
         (kill-buffer)))
     "."
     (lambda (paths) (list new-file))
     (concat "test changes for " current-build-id)
     "deposit some test changes")))

;;; TODO
;; workflow needs to check for existing opened pull requests and select that branch
;; and add a commit to that branch rather than opening yet another new branch

;;; entrypoint

(defun main ()
  (setup-reval)
  (ow-cli-gen
      (((:current-build-id (make-build-id))) ; set `current-build-id' from cli if needed
       ((:push-pr) prm--push-pr)
       (:resume) ; start from results of a pretend run ; TODO implement this
       ;; paths
       ((:build-dir prm-build-dir) prm-build-dir)
       ((:user-emacs-directory nil) cli-ued)
       ((:user-init-file nil) cli-uif)
       ((:auth-source nil) auth-source) ; pass an additional path to add to `auth-sources'
       ((:secrets oa-secrets) oa-secrets)
       ;; debug
       (:debug)
       (:debug-on-quit)
       ;; commands -- sync command options
       ;;(sync)
       ;;(apinat)
       ;;(nif) (npo) (scr) (sct) (slim) ; XXX fun but incompatible with arbitrary sync names
       ;;(test)

       ;; dynamic sync
       ((:specs prm-sync-specs) prm-sync-specs) ; path to elisp file with sync defs
       ((:fun nil) cli-current-sync))
    (let* ((user-emacs-directory (or cli-ued (expand-file-name ".emacs.d/" prm-build-dir)))
           (user-init-file (or cli-uif (expand-file-name "init.el" user-emacs-directory)))
           ;; XXX `package-user-dir' MUST be manually reset because ~/.emacs.d/ sticks
           (package-user-dir (expand-file-name "elpa" user-emacs-directory)))
      (setup)
      (let ((auth-sources (or (and auth-source (cons auth-source auth-sources)) auth-sources))
            ;; have to disable save buffers or for some reason even if
            ;; there is nothing to save we hit quit inside
            ;; `map-y-or-n-p', `inhibit-quit' will cause an infinite
            ;; hang for reasons I do not entirely understand this is
            ;; safe because we know that there are no modified buffers
            (magit--disable-save-buffers t)
            current-sync)
        (unless (file-directory-p prm-build-dir)
          (make-directory prm-build-dir t))
        (when debug
          (setq debug-on-error t)
          (message "debug info:\n    :secrets     %s\n    :auth-source %s\n    :push-pr     %s\n    :sync-specs  %s\n    :sync-fun    sync-%s"
                                   oa-secrets         (car auth-sources)   prm--push-pr          prm-sync-specs    cli-current-sync))
        (when (or (and prm-sync-specs cli-current-sync) (string= cli-current-sync "test"))
          (let ((fun (intern (concat "sync-" cli-current-sync))))
            (load prm-sync-specs)
            (unless (fboundp fun)
              (error "No sync function named: %s" fun))
            (setq current-sync (intern cli-current-sync))
            (funcall fun)))
        ' ; old
        (when sync ; FIXME mutex with `cli-current-sync'
          (message "starting sync with build-id: %s" current-build-id)
          (condition-case err
              (cond
               (test      (setq current-sync 'test)      (sync-test))
               (apinat    (setq current-sync 'apinat)    (sync-apinat))
               (nif
                (cond
                 (npo     (setq current-sync 'nif-npo)   (sync-nif-npo))
                 (scr     (setq current-sync 'nif-scr)   (sync-nif-scr))
                 (sct     (setq current-sync 'nif-sct)   (sync-nif-sct))
                 (slim    (setq current-sync 'nif-slim)  (sync-nif-slim))))
               (t         (message "no sync selected")))
            (error
             (message "%s: %s" (car err) (cadr err))
             (signal 'sync-failed (cons "sync failed due to" err)))))
        (when current-sync
          (message "%ssync complete for %s %s"
                   (if prm--push-pr "" "pretend ")
                   current-sync current-build-id))))))

(defun macroexpand-main ()
  (save-excursion
    (find-function #'main)
    ;; (beginning-of-defun) ; not needed ff takes us there
    (let* ((fdef (sexp-at-point))
           (expanded (macroexpand-all fdef)))
      ;;(message "%s" fdef)
      (let (print-level print-length)
        (message "%s" (pp-to-string expanded)))
      expanded)))

(when noninteractive
  (cl-letf (((symbol-function #'ow-run-command) #'ow-run-command-no-thread))
    (main)))

;;; prrequaestor.el ends here
