;;; git-groups.el --- take actions on a group of git working copies
;; Copyright (C) 2014+ Eike Kettner
;;
;; Author: Eike Kettner <eike.kettner at posteo dot de>
;; Version: 0.1.0
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;; Commentary:
;; ** motivation
;;
;; My use case is that I have several private git repositories that I
;; use to synchronise things between different machines. When I start
;; working in the morning I usually want all those working copies to
;; be updated to the latest state. Later, before I go, I usually want
;; to push several working copies to its remotes. Since I use emacs
;; more often than not, I found it convenient to have a key binding
;; for those tasks. This is what this file provides: two functions
;;
;; 1) ~git-groups/check-unclean~ goes through all configured working
;;    copies and checks if there are changes. If so, it executes a
;;    configured action for this working copy.
;; 2) ~git-groups/pull-all~ Runs ~git pull~ on all configured working
;;    copies, collecting the output in a separate buffer. Since I also
;;    use orgmode a lot, the output is formatted to orgmode syntax,
;;    i.e. a failed git command is marked with a TODO.
;;
;; ** installation
;;
;; Put this file somewhere emacs can find it and require the feature
;; via:
;;
;; #+begin_src emacs-lisp :tangle yes
;;   (require 'git-groups)
;; #+end_src
;;
;; You may want to add key bindings for the two main functions:
;;
;; #+begin_src emacs-lisp :tangle yes
;;   (global-set-key "\C-cr" 'git-groups/check-unclean)
;;   (global-set-key "\C-cu" 'git-groups/pull-all)
;; #+end_src
;;
;; ** configuration
;;
;; You must provide a list of the working copies that should be
;; inspected. This is done by setting the variable
;; ~git-groups/repositories~ which is a bit complicated. It is an a-list
;; where the car is a string denoting the full path to the working
;; directory. The cdr is again an a-list with key-value pairs defining
;; the settings for this working copy. An example:
;;
;; #+begin_src emacs-lisp :tangle yes
;;   (setq git-groups/repositories
;;         '(("/home/eike/shelf" . ((:unclean-action . git-groups/action-magit-status)))
;;           ("/home/eike/org" . ((:unclean-action . git-groups/action-git-pushc)
;;                                (:stash-on-pull . t)))))
;; #+end_src
;;
;; Non-existent directories are removed automatically, so no error is
;; issued. This maybe useful when there are different machines with
;; different repositories. The property ~:unclean-action~ is a
;; function that is called on each unclean working copy in
;; ~git-groups/check-unclean~. It takes a config element as parameter and
;; does whatever necessary. It's return value is dropped. There are
;; the following ones predefined:
;;
;; 1) ~git-groups/action-magit-status~ runs ~magit-status~ on the working
;;    copy so that you can take any actions manually. You can move to
;;    the next working copy with ~C-c n~.
;; 2) ~git-groups/action-git-commit~ auto-commits all current changes.
;; 3) ~git-groups/action-git-commitc~ the same as 2) but asks the user if
;;    it should proceed.
;; 4) ~git-groups/action-git-push~ auto-commits and pushes all current
;;    changes.
;; 5) ~git-groups/action-git-pushc~ the same as 4) but asks the user if it
;;    should proceed.
;;
;; You can customize the git commands by setting appropriate
;; properties for a working copy:
;;
;; - ~git-commit-cmd~ the git command used for committing changes. It
;;   is set to ~git commit -am some_message~ by default.
;; - ~git-push-cmd~ the git command used for pushing. It is ~git push~
;;   by default.
;; - ~git-pull-cmd~ the git command used when pulling changes. It is
;;   ~git pull~ by default.
;;
;; Other properties include:
;;
;; - ~stash-on-pull~ whether ~git stash~ should be used when pulling
;;   changes into an unclean working copy. By default it is ~nil~.
;; - ~skip-pull~ whether to skip this working copy in
;;   ~git-groups/pull-all~.
;;
;; There is a function that can be added to the "emacs-kill-hook" that
;; prevents quitting emacs and runs ~git-groups/check-unclean~. If there
;; are no unclean working copies it won't prevent emacs from being
;; shut down. Also, it only does this exactly once. Thus if you close
;; emacs again, it will not run a second time. You can add the hook
;; like this:
;;
;; #+begin_src emacs-lisp :tangle yes
;; (add-hook 'kill-emacs-query-functions 'git-groups/check-unclean-kill-hook)
;; #+end_src
;;

(defvar git-groups/repositories nil
  "An a-list of directories and a set of settings also an a-list.")

(defvar git-groups/git-commit-cmd "git commit -am \"git-groups: autocommit\""
  "The shell command used to commit a working copy.")
(defvar git-groups/git-push-cmd "git push"
  "The shell command used to push a working copy.")
(defvar git-groups/git-pull-cmd "git pull"
  "The shell command used to pull from a remote.")


;; ---* configuration

(defun git-groups/configel-p (el)
  (and (listp el)
       (stringp (car el))
       (listp (cdr el))))

(defun git-groups/configlist-p (cfg)
  (and (listp cfg)
       (git-groups/configel-p (car cfg))))

(defun git-groups/config-get (el name &optional default)
  (when (not (git-groups/configel-p el))
    (error "Not a git-groups/config element"))
  (let ((vals (cdr el)))
    (or (cdr (assoc name vals)) default)))

(defun git-groups/config-getdir (el)
  (when (not (git-groups/configel-p el))
    (error "Not a git-groups/config element"))
  (car el))

(defun git-groups/-config-filter (funp cfg)
  (when (not (git-groups/configlist-p cfg))
    (error "Not a git-groups/config list"))
  (remq nil
        (mapcar
         (lambda (x) (if (funcall funp x) x nil))
         cfg)))

(defun git-groups/config-exists (cfg)
  "Filters out all config elements where the directory does not
exist."
  (git-groups/-config-filter
   (lambda (el) (file-exists-p (car el)))
   cfg))


(defun git-groups/-is-unclean-p (dir)
  "Runs git status via shell command and returns `t' if the given
directory is an unclean working copy. Otherwise returns `nil'."
  (condition-case nil
      (progn (git-groups/git-exec dir "git status --porcelain | grep -e \"[MADRCU]\"") t)
    (error nil)))

(defun git-groups/config-unclean (cfg)
  "Filters out all config elements where the directory does not
exist or is a clean git repository. This function returns a new
configuration consisting only of directories that are unclean git
working copies."
  (git-groups/-config-filter
   (lambda (el)
     (and (file-exists-p (expand-file-name ".git" (car el)))
          (git-groups/-is-unclean-p (car el))))
   (git-groups/config-exists cfg)))


(defun git-groups/config-to-pull (cfg)
  "Returns a new config list only with elements that are
  considered to run git pull on."
  (git-groups/-config-filter
   (lambda (el)
     (not (git-groups/config-get el :skip-pull)))
   (git-groups/config-exists cfg)))


;; ---* git commands

(defun git-groups/git-exec (repo cmd &optional verbose)
  "Executes the given git subcommand CMD inside the working
directory REPO. If VERBOSE is non-nil, the complete output (error
and standard) is returned as string - regardless if the command
succeeded or not and the exit code as a cons cell '(rc .
string)'. If VERBOSE is nil, then an error is signalled if the
command fails (non-zero exit code), otherwise a message string is
returned."
  (let* ((dir (if (stringp repo) repo (car repo)))
         (gitcmd (format (concat "cd %s && " cmd) dir)))
    (with-temp-buffer
      (let ((rc (shell-command gitcmd (current-buffer) (current-buffer))))
        (if verbose
            (cons rc (buffer-string))
          (if (eq 0 rc)
              "Git command done."
            (error (concat "Git command failed: " cmd " on " repo))))))))

(defun git-groups/cfg-exec (cfg commandf &optional appd around-hook)
  "Executes a git command on all elements of CFG. The config list
is iterated and COMMANDF is applied to each element to obtain the
git command to execute. COMMANDF can be a string denoting the git
command directly.

Optional AROUND-HOOK can be specified that is invoked before and
after the git command is executed. The arguments are either
`:pre' or `:post' (depending on the position) and the current
config element.

The output is collected in a separate buffer with name
*chg-exec*. If APPD is nil, the buffer is cleared and setup with
a short message. If APPD is truthy it is simply reused."
  (when (not (git-groups/configlist-p cfg))
    (error "Not a git-groups/config list"))
  (when (not (null cfg))
    (let ((outb (get-buffer-create "*chg-exec*")))
      (set-buffer outb)
      (when (not appd)
        (erase-buffer)
        (when (featurep 'org)
          (org-mode))
        (insert (format "* Executing at %s\n" (current-time-string))))
      (dolist (el cfg)
        (let ((cmd (if (stringp commandf) commandf
                     (if (functionp commandf)
                         (funcall commandf el)
                       (error "Commandf not a function or string"))))
              (dir (git-groups/config-getdir el)))
          (when (not (null cmd))
            (when (not appd)
              (insert (format "** working copy ~%s~\n" dir)))
            (when (functionp around-hook)
              (funcall around-hook :pre el))
            (let* ((result (git-groups/git-exec dir cmd t))
                   (success (eq 0 (car result))))
              (insert (format "*** ~%s~ on ~%s~\n " cmd dir))
              (when (and (featurep 'org) (not success))
                ;; if not successful mark parent headings with TODO
                ;; but not the first heading
                (save-excursion
                  (org-todo "TODO")
                  (while (not (eq 1 (or (org-up-heading-safe) 1)))
                    (org-todo "TODO"))))
              (insert (cdr result)))
            (insert "\n")
            (when (functionp around-hook)
              (funcall around-hook :post el)))))
      (when (not appd)
        (message "All actions performed. See buffer *chg-exec* for details.")))))


;; ---* actions

(defun git-groups/action-magit-status (el)
  "Runs `magit-status' on the given working directory."
  (when (eq major-mode 'magit-status-mode)
    (magit-mode-quit-window))
  (magit-status (git-groups/config-getdir el)))

;; this property indicates that this action waits for user input
(put 'git-groups/action-magit-status 'interactive t)

(defun git-groups/action-git-commit (el)
  "Runs git commit via shell command interface on the given
working directory. There is a default commit message made up."
  (let ((dir (git-groups/config-getdir el))
        (cmd (git-groups/config-get el :git-commit-cmd git-groups/git-commit-cmd)))
  (message "Commiting directory %s" dir)
  (git-groups/git-exec dir cmd)))

(defun git-groups/action-git-commitc (el)
  "Same as `git-groups/action-git-commit' but asks the user before."
  (when (yes-or-no-p (format "Commit %s? " (git-groups/config-getdir el)))
    (git-groups/action-git-commit el)))

(defun git-groups/action-git-push (el)
  "Runs git commit and git push via shell command."
  (let ((dir (git-groups/config-getdir el))
        (cmd (git-groups/config-get el :git-push-cmd git-groups/git-push-cmd)))
    (git-groups/action-git-commit el)
    (git-groups/git-exec dir cmd)))

(defun git-groups/action-git-pushc (el)
  "Same as `git-groups/action-git-push' but asks the user before."
  (when (yes-or-no-p (format "Push %s? " (git-groups/config-getdir el)))
    (git-groups/action-git-push el)))

(defun git-groups/action-none (&rest r)
  "A noop action. This maybe usful if you only want to pull a
  repository."
  t)


;; ---* execution

(defvar git-groups/-all-unclean nil
  "Internal variable used to store remaining unclean repos.")

(defun git-groups/-do-next-action ()
  "Executes the next action from the list. Returns the action
just performed, or `nil' if nothing is left to do."
  (when (not (null git-groups/-all-unclean))
    (let* ((el (car git-groups/-all-unclean))
           (act (git-groups/config-get el :unclean-action 'git-groups/action-none)))
      (setq git-groups/-all-unclean (cdr git-groups/-all-unclean))
      (funcall act el)
      act)))

(defun git-groups/run-actions ()
  "Runs the actions against unclean working directories. It calls
`git-groups/-do-next-action' until it is exhausted (returns nil). It
returns `t' if there is no working directory left, otherwise
nil."
  (interactive)
  (let ((act (git-groups/-do-next-action)))
    (if (null act)
        (progn
          (git-groups/action-mode 0)
          (message "Visiting unclean repos done.") t)
      (if (not (get act 'interactive))
          (git-groups/run-actions)
        (git-groups/action-mode 1)
        nil))))


;; --- public api

(defun git-groups/check-unclean ()
  "Finds all unclean working copies by traversing the list at
`git-groups/repositories'. For each one, its defined
`:unclean-action' is executed."
  (interactive)
  (setq git-groups/-all-unclean (git-groups/config-unclean git-groups/repositories))
  (git-groups/run-actions))

(defun git-groups/check-unclean-kill-hook ()
  "A hook for `kill-emacs-query-functions' that will stop killing
emacs and remind for unclean working copies. If all configured
working copies are clean, emacs is killed. It will only prevent
killing emacs exactly once! Thus, if you don't care, simply kill
emacs again."
  (cond
   ((null git-groups/repositories) t)
   ((get 'git-groups/check-unclean-kill-hook 'kill-hook-run) t)
   (:else
    (put 'git-groups/check-unclean-kill-hook 'kill-hook-run t)
    (git-groups/check-unclean))))

(defun git-groups/pull-all ()
  "Loops through all configured repositories and runs git pull via
shell command."
  (interactive)
  (let ((stashed '()))
    (git-groups/cfg-exec (git-groups/config-to-pull git-groups/repositories)
                     (lambda (el)
                       (git-groups/config-get el :git-pull-cmd git-groups/git-pull-cmd))
                     nil
                     (lambda (state el)
                       (when (and (eq state :pre)
                                  (git-groups/config-get el :stash-on-pull)
                                  (git-groups/-is-unclean-p (git-groups/config-getdir el)))
                         (git-groups/cfg-exec (list el) "git stash" t)
                         (setq stashed (cons el stashed)))
                       (when (and (eq state :post) (member el stashed))
                         (git-groups/cfg-exec (list el) "git stash pop" t))))))

(setq git-groups/keymap (make-sparse-keymap))
(define-key git-groups/keymap "\C-cn" 'git-groups/run-actions)

(define-minor-mode git-groups/action-mode
  "Enabled when iterating through unclean working copies."
  :lighter " ☃"
  :init-value nil
  :keymap git-groups/keymap)

(provide 'git-groups)

;;; git-groups.el ends here
