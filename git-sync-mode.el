;;; git-sync-mode.el --- Sync your git repositories on save  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords: vc, convenience
;; Version: 0.5.0
;; Homepage: https://github.com/justinbarclay/git-sync-mode
;; Package-Requires: ((emacs "29.1") (async-await))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically sync your git-repos

;;; Code:

(require 'cl-lib)
(require 'async-await)
(require 'ansi-color)

(defgroup git-sync
  nil
  "Customizations for git-sync"
  :group 'vc)

(defcustom git-sync-allow-list '()
  "A list of directories or files that git-sync is allowed to run in.

If any of the directories of files are a prefix of the current-file
git-sync-mode will be enabled."
  :type '(repeat directory)
  :group 'git-sync)

(defcustom git-sync-generate-message #'git-sync--commit-message
  "A function that generates the commit message for git-sync."
  :type '(function)
  :group 'git-sync)

(defcustom git-sync-add-new-files t
  "If non-nil, git-sync will add new files to the repository."
  :type 'boolean
  :group 'git-sync)

(defcustom git-sync-skip-verify nil
  "If non-nil, git-sync will skip pre-commit and commit-msg hooks."
  :type 'boolean
  :group 'git-sync)

(defvar-local git-sync--last-output nil
  "Holds the last output from git-sync process.")

(defvar-local git-sync-state nil
  "Current state of git-sync.

Possible values:
- :starting        - Sync started
- :committing      - Committing local changes
- :fetching        - Fetching from remote
- :fast-forwarding - Fast-forwarding local branch
- :synced          - Sync complete
- :failed          - Sync failed (check *git-sync:<dir>* buffer)
- :locked          - Repository locked
- :special-state   - Repository in special state (rebase/merge)")

(defcustom git-sync-state-change-hook nil
  "Hook run after `git-sync-state' changes."
  :type 'hook
  :group 'git-sync)

(defun git-sync--set-state (new-state &optional buffer)
  "Set `git-sync-state' to NEW-STATE and run `git-sync-state-change-hook'.
If BUFFER is non-nil, set the state in that buffer."
  (if (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (setq git-sync-state new-state)
        (run-hooks 'git-sync-state-change-hook))
    (setq git-sync-state new-state)
    (run-hooks 'git-sync-state-change-hook)))

(defun git-sync--commit-message ()
  (format "changes from %s on %s" (system-name) (current-time-string)))

(defun git-sync--sentinel-fn (process _event)
  "Colourizes the git-sync log buffer for `PROCESS' on `EVENT'."
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (let ((inhibit-read-only t))
        (with-current-buffer buf
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-max))
          (unless (derived-mode-p 'special-mode)
            (special-mode)))))))

(defun git-sync--process-filter (process string)
  "Filter function for git-sync."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process)))
            (inhibit-read-only 't))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert string)
          (set-marker (process-mark process) (point)))
        (when moving
          (goto-char (process-mark process)))
        (setq-local git-sync--last-output string)))))

(defun git-sync--process-sentinel (process event)
  "Sentinel function for git-sync."
  (git-sync--sentinel-fn process event)
  (with-current-buffer (process-buffer process)
    (when (memq (process-status process) '(exit signal))
      (let ((resolve (process-get process 'git-sync-resolve))
            (reject (process-get process 'git-sync-reject))
            (ignore-error (process-get process 'git-sync-ignore-error)))
        (if (or ignore-error
                (zerop (process-exit-status process)))
            (funcall resolve (string-trim (or git-sync--last-output "")))
          (funcall reject (format "Command failed: %s" event)))))))

(defun git-sync--execute-command (command dir &optional ignore-error)
  "Execute `COMMAND' as a promise in the git-sync buffer.

If `DIR' is provided, set `default-directory' to it for the command.

If `IGNORE-ERROR' is non-nil, resolve even if the command fails.

On success the promise returns the process-status for the command
otherwise it rejects with the process event."
  ;; Turn off pager
  (make-local-variable 'process-environment)
  (setq process-environment (cons "GIT_PAGER=cat" process-environment))
  (promise-new (lambda (resolve reject)
                 (let* ((default-directory dir)
                        (process (make-process :name "git-sync"
                                               :buffer (get-buffer-create (format "*git-sync:%s*" default-directory))
                                               :filter #'git-sync--process-filter
                                               :command command
                                               :sentinel #'git-sync--process-sentinel)))
                   (process-put process 'git-sync-resolve resolve)
                   (process-put process 'git-sync-reject reject)
                   (process-put process 'git-sync-ignore-error ignore-error)))))

;; State functions
(async-defun git-sync--get-upstream-branch (dir)
  "Get the upstream branch for the current branch in `DIR`.

If no upstream branch is found, return nil."
  (condition-case _err
      (let ((response (await (git-sync--execute-command
                              '("git" "rev-parse" "--abbrev-ref" "@{u}")
                              dir))))
        (string-trim response))
    (error
     (message "git-sync: No upstream branch found. Check *git-sync:%s* buffer." dir)
     nil)))

(async-defun git-sync--get-sync-state (dir upstream)
  "Get the sync state between HEAD and `UPSTREAM` in `DIR`."
  (let* ((output (string-trim
                  (await (git-sync--execute-command
                          (list "git" "rev-list" "--count" "--left-right" (concat upstream "...HEAD"))
                          dir))))
         (parts (split-string output "\t"))
         (behind (string-to-number (car parts)))
         (ahead (string-to-number (cadr parts))))
    (cond
     ((and (= 0 ahead) (= 0 behind)) :equal)
     ((> ahead 0) (if (> behind 0) :diverged :ahead))
     ((> behind 0) :behind)
     (t (error "Could not determine sync state")))))

(defun git-sync--repo-state (dir)
  "Return the current git repository state in `DIR'."
  (let* ((root (locate-dominating-file dir ".git"))
         (git-dir (and root (expand-file-name ".git" root))))
    (cond
     ((and git-dir (file-exists-p (expand-file-name "rebase-merge/interactive" git-dir))) "REBASE-i")
     ((and git-dir (file-exists-p (expand-file-name "rebase-merge" git-dir))) "REBASE-m")
     ((and git-dir (file-exists-p (expand-file-name "rebase-apply" git-dir))) "AM/REBASE")
     ((and git-dir (file-exists-p (expand-file-name "MERGE_HEAD" git-dir))) "MERGING")
     ((and git-dir (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git-dir))) "CHERRY-PICKING")
     ((and git-dir (file-exists-p (expand-file-name "BISECT_LOG" git-dir))) "BISECTING")
     ((and git-dir (file-exists-p (expand-file-name "REVERT_HEAD" git-dir))) "REVERTING")
     (t "NORMAL"))))

;; Guard functions
(async-defun git-sync--has-changes-p (dir)
  "Return non-nil if there are staged changes in `DIR'."
  (or (length>
       (await (git-sync--execute-command '("git" "diff" "--cached" "--name-only") dir))
       0)
      (length>
       (await (git-sync--execute-command '("git" "diff" "--name-only") dir))
       0)))

(defun git-sync--is-locked-p (dir)
  "Return non-nil if a .git/index.lock file exists in the repository root of `DIR'."
  (let* ((root (locate-dominating-file dir ".git"))
         (lock-file (and root (expand-file-name ".git/index.lock" root))))
    (and lock-file (file-exists-p lock-file))))

;; Command generators
(defun git-sync--add-command ()
  "Return the git add command based on `git-sync-add-new-files'."
  (if git-sync-add-new-files
      '("git" "add" "--all" ".")
    '("git" "add" "-u")))

(defun git-sync--commit-command ()
  "Return the git commit command."
  (nconc (list "git"
               "commit"
               "-m"
               (funcall git-sync-generate-message))
         (when git-sync-skip-verify
           '("--no-verify"))))

;; Execute
(async-defun git-sync--execute (dir)
  "Execute the git-sync process in `DIR`.

The git sync process includes:
  1.  Committing local changes
  2.  Validating the existence of an upstream branch
  3.1 Finish if no upstream branch
  3.2 Fetching from remote
  4.  Determining sync state
  5.  Performing necessary actions based on sync state
      (fast-forward, rebase, push)."
  (condition-case err
      (let (upstream)
        (when (await (git-sync--has-changes-p dir))
          (git-sync--set-state :committing)
          (await (git-sync--execute-command (git-sync--add-command) dir))
          (await (git-sync--execute-command (git-sync--commit-command) dir)))

        (setq upstream (await (git-sync--get-upstream-branch dir)))

        (when upstream
          (git-sync--set-state :fetching)
          (await (git-sync--execute-command '("git" "fetch") dir))

          (let ((state (await (git-sync--get-sync-state dir upstream))))
            (pcase state
              (:equal) ;; No action needed

              (:ahead
               (await (git-sync--execute-command '("git" "push") dir)))

              (:behind
               (git-sync--set-state :fast-forwarding)
               ;; Safe guard with --ff-only to avoid unwanted merges.
               ;;
               ;; If the state was misidentified and the branches had
               ;; actually diverged, =--ff-only= would fail,
               ;; preventing an unwanted merge commit.
               (await (git-sync--execute-command '("git" "merge" "--ff-only" "@{u}") dir)))

              (:diverged
               (await (git-sync--execute-command '("git" "rebase" "@{u}") dir))
               (await (git-sync--execute-command '("git" "push") dir))))
            (git-sync--set-state :synced))))
    (error
     (message "git-sync failed. Check *git-sync:%s* buffer." dir)
     (git-sync--set-state :failed))))

(async-defun git-sync--validate-and-run ()
  "Validate the git repository state and run git-sync."
  (let* ((dir default-directory))
    ;; We await here to ensure the async function completes before exiting.
    (cond
     ((git-sync--is-locked-p dir)
      (git-sync--set-state :locked)
      (await (promise-resolve nil)))
     ((not (string= (git-sync--repo-state dir) "NORMAL"))
      (git-sync--set-state :special-state)
      (await (promise-resolve nil)))
     (t
      (git-sync--set-state :starting)
      (await (git-sync--execute dir))))))

(defun git-sync--allowed-directory (current-file)
  "Return non-nil if CURRENT-FILE is in the allow list."
  (and current-file
       (not (minibufferp))
       (cl-reduce (lambda (any-p allowed-dir)
                    (or any-p
                        (string-prefix-p (expand-file-name allowed-dir)
                                         (expand-file-name current-file))))
                  git-sync-allow-list
                  :initial-value nil)))

(defun git-sync--maybe ()
  "Determine if current buffer is apart of allowed directory."
  (when (git-sync--allowed-directory (buffer-file-name))
    (git-sync-mode)))

(defun git-sync--after-save ()
  "Run git-sync on-save."
  (git-sync--validate-and-run))

;;;###autoload
(define-minor-mode git-sync-mode
  "Commit, save and push your changes on-save."
  :lighter " git-sync"
  :group 'git-sync
  (cond
   (git-sync-mode
    (unless (and (executable-find "git")
                 (locate-dominating-file default-directory ".git"))
      (setq git-sync-mode nil)
      (user-error "git-sync-mode: git executable or .git directory not found"))
    ;; Runs asynchronously
    (git-sync--validate-and-run)
    (add-hook 'after-save-hook #'git-sync--after-save nil 'local))
   (t
    (remove-hook 'after-save-hook #'git-sync--after-save 'local))))

;;;###autoload
(define-globalized-minor-mode git-sync-global-mode
  git-sync-mode
  git-sync--maybe
  :group 'git-sync)

(provide 'git-sync-mode)
;;; git-sync-mode.el ends here
