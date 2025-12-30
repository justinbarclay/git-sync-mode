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

;; This package provides `git-sync-mode`, a minor mode that
;; automatically and asynchronously synchronizes your local Git
;; repository with its upstream remote each time you save a file.
;;
;; The synchronization workflow is as follows:
;; 1. Stage and commit any local changes.
;; 2. Fetch the latest updates from the remote.
;; 3. Analyze the state relative to the upstream branch.
;; 4. Automatically push, fast-forward, or rebase to fully sync.
;;
;; To ensure safety, `git-sync-mode` will only activate in repositories
;; whose paths are included in the `git-sync-allow-list`. It also includes
;; safeguards to prevent execution if the repository is in a special state
;; (e.g., during a merge or rebase) or if it is locked.

;;; Code:

(require 'cl-lib)
(require 'async-await)
(require 'ansi-color)
(require 'subr-x)

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

(defvar-local git-sync-modeline-string " git-sync"
  "String to display in the mode line.")

(defcustom git-sync-state-change-hook nil
  "Hook run after `git-sync-state' changes."
  :type 'hook
  :group 'git-sync)

(defvar git-sync--locks '()
  "A plist tracking locks for git-sync per directory.")

(defun git-sync--nerd-icons-icon (state)
  "Return nerd-icon for STATE."
  (pcase state
    (:synced (nerd-icons-octicon "nf-oct-check" :face 'success :height 0.8))
    (:failed (nerd-icons-octicon "nf-oct-alert" :face 'error :height 0.8))
    (:locked (nerd-icons-octicon "nf-oct-lock" :face 'warning :height 0.8))
    (:special-state (nerd-icons-octicon "nf-oct-git_merge" :face 'warning :height 0.8))
    ((or :starting :committing :fetching :fast-forwarding)
     (nerd-icons-faicon "nf-fa-down_left_and_up_right_to_center" :face 'nerd-icons-lyellow :height 0.8))
    (_ "")))

(defun git-sync--all-the-icons-icon (state)
  "Return all-the-icons icon for STATE."
  (pcase state
    (:synced (all-the-icons-octicon "check" :face 'success))
    (:failed (all-the-icons-octicon "alert" :face 'error))
    (:locked (all-the-icons-octicon "lock" :face 'warning))
    (:special-state (all-the-icons-octicon "git-merge" :face 'warning))
    ((or :starting :committing :fetching :fast-forwarding)
     (all-the-icons-octicon "sync" :face 'all-the-icons-lyellow))
    (_ "")))

(defun git-sync--state-icon (state)
  "Return an icon for STATE if available."
  (cond
   ((and (featurep 'nerd-icons) (fboundp 'nerd-icons-octicon))
    (git-sync--nerd-icons-icon state))
   ((and (featurep 'all-the-icons) (fboundp 'all-the-icons-octicon))
    (git-sync--all-the-icons-icon state))
   (t nil)))

(defun git-sync--set-state (new-state &optional buffer)
  "Set `git-sync-state' to NEW-STATE and run `git-sync-state-change-hook'.
If BUFFER is non-nil, set the state in that buffer."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq git-sync-state new-state)
        (git-sync--update-mode-line new-state)
        (run-hooks 'git-sync-state-change-hook)))))

(put 'git-sync-modeline-string 'risky-local-variable t)
(defun git-sync--update-mode-line (state)
  "Update `git-sync-modeline-string` for STATE and refresh mode line."
  (let ((icon (git-sync--state-icon state)))
    (setq git-sync-modeline-string
          (if icon
              (list " git-sync:"icon"")
            (if (or (null state) (eq state :synced))
                " git-sync"
              (format " git-sync[%s]" (substring (symbol-name state) 1)))))
    (force-mode-line-update)))

(defun git-sync--commit-message ()
  (format "changes from %s on %s" (system-name) (current-time-string)))

(defun git-sync--process-buffer (process _event)
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
        (process-put process 'git-sync-output string)))))

(defun git-sync--process-sentinel (process event)
  "Sentinel function for git-sync."
  (git-sync--process-buffer process event)
  (with-current-buffer (process-buffer process)
    (when (memq (process-status process) '(exit signal))
      (let ((resolve (process-get process 'git-sync-resolve))
            (reject (process-get process 'git-sync-reject))
            (ignore-error (process-get process 'git-sync-ignore-error)))
        (if (or ignore-error
                (zerop (process-exit-status process)))
            (funcall resolve (string-trim (or (process-get process 'git-sync-output)
                                              "")))
          (funcall reject (format "Command failed: %s" event)))))))

(defun git-sync--execute-command (command dir &optional ignore-error)
  "Execute `COMMAND' as a promise in the git-sync buffer.

If `DIR' is provided, set `default-directory' to it for the command.

If `IGNORE-ERROR' is non-nil, resolve even if the command fails.

On success the promise returns the process-status for the command
otherwise it rejects with the process event."
  ;; Turn off pager
  (make-local-variable 'process-environment)
  (let  ((process-environment (cons "GIT_PAGER=cat" process-environment)))
    (promise-new (lambda (resolve reject)
                   (let* ((default-directory dir)
                          (process (make-process :name "git-sync"
                                                 :buffer (get-buffer-create (format "*git-sync:%s*" default-directory))
                                                 :filter #'git-sync--process-filter
                                                 :command command
                                                 :sentinel #'git-sync--process-sentinel)))
                     (process-put process 'git-sync-resolve resolve)
                     (process-put process 'git-sync-reject reject)
                     (process-put process 'git-sync-ignore-error ignore-error))))))

;;;;;;;;;;
;; Git State
;;;;;;;;;;

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

;;;;;;;;;;
;; Guards
;;;;;;;;;;

(async-defun git-sync--has-changes-p (dir)
  "Return non-nil if git detected changes in `DIR'."
  (let ((status (thread-first
                  '("git" "status" "--porcelain")
                  (git-sync--execute-command dir)
                  (await)
                  (string-trim)
                  (string-split "\n"))))
    (any
     (lambda (line)
       (length> line 0))
     status)))

(defun git-sync--index-locked-p (dir)
  "Return non-nil if a .git/index.lock file exists in the repository root of `DIR'."
  (let* ((root (locate-dominating-file dir ".git"))
         (lock-file (and root (expand-file-name ".git/index.lock" root))))
    (and lock-file (file-exists-p lock-file))))

;;;;;;;;;;
;; Commands
;;;;;;;;;;

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
;;;;;;;;;;
;; Sync
;;;;;;;;;;

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

;; We need to lock based on root dir because Emacs can open up
;; multiple files at once in different buffers, and each buffer would
;; try to run git-sync independently. For example, this can happen
;; when opening up org-agenda and your agenda is made up of multiple
;; files from the same repo.
(defun git-sync--lock (dir)
  "Test if the mutex for `DIR' is set, if not set it.

Returns t if the mutex was successfully set, nil otherwise."
  (if (plist-get git-sync--locks
                 dir
                 #'equal)
      nil
    (progn
      (setq git-sync--locks (plist-put git-sync--locks dir 't #'equal))
      't)))

(defun git-sync--unlock (dir)
  "Unlock the mutex for DIR."
  (setq git-sync--locks (plist-put git-sync--locks dir nil #'equal)))

(async-defun git-sync--validate-and-run ()
  "Validate the git repository state and run git-sync."
  (when-let* ((dir (locate-dominating-file default-directory ".git"))
              (has-lock (and dir
                             ;; Skip running if another git-sync operation is in effect
                             (git-sync--lock dir))))
    (condition-case err
        (progn
          (cond
           ((git-sync--index-locked-p dir)
            (git-sync--set-state :locked)
            (await (promise-resolve nil)))
           ((not (string= (git-sync--repo-state dir) "NORMAL"))
            (git-sync--set-state :special-state)
            (await (promise-resolve nil)))
           (t
            (git-sync--set-state :starting)
            (await (git-sync--execute dir))))
          (git-sync--unlock dir))
      (error
       (git-sync--unlock dir)
       (message "git-sync: Error in validate-and-run: %s" err)))))

(defun git-sync--maybe ()
  "Determine if current buffer is a child of the allowed directory."
  (when (git-sync--allowed-directory (buffer-file-name))
    (git-sync-mode)))

(defun git-sync--after-save ()
  "Run git-sync on-save."
  (git-sync--validate-and-run))

;;;###autoload
(define-minor-mode git-sync-mode
  "Commit, save and push your changes on-save."
  :lighter git-sync-modeline-string
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
