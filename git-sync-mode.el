;;; git-sync-mode.el --- Automatically sync your git-repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords: vc, convenience
;; Version: 0.1.2
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
  "Customizations for git-sync")

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

(defun git-sync--commit-message ()
  (format "changes from %s on %s" (system-name) (current-time-string)))

(defun git-sync--sentinel-fn (process event)
  "Colourizes the git-sync log buffer for `PROCESS' on `EVENT'."
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (let ((inhibit-read-only t))
        (with-current-buffer buf
          (ansi-color-apply-on-region (point-min) (point-max))
          (goto-char (point-max))
          (unless (derived-mode-p 'special-mode)
            (special-mode)))))))

(defun git-sync--execute-command (command dir &optional ignore-error)
  "Execute `COMMAND' as a promise in the git-sync buffer.

If `DIR' is provided, set `default-directory' to it for the command.

If `IGNORE-ERROR' is non-nil, resolve even if the command fails.

On success the promise returns the process-status for the command
otherwise it rejects with the process event."
  (promise-new (lambda (resolve reject)
                 (let ((default-directory dir)
                       (last-output)
                       (sentinel-fn (lambda (process event)
                                      (git-sync--sentinel-fn process event)
                                      (when (memq (process-status process) '(exit signal))
                                        (if (or ignore-error
                                                (zerop (process-exit-status process)))
                                            (funcall resolve last-output)
                                          (funcall reject (format "Command failed: %s" event))))))
                       (filter-fn (lambda (process string)
                                    (when (buffer-live-p (process-buffer process))
                                      (with-current-buffer (process-buffer process)
                                        (let ((moving (= (point) (process-mark process)))
                                              (inhibit-read-only 't))
                                          (save-excursion
                                            ;; Insert the text, advancing the process marker.
                                            (goto-char (process-mark process))
                                            (insert string)
                                            (set-marker (process-mark process) (point)))
                                          (if moving (goto-char (process-mark process)))
                                          (setq last-output string)))))))
                   (make-process :name "git-sync"
                                 :buffer (get-buffer-create (format "*git-sync:%s*" default-directory))
                                 :filter filter-fn
                                 :command command
                                 :sentinel sentinel-fn)))))

;; Guard functions
(async-defun git-sync--has-changes-p (dir)
  "Return non-nil if there are staged changes in `DIR'."
  (or (length>
       (await (git-sync--execute-command '("git" "diff" "--cached" "--name-only") dir))
       0)
      (length>
       (await (git-sync--execute-command '("git" "diff" "--name-only") dir))
       0)))

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

(defun git-sync--commit-command (message)
  "Return the git commit command with `MESSAGE'."
  (nconc (list "git"
               "commit"
               "-m"
               (funcall git-sync-generate-message))
         (when git-sync-skip-verify
           "--no-verify")))

(async-defun git-sync--validate-and-run ()
  "Validate the git repository state and run git-sync."
  (let* ((dir default-directory))
    (cond
     ((git-sync--is-locked-p dir)
      (message "git-sync: repository is locked, skipping sync"))
     ((not (string= (git-sync--repo-state dir) "NORMAL"))
      (message "git-sync: repository is in a special state (%s), skipping sync"
               (git-sync--repo-state dir)))
     ((not (await (git-sync--has-changes-p dir)))
      (message "git-sync: no changes to commit, skipping sync"))
     (t
      (await (git-sync--execute))))))

(async-defun git-sync--execute (dir)
  (condition-case err
      (progn
        (await (git-sync--execute-command (git-sync--add-command) dir)) 'exit
        (await (git-sync--execute-command (git-sync--commit-command) dir))
        (await (git-sync--execute-command '("git" "pull") dir))
        (await (git-sync--execute-command '("git" "push") dir))
        (message "git-sync complete"))
    (error (message "git-sync failed: %s" err))))
(defun git-sync--allowed-directory (current-file)
  "Return non-nil if CURRENT-FILE is in the allow list."
  (cl-reduce (lambda (any-p allowed-dir)
               (or any-p
                   (not (minibufferp))
                   (string-prefix-p allowed-dir current-file)))
             git-sync-allow-list
             :initial-value nil))

(defun git-sync--maybe ()
  "Call `git-sync--allowed-directory' to determine if git-sync is allowed to be enabled for this buffer."
  (when (git-sync--allowed-directory (buffer-file-name))
    (git-sync-mode)))

(defun git-sync--after-save ()
  "Run git-sync on-save."
  (git-sync--validate-and-run default-directory))

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
    (await (git-sync--validate-and-run default-directory))
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
