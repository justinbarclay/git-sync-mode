;;; git-sync-mode.el --- Automatically sync your git-repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords: vc, convenience
;; Version: 0.1.1
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

(defgroup git-sync
  nil
  "Customizations for git-sync")

(defcustom git-sync-allow-list '()
  "A list of files that git-sync is allowed to run in. In case of conflict with the deny-list, the deny-list wins out."
  :type '(repeat director)
  :group 'git-sync)

(defcustom git-sync-deny-list
  '()
  "A list of files that git-sync is not allowed to run in. In case of conflict with the allow-list, the deny-list wins out."
  :type '(repeat directory)
  :group 'git-sync)

(defun git-sync--commit-message ()
  (format "changes from %s on %s" (system-name) (current-time-string)))

(defun git-sync--sentinel-fn (process event)
  (with-current-buffer (process-buffer process)
    (special-mode)))

(defun git-sync--execute-command (command)
  "Execute `COMMAND' as a promise in the git-sync buffer.

The promise returns the event passed in by the sentinel functions"
  (promise-new (lambda (resolve reject)
                 (let ((sentinel-fn (lambda (process event)
                                      (git-sync--sentinel-fn process event)
                                      (funcall resolve event))))
                   (make-process :name "git-sync"
                                 :buffer (get-buffer-create "*git-sync*")
                                 :command command
                                 :sentinel sentinel-fn)))))

(async-defun git-sync--execute ()
  (await (git-sync--execute-command '("git" "add" ".")))
  (await (git-sync--execute-command (list "git" "commit" "-m" (git-sync--commit-message))))
  (await (git-sync--execute-command '("git" "pull")))
  (await (git-sync--execute-command '("git" "push")))
  (message "git-sync complete"))

(defun git-sync-add-to-allow-list ()
  "Add directory to the `git-sync-allow-list'."
  (interactive)
  (add-to-list 'git-sync-allow-list (read-directory-name "Directory to add to git-sync-allow-list: ")))

(defun git-sync-add-to-deny-list ()
  "Add directory to the `git-sync-deny-list'."
  (interactive)
  (add-to-list 'git-sync-allow-list (read-directory-name "Directory to add to git-sync-deny-list: ")))

(defun git-sync-remove-from-allow-list ()
  "Remove an item from the `git-sync-allow-list'."
  (setq git-sync-allow-list (remove (completing-read
                                     "Select the item to remove: "
                                     git-sync-allow-list))))

(defun git-sync-remove-from-deny-list ()
  "Remove an item from the `git-sync-deny-list'."
  (setq git-sync-deny-list (remove (completing-read
                                    "Select the item to remove: "
                                    git-sync-deny-list))))

(defun git-sync--allowed-directory (current-file allowed-dirs)
  "Return t if CURRENT-FILE is in one of the ALLOWED-DIRS."
  (cl-reduce (lambda (any-p allowed-dir)
               (or any-p
                   (string-prefix-p allowed-dir current-file)))
             allowed-dirs
             :initial-value nil))

(defun git-sync--global-after-save ()
  "Run git-sync on-save if the current buffer is in a subdirectory of one of the allowed directories."
  (when (git-sync--allowed-directory (buffer-file-name) git-sync-allow-list)
    (git-sync--execute)))

(defun git-sync--after-save ()
  "Run git-sync on-save."
  (git-sync--execute))

;;;###autoload
(define-minor-mode git-sync-global-mode
  "A global minor mode to run git-sync."
  :lighter " git-sync"
  :global 't
  :group 'git-sync
  :after-hook (if git-sync-global-mode
                  (setq-local after-save-hook (cons 'git-sync--global-after-save after-save-hook))
                (setq-local after-save-hook (remove 'git-sync--global-after-save after-save-hook))))

;;;###autoload
(define-minor-mode git-sync-mode
  "Run git-sync on-save."
  :lighter " git-sync"
  :group 'git-sync
  (if git-sync-mode
      (setq-local after-save-hook (cons 'git-sync--after-save after-save-hook))
    (setq-local after-save-hook (remove 'git-sync--after-save after-save-hook))))

(provide 'git-sync-mode)
;;; git-sync-mode.el ends here
