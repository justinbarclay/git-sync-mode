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

(defgroup git-sync
  nil
  "Customizations for git-sync")

(defcustom git-sync-allow-list '()
  "A list of files that git-sync is allowed to run in. In case of conflict with the deny-list, the deny-list wins out."
  :type '(repeat directory)
  :group 'git-sync)

(defcustom git-sync-generate-message #'git-sync--commit-message
  "A function that generates the commit message for git-sync."
  :type '(function)
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
  (await (git-sync--execute-command (list "git" "commit" "-m" (git-sync-generate-message))))
  (await (git-sync--execute-command '("git" "pull")))
  (await (git-sync--execute-command '("git" "push")))
  (message "git-sync complete"))

(defun git-sync--allowed-directory (current-file)
  "Return non-nil if CURRENT-FILE is in the allow list."
  (cl-reduce (lambda (any-p allowed-dir)
               (or any-p
                   (string-prefix-p allowed-dir current-file)))
             git-sync-allow-list
             :initial-value nil))


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
