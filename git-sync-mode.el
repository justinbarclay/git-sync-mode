;;; git-sync-mode.el --- Automatically sync your git-repos  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Justin Barclay

;; Author: Justin Barclay <github@justincbarclay.ca>
;; Keywords: vc, convenience
;; Version: 0.1.0
;; Homepage: https://github.com/justinbarclay/git-sync-mode
;; Package-Requires: ((emacs "29.1"))

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
(require 'ansi-color)
(require 'ansi-osc)

(defcustom git-sync-allow-list nil
  "List of directories to sync with git-sync."
  :type '(repeat directory)
  :group 'git-sync
  :safe #'listp)

(defun git-sync--sentinel-fn (process _event)
  "Sentinel function for the git-sync PROCESS."
  (with-current-buffer (process-buffer process)
    (ansi-color-apply-on-region (point-min) (point-max))
    (ansi-osc-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (special-mode)))

(defun git-sync--execute ()
  "Create a buffer and run git-sync."
  (when-let ((buffer (get-buffer "*git-sync*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode))))
  (make-process :name "git-sync"
                :buffer (get-buffer-create "*git-sync*")
                :command '("git-sync" "-n" "-s")
                :sentinel 'git-sync--sentinel-fn))

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

(define-minor-mode git-sync-global-mode
  "A global minor mode to run git-sync."
  :lighter " git-sync"
  :global 't
  :group 'git-sync
  :after-hook (if git-sync-global-mode
                  (setq-local after-save-hook (cons 'git-sync--global-after-save after-save-hook))
                (setq-local after-save-hook (remove 'git-sync--global-after-save after-save-hook))))

(defun git-sync--after-save ()
  "Run git-sync on-save."
  (git-sync--execute))

(define-minor-mode git-sync-mode
  "Run git-sync on-save."
  :lighter " git-sync"
  :group 'git-sync
  (if git-sync-mode
      (setq-local after-save-hook (cons 'git-sync--after-save after-save-hook))
    (setq-local after-save-hook (remove 'git-sync--after-save after-save-hook))))

(provide 'git-sync-mode)
;;; git-sync-mode.el ends here
