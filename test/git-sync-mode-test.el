;;; -*- lexical-binding: t; -*-
;;; git-sync-mode-tests.el --- Tests for git-sync-mode

;;; Commentary:
;; Tests for git-sync-mode.el

;;; Code:
(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:send-report nil))


(require 'ert)
(require 'promise)
(require 'ert-async)
(require 'git-sync-mode)

;;;---------------------------------------------------------------------
;;; Mocking Framework
;;;---------------------------------------------------------------------

(defvar git-sync-mock-executed-commands nil
  "List of commands executed by the mocked `git-sync--execute-command`.")

(defvar git-sync-mock-responses nil
  "A-list of command patterns and their corresponding mock outputs.")

(defun git-sync-mock--execute-command (command dir &optional ignore-error)
  "Mock version of `git-sync--execute-command' for testing."
  (add-to-list 'git-sync-mock-executed-commands command)
  (let* ((cmd-str (mapconcat 'identity command " "))
         (response (cdr (cl-assoc-if (lambda (pattern)
                                       (string-match-p pattern cmd-str))
                                     git-sync-mock-responses))))
    (if response
        (promise-resolve response)
      (promise-reject (format "No mock response for: %s" cmd-str)))))

(defmacro with-mock-git-sync-commands (responses &rest body)
  "Execute BODY with `git-sync--execute-command' mocked.
RESPONSES is an alist of (REGEX-PATTERN . OUTPUT-STRING) for mock responses."
  `(progn
     (setq-local git-sync-mock-executed-commands nil)
     (setq-local git-sync-mock-responses ,responses)
     (advice-add 'git-sync--execute-command :override #'git-sync-mock--execute-command)
     (let ((cleanup (lambda (&optional _)
                      (advice-remove 'git-sync--execute-command #'git-sync-mock--execute-command)
                      (setq git-sync-mock-responses nil))))
       (condition-case err
           (let ((res (progn ,@body)))
             (condition-case nil
                 (promise-finally res cleanup)
               (error
                (funcall cleanup)
                res)))
         (error
          (funcall cleanup)
          (signal (car err) (cdr err)))))))

(defvar git-sync-execute-called nil
  "Tracks if `git-sync--execute` was called.")

(defun git-sync-track--execute (_dir)
  "Mock `git-sync--execute`."
  (setq git-sync-execute-called t)
  (promise-resolve nil))

(defmacro with-mock-execute (&rest body)
  "Execute BODY with `git-sync--execute' mocked.
Removes advice ensuring cleanup."
  `(let ((cleanup (lambda (&rest _)
                    (advice-remove 'git-sync--execute #'git-sync-track--execute))))
     (setq git-sync-execute-called nil)
     (advice-add 'git-sync--execute :override #'git-sync-track--execute)
     (condition-case err
         (promise-finally (progn ,@body) cleanup)
       (error
        (funcall cleanup)
        (signal (car err) (cdr err))))))

(defmacro with-git-sync-test-repo (args &rest body)
  "Create a temp git repo and bind `default-directory` to it.
ARGS is a plist with keys :state (string) and :locked (boolean)."
  (declare (indent 1))
  `(let* ((tmpdir (make-temp-file "git-sync-test" t))
          (default-directory tmpdir)
          (gitdir (expand-file-name ".git" tmpdir)))
     ;; run git init in default directory
     (call-process "git" nil nil nil "init")
     (let ((state (plist-get ,args :state)))
       (cond
        ((equal state "MERGING") (write-region "" nil (expand-file-name "MERGE_HEAD" gitdir)))))
     (when (plist-get ,args :locked)
       (write-region "" nil (expand-file-name "index.lock" gitdir)))
     (unwind-protect
         (progn ,@body)
       (delete-directory tmpdir t))))

;;;---------------------------------------------------------------------
;;; Test Command Variables
;;;---------------------------------------------------------------------
(defvar git-sync-test-cmd-rev-list "rev-list --count --left-right"
  "Command for `git rev-list --count --left-right`.")

(defvar git-sync-test-cmd-rev-parse-upstream "rev-parse --abbrev-ref @{u}"
  "Command for `git rev-parse --abbrev-ref @{u}`.")

(defvar git-sync-test-cmd-status-porcelain "status --porcelain"
  "Command for `git status --porcelain`.")

(defvar git-sync-test-cmd-fetch "fetch"
  "Command for `git fetch`.")

(defvar git-sync-test-cmd-add "add --all"
  "Command for `git add --all`.")

(defvar git-sync-test-cmd-commit "commit -m"
  "Command for `git commit -m`.")

(defvar git-sync-test-cmd-push "push"
  "Command for `git push`.")

(defvar git-sync-test-cmd-rebase "rebase"
  "Command for `git rebase`.")

(defvar git-sync-test-cmd-unmatched "unmatched-command"
  "A command pattern that is not expected to match anything in the tests.")


;;;---------------------------------------------------------------------
;;; Tests for git-sync--get-sync-state
;;;---------------------------------------------------------------------

(ert-deftest-async git-sync-get-sync-state-equal (done)
  "Test :equal state from git-sync--get-sync-state."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-rev-list . "0	0"))
                 (git-sync--get-sync-state "/path/to/repo" "origin/main"))
                (lambda (state)
                  (should (eq state :equal))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-get-sync-state-ahead (done)
  "Test :ahead state from git-sync--get-sync-state."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-rev-list . "0	5"))
                 (git-sync--get-sync-state "/path/to/repo" "origin/main"))
                (lambda (state)
                  (should (eq state :ahead))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-get-sync-state-behind (done)
  "Test :behind state from git-sync--get-sync-state."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-rev-list . "3	0"))
                 (git-sync--get-sync-state "/path/to/repo" "origin/main"))
                (lambda (state)
                  (should (eq state :behind))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-get-sync-state-diverged (done)
  "Test :diverged state from git-sync--get-sync-state."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-rev-list . "3	5"))
                 (git-sync--get-sync-state "/path/to/repo" "origin/main"))
                (lambda (state)
                  (should (eq state :diverged))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-get-sync-state-error (done)
  "Test error handling in git-sync--get-sync-state."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-unmatched . "foo"))
                 (git-sync--get-sync-state "/path/to/repo" "origin/main"))
                (lambda (value)
                  (funcall done "Promise resolved unexpectedly")) ; Fail test if it resolves
                (lambda (err)
                  (should (and (listp err)
                               (eq (car err) 'error)
                               (string-match-p "No mock response" (cadr err))))
                  (funcall done))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--get-upstream-branch
;;;---------------------------------------------------------------------
(ert-deftest-async git-sync-get-upstream-branch-success (done)
  "Test successful retrieval of upstream branch."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-rev-parse-upstream . "origin/main\n"))
                 (git-sync--get-upstream-branch "/path/to/repo"))
                (lambda (upstream)
                  (should (string= upstream "origin/main"))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-get-upstream-branch-failure (done)
  "Test failure to retrieve upstream branch."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-unmatched . "foo")) ; No match for rev-parse
                 (git-sync--get-upstream-branch "/path/to/repo"))
                (lambda (upstream)
                  (should (null upstream))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise was rejected: %s" err)))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--has-changes-p
;;;---------------------------------------------------------------------

(ert-deftest-async git-sync-has-changes-p-no-changes (done)
  "Test git-sync--has-changes-p with no changes."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-status-porcelain . ""))
                 (git-sync--has-changes-p "/path/to/repo"))
                (lambda (has-changes)
                  (should (not has-changes))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-has-changes-p-with-changes (done)
  "Test git-sync--has-changes-p with changes."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-status-porcelain . " M file.txt"))
                 (git-sync--has-changes-p "/path/to/repo"))
                (lambda (has-changes)
                  (should has-changes)
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--execute (Integration-style)
;;;---------------------------------------------------------------------

(ert-deftest-async git-sync-execute-does-nothing-when-equal-and-no-changes (done)
  "Test that git-sync--execute does nothing when repo is clean and equal."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-status-porcelain . "")
                   (,git-sync-test-cmd-rev-parse-upstream . "origin/main")
                   (,git-sync-test-cmd-fetch . "ok")
                   (,git-sync-test-cmd-rev-list . "0	0"))
                 (git-sync--execute "/path/to/repo"))
                (lambda (val)
                  (let ((commands (reverse git-sync-mock-executed-commands)))
                    (should (= (length commands) 4))
                    (should (string-match-p "status --porcelain" (mapconcat #'identity (nth 0 commands) " ")))
                    (should (string-match-p "rev-parse --abbrev-ref @{u}" (mapconcat #'identity (nth 1 commands) " ")))
                    (should (string-match-p "fetch" (mapconcat #'identity (nth 2 commands) " ")))
                    (should (string-match-p "rev-list" (mapconcat #'identity (nth 3 commands) " "))))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))


(ert-deftest-async git-sync-execute-commits-and-pushes-when-ahead (done)
  "Test that git-sync--execute commits and pushes when ahead."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-status-porcelain . " M file.txt")
                   (,git-sync-test-cmd-add . "ok")
                   (,git-sync-test-cmd-commit . "ok")
                   (,git-sync-test-cmd-rev-parse-upstream . "origin/main")
                   (,git-sync-test-cmd-fetch . "ok")
                   (,git-sync-test-cmd-rev-list . "0	5")
                   (,git-sync-test-cmd-push . "ok"))
                 (git-sync--execute "/path/to/repo"))
                (lambda (val)
                  (let ((commands (reverse git-sync-mock-executed-commands)))
                    (should (= (length commands) 7))
                    (should (string-match-p "status --porcelain" (mapconcat #'identity (nth 0 commands) " ")))
                    (should (string-match-p "add --all" (mapconcat #'identity (nth 1 commands) " ")))
                    (should (string-match-p "commit -m" (mapconcat #'identity (nth 2 commands) " ")))
                    (should (string-match-p "rev-parse --abbrev-ref @{u}" (mapconcat #'identity (nth 3 commands) " ")))
                    (should (string-match-p "fetch" (mapconcat #'identity (nth 4 commands) " ")))
                    (should (string-match-p "rev-list" (mapconcat #'identity (nth 5 commands) " ")))
                    (should (string-match-p "push" (mapconcat #'identity (nth 6 commands) " "))))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

(ert-deftest-async git-sync-execute-rebases-when-diverged (done)
  "Test that git-sync--execute rebases and pushes when diverged."
  (promise-then (with-mock-git-sync-commands
                 `((,git-sync-test-cmd-status-porcelain . "")
                   (,git-sync-test-cmd-rev-parse-upstream . "origin/main")
                   (,git-sync-test-cmd-fetch . "ok")
                   (,git-sync-test-cmd-rev-list . "3	5")
                   (,git-sync-test-cmd-rebase . "ok")
                   (,git-sync-test-cmd-push . "ok"))
                 (git-sync--execute "/path/to/repo"))
                (lambda (val)
                  (let ((commands (reverse git-sync-mock-executed-commands)))
                    (should (= (length commands) 6))
                    (should (string-match-p "status --porcelain" (mapconcat #'identity (nth 0 commands) " ")))
                    (should (string-match-p "rev-parse --abbrev-ref @{u}" (mapconcat #'identity (nth 1 commands) " ")))
                    (should (string-match-p "fetch" (mapconcat #'identity (nth 2 commands) " ")))
                    (should (string-match-p "rev-list" (mapconcat #'identity (nth 3 commands) " ")))
                    (should (string-match-p "rebase" (mapconcat #'identity (nth 4 commands) " ")))
                    (should (string-match-p "push" (mapconcat #'identity (nth 5 commands) " "))))
                  (funcall done))
                (lambda (err)
                  (funcall done (format "Promise rejected: %s" err)))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--allowed-directory
;;;---------------------------------------------------------------------

(ert-deftest git-sync-allowed-directory-empty-list ()
  "Test that git-sync--allowed-directory returns nil when list is empty."
  (let ((git-sync-allow-list '()))
    (should-not (git-sync--allowed-directory "/some/file.txt"))))

(ert-deftest git-sync-allowed-directory-match ()
  "Test that git-sync--allowed-directory returns t when file matches."
  (let ((git-sync-allow-list '("/allowed/dir/")))
    (should (git-sync--allowed-directory "/allowed/dir/file.txt"))
    (should (git-sync--allowed-directory "/allowed/dir/subdir/file.txt"))))

(ert-deftest git-sync-allowed-directory-no-match ()
  "Test that git-sync--allowed-directory returns nil when file does not match."
  (let ((git-sync-allow-list '("/allowed/dir/")))
    (should-not (git-sync--allowed-directory "/other/dir/file.txt"))
    (should-not (git-sync--allowed-directory "/allowed/file.txt"))))

(ert-deftest git-sync-allowed-directory-nil-file ()
  "Test that git-sync--allowed-directory returns nil when file is nil."
  (let ((git-sync-allow-list '("/allowed/dir/")))
    (should-not (git-sync--allowed-directory nil))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--maybe
;;;---------------------------------------------------------------------

(ert-deftest git-sync-maybe-enables-mode ()
  "Test that git-sync--maybe enables git-sync-mode when directory is allowed."
  (let ((git-sync-allow-list '("/allowed/dir/"))
        (mode-enabled nil))
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/allowed/dir/file.txt"))
              ((symbol-function 'git-sync-mode) (lambda () (setq mode-enabled t))))
      (git-sync--maybe)
      (should mode-enabled))))

(ert-deftest git-sync-maybe-does-not-enable-mode ()
  "Test that git-sync--maybe does not enable git-sync-mode when directory is not allowed."
  (let ((git-sync-allow-list '("/allowed/dir/"))
        (mode-enabled nil))
    (cl-letf (((symbol-function 'buffer-file-name) (lambda () "/other/dir/file.txt"))
              ((symbol-function 'git-sync-mode) (lambda () (setq mode-enabled t))))
      (git-sync--maybe)
      (should-not mode-enabled))))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--repo-state
;;;---------------------------------------------------------------------

(ert-deftest git-sync-repo-state-normal ()
  "Test NORMAL state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (should (string= (git-sync--repo-state tmpdir) "NORMAL"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-merging ()
  "Test MERGING state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (write-region "" nil (expand-file-name "MERGE_HEAD" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "MERGING"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-rebase-interactive ()
  "Test REBASE-i state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (make-directory (expand-file-name "rebase-merge" gitdir))
    (write-region "" nil (expand-file-name "rebase-merge/interactive" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "REBASE-i"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-rebase-merge ()
  "Test REBASE-m state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (make-directory (expand-file-name "rebase-merge" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "REBASE-m"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-am-rebase ()
  "Test AM/REBASE state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (make-directory (expand-file-name "rebase-apply" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "AM/REBASE"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-cherry-picking ()
  "Test CHERRY-PICKING state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (write-region "" nil (expand-file-name "CHERRY_PICK_HEAD" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "CHERRY-PICKING"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-bisecting ()
  "Test BISECTING state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (write-region "" nil (expand-file-name "BISECT_LOG" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "BISECTING"))
    (delete-directory tmpdir t)))

(ert-deftest git-sync-repo-state-reverting ()
  "Test REVERTING state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (write-region "" nil (expand-file-name "REVERT_HEAD" gitdir))
    (should (string= (git-sync--repo-state tmpdir) "REVERTING"))
    (delete-directory tmpdir t)))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--repo-state
;;;---------------------------------------------------------------------

(ert-deftest git-sync--index-locked-p-without-lock-file ()
  "Test NORMAL state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (should-not (git-sync--index-locked-p  tmpdir))
    (delete-directory tmpdir t)))

(ert-deftest git-sync--index-locked-p-with-lock-file ()
  "Test MERGING state."
  (let* ((tmpdir (make-temp-file "git-sync-test" t))
         (gitdir (expand-file-name ".git" tmpdir)))
    (make-directory gitdir)
    (write-region "" nil (expand-file-name "index.lock" gitdir))
    (should (git-sync--index-locked-p tmpdir))
    (delete-directory tmpdir t)))

;;;---------------------------------------------------------------------
;;; Tests for git-sync--validate-and-run
;;;---------------------------------------------------------------------

(ert-deftest-async git-sync-validate-and-run-locked (done)
  "Test skip when locked."
  (with-git-sync-test-repo '(:locked t)
                           (promise-then (with-mock-execute (git-sync--validate-and-run))
                                         (lambda (_)
                                           (should-not git-sync-execute-called)
                                           (funcall done))
                                         (lambda (err) (funcall done err)))))

(ert-deftest-async git-sync-validate-and-run-special (done)
  "Test skip when special state."
  (with-git-sync-test-repo '(:state "MERGING")
                           (promise-then (with-mock-execute (git-sync--validate-and-run))
                                         (lambda (_)
                                           (should-not git-sync-execute-called)
                                           (funcall done))
                                         (lambda (err) (funcall done err)))))

(ert-deftest-async git-sync-validate-and-run-normal (done)
  "Test run when normal."
  (with-git-sync-test-repo '()
                           (promise-then (with-mock-execute (git-sync--validate-and-run))
                                         (lambda (_)
                                           (should git-sync-execute-called)
                                           (funcall done))
                                         (lambda (err) (funcall done err)))))

(provide 'git-sync-mode-tests)
;;; git-sync-mode-tests.el ends here
