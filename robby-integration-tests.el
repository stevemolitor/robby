;;; robby-integration-test.el  --- integration test for robby commands  -*- lexical-binding:t -*-

(require 'cl)
(require 'ert-async)
(require 'seq)

(require 'robby-commands)
(require 'robby-define-command)
(require 'robby-history)
(require 'robby-customization)
(require 'robby-prompts)

;;; Code:

(defmacro robby-async-region-test (before re done)
  "Test async robby region command.

BEFORE is the setup body to run.  It should invoke the robby
command.

RE is a regular expression.  It is used to search from the
beginning of the current buffer after the command completes.  It
is invoked after OpenAI has responded and the robby command has
manipulated the current buffer.

DONE is the `ert-deftest-async' callback indicating that the test
is complete."
  `(let ((buffer (generate-new-buffer "*robby-commands-test*"))
         (cb (lambda ()
               (goto-char (point-min))
               (should (not (null (re-search-forward ,re))))
               (kill-buffer (current-buffer))
               (funcall done))))
     (if (not robby-openai-api-key)
         (progn
           (load-file (concat user-emacs-directory ".secrets.el.gpg"))
           (setq robby-openai-api-key sm-openai-key)))
     (robby-clear-history)
     (with-current-buffer buffer
       (add-hook 'robby-command-complete-hook cb nil t)
       (cl-letf (((symbol-function 'read-string)
                  (lambda (&rest _) "")))
         ,before))))

;;; prepend-region tests
(defun robby--test-prepend-region (done)
  (robby-async-region-test
   (progn
     (insert "What year did Abraham Lincoln die?")
     (robby-prepend-region nil))
   "1865.*\n*What year did Abraham Lincoln die?"
   done))

(ert-deftest-async robby-integration-test--prepend-region--completions-api (done)
  (let ((robby-api "completions"))
    (robby--test-prepend-region done)))

(ert-deftest-async robby-integration-test--prepend-region--chat-api (done)
  (let ((robby-api "chat"))
    (robby--test-prepend-region done)))


;;; append region tests
(defun robby--test-append-region (done)
  (robby-async-region-test
   (progn
     (insert "What year did Abraham Lincoln die?")
     (robby-append-region nil))
   "What year did Abraham Lincoln die?.*\n*.*1865"
   done))

(ert-deftest-async robby-integration-test--append-region--completions-api (done)
  (let ((robby-api "completions"))
    (robby--test-append-region done)))

(ert-deftest-async robby-integration-test--append-region--chat-api (done)
  (let ((robby-api "chat"))
    (robby--test-append-region done)))

;;; replace region tests
(defun robby--test-replace-region (done)
  (robby-async-region-test
   (progn
     (insert "What year did Abraham Lincoln die?")
     (robby-replace-region nil))
   "1865"
   done))

(ert-deftest-async robby-integration-test--replace-region--completions-api (done)
  (let ((robby-api "completions"))
    (robby--test-replace-region done)))

(ert-deftest-async robby-integration-test--replace-region--chat-api (done)
  (let ((robby-api "chat"))
    (robby--test-replace-region done)))

;;; suite
(defun robby-run-integration-tests ()
  (interactive)
  (setq ert-async-timeout 20)
  (ert "^robby-integration"))

(provide 'robby-integration-tests)

;;; robby-integration-tests.el ends here
