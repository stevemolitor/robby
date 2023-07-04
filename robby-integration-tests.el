;;; robby-integration-test.el  --- integration test for robby commands  -*- lexical-binding:t -*-

(require 'cl)
(require 'ert-async)
(require 'seq)

(require 'robby-commands)
(require 'robby-define-command)
(require 'robby-history)
(require 'robby-customization)
(require 'robby-prompts)
(require 'robby-request)
(require 'robby-api-key)

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
     (robby-clear-history)
     (with-current-buffer buffer
       (add-hook 'robby-command-complete-hook cb)
       ,before)))

;;; prepend-region tests
(ert-deftest-async robby-integration-test--run-command-get-prompt-from-region (done)
  (let ((buffer (generate-new-buffer "*robby-commands-test*")))
    (with-current-buffer buffer
      (let ((cb (cl-function (lambda (&key text &allow-other-keys)
                               (should (string-match-p "1865" text))
                               (kill-buffer buffer)
                               (funcall done)))))
        (insert "What year did Abraham Lincoln die?")
        (robby-run-command
         :prompt #'robby-get-prompt-from-region :prompt-args '(:never-ask-p t)
         :action cb)))))

(ert-deftest-async robby-integration-test--run-command (done)
  (let ((cb (cl-function (lambda (&key text &allow-other-keys)
                           (should (string-match-p "1865" text))
                           (funcall done)))))
    (robby-run-command
     :prompt "What year did Abraham Lincoln die?"
     :action cb)))

(defun robby--test-prepend-region (done)
  (robby-async-region-test
   (progn
     (insert "What year did Abraham Lincoln die?")
     (robby-prepend-region 4))
   "1865.*\n*What year did Abraham Lincoln die?"
   done))

;;; suite
(defun robby-run-integration-tests ()
  (interactive)
  (setq ert-async-timeout 20)
  (ert "^robby-integration"))

(provide 'robby-integration-tests)

;;; robby-integration-tests.el ends here
