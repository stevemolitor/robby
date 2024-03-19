;;; robby-process.el  --- robby process var and function  -*- lexical-binding:t -*-

;;; Commentary:

;; robby process buffer-local variable, and function to kill it.

;;; Code:
(require 'robby-spinner)

(defvar-local robby--last-process nil)
(put 'robby--last-process 'permanent-local t)

(defun robby--process-running-p ()
  "Return non-nil if robby process is currently running."
  (and
   (not (null robby--last-process))
   (process-live-p robby--last-process)))

;;;###autoload (autoload 'robby-kill-last-process "robby-process" "If a robby process is currently running, kill it." t)
(defun robby-kill-last-process (&optional silentp)
  "If a robby process is currently running, kill it.

Do nothing if no process is currently running. If called from
Emacs Lisp, do not print messages if SILENTP is t.

Note that you cannot currently kill the last robby process if you
are using `url-retreive'; you must be using `curl'"
  (interactive)
  (if (robby--process-running-p)
      (progn
        (robby--spinner-stop)
        (kill-process robby--last-process)
        (when (not silentp)
          (message "robby process killed")))
    (if (not silentp)
        (message "no robby process associated with current buffer"))))

(provide 'robby-process)

;;; robby-process.el ends here
