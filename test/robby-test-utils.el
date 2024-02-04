;;; robby-test-utils.el  --- robby test util functions  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby test util functions.

(require 'ert)

;;; Code:

(defmacro robby--with-history (history &rest body)
  "Execute BODY with history set to HISTORY."
  `(let ((robby--history ,history))
     ,@body))

(defun robby--read-file-into-string (filepath)
  "Return filepath's file content."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string))) 

(provide 'robby-test-utils)

;;; robby-test-utils.el ends here
