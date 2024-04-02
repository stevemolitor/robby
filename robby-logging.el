;;; robby-logging.el  --- robby logging utility  -*- lexical-binding:t -*-

;;; Commentary:

;; Robby logging function.

;;; Code:

(require 'robby-customization)

(defvar robby--log-buffer "*robby-log*")

(defun robby--log (msg)
  "Insert MSG in `robby--log' buffer."
  (if robby-logging
      (with-current-buffer (get-buffer-create robby--log-buffer)
        (setq buffer-read-only nil)
        (insert msg)
        (setq buffer-read-only t))))

(provide 'robby-logging)

;;; robby-logging.el ends here
