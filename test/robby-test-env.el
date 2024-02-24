;;; robby-test-env.el  --- setup Robby test environment for batch mode testing  -*- lexical-binding:t -*-

(require 'ert)

;;; Code:

(defconst robby--test-deps '(markdown-mode spinner))

(defconst robby--test-dir (file-name-directory (or load-file-name ".")))

(setq package-user-dir
      (expand-file-name (format "%s.elpa/%s/elpa" robby--test-dir emacs-version)))
(package-initialize)
  (add-to-list 'load-path default-directory)

(defun robby--install-test-deps ()
  (when (not (file-exists-p package-user-dir))
    (setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
    (package-refresh-contents)
    (dolist (package robby--test-deps)
      (unless (package-installed-p package)
        (package-install package)))))

;; robby-test-env.el ends here
