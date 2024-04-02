;;; robby-providers.el  --- AI Provider Backends  -*- lexical-binding:t -*-

;;; Commentary:

;; Defines a generic function and implementions to customize robby for
;; various AI backends.

;;; Code:

;; declared in robby-customization.el
(defvar robby-provider)
(defvar robby-providers-settings)

(defun robby--provider-settings ()
  "Return the settings of the current provider."
  (alist-get robby-provider robby-providers-settings))

(defun robby--providers-host ()
  "Return the host of the current provider."
  (plist-get (robby--provider-settings) :host))

(defun robby--providers-default-model ()
  "Return the default model to use with the the current provider."
  (plist-get (robby--provider-settings) :default-model))

(defun robby--providers-api-base-path ()
  "Return the host of the current provider."
  (plist-get (robby--provider-settings) :api-base-path))

(defun robby--providers-models-filter-re ()
  "Return regexp to filter models by for the current provider.

Nil means do not filter the models list. OpenAI returns models
that are not usable with the chat API, so we have to filter to
gpt*."
  (plist-get (robby--provider-settings) :models-filter-re))

(provide 'robby-providers)

;;; robby-providers.el ends here
