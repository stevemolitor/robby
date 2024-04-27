;;; robby-utils-test.el  --- tests for robby utility functions -*- lexical-binding:t -*-

(require 'ert)

(require 'robby-customization)
(require 'robby-utils)

;;; Code:

;;; string utils tests
(ert-deftest robby--kebab-to-snake-case ()
  (should (equal (robby--kebab-to-snake-case "a-b-c") "a_b_c")))

(ert-deftest robby--snake-to-space-case ()
  (should (equal (robby--snake-to-space-case "a_b_c") "a b c")))

(ert-deftest robby--snake-to-kebob-case ()
  (should (equal (robby--snake-to-kebob-case "a_b_c") "a-b-c")))

;;; property list utils tests
(ert-deftest robby--plist-to-alist ()
  (should (equal (robby--plist-to-alist '(:a 1 :b 2))
                 '((:a . 1) (:b . 2)))))

(ert-deftest robby--plist-to-transient-args ()
  (should (equal (robby--plist-to-transient-args '(:a 1 :b "2"))
                 '("a=1" "b=2"))))

(ert-deftest robby--plist-keys ()
  (should (equal (robby--plist-keys '(:a 1 :b "2"))
                 '(:a :b))))

(ert-deftest robby--decimal-p ()
  (should (not (null (robby--decimal-p "1.0"))))
  (should (not (null (robby--decimal-p "0"))))
  (should (not (null (robby--decimal-p "-0.1"))))
  (should (not (null (robby--decimal-p "-0000.1"))))
  (should (not (null (robby--decimal-p "+0.1"))))
  (should (not (null (robby--decimal-p "+00.1"))))
  (should (not (null (robby--decimal-p "0"))))
  (should (not (null (robby--decimal-p "001"))))
  (should (not (null (robby--decimal-p "001.1"))))
  (should (null (robby--decimal-p "x")))
  ;; (should (null (robby--decimal-p "0.")))
  (should (null (robby--decimal-p "a0")))
  (should (null (robby--decimal-p "0 "))))

;;; robby API options tests
(ert-deftest robby--options-alist-for-api-request ()
  (let ((robby-chat-model "gpt-4")
        (robby-chat-max-tokens 100)
        (robby-chat-temperature 1.0))
    (should (equal (robby--options-alist-for-api-request '(:max-tokens 2))
                   '(("max_tokens" . 2)
                     ("model" . "gpt-4")
                     ("temperature" . 1.0))))))

(ert-deftest robby--options-alist-for-api-request-returns-stop-array ()
  ;; we only support a single "stop" string, but the API expects an array
  (let ((robby-chat-model "gpt-4")
        (robby-chat-max-tokens 100)
        (robby-chat-temperature 1.0)
        (robby-chat-stop "stop"))
    (should (equal (robby--options-alist-for-api-request '())
                   '(("max_tokens" . 100)
                     ("model" . "gpt-4")
                     ("stop" . ("stop")) 
                     ("temperature" . 1.0))))))

;;; request input tests
(ert-deftest robby--request-input--no-history ()
  (let ((input (robby--request-input "hello" nil nil "I am a helpful assistant.")))
    (should (equal input `((messages . [((role . "system") (content . "I am a helpful assistant."))
                                        ((role . "user") (content . "hello"))]))))))

(ert-deftest robby--request-input--with-history ()
  (should (equal
           (robby--request-input
            "Where was it played?"
            t
            '(("Who won the world series in 2020?" . "The Los Angeles Dodgers won the World Series in 2020."))
            "I am a helpful assistant.")
           `((messages .
                       [((role . "system") (content . "I am a helpful assistant."))
                        ((role . "user") (content . "Who won the world series in 2020?"))
                        ((role . "assistant") (content . "The Los Angeles Dodgers won the World Series in 2020."))
                        ((role . "user") (content . "Where was it played?"))
                        ])))))

(ert-deftest robby--request-input--with-two-itemhistory ()
  (should (equal
           (robby--request-input
            "Bonjour!"
            t
            '(("hi" . "Hello!") ("answer in french" . "Bien sûr!"))
            "I am a helpful assistant.")

           `((messages .
                       [((role . "system") (content . "I am a helpful assistant."))
                        ((role . "user") (content . "hi"))
                        ((role . "assistant") (content . "Hello!"))
                        ((role . "user") (content . "answer in french"))
                        ((role . "assistant") (content . "Bien sûr!"))
                        ((role . "user") (content . "Bonjour!"))
                        ])))))

;;; chunk content tests
(ert-deftest robby--chunk-content--no-streaming ()
  (let ((resp '((choices . [((index . 0)
                             (message
                              (role . "assistant")
                              (content . "Hello! How can I assist you today?"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content resp nil)
             "Hello! How can I assist you today?"))))

(ert-deftest robby--chunk-content--streaming ()
  (let ((resp '((choices . [((index . 0)
                             (delta
                              (role . "assistant")
                              (content . "Hello"))
                             (finish_reason . "stop"))]))))
    (should (equal
             (robby--chunk-content resp t)
             "Hello"))))

;;; prompt templates
(ert-deftest robby--format-prompt--with-prompt-spec-arg ()
  (should (equal
           (robby--format-prompt "file extension: %e" nil '((?e . "el")))
           "file extension: el")))

(ert-deftest robby--format-prompt--with-repeats ()
  (should (equal
           (robby--format-prompt "file extension: %e %e" nil '((?e . "el")))
           "file extension: el el")))

(ert-deftest robby--format-prompt--with-missing-args ()
  (should (equal
           (robby--format-prompt "file extension:" nil '((?e . "el")))
           "file extension:")))

;;; grounding
(ert-deftest robby--ground-response--chain-fns ()
  (cl-letf (((symbol-function 'upper) (lambda (resp) (upcase resp)))
            ((symbol-function 'twice) (lambda (resp) (concat resp resp))))
    (should (equal (robby--ground-response "response" '(upper twice))
                   "RESPONSERESPONSE"))))

(ert-deftest robby--ground-response--single-fn ()
  (cl-letf (((symbol-function 'upper) (lambda (resp) (upcase resp))))
    (should (equal (robby--ground-response "response" #'upcase)
                   "RESPONSE"))))

;;; request utils
(ert-deftest robby--parse-http-status ()
  (let ((resp "logprobs\":null,\"finish_reason\":\"stop\"}]}

data: [DONE]

 HTTP STATUS: 200

"))
    (should (equal
             (robby--parse-http-status resp)
             200))))

(ert-deftest robby--parse-http-status-no-status ()
  (let ((chunk "data: {\"id\":\"chatcmpl-9GR46WElbBCZQwFD2WoGeYKnd8I5z\",\"object\":\"chat.completion.chunk\",\"created\":1713704314,\"model\":\"gpt-3.5-turbo-0125\",\"system_fingerprint\":\"fp_c2295e73ad\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\"},\"logprobs\":null,\"finish_reason\":null}]}

data: {\"id\":\"chatcmpl-9GR46WElbBCZQwFD2WoGeYKnd8I5z\",\"object\":\"chat.completion.chunk\",\"created\":1713704314,\"model\":\"gpt-3.5-turbo-0125\",\"system_fingerprint\":\"fp_c2295e73ad\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"logprobs\":null,\"finish_reason\":null}]}

data: {\"id\":\"chatcmpl-9GR46WElbBCZQwFD2WoGeYKnd8I5z\",\"object\":\"chat.completion.chunk\",\"created\":1713704314,\"model\":\"gpt-3.5-turbo-0125\",\"system_fingerprint\":\"fp_c2295e73ad\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"!\"},\"logprobs\":null,\"finish_reason\":null}]}


"))
    (should (null (robby--parse-http-status chunk)))))

(provide 'robby-utils-test)

;;; robby-utils-test.el ends here
