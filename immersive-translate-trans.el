;;; immersive-translate-trans.el --- Translate-shell backend for immersive-translate -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Eli Qian

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; Url: https://github.com/Elilif/emacs-immersive-translate

;; Version: 0.3.0
;; SPDX-License-Identifier: GPL-3.0-or-later


;;; Commentary:
;; translate-shell backends

;;; Code:

(defvar immersive-translate--process-alist)
(declare-function immersive-translate-callback "ext:immersive-translate")

(defgroup immersive-translate-trans nil
  "Immersive translate translate-shell backend."
  :group 'immersive-translate)

(defcustom immersive-translate-trans-exec "C:/Users/zhijia.zhang/go/bin/trans.exe"
  "Translation exec used by trans.

See https://github.com/soimort/translate-shell for more details."
  :group 'immersive-translate-trans
  :type 'string)

(defcustom immersive-translate-trans-engine "google"
  "Translation engine used by trans.

See https://github.com/soimort/translate-shell for more details."
  :group 'immersive-translate-trans
  :type 'string)

(defcustom immersive-translate-trans-source-language "EN"
  "The source language need to be translated.

See https://github.com/soimort/translate-shell#code-list for more
details."
  :group 'immersive-translate-trans
  :type 'string)

(defcustom immersive-translate-trans-target-language "ZH"
  "The target language.

See https://github.com/soimort/translate-shell#code-list for more
details."
  :group 'immersive-translate-trans
  :type 'string)

(defcustom immersive-translate-trans-default-args "-show-original n -show-original-phonetics n -show-translation y -no-ansi -show-translation-phonetics n -show-prompt-message n -show-languages n -show-original-dictionary n -show-dictionary n -show-alternatives n"
  "Options passed to trans.

See https://github.com/soimort/translate-shell for more details."
  :group 'immersive-translate-trans
  :type 'string)


(defun immersive-translate-trans-make-command-detect-lang (source target text)
  "Generate the translate-shell command list for TEXT."
  (list immersive-translate-trans-exec
        "-s" source
        "-t" target
        text))

(defun immersive-translate-trans--parse-response (buf)
  "Parse the buffer BUF with translate-shell's response."
  (with-current-buffer buf
    (string-trim
     (buffer-substring-no-properties (point-min) (point-max)))))


;; TODO: define a more generic sentinel
(defun immersive-translate-trans--sentinel (process _status)
  "Process sentinel for immersive-translate translate-shell process.

PROCESS and _STATUS are process parameters."
  (let ((proc-buf (process-buffer process)))
    (when-let* (((eq (process-status process) 'exit))
                (proc-info (alist-get process immersive-translate--process-alist))
                (proc-content (plist-get proc-info :content))
                (proc-callback (plist-get proc-info :callback)))
      (pcase-let ((response
                   (immersive-translate-trans--parse-response proc-buf)))
        (plist-put proc-info :status t)
        (when (and (plist-get proc-info :retry)
                   (string-empty-p response))
          (setq response "No response."))
        (when (and proc-content
                   (string-empty-p response)
                   (not (plist-get proc-info :retry)))
          (plist-put proc-info :retry t)
          (immersive-translate-trans-translate proc-info proc-callback))
        (funcall proc-callback response proc-info)))
    (setf (alist-get process immersive-translate--process-alist nil 'remove) nil)
    (kill-buffer proc-buf)))

(defun contains-chinese-p (text)
  "Check if TEXT contains any Chinese characters."
  (cl-loop for char across text
           thereis (or (and (>= char #x4E00) (<= char #x9FFF))
                       (and (>= char #x3400) (<= char #x4DBF))
                       (and (>= char #x20000) (<= char #x2A6DF))
                       (and (>= char #x2A700) (<= char #x2B73F))
                       (and (>= char #x2B740) (<= char #x2B81F))
                       (and (>= char #x2B820) (<= char #x2CEAF))
                       (and (>= char #x2CEB0) (<= char #x2EBEF))
                       (and (>= char #xF900) (<= char #xFAFF))
                       (and (>= char #x2F800) (<= char #x2FA1F)))))

(defun detect-language (text)
  "Detect if TEXT is Chinese or English. Return a list where the first element is the source language and the second element is the target language."
  (if (contains-chinese-p text)
      '("ZH" "EN")
    '("EN" "ZH")))

;; TODO: define a more generic function
(defun immersive-translate-trans-translate (info &optional callback)
  "Translate the content in INFO.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards. If omitted
the response is inserted into the current buffer after point."
  (let* ((text (plist-get info :content))
         (languages (detect-language text))
         (source (nth 0 languages))
         (target (nth 1 languages))
         (token (md5 (format "%s%s%s%s"
                             (random) (emacs-pid) (user-full-name)
                             (recent-keys))))
         ;; (command (immersive-translate-trans-make-command (plist-get info :content)))
         (command (immersive-translate-trans-make-command-detect-lang source target text))
         (process (apply #'start-process "immersive-translate-trans"
                         (generate-new-buffer "*immersive-translate-trans*") command)))
    (with-current-buffer (process-buffer process)
      (set-process-query-on-exit-flag process nil)
      (setf (alist-get process immersive-translate--process-alist)
            (nconc (list :token token
                         :callback (or callback
                                       #'immersive-translate-callback))
                   info))

      (set-process-sentinel process #'immersive-translate-trans--sentinel))))


(provide 'immersive-translate-trans)
;;; immersive-translate-trans.el ends here
