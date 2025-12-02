;;; debbugs-summarize.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/debbugs-summarize
;; Package-Requires: ((vterm "0.0.4"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'debbugs)
(require 'auth-source)

(define-key gnus-summary-mode-map (kbd "z") #'debsum-summarize-thread)

(defgroup debbugs-summarize nil
  "Summarize shit."
  :group 'tools
  :prefix "debbugs-summarize-")

(defun debsum--get-api-key ()
  "Get Gemini API key from auth-source."
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com"
       :user "gemini-api")
      (error "No Gemini API key in auth-source")))

(defun debsum--collect-thread-content (article-numbers)
  "Collect headers and content for ARTICLE-NUMBERS.
Returns a list of plists with :header and :content."
  (let (thread-data)
    (dolist (art article-numbers)
      (when-let ((header (gnus-summary-article-header art)))
        (gnus-summary-select-article t t nil art)
        (when-let ((content
                    (when (buffer-live-p (get-buffer gnus-original-article-buffer))
                      (with-current-buffer gnus-original-article-buffer
                        (buffer-substring-no-properties (point-min) (point-max))))))
          (push (list :header header :content content) thread-data)))))
    (nreverse thread-data))

(defun debsum--format-summary-prompt (thread-data)
  "Format THREAD-DATA into a prompt for LLM summarization."
  (with-temp-buffer
    (insert "Please summarize this email thread. ")
    (insert "Include key points, main participants, and outcomes. ")
    (insert "When referencing what someone said, cite them by name.\n\n")
    (insert "Thread messages:\n\n")
    (dolist (item thread-data)
      (let* ((header (plist-get item :header))
             (content (plist-get item :content))
             (from (mail-header-from header))
             (subject (mail-header-subject header))
             (date (mail-header-date header))
             (number (mail-header-number header)))
        (insert (format "--- Message %d ---\n" number))
        (insert (format "From: %s\n" from))
        (insert (format "Subject: %s\n" subject))
        (insert (format "Date: %s\n\n" date))
        (insert content)
        (insert "\n\n")))
    (buffer-string)))

(defun debsum--make-citations-clickable ()
  "Find article number references and make them clickable."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "Message \\([0-9]+\\)" nil t)
      (let ((article-num (string-to-number (match-string 1)))
            (start (match-beginning 0))
            (end (match-end 0)))
        (make-text-button
         start end
         'action (lambda (_button)
                   (let ((art article-num))
                     (other-window 1)
                     (gnus-summary-goto-subject art)
                     (gnus-summary-select-article)))
         'face 'link
         'follow-link t
         'help-echo (format "Jump to article %d" article-num))))))

(defun debsum-summarize-thread ()
  "Summarize thread and display in article buffer."
  (interactive nil gnus-summary-mode)
  (gnus-summary-top-thread)
  (let* ((article (gnus-summary-article-number))
         (thread-articles (gnus-summary-articles-in-thread article))
         (thread-data (debsum--collect-thread-content thread-articles))
         (prompt (debsum--format-summary-prompt thread-data)))
    (debsum--get-summary-async prompt)))

(defun debsum--get-summary-async (prompt)
  "Get summary via Python script, display in article buffer."
  (let* ((api-key (debsum--get-api-key))
	 (name "debbugs-summary")
	 (bname (format "*%s*" name))
         (proc (make-process
                :name name
                :buffer bname
                :command (split-string "uv run python summarize.py")
                :sentinel (lambda (proc event)
			    (when (equal (string-trim event) "finished")
			      (debsum--display-summary
			       (with-current-buffer bname (buffer-string))
			       prompt))
			    (unless (process-live-p proc)
			      (let (kill-buffer-query-functions)
				(kill-buffer bname)))))))
    (setenv "GEMINI_API_KEY" api-key)
    (process-send-string proc prompt)
    (process-send-eof proc)))

(defun debsum--display-summary (summary initial-prompt)
  "Display SUMMARY in article buffer, store INITIAL-PROMPT."
  (gnus-with-article-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert summary)
      (insert "\n\n---\nPress C-' to ask follow-up questions.\n")
      (debsum--make-citations-clickable)
      (goto-char (point-min))
      (setq-local debsum-initial-prompt initial-prompt)
      (local-set-key (kbd "C-'") #'debsum-open-chat)))
  (gnus-configure-windows 'article))

(defun debsum-open-chat ()
  "Open comint buffer for LLM chat."
  (interactive)
  (setenv "GEMINI_API_KEY" (debsum--get-api-key))
  ;; make-comint is idempotent
  (let ((buf (apply #'make-comint "debsum-chat" "uv" nil
		    (split-string "run python chat.py"))))
    (with-current-buffer buf
      (debsum-chat-mode)
      (goto-char (point-max))
      (insert debsum-initial-prompt)
      (comint-send-input))
    (pop-to-buffer buf '((display-buffer-at-bottom)
			 (window-height . 0.3)))))

(define-derived-mode debsum-chat-mode comint-mode "Debsum-Chat"
  "Comint mode for LLM chat."
  (setq-local comint-prompt-regexp "^Gemini> ")
  (setq-local comint-use-prompt-regexp t))

(provide 'debsum)

;; Local Variables:
;; read-symbol-shorthands: (("debsum-" . "debbugs-summarize-"))
;; End:

;;; debbugs-summarize.el ends here
