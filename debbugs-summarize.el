;;; debbugs-summarize.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/debbugs-summarize
;; Package-Requires: ((debbugs "0.46") (spinner "1.7.3"))

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
(require 'gnus-sum)
(require 'gnus-art)
(require 'spinner)
(require 'auth-source)
(require 'soap-client)

;; gnus reflects this into gnus-article-mode-map
(define-key gnus-summary-mode-map (kbd "z") #'debsum-bug)

(defgroup debbugs-summarize nil
  "Summarize shit."
  :group 'tools
  :prefix "debbugs-summarize-")

(defvar debsum--buffer-alist nil "Alist (BUGNUM . BUFFER)")

(defun debsum--get-api-key ()
  "Get Gemini API key from auth-source."
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com"
       :user "gemini-api")
      (error "No Gemini API key in auth-source")))

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

(defmacro debsum-assume-in-summary (&rest body)
  "If we are not in an summary buffer, go there, and execute BODY.  Restore."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (when (or (derived-mode-p 'gnus-summary-mode)
               (when (gnus-buffer-live-p gnus-summary-buffer)
                 (set-buffer gnus-summary-buffer)))
       ,@body)))

(defun debsum--strip-base64-attachments (body)
  "Remove base64 attachments from BODY."
  (replace-regexp-in-string
   "--=_[a-f0-9]+\n\\(?:.*\n\\)*?--=_[a-f0-9]+--\n?" ""
   body))

(defun debsum--strip-emacsbug-template (body)
  "Remove emacsbug.el configuration template from BODY."
  (let* ((marker-regex "^In GNU Emacs")
         (match-pos (string-match marker-regex body)))
    (if (and match-pos (>= (length (replace-regexp-in-string
				    "\\s-" "" (substring body 0 match-pos)))
			   80))
        (substring body 0 match-pos)
      body)))

(defun debsum--format-bug (bug-number status messages)
  "Format bug STATUS and MESSAGES for LLM processing."
  (let* ((subject (alist-get 'subject status))
         (severity (alist-get 'severity status))
         (pending-status (alist-get 'pending status))
         (package (car (alist-get 'package status)))
         (date (alist-get 'date status))
         (log-modified (alist-get 'log_modified status))
         (total-count (length messages)))
    (with-temp-buffer
      (insert (format "Bug #%d: %s\n" bug-number subject))
      (insert (format "Status: %s\n" pending-status))
      (insert (format "Severity: %s\n" severity))
      (when package
        (insert (format "Package: %s\n" package)))
      (when date
        (insert (format "Submitted: %s\n"
                        (format-time-string "%Y-%m-%d" date))))
      (when log-modified
        (insert (format "Last Modified: %s\n"
                        (format-time-string "%Y-%m-%d" log-modified))))
      (insert "\n")
      (insert (format "Discussion (%d message%s):\n\n"
                      total-count
                      (if (= total-count 1) "" "s")))
      (dolist (msg messages)
        (let* ((msg-num (alist-get 'msg_num msg))
               (header (alist-get 'header msg))
               (body (alist-get 'body msg))
               (from nil) (date nil) (subj nil)
               (msg-id nil) (in-reply-to nil) (references nil))
          (with-temp-buffer
            (insert header)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (cond
                 ((string-match "^From:\\s-*\\(.*\\)$" line)
                  (setq from (match-string 1 line)))
                 ((string-match "^Date:\\s-*\\(.*\\)$" line)
                  (setq date (match-string 1 line)))
                 ((string-match "^Subject:\\s-*\\(.*\\)$" line)
                  (setq subj (match-string 1 line)))
                 ((string-match "^Message-[Ii][Dd]:\\s-*\\(.*\\)$" line)
                  (setq msg-id (match-string 1 line)))
                 ((string-match "^In-Reply-To:\\s-*\\(.*\\)$" line)
                  (setq in-reply-to (match-string 1 line)))
                 ((string-match "^References:\\s-*\\(.*\\)$" line)
                  (setq references (match-string 1 line)))))
              (forward-line 1)))
          (insert (format "---\nMessage %d:\n" msg-num))
          (when from
            (insert (format "  From: %s\n" from)))
          (when date
            (insert (format "  Date: %s\n" date)))
          (when subj
            (insert (format "  Subject: %s\n" subj)))
          (when msg-id
            (insert (format "  Message-ID: %s\n" msg-id)))
          (when in-reply-to
            (insert (format "  In-Reply-To: %s\n" in-reply-to)))
          (when references
            (insert (format "  References: %s\n" references)))
          (insert "\n")
          (let ((cleaned-body (debsum--strip-base64-attachments
                               (debsum--strip-emacsbug-template body))))
            (dolist (line (split-string cleaned-body "\n"))
              (insert "  " line "\n")))
          (insert "\n")))
      (buffer-string))))

;;;###autoload
(defun debsum-bug ()
  "Summarize Bug#XXXX and display in article buffer."
  (interactive)
  (cl-assert (setenv "GEMINI_API_KEY" (debsum--get-api-key)))
  (debsum-assume-in-summary
    (when-let* ((subject (gnus-summary-article-subject))
		(bug-num (when (string-match "bug#\\([0-9]+\\)" subject)
			   (string-to-number (match-string 1 subject))))
		(status (car (debbugs-get-status bug-num)))
		(messages (cl-letf (((symbol-function 'soap-validate-xs-basic-type)
				     #'ignore))
			    (debbugs-get-bug-log bug-num)))
		(bug-text (debsum--format-bug bug-num status messages)))
      ;; (with-current-buffer "*scratch*"
      ;; 	(goto-char (point-max))
      ;; 	(insert bug-text))

      (debsum--get-summary-async bug-num bug-text))))

(defun debsum--elpa-dir ()
  (let ((elpa-dir (directory-file-name
		   (file-name-directory
		    (or (locate-library "debbugs-summarize")
			default-directory)))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun debsum--get-summary-async (bug-num bug-text)
  "Get summary via Python script, display in article buffer."
  (setq debsum--buffer-alist (assq-delete-all bug-num debsum--buffer-alist))
  (let* ((name (format "debbugs-summarize-Bug#%d" bug-num))
	 (bname (format "*%s*" name))
	 timeout spin-stopper)
    (when (buffer-live-p (get-buffer bname))
      (let (kill-buffer-query-functions)
	(kill-buffer bname)))
    (unwind-protect
	(cl-loop
	 with success-p = nil
	 with default-directory = (debsum--elpa-dir)
	 with proc = (make-process
		      :name name
		      :buffer bname
		      :command (split-string "uv run python summarize.py")
		      :sentinel (lambda (_proc event)
				  (unless success-p
				    (setq success-p (equal (string-trim event)
							   "finished")))))
	 initially (setq spin-stopper (spinner-start)
			 timeout (run-with-timer 30 nil spin-stopper))
	 initially (progn (process-send-string proc bug-text)
			  (process-send-eof proc))
	 do (accept-process-output proc 0.1)
	 until (or (not (memq timeout timer-list)) (not (process-live-p proc)))
	 finally do (when success-p
		      (setf (alist-get bug-num debsum--buffer-alist)
			    (process-buffer proc))))
      (cancel-timer timeout)
      (funcall spin-stopper)))
  (when-let ((buf (alist-get bug-num debsum--buffer-alist)))
    (debsum--display-article buf)))

(defun debsum--display-article (buffer)
  "Display BUFFER using Gnus article display routines."
  (debsum-assume-in-summary
    (cl-letf (((symbol-function 'gnus-request-article-this-buffer)
	       (lambda (_article _group)
		 (erase-buffer)
		 (insert-buffer-substring buffer)
		 (debsum--make-citations-clickable)
		 (goto-char (point-max))
		 (insert "\n\n---\nPress C-' to ask follow-up questions.\n")
		 (goto-char (point-min))
		 (local-set-key (kbd "C-'") #'debsum-open-chat)
		 (setq gnus-article-current-summary gnus-summary-buffer)
                 'article)))
      (gnus-article-prepare 0 nil))))

(defun debsum-open-chat ()
  "Open comint buffer for LLM chat."
  (interactive)
  ;; make-comint is idempotent
  (let ((buf (apply #'make-comint "debsum-chat" "uv" nil
		    (split-string "run python chat.py"))))
    (with-current-buffer buf
      (debsum-chat-mode)
      (goto-char (point-max))
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
