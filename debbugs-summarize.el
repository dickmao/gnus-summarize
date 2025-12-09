;;; debbugs-summarize.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/debbugs-summarize
;; Package-Requires: ((debbugs "0.46"))

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

;;; Commentary:

;; M-x debbugs-summarize-bug
;;;

(require 'debbugs)
(require 'nndebsum)
(require 'gnus-sum)
(require 'gnus-art)
(require 'comint)
(require 'auth-source)
(require 'soap-client)

(defgroup debbugs-summarize nil
  "Summarize shit."
  :group 'tools
  :prefix "debbugs-summarize-")

(defvar debsum--buffer-alist nil "Alist (BUG-NUM-OR-MESSAGE-ID . BUFFER)")
(defvar debsum--full-text-alist nil "Alist (BUG-NUM-OR-MESSAGE-ID . CORPUS)")

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

(defun debsum--strip-base64-attachments (string)
  "Remove all base64-encoded MIME blocks from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position)
						  (line-end-position))))
        (if (string-match "^\\([^- ][^ ]*\\): +base64$" line)
            (let ((base64-pos (point))
                  opening-start opening-id closing-end)
              (save-excursion
                (while (and (not opening-id) (not (bobp)))
                  (forward-line -1)
                  (let ((l (buffer-substring-no-properties (line-beginning-position)
							   (line-end-position))))
                    (when (string-match "^-+\\(.+?\\)-*$" l)
                      (setq opening-start (line-beginning-position)
                            opening-id (match-string 1 l))))))
              (if (and opening-id
                       (save-excursion
                         (goto-char base64-pos)
                         (while (and (not closing-end) (not (eobp)))
                           (forward-line 1)
                           (let ((l (buffer-substring-no-properties
				     (line-beginning-position) (line-end-position))))
                             (when (and (string-match "^-+\\(.+?\\)-*$" l)
                                        (string= (match-string 1 l) opening-id))
                               (setq closing-end (line-end-position)))))
                         (and closing-end (> closing-end base64-pos))))
                  (progn
                    (delete-region opening-start (min (1+ closing-end) (point-max)))
                    (goto-char opening-start))
                (forward-line 1)))
          (forward-line 1))))
    (buffer-string)))

(defun debsum--strip-emacsbug-template (body)
  "Remove emacsbug.el configuration template from BODY."
  (let* ((marker-regex "^In GNU Emacs")
         (match-pos (string-match marker-regex body)))
    (if (and match-pos (>= (length (replace-regexp-in-string
				    "\\s-" "" (substring body 0 match-pos)))
			   80))
        (substring body 0 match-pos)
      body)))

(defun debsum--bug-header (bug-num status log)
  (let* ((subject (alist-get 'subject status))
         (severity (alist-get 'severity status))
         (pending-status (alist-get 'pending status))
         (package (car (alist-get 'package status)))
         (date (alist-get 'date status))
         (log-modified (alist-get 'log_modified status))
         (total-count (length log)))
    (mapconcat
     #'identity
     (delq nil (list (format "Bug #%d: %s" bug-num subject)
		     (format "Status: %s" pending-status)
		     (format "Severity: %s" severity)
		     (when package
		       (format "Package: %s" package))
		     (when date
		       (format "Submitted: %s"
			       (format-time-string "%Y-%m-%d" date)))
		     (when log-modified
		       (format "Last Modified: %s"
			       (format-time-string "%Y-%m-%d" log-modified)))
		     (format "Message Count: %d" total-count)
		     ""))
     "\n")))

(defun debsum--thread-full-text (message-id)
  (mapconcat
   #'identity
   (when-let ((thread (gnus-id-to-thread (gnus-root-id message-id)))
              (article-nums (gnus-articles-in-thread thread)))
     (let (bodies)
       (dolist (num article-nums)
         (save-window-excursion
           (gnus-summary-display-article num)
           (with-current-buffer gnus-article-buffer
             (push (debsum--strip-base64-attachments
		    (buffer-substring-no-properties (point-min) (point-max)))
                   bodies))))
       (nreverse bodies)))
   "\n"))

(defun debsum--bug-full-text (log)
  (let (lines)
    (dolist (msg log)
      (let* ((msg-num (alist-get 'msg_num msg))
             (header (alist-get 'header msg))
             (body (alist-get 'body msg))
	     from date subj msg-id in-reply-to references)
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
        (push (format "---\nMessage %d:" msg-num) lines)
        (when from
          (push (format "From: %s" from) lines))
        (when date
          (push (format "Date: %s" date) lines))
        (when subj
          (push (format "Subject: %s" subj) lines))
        (when msg-id
          (push (format "Message-ID: %s" msg-id) lines))
        (when in-reply-to
          (push (format "In-Reply-To: %s" in-reply-to) lines))
        (when references
          (push (format "References: %s" references) lines))
	(push "" lines)

        (push (debsum--strip-base64-attachments
               (debsum--strip-emacsbug-template body)) lines)
        (push "" lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun debsum--init ()
  (cl-assert (setenv "GEMINI_API_KEY" (debsum--get-api-key)))
  ;; clean zombies
  (setq debsum--buffer-alist
	(cl-remove-if-not (lambda (pair)
			    (and (bufferp (cdr pair))
				 (buffer-live-p (cdr pair))))
			  debsum--buffer-alist)))

(defun debsum--add-buf (key buf header)
  (setq debsum--buffer-alist (assoc-delete-all key debsum--buffer-alist))
  (push (cons key buf) debsum--buffer-alist)
  (with-current-buffer buf
    (special-mode)
    (let ((inhibit-read-only t))
      (fill-region (point-min) (point-max))
      (goto-char (point-min))
      (insert header)
      (insert "\n")
      (debsum--make-citations-clickable)
      (goto-char (point-max))
      (insert "\n\n---\nPress C-c ' to ask follow-up questions.\n")
      (goto-char (point-min))
      (debsum--chat-keyable key))))

(defun debsum-bug (bug-num)
  (interactive (list (read-number "Enter bug number: ")))
  (when-let ((buf (debsum--bug bug-num)))
    (debsum--display-article bug-num buf)))

(defun debsum-thread ()
  (interactive)
  (if-let ((subj (gnus-summary-article-subject))
	   (bug-p (string-match "bug#\\([0-9]+\\)" subj))
	   (bug-num (string-to-number (match-string 1 subj)))
	   (buf (debsum--bug bug-num)))
      (debsum--display-article bug-num buf)
    (if-let ((header (gnus-summary-article-header))
	     (message-id (mail-header-id header))
	     (buf (debsum--thread message-id)))
	(debsum--display-article message-id buf)
      (message "Nothing happens here"))))

(defun debsum--bug (bug-num)
  (debsum--init)
  (when-let ((reget-p (not (assoc-default bug-num debsum--buffer-alist)))
	     (name (format "debbugs-summarize-Bug#%d" bug-num))
	     (bname (format "*%s*" name))
	     (status (car (debbugs-get-status bug-num)))
	     (log (cl-letf (((symbol-function 'soap-validate-xs-basic-type)
			     #'ignore))
		    (debbugs-get-bug-log bug-num)))
	     (header (debsum--bug-header bug-num status log))
	     (full-text (or (assoc-default bug-num debsum--full-text-alist)
			    (let ((ret (debsum--bug-full-text log)))
			      (prog1 ret
				(push (cons bug-num ret)
				      debsum--full-text-alist)))))
	     (buf (debsum--reget-summary name bname full-text)))
    (debsum--add-buf bug-num buf header))
  (assoc-default bug-num debsum--buffer-alist))

(defun debsum--thread (message-id)
  (debsum--init)
  (when-let ((reget-p (not (assoc-default message-id debsum--buffer-alist)))
	     (name (format "debbugs-summarize-%s" message-id))
	     (bname (format "*%s*" name))
	     (full-text (or (assoc-default message-id debsum--full-text-alist)
			   (let ((ret (debsum--thread-full-text message-id)))
			     (prog1 ret
			       (push (cons message-id ret)
				     debsum--full-text-alist)))))
	     (buf (debsum--reget-summary name bname full-text)))
    (debsum--add-buf message-id buf nil))
  (assoc-default message-id debsum--buffer-alist))

(defun debsum--elpa-dir ()
  (let ((elpa-dir (directory-file-name
		   (file-name-directory
		    (or (locate-library "debbugs-summarize")
			default-directory)))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun debsum--chat-keyable (bug-num)
  (let ((map (copy-keymap (current-local-map))))
    (define-key map (kbd "C-c '")
		(lambda ()
		  (interactive)
		  (debsum-open-chat bug-num)))
    (use-local-map map)))

(defun debsum--reget-summary (name bname full-text)
  "Return process buffer."
  (when (buffer-live-p (get-buffer bname))
    (let (kill-buffer-query-functions)
      (kill-buffer bname)))
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
   initially (progn (process-send-string proc full-text)
		    (process-send-eof proc))
   repeat 225
   do (accept-process-output proc 0.2)
   until (not (process-live-p proc))
   finally return (prog1 (if success-p
			     (process-buffer proc)
			   (message "Bummer")
			   (pop-to-buffer (with-current-buffer bname
					    (prog1 (current-buffer)
					      (special-mode))))
			   nil)
		    (when (process-live-p proc)
		      (kill-process proc)))))

(defun debsum--display-article (bug-num buffer)
  "Display BUFFER using Gnus article display routines."
  (if (not (derived-mode-p 'gnus-summary-mode))
      (pop-to-buffer buffer)
    (let ((gnus-override-method '(nndebsum ""))
	  (gnus-article-prepare-hook
	   (list (lambda ()
		   (with-current-buffer gnus-article-buffer
		     (let ((inhibit-read-only t))
		       (erase-buffer)
		       (insert-buffer-substring buffer)
		       (debsum--chat-keyable bug-num)
		       (goto-char (point-min))))))))
      (gnus-article-prepare "foo" nil)
      (setq gnus-current-article nil))))

(defun debsum-open-chat (bug-num)
  "Open comint buffer for LLM chat."
  (let* ((default-directory (debsum--elpa-dir))
	 (full-text (or (assoc-default bug-num debsum--full-text-alist)
		       (error "Missing full-text for Bug#%d" bug-num)))
	 (temp-file (make-temp-file "debsum-full-text-"))
	 ;; make-comint is idempotent
	 (buf (progn
		(let ((coding-system-for-write 'utf-8))
		  (with-temp-file temp-file
		    (insert full-text)))
		(apply #'make-comint (format "debsum-chat-bug#%d" bug-num) "uv" nil
		       (split-string (format "run python chat.py %s" temp-file))))))
    (with-current-buffer buf
      (debsum-chat-mode))
    (when (> (length (window-list)) 1)
      (delete-other-windows))
    (pop-to-buffer buf '((display-buffer-at-bottom)
			 (window-height . 0.5)))))

(define-derived-mode debsum-chat-mode comint-mode "Debsum-Chat"
  "Comint mode for LLM chat."
  (setq-local comint-prompt-regexp "^Gemini> ")
  (setq-local comint-use-prompt-regexp t))

(define-key gnus-summary-mode-map (kbd "z") #'debsum-thread)
;; gnus-article-read-summary-keys clobbers article, ergo explicit mapping
(define-key gnus-article-mode-map (kbd "z") #'debsum-thread)

(provide 'debsum)

;; Local Variables:
;; read-symbol-shorthands: (("debsum-" . "debbugs-summarize-"))
;; End:

;;; debbugs-summarize.el ends here
