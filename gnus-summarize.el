;;; gnus-summarize.el --- A project.el plugin -*- lexical-binding: t; -*-

;; Copyright (C) 2025 dickmao
;;
;; Author: dickmao
;; Version: 0.0.1
;; URL: https://github.com/dickmao/gnus-summarize
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

;; Hit "z" in a summary or article buffer, and wait.
;;;

(require 'debbugs)
(require 'nnsummarize)
(require 'gnus-sum)
(require 'gnus-art)
(require 'comint)
(require 'auth-source)
(require 'soap-client)

(defgroup gnus-summarize nil
  "Summarize shit."
  :group 'tools
  :prefix "gnus-summarize-")

(defvar gnus-summarize--buffer-alist nil "Alist (BUG-NUM-OR-MESSAGE-ID . BUFFER)")
(defvar gnus-summarize--full-text-alist nil "Alist (BUG-NUM-OR-MESSAGE-ID . CORPUS)")

(defun gnus-summarize--get-api-key ()
  "Get Gemini API key from auth-source."
  (or (auth-source-pick-first-password
       :host "generativelanguage.googleapis.com"
       :user "gemini-api")
      (error "No Gemini API key in auth-source")))

(defun gnus-summarize--strip-base64-attachments (string)
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

(defun gnus-summarize--strip-emacsbug-template (body)
  "Remove emacsbug.el configuration template from BODY."
  (let* ((marker-regex "^In GNU Emacs")
         (match-pos (string-match marker-regex body)))
    (if (and match-pos (>= (length (replace-regexp-in-string
				    "\\s-" "" (substring body 0 match-pos)))
			   80))
        (substring body 0 match-pos)
      body)))

(defun gnus-summarize--bug-header (bug-num status)
  (let* ((subject (alist-get 'subject status))
         (severity (alist-get 'severity status))
         (pending-status (alist-get 'pending status))
         (package (car (alist-get 'package status)))
         (date (alist-get 'date status))
         (log-modified (alist-get 'log_modified status)))
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
		     ""))
     "\n")))

(defun gnus-summarize--trim-text (texts max-length)
  (let* ((indexed-lengths (cl-loop for body in texts
				   for idx from 0
				   collect (cons idx (length body))))
	 (sorted-by-length (sort (copy-sequence indexed-lengths)
				 (lambda (a b) (> (cdr a) (cdr b)))))
	 (total-length (cl-reduce #'+ indexed-lengths :key #'cdr))
	 (indices-to-drop nil))
    (while (and sorted-by-length (> total-length max-length))
      (let ((largest (pop sorted-by-length)))
	(push (car largest) indices-to-drop)
	(cl-decf total-length (cdr largest))))
    (cl-loop for body in texts
	     for idx from 0
	     unless (memq idx indices-to-drop)
	     collect body)))

(defun gnus-summarize--thread-full-text (root-id)
  (when (or (derived-mode-p 'gnus-summary-mode)
	    (derived-mode-p 'gnus-article-mode))
    (when-let ((thread (gnus-id-to-thread root-id))
	       (article-nums (gnus-articles-in-thread thread))
	       (restore (gnus-summary-article-number)))
      (prog1 (save-excursion
	       (save-window-excursion
		 (let (texts)
		   (dolist (num article-nums)
		     (gnus-summary-select-article nil nil nil num)
		     (with-current-buffer gnus-article-buffer
		       (push (gnus-summarize--strip-base64-attachments
			      (buffer-substring-no-properties
			       (point-min) (point-max)))
			     texts)))
		   (setq texts (nreverse texts))
		   (mapconcat #'identity
			      (gnus-summarize--trim-text texts 100000)
			      "\n"))))
	(gnus-summary-select-article nil nil nil restore)))))

(defun gnus-summarize--bug-full-text (log)
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

        (push (gnus-summarize--strip-base64-attachments
               (gnus-summarize--strip-emacsbug-template body)) lines)
        (push "" lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun gnus-summarize--init ()
  (cl-assert (setenv "GEMINI_API_KEY" (gnus-summarize--get-api-key)))
  ;; clean zombies
  (setq gnus-summarize--buffer-alist
	(cl-remove-if-not (lambda (pair)
			    (and (bufferp (cdr pair))
				 (buffer-live-p (cdr pair))))
			  gnus-summarize--buffer-alist)))

(defun gnus-summarize-bug (bug-num)
  (interactive (list (read-number "Enter bug number: ")))
  (gnus-summarize--bug bug-num)
  (gnus-summarize--display-article
   bug-num (gnus-summarize--bug-header
	    bug-num (car (debbugs-get-status bug-num)))))

;;;###autoload
(defun gnus-summarize-thread ()
  "Main entry point."
  (interactive nil gnus-summary-mode)
  (if-let ((subj (gnus-summary-article-subject))
	   (bug-p (string-match "bug#\\([0-9]+\\)" subj))
	   (bug-num (string-to-number (match-string 1 subj))))
      (gnus-summarize-bug bug-num)
    (if-let ((header (gnus-summary-article-header))
	     (message-id (mail-header-id header))
	     (root-id (progn (gnus-summary-refer-thread)
			     (gnus-root-id message-id))))
	(progn
	  (gnus-summarize--thread root-id)
	  (gnus-summarize--display-article root-id nil))
      (message "Nothing happens here"))))

(defun gnus-summarize--bug (bug-num)
  (gnus-summarize--init)
  (when-let ((reget-p (not (assoc-default bug-num gnus-summarize--buffer-alist)))
	     (log (cl-letf (((symbol-function 'soap-validate-xs-basic-type)
			     #'ignore))
		    (debbugs-get-bug-log bug-num)))
	     (full-text (or (assoc-default bug-num gnus-summarize--full-text-alist)
			    (let ((ret (gnus-summarize--bug-full-text log)))
			      (prog1 ret
				(push (cons bug-num ret)
				      gnus-summarize--full-text-alist))))))
    (gnus-summarize--reget-summary bug-num))
  (assoc-default bug-num gnus-summarize--buffer-alist))

(defun gnus-summarize--thread (root-id)
  (gnus-summarize--init)
  (when-let ((reget-p (not (assoc-default root-id gnus-summarize--buffer-alist)))
	     (full-text (or (assoc-default root-id gnus-summarize--full-text-alist)
			    (let ((ret (gnus-summarize--thread-full-text root-id)))
			      (prog1 ret
				(push (cons root-id ret)
				      gnus-summarize--full-text-alist))))))
    (gnus-summarize--reget-summary root-id))
  (assoc-default root-id gnus-summarize--buffer-alist))

(defun gnus-summarize--elpa-dir ()
  (let ((elpa-dir (directory-file-name
		   (file-name-directory
		    (or (locate-library "gnus-summarize")
			default-directory)))))
    (if (equal "lisp" (file-name-nondirectory elpa-dir))
        (directory-file-name (file-name-directory elpa-dir))
      elpa-dir)))

(defun gnus-summarize--chat-keyable (bug-num)
  (let ((map (copy-keymap (current-local-map))))
    (define-key map (kbd "C-c '")
		(lambda ()
		  (interactive)
		  (with-current-buffer (gnus-summarize-open-chat bug-num)
		    (when (bound-and-true-p gnus-summarize--kill-timer)
		      (cancel-timer gnus-summarize--kill-timer)))))
    (use-local-map map)))

(defun gnus-summarize--extract-summary (b)
  (with-current-buffer b
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward comint-prompt-regexp nil t)
	(string-trim
	 (buffer-substring-no-properties
	  (point-min)
	  (match-beginning 0)))))))

(defun gnus-summarize--reget-summary (key)
  "Return summary."
  (when-let ((old (assoc-default key gnus-summarize--buffer-alist)))
    (when (buffer-live-p old)
      (let (kill-buffer-query-functions)
	(kill-buffer old))))
  (setq gnus-summarize--buffer-alist
	(assoc-delete-all key gnus-summarize--buffer-alist))
  (cl-loop
   with buf = (save-excursion
		(save-window-excursion
		  (gnus-summarize-open-chat key)))
   repeat 225
   do (accept-process-output (get-buffer-process buf) 0.2)
   while (and (process-live-p (get-buffer-process buf))
	      (not (gnus-summarize--extract-summary buf)))
   finally do (if (gnus-summarize--extract-summary buf)
		  (push (cons key buf) gnus-summarize--buffer-alist)
		(message "Bummer")
		(when (process-live-p (get-buffer-process buf))
		  (kill-process (get-buffer-process buf)))
		(with-current-buffer buf
		  (special-mode))
		(pop-to-buffer buf))))

(defun gnus-summarize--display-article (key header)
  "Display BUFFER using Gnus article display routines."
  (when-let (b (assoc-default key gnus-summarize--buffer-alist))
    (if (and (not (derived-mode-p 'gnus-summary-mode))
	     (not (derived-mode-p 'gnus-article-mode)))
	(with-current-buffer (pop-to-buffer b)
	  (when (bound-and-true-p gnus-summarize--kill-timer)
	    (cancel-timer gnus-summarize--kill-timer)))
      (let ((gnus-override-method '(nnsummarize ""))
	    (gnus-article-prepare-hook
	     (list (lambda ()
		     (with-current-buffer gnus-article-buffer
		       (let ((inhibit-read-only t))
			 (erase-buffer)
			 (insert (gnus-summarize--extract-summary b))
			 (fill-region (point-min) (point-max))
			 (goto-char (point-min))
			 (when header
			   (insert header "\n"))
			 (goto-char (point-max))
			 (insert "\n\n---\nPress C-c ' to ask follow-up questions.\n")
			 (goto-char (point-min))
			 (gnus-summarize--chat-keyable key)))))))
	(gnus-article-prepare "foo" nil)
	;; so subsequent gnus-summary-select-article doesn't return 'old
	(setq gnus-current-article nil)))))

(defun gnus-summarize-open-chat (key)
  "Open comint buffer for LLM chat."
  (let* ((bname (format "gnus-summarize-chat-%s" key))
	 (b (get-buffer bname)))
    (unless b
      (let ((default-directory (gnus-summarize--elpa-dir))
	    (coding-system-for-write 'utf-8)
	    (temp-file (make-temp-file "gnus-summarize-full-text-"))
	    (full-text (or (assoc-default key gnus-summarize--full-text-alist)
			   (error "Missing full-text for %s" key))))
	(with-temp-file temp-file
	  (insert full-text))
	(setq b (apply #'make-comint bname "uv" nil
		       (split-string (format "run python chat.py %s"
					     temp-file))))
	(with-current-buffer b
	  (set-process-query-on-exit-flag
	   (get-buffer-process (current-buffer)) nil)
	  (gnus-summarize-chat-mode))))
    (when (> (length (window-list)) 1)
      (delete-other-windows))
    (pop-to-buffer b '((display-buffer-at-bottom)
		       (window-height . 0.5)))))

(define-derived-mode gnus-summarize-chat-mode comint-mode "Gnus-Summarize-Chat"
  "Comint mode for LLM chat."
  (visual-line-mode)
  (setq-local comint-prompt-regexp "^Gemini> ")
  (setq-local comint-use-prompt-regexp t)
  (let ((b (current-buffer)))
    (setq-local gnus-summarize--kill-timer
		(run-with-timer
		 240 nil (lambda ()
			   (when (buffer-live-p b)
			     (let (kill-buffer-query-functions)
			       (kill-buffer b)))))))
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (when (bound-and-true-p gnus-summarize--kill-timer)
		(cancel-timer gnus-summarize--kill-timer)))
	    nil t))

;;;###autoload
(with-eval-after-load 'gnus-art
  (define-key gnus-summary-mode-map (kbd "z") #'gnus-summarize-thread)
  ;; gnus-article-read-summary-keys clobbers article, ergo explicit mapping
  (define-key gnus-article-mode-map (kbd "z") #'gnus-summarize-thread))

(provide 'gnus-summarize)

;;; gnus-summarize.el ends here
