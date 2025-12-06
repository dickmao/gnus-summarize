;;; nndebsum.el --- nnnil modulo request-article  -*- lexical-binding: t; -*-

(require 'nnheader)

(defun nndebsum-retrieve-headers (_articles &optional _group _server _fetch-old)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  'nov)

(defun nndebsum-open-server (_server &optional _definitions)
  t)

(defun nndebsum-close-server (&optional _server _defs)
  t)

(defun nndebsum-request-close ()
  t)

(defun nndebsum-server-opened (&optional _server)
  t)

(defun nndebsum-status-message (&optional _server)
  "")

(defun nndebsum-request-head (_id &optional _group _server)
  nil)

(defun nndebsum-request-article (_article &optional group _server _to-buffer)
  (setq-local gnus-article-buffer (current-buffer))
  (cons (or group gnus-newsgroup-name) "foo"))

(defun nndebsum-request-group (_group &optional _server _fast _info)
  (let (deactivate-mark)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert "411 no such news group\n")))
  nil)

(defun nndebsum-close-group (_group &optional _server)
  t)

(defun nndebsum-request-list (&optional _server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer))
  t)

(defun nndebsum-request-post (&optional _server)
  nil)

(provide 'nndebsum)

;;; nndebsum.el ends here
