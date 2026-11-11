;;; nst.el --- Generating news server statistics  -*- lexical-binding: t; -*-
;; Copyright (C) 2026 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; This file is not part of GNU Emacs.

;; nst.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; nst.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:


;;; Code:

(defvar nst--db nil)

(defun nst-sel (statement &rest args)
  (sqlite-select nst--db statement args))

(defun nst-exec (statement &rest args)
  (sqlite-execute nst--db statement args))

(defun nst--initialize ()
  (unless nst--db
    (setq nst--db (sqlite-open
		   (expand-file-name "nst.sqlite" user-emacs-directory)))
    (nst-exec "create table if not exists stats (date date, new_newsgroups number, ips number, articles_read number, groups_selected number, bytes_transferred number, articles_posted number)")
    (nst-exec "create table if not exists top_groups (date date, name string, requests number)")))
    
(defun nst ()
  "Get new data and generate the stats."
  (nst--initialize)
  (with-temp-buffer
    (insert-file-contents-literally "/tmp/mail")
    (while (re-search-forward "^Subject: .*Daily Usenet report for \\(.*\\)"
			      nil t)
      (let ((date (format-time-string
		   "%F" (encode-time
			 (parse-time-string (match-string 1))))))
	(unless (nst-sel "select * from stats where date = ?" date)
	  ;; Not recorded this date -- get the stats.
	  (save-restriction
	    (narrow-to-region (point) (or (search-forward "\nSubject:" nil t)
					  (point-max)))
	    (goto-char (point-min))
	    (let ((stats (nst--gather-stats)))
	      (nst-exec "insert into stats(date, new_newsgroups, ips, articles_read, groups_selected, bytes_transferred, articles_posted) values(?, ?, ?, ?, ?, ?, ?)"
			date
			(plist-get stats :new-newsgroups)
			(plist-get stats :ips)
			(plist-get stats :articles-read)
			(plist-get stats :groups-selected)
			(plist-get stats :bytes-transferred)
			(plist-get stats :articles-posted))
	      (dolist (group (plist-get stats :top-groups))
		(nst-exec "insert into top_groups(date, name, requests) values(?, ?, ?)"
			  date
			  (plist-get group :group)
			  (plist-get group :requests)))))
	  (goto-char (point-max))))))
  (nst--generate-charts))

(defun nst--gather-stats ()
  (let ((stats nil))
    (when (and (search-forward "\nNewsgroups created:" nil t)
	       (re-search-forward "^TOTAL: \\(.*\\)" nil t))
      (setq stats
	    (nconc stats
		   (list :new-newsgroups
			 (string-to-number
			  (nth 0 (split-string (match-string 1))))))))
    (when (and (search-forward "\nNNRP readership statistics [Top 100]" nil t)
	       (re-search-forward "^TOTAL: \\(.*\\)" nil t))
      (let ((bits (split-string (match-string 1))))
	(setq stats
	      (nconc stats
		     (list :ips (string-to-number (nth 0 bits))
			   :articles-read (string-to-number (nth 2 bits))
			   :bytes-transferred (* (string-to-number (nth 3 bits))
						 (nst-size-unit-multiplier
						  (nth 4 bits)))
			   :groups-selected (string-to-number (nth 5 bits))
			   :articles-posted (string-to-number (nth 6 bits)))))))
    (when (search-forward "\nNewsgroup request counts (by newsgroup) [Top 100]:"
			  nil t)
      (forward-line 2)
      (let ((groups nil))
	(while (and (not (eolp))
		    (length< groups 30))
	  (let ((bits (split-string (buffer-substring (pos-bol) (pos-eol)))))
	    (push (list :group (nth 0 bits)
			:requests (string-to-number (nth 1 bits)))
		  groups))
	  (forward-line 1))
	(setq stats (nconc stats
			   (list :top-groups (nreverse groups))))))
    stats))

(defun nst-size-unit-multiplier (unit)
  "Return the number of bytes in UNIT, such as \"KB\" or \"GB\"."
  (let ((units '("B" "KB" "MB" "GB" "TB" "PB")))
    (if-let ((pos (seq-position units (upcase (string-trim unit)))))
        (expt 1024 pos)
      (error "Unknown unit: %s" unit))))

(defun nst--generate-charts ()
  )

(provide 'nst)

;;; nst.el ends here
