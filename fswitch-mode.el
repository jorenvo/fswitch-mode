;; -*- mode: Emacs-Lisp; lexical-binding: t -*-
;;; fswitch.el --- minor mode for switching quickly between buffers with the <f> keys

;; Copyright (C) 2014 Joren Van Onder

;; fswitch is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

(defvar fswitch-list (make-list 13 nil)
  "Holds the buffers associated with every <f> key.")

(defun fswitch-clear (&optional fkey)
  "Unassociate buffers from <f> keys. When fkey is nil, unassociate all."
  (interactive)
  (if fkey
      (progn
	(setcar (nthcdr fkey fswitch-list) nil)
	(message (format "%s%d%s" "unassociated <f" (+ fkey 1) ">")))
    (progn
      (setq fswitch-list (make-list 13 nil))
      (message "unassociated all keys"))))

(defun fswitch (fkey &optional buffer)
  (if current-prefix-arg
      ;; clear
      (fswitch-clear fkey)
    (if (nth fkey fswitch-list)
	;; jump
	(let ((buffer-to-switch-to (nth fkey fswitch-list)))
	  (if (buffer-live-p buffer-to-switch-to)
	      (switch-to-buffer buffer-to-switch-to)
	    (fswitch-clear fkey)
	    (message (format "%s%d%s" "unassociated <f" (+ fkey 1) "> because buffer is dead"))))
      ;; define
      (let ((buffer-to-bind (if buffer buffer (current-buffer))))
	(setcar (nthcdr fkey fswitch-list) buffer-to-bind)
	(message (format "%s%d%s%s" "associated <f" (+ fkey 1) "> with " buffer-to-bind))))))

;;;###autoload
(define-minor-mode fswitch-mode
  "Minor mode for switching quickly between buffers with the <f> keys."
  :lighter " fswitch"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
	    (mapc (lambda (number)
		    (define-key map
		      (kbd (concat "<f" (number-to-string (1+ number)) ">"))
		      (lambda () (interactive) (fswitch number))))
		  (number-sequence 0 11))
	    map))

(provide 'fswitch-mode)
