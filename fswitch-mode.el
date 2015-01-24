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
	    (define-key map (kbd "<f1>") (lambda () (interactive) (fswitch 0)))
	    (define-key map (kbd "<f2>") (lambda () (interactive) (fswitch 1)))
	    (define-key map (kbd "<f3>") (lambda () (interactive) (fswitch 2)))
	    (define-key map (kbd "<f4>") (lambda () (interactive) (fswitch 3)))
	    (define-key map (kbd "<f5>") (lambda () (interactive) (fswitch 4)))
	    (define-key map (kbd "<f6>") (lambda () (interactive) (fswitch 5)))
	    (define-key map (kbd "<f7>") (lambda () (interactive) (fswitch 6)))
	    (define-key map (kbd "<f8>") (lambda () (interactive) (fswitch 7)))
	    (define-key map (kbd "<f9>") (lambda () (interactive) (fswitch 8)))
	    (define-key map (kbd "<f10>") (lambda () (interactive) (fswitch 9)))
	    (define-key map (kbd "<f11>") (lambda () (interactive) (fswitch 10)))
	    (define-key map (kbd "<f12>") (lambda () (interactive) (fswitch 11)))
            map))

;;;###autoload
(provide 'fswitch-mode)
