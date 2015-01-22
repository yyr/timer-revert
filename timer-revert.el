;;; timer-revert.el --- minor mode to revert buffer for a given time interval.

;; Copyright (C) 2015 Yagnesh Raghava Yakkala

;; Author: Yagnesh Raghava Yakkala. http://yagnesh.org
;; Maintainer: hi@yagnesh.org
;; Created: Tuesday, January 20 2015
;; Keywords: timer, revert, auto-revert.
;; Version: 0.1
;; URL: http://github.com/yyr/timer-revert

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; A minor mode to revert buffer for a given time interval.
;;
;; This is more like `auto-revert-mode' but with a specified time
;; interval.  see `timer-revert-delay', defaults to 15 seconds.  This is useful
;; because Emacs' auto-revert-mode doesn't have a facility to tell the
;; frequency.

;; My use case is while writing latex documents, background running make needs
;; some time to finish, usually 5 to 10 seconds.  unlike `auto-revert-mode'
;; which is very eager to load the file as soon as its modified outside, this
;; one lazily waits for 15 seconds.  For best experience, if the background
;; process takes 5 seconds then `timer-revert-delay' should be around 10
;; seconds.  Okay the logic is not perfect though but minimizes conflicts.


;;; Code:
(defgroup timer-revert nil
  "timer-revert customizations."
  :group 'processes)

(defcustom timer-revert-delay 15
  "Time frequency in seconds to run revert."
  :group 'timer-revert)

(defvar timer-revert-timer nil)

(defun timer-revert-buffer (buf)
  "Revert a buffer file of BUF if it is modified outside of Emacs.
But do it only when the buffer is not modified."
  (when (bufferp buf)
    (with-current-buffer buf
      (if (and (buffer-file-name)
               (file-exists-p (buffer-file-name))
               (not (verify-visited-file-modtime (current-buffer))))
          (if (buffer-modified-p)
              "buffer modified. not reverting."
            (progn
              (revert-buffer t t t)
              (message "%s refreshed buffer" (buffer-name))))
        ;;      (message "%s file has not changed outside" (buffer-name))
        ))))


(defun timer-revert-clear-all-timer ()
  "Clear timers from the timer-revert."
  (interactive)
  (cancel-function-timers #'timer-revert-buffer)
  (setq timer-revert-timer nil))


(defun timer-revert-clear-timer ()
  "Clear timer."
  (when timer-revert-timer
    (cancel-timer timer-revert-timer)
    (setq timer-revert-timer nil)))

;;; debug
;; (setq timer-revert-delay 7)
;; (setq timer-revert-timer nil)

;;;###autoload
(define-minor-mode timer-revert-mode
  "Revert buffer for every `timer-revert-delay'."
  :init-value nil
  :group 'timer-revert
  (make-local-variable 'timer-revert-timer)
  (make-local-variable 'timer-revert-delay)
  (cond (timer-revert-mode
         (timer-revert-clear-timer)
         (add-hook 'kill-buffer-hook 'timer-revert-clear-timer nil 'local)
         (setq timer-revert-timer
               (apply 'run-at-time t timer-revert-delay
                      'timer-revert-buffer (list  (current-buffer)))))
        (t
         (timer-revert-clear-timer)
         (remove-hook 'kill-buffer-hook 'timer-revert-clear-timer 'local))))

(provide 'timer-revert)
;;; timer-revert.el ends here
