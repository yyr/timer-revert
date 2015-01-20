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
;; This is more like `auto-revert-mode` but with a specified time
;; interval. see `timer-revert-delay`, defaults to 15 seconds.
;;
;;
;; Caveats:
;; - Currently works for only one buffer.


;;; Code:

(defcustom timer-revert-delay 15
  "time frequency in seconds to run revert"
  :group 'timer-revert)

(defvar-local timer-revert-buffer (current-buffer))
(defvar-local timer-revert-timer nil)


(defun timer-revert-buffer ()
  "revert buffer if not modified."
  (with-current-buffer timer-revert-buffer
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (buffer-modified-p))
        (progn
          (revert-buffer t t t)
          (message "%s refreshed buffer" (buffer-name)))
      ;; (message "%s file has not changed outside" (buffer-name))
      )))

;;; debug
;; (setq timer-revert-delay 3)
;; (setq timer-revert-timer nil)

;;;###autoload
(define-minor-mode timer-revert-mode
  "revert buffer for every `timer-revert-delay'"
  :group timer-revert
  :init-value nil
  (and timer-revert-timer (cancel-timer timer-revert-timer))
  (if timer-revert-mode
      (progn
        (setq timer-revert-timer
              (run-at-time t timer-revert-delay
                           'timer-revert-buffer)))
    (and timer-revert-timer
         (cancel-timer  timer-revert-timer))))

(provide 'timer-revert)
;;; timer-revert.el ends here
