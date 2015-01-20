;;; timer-revert.el --- minor mode revert buffer for a time interval.
;; Author: Yagnesh Raghava Yakkala. http://yagnesh.org
;; Created: Tuesday, January 20 2015
;; URL: http://github.com/yyr/timer-revert
;; Version: 0.1
;; Keywords: timer, revert, auto revert.
;;
;;; Commentary:
;; This is more like auto revert but with specified time interval
;; see `timer-revert-delay'


;;; Code:

(defcustom timer-revert-delay 15
  "time frequency in seconds to run revert")

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

(defvar-local timer-revert-buffer (current-buffer))
(defvar-local timer-revert-timer nil)
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
