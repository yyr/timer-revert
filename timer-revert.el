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
  (let ((buf (buffer-name)))
    (if (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (buffer-modified-p))
        (progn
          (revert-buffer t t t)
          (message "Refreshed buffer"))
      (message "file has not changed"))))


(defvar timer-revert-timer nil)
(define-minor-mode timer-revert
  "revert time for ever `timer-revert-delay'"
  :group timer-revert
  :init-value nil
  (if timer-revert
      (progn
        (setq timer-revert-timer
              (run-at-time t timer-revert-delay
                           'timer-revert-buffer)))
    (and timer-revert-timer
         (cancel-timer timer-revert-timer))))

(provide 'timer-revert)
;;; timer-revert.el ends here
