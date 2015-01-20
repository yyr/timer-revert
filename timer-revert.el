;;; timer-revert.el
;; Author: Yagnesh Raghava Yakkala. http://yagnesh.org
;; Created: Tuesday, January 20 2015

(defcustom  timer-revert-delay 15
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
