;;; org-pomodoro.el --- pomodoro timing in org mode

;; Copyright (C) 2013 Devin Homan.

;; Authors: Devin Homan

;; This file is not currently part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; For commentary, see README.org.


(require 'org-timer)
(require 'org-clock)

(defgroup org-pomodoro nil
  "Options concerning clocking working time in Org-mode using Pomodoros."
  :tag "Org Pomodoro"
  :group 'org-pomodoro)

(defcustom org-pomodoro-work-time 25
  "Pomodoro time in minutes."
  :group 'org-pomodoro
  :type '(integer))

(defcustom org-pomodoro-break-time 5
  "Pomodoro break time in minutes."
  :group 'org-pomodoro
  :type '(integer))

(defcustom org-pomodoro-long-break-time 30
  "Pomodoro break time between sets, in minutes."
  :group 'org-pomodoro
  :type '(integer))

(defcustom org-pomodoro-clock-out-sound nil
  "Audio to play when pomodoro work-time is finished."
  :group 'org-pomodoro
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file  :tag "Play sound file"))
  :link '(variable-link org-clock-sound))
(defcustom org-pomodoro-clock-out-sound nil
  "Audio to play when pomodoro work-time is finished."
  :group 'org-pomodoro
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file  :tag "Play sound file"))
  :link '(variable org-clock-sound))
(defcustom org-pomodoro-clock-in-sound nil
  "File path to audio to play when pomodoro break is finished."
  :group 'org-pomodoro
  :type '(choice
	  (const :tag "No sound" nil)
	  (const :tag "Standard beep" t)
	  (file  :tag "Play sound file"))
  :link '(variable-link org-clock-sound))
(defcustom org-pomodoro-break-file nil
  "File to record pomodoro break times."
  :group 'org-pomodoro
  :type '(choice
	  (const :tag "Temporary file" nil)
	  (file :tag "Store break times in a file.")))
(defcustom org-pomodoro-break-target "pomodoro-break"
  "The Org target in the `org-pomodoro-break-file' to record break times."
  :group 'org-pomodoro
  :type '(string))

(defun org-pomodoro-start ()
  "When the current item has the :POMODORO: property set to a
non-nil value, start a new pomodoro timer."
  (when (and (assoc "POMODORO" (org-entry-properties))
	     (not (string= "nil" (cdr (assoc "POMODORO" (org-entry-properties))))))
    (let ((org-clock-sound org-pomodoro-clock-out-sound))
      (when org-timer-start-time
	(org-timer-stop))
      (org-timer-set-timer org-pomodoro-work-time))))

(defun org-pomodoro-break ()
  "When the current item has the :POMODORO: property set to a
non-nil value, stop the pomodoro timer, and ask the user if they
want to start a break timer."
  (org-pomodoro-cancel)
  ;; check if current item is marked for pomodoro
  (when (and (assoc "POMODORO" (org-entry-properties)) 
             (not (string= "nil" (cdr (assoc "POMODORO" (org-entry-properties))))))
    ;; ask the user if they are taking a short or long break
    (let ((duration
           (if (y-or-n-p (format "%d minute break?" org-pomodoro-break-time))
               org-pomodoro-break-time
             (when (y-or-n-p (format "%d minute break?" org-pomodoro-long-break-time))
               org-pomodoro-long-break-time))))

      (save-window-excursion
        
        (beginning-of-line)
        (re-search-forward "CLOCK:\s+\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\s+[^]-+>0-9\s\n]+\s+[0-9]\\{1,2\\}:[0-9]\\{2\\}\\]--\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\s+[^]-+>0-9\s]+\s+[0-9]\\{1,2\\}:[0-9]\\{2\\}\\]\s+=>\s+\\([0-9]+\\):\\([0-9][0-9]\\)")
        ;; determine if a Pomodoro was completed
        (when (> org-pomodoro-work-time 
                 (+
                  (* 60 
                     (string-to-number (match-string 1))) ;hours
                  (string-to-number (match-string 2)))) ;minutes
          (end-of-line)
          (open-line 1)
          (forward-line)
          (indent-relative)
          (if (y-or-n-p "Voided Pomodoro: External distraction?")
              (insert "VOIDED_POMODORO: external distraction")
            (insert "VOIDED_POMODORO: internal distraction")))
        (when duration          
          ;;If the `org-pomodoro-break-file' does not exist,
          ;;generate a file for it and save the path to
          ;;`org-pomodoro-break-file'.
          (unless (and (stringp org-pomodoro-break-file)
		       (< 0 (length org-pomodoro-break-file))
                       (file-readable-p org-pomodoro-break-file)
                       (file-writable-p org-pomodoro-break-file))
            (let ((break-file 
                   (make-temp-file
                    ;; from elisp info 25.8.5 Generating Unique File Names
                    (expand-file-name "pomodoro-break"
                                      (or small-temporary-file-directory
                                          temporary-file-directory))
                    nil ".org")))
              (with-temp-buffer
		(unless (< 0 (length org-pomodoro-break-target))
		  (setq org-pomodoro-break-target (car (get 'org-pomodoro-break-target 'standard-value))))
		(insert (format "* Pomodoro break\n#<<%s>>" org-pomodoro-break-target))

                (write-file break-file))
              (setq org-pomodoro-break-file break-file)))
          (find-file org-pomodoro-break-file)
          (org-open-link-from-string 
           (format "file:%s::%s" 
                   org-pomodoro-break-file
                   org-pomodoro-break-target))
          (read-only-mode -1)           ;in case buffer-read-only is set to t 
          (let ((org-clock-sound org-pomodoro-clock-in-sound))
            (org-clock-in)
            (org-timer-set-timer duration)))))))

(defun org-pomodoro-cancel ()
  "Cancel the current pomodoro."
  (when org-timer-start-time
    (org-timer-stop)))

(add-hook 'org-clock-in-hook 'org-pomodoro-start)
(add-hook 'org-clock-out-hook 'org-pomodoro-break)
(add-hook 'org-clock-cancel-hook 'org-pomodoro-cancel)

(provide 'org-pomodoro)
