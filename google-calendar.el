;;; google-calendar.el --- iCalendar implementation -*-coding: utf-8 -*-

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.

;; Author:         Cristiano lazzari <crlazzari@gmail.com>
;; Created:        Nov 2009
;; Keywords:       google calendar
;; Human-Keywords: google, calendar, diary

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;   The Google Calendar package aims at providing an implementation of an
;; interface between emacs  diary and Google Calendar
;;
;;  Dependences:
;;  - icalendar.el
;;  - Python
;;  -- dateutil
;;  - Google GData (download in the google calendar website).
;;

;;; Code:

(require 'icalendar)

(defconst google-calendar-version "0.0.3a"
  "Version number of google-calendar.el.")

;; ======================================================================
;; Customizables
;; ======================================================================
(defgroup google-calendar nil
  "Google Calendar support."
  :prefix "google-calendar-"
  :group 'calendar)

(defcustom google-calendar-retrieval-program "wget -q --no-check-certificate -O '%s' '%s'"
  "Google Calendar iCal retrieving program.
If you use curl, set it to \"curl --silent -o '%s' '%s'\"."
  :group 'calendar)

;;;;;;;;;;;;;;;;;;;
;; Variables
(defvar google-calendar-user           nil "Google calendar user")
(defvar google-calendar-password       nil "Google calendar password")
(defvar google-calendar-directory      "/tmp" "Calendar files directory (temporary directory)")
(defvar google-calendar-url            nil "Google calendar URL")
(defvar google-calendar-code-directory nil "Python code")
(defvar google-calendar-auto-update    nil "Download calendar after updates")


;;;;;;;;;;;;;;;;;;;;
;; Function

(defun encode-if-necessary (string)
  (if (eq system-type 'windows-nt)
      (encode-coding-string string 'shift_jis)
    string))

;; Download Google Calendar
(defun google-calendar-download ()
  "Download diary from Google and replace local diary using shell commands."
  (interactive)
  (setq bfname (buffer-name))
  (let ((ical-file (concat google-calendar-directory "/basic.ics"))
        (gcal-diary-file (concat google-calendar-directory "/diary"))
        (diary-backup-file (concat diary-file ".backup")))
    (condition-case e
        (progn
          (if (not (file-exists-p diary-file))
              (signal 'quit (format "%s does not exist." diary-file)))
          (copy-file diary-file diary-backup-file t)
          (if (file-exists-p ical-file)
              (delete-file ical-file))
          (if (file-exists-p gcal-diary-file)
              (delete-file gcal-diary-file))
          (let ((retrieve-command
                 (format google-calendar-retrieval-program
                         (expand-file-name ical-file)
                         google-calendar-url)))
            (if (= 0 (shell-command retrieve-command))
                (signal 'error (format "Failed to retrieve ical: %s" retrieve-command)))
            )
          (icalendar-import-file ical-file gcal-diary-file)
          (switch-to-buffer (find-file-noselect (concat  google-calendar-directory "/basic.ics" )))
          (kill-buffer nil)
          (switch-to-buffer (find-file-noselect (concat  google-calendar-directory "/diary" )))
          (kill-buffer nil)
          (copy-file (concat google-calendar-directory "/diary") diary-file t)
          (delete-file diary-backup-file)
          (message "Diary updated from Google Calendar."))
      (error
       (if (file-exists-p diary-backup-file)
           (delete-file diary-backup-file))
       (message "Error: %s" (cdr e)) nil)
      (quit (message "Quit: %s" (cdr e)) nil)))
  (switch-to-buffer bfname)
  )

;; Quick add in the Google Calendar
(defun google-calendar-quick-add-event ()
  "Google Calendar Add Quick Event. (e.g Dinner with Angelina Jolie 11/21 at 21pm)"
  (interactive)
  (if (equal google-calendar-password nil)
      (setq google-calendar-password
            (read-passwd (concat "Type the password for the Google Account "
                                 google-calendar-user
                                 "@gmail.com : "))))
  (setq month (format "%d" (nth 0 (calendar-cursor-to-date t))))
  (setq day (format "%d" (nth 1 (calendar-cursor-to-date t))))
  (setq google-calendar-quick-add-event (read-from-minibuffer "Add Quick Event : " (format "%s/%s " month day)))
  (shell-command-to-string (concat "python " google-calendar-code-directory "/insertQuickEvent.py "
                                   "--user " google-calendar-user " "
                                   "--pw " google-calendar-password " "
                                   "--msg \""
                                   (encode-if-necessary google-calendar-quick-add-event) "\""))
  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download))
  )

;; Add in the Google Calendar
(defun google-calendar-add-event ()
  "Google Calendar add event."
  (interactive)
  (if (equal google-calendar-password nil)
      (setq google-calendar-password
            (read-passwd (concat "Type the password for the Google Account "
                                 google-calendar-user
                                 "@gmail.com : "))))
  (setq google-calendar-title (read-from-minibuffer "What : "))
  (setq google-calendar-where (read-from-minibuffer "Where : "))
  (if (equal (length google-calendar-where) 0)
      (setq google-calendar-where "-"))
  (setq google-calendar-content (read-from-minibuffer "Description : "))
  (if (equal (length google-calendar-content) 0)
      (setq google-calendar-content "-"))
  ;; All day
  (if (y-or-n-p "All day event : ")
      (setq google-calendar-allday "Y")
    (setq google-calendar-allday "N"))
  (setq year (format "%d" (nth 2 (calendar-cursor-to-date t))))
  (setq month (format "%02d" (nth 0 (calendar-cursor-to-date t))))
  (setq day (format "%02d" (nth 1 (calendar-cursor-to-date t))))
  (setq time (format-time-string "%H%M"))
  (setq google-calendar-start-time "0000")
  (setq google-calendar-end-time "0000")
  ;; Start DATE TIME
  (setq google-calendar-start-day (read-from-minibuffer "Start (day) YYYYMMDD: " (format "%s%s%s" year month day)))
  (if (equal (length google-calendar-start-day) 0)
      (setq google-calendar-start-day (concat year month day))
    (if (equal (length google-calendar-start-day) 2)
        (setq google-calendar-start-day (concat year month google-calendar-start-day))
      (if (equal (length google-calendar-start-day) 4)
          (setq google-calendar-start-day (concat year google-calendar-start-day)))))
  (setq google-calendar-start google-calendar-start-day)
  (if (equal google-calendar-allday "N")
      (setq google-calendar-start-time
            (read-from-minibuffer "Start (time) HHMM: " (format-time-string "%H%M"))))
  (setq google-calendar-start (concat google-calendar-start-day "T" google-calendar-start-time "00"))
  ;; End DATE TIME
  (setq google-calendar-end-day (read-from-minibuffer "End (day) YYYYMMDD: " google-calendar-start-day))
  (if (equal (length google-calendar-end-day) 0)
      (setq google-calendar-end-day google-calendar-start-day)
    (if (equal (length google-calendar-end-day) 2)
        (setq google-calendar-end-day (concat year month google-calendar-end-day))
      (if (equal (length google-calendar-end-day) 4)
          (setq google-calendar-end-day (concat year google-calendar-end-day)))))
  (setq google-calendar-end google-calendar-end-day)
  (if (equal google-calendar-allday "N")
      (setq google-calendar-end-time
            (read-from-minibuffer "End (time) HHMM: " google-calendar-start-time)))
  (setq google-calendar-end (concat google-calendar-end-day "T" google-calendar-end-time "00"))
  ;; Recurring
  (if (y-or-n-p "Recurring : ")
      (setq google-calendar-recurr "Y")
    (setq google-calendar-recurr "N"))
  (setq google-calendar-rrule "")
  (if (equal google-calendar-recurr "Y")
      (setq google-calendar-freq-q (read-from-minibuffer "Frequency (D=DAYLY,W=WEEKLY,M=MONTHLY,Y=YEARLY) : ")))
  (if (equal google-calendar-recurr "Y")
      (if (or (equal google-calendar-freq-q "Y") (equal google-calendar-freq-q "y"))
          (setq google-calendar-rrule "FREQ=YEARLY;WKST=SU")
        (if (or (equal google-calendar-freq-q "M") (equal google-calendar-freq-q "m"))
            (setq google-calendar-rrule (concat "FREQ=MONTHLY;INTERVAL="
                                                (read-from-minibuffer "Interval (1 means every month) : " "1" ) ";"
                                                "BYMONTHDAY="
                                                (substring google-calendar-start-day 6 8) ";"
                                                )
                  )
          (if (or (equal google-calendar-freq-q "W") (equal google-calendar-freq-q "w"))
              (setq google-calendar-rrule (concat "FREQ=WEEKLY;INTERVAL="
                                                  (read-from-minibuffer "Interval (1 means every week) : " "1" ) ";"
                                                  "BYDAY="
                                                  (read-from-minibuffer "Days SU,MO,TU,WE,TH,FR,SA sepperated by comma: "))
                    )
            (
             (setq google-calendar-rrule (concat "FREQ=DAYLY;INTERVAL="
                                                 (read-from-minibuffer "Interval (1 means every day) : " "1")
                                                 )
                   ))))))
  (shell-command-to-string (concat "python " google-calendar-code-directory "/insertEvent.py "
                                   "--user " google-calendar-user " "
                                   "--pw " google-calendar-password " "
                                   "--t \"" (encode-if-necessary google-calendar-title) "\" "
                                   "--c \"" (encode-if-necessary google-calendar-content) "\" "
                                   "--w \"" (encode-if-necessary google-calendar-where) "\" "
                                   "--st " google-calendar-start " "
                                   "--et " google-calendar-end " "
                                   "--r " google-calendar-recurr " "
                                   "--rr \"" google-calendar-rrule "\" "
                                   "--ad " google-calendar-allday ))

  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download))
  )

;; Delete rvent in the Google Calendar
(defun google-calendar-delete-event ()
  "Google Calendar delete event."
  (interactive)

  (if (equal google-calendar-password nil)
      (setq google-calendar-password
            (read-passwd (concat "Type the password for the Google Account "
                                 google-calendar-user
                                 "@gmail.com : "))))

  (beginning-of-line)(setq begpos (point))
  (search-forward "UID:")(setq uidpos (point))
  (search-forward "@")(setq endpos (point))
  (setq uid (buffer-substring (+ uidpos 1) (- endpos 1)))
  (if (> uidpos begpos)
      (if (y-or-n-p (concat "Ok to delete event \"" uid "\" ? "))
          (shell-command-to-string (concat "python " google-calendar-code-directory "/deleteEvent.py "
                                           "--user " google-calendar-user " "
                                           "--pw " google-calendar-password " "
                                           "--event \"" uid "\"" ))
        (message "Event was not deleted!!")
        ))
  (if (not (equal google-calendar-auto-update nil))
      (google-calendar-download))
  )

;;;;;;;
;; Key defs
(define-key calendar-mode-map "gd" 'google-calendar-download)
(define-key calendar-mode-map "gq" 'google-calendar-quick-add-event)
(define-key calendar-mode-map "g+" 'google-calendar-add-event)

(provide 'google-calendar)

;;; google-calendar.el ends here
