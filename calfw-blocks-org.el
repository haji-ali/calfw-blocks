;;; calfw-blocks-org.el --- A block view for calfw  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, ml729 and Al Haji-Ali

;; Author: ml729
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: Author
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/haji-ali/maccalfw
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;;; Commentary:
;;
;; This package provides several views for calfw that show events as blocks.
;;
;;; Code:
(require 'calfw-org)

(defun calfw-blocks--cfw-org-get-timerange (text)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil."
  ;; TODO: This is exactly the same as `cfw:org-get-timerange' except that
  ;; it fixed cur-day = 0
  (let* ((dotime (cfw:org-tp text 'dotime)))
    (and (stringp dotime) (string-match org-ts-regexp dotime)
         (let ((date-string  (match-string 1 dotime))
               (extra (cfw:org-tp text 'extra)))
           (if (and extra (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra))
               (let* (;; (cur-day (string-to-number
                      ;;           (match-string 1 extra)))
                      (total-days (string-to-number
                                   (match-string 2 extra)))
                      (start-date (org-read-date nil t date-string))
                      (end-date (time-add
                                 start-date
                                 (seconds-to-time (* 3600 24 (- total-days 1))))))
                 ;; (unless (= cur-day total-days)
                 (list (calendar-gregorian-from-absolute (time-to-days start-date))
                       (calendar-gregorian-from-absolute (time-to-days end-date)) text)))))))

(defun calfw-blocks-org-summary-format (item)
  "Version of cfw:org-summary-format that adds time data needed to draw blocks."
  (let* ((time-of-day (cfw:org-tp item 'time-of-day))
         (start-time (if time-of-day (list (/ time-of-day 100) (mod time-of-day 100))))
         (duration (cfw:org-tp item 'duration))
         (end-time (if (and start-time duration) (list (+ (nth 0 start-time) (/ duration 60))
                                                       (+ (nth 1 start-time) (mod duration 60)))
                     start-time))
         (time-str (and time-of-day
                        (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
         ;; (marker (cfw:org-tp item 'org-marker))
         ;; (buffer (and marker (marker-buffer marker)))
         (text (cfw:org-extract-summary item))
         (props (cfw:extract-text-props item 'face 'keymap))
         (extra (cfw:org-tp item 'extra)))
    (setq text (substring-no-properties text))
    (when (and extra (string-match (concat "^" org-deadline-string ".*") extra))
      (add-text-properties 0 (length text) (list 'face (org-agenda-deadline-face 1.0)) text))
    (if org-todo-keywords-for-agenda
        (when (string-match (concat "^[\t ]*\\<\\(" (mapconcat 'identity org-todo-keywords-for-agenda "\\|") "\\)\\>") text)
          (add-text-properties (match-beginning 1) (match-end 1) (list 'face (org-get-todo-face (match-string 1 text))) text)))
    ;;; ------------------------------------------------------------------------
    ;;; act for org link
    ;;; ------------------------------------------------------------------------
    (setq text (replace-regexp-in-string "%[0-9A-F]\\{2\\}" " " text))
    (if (string-match org-link-bracket-re text)
        (let* ((desc (if (match-end 3) (match-string-no-properties 3 text)))
               (link (org-link-unescape (match-string-no-properties 1 text)))
               (help (concat "LINK: " link))
               (link-props (list
                            'face 'org-link
                            'mouse-face 'highlight
                            'help-echo help
                            'org-link link)))
          (if desc
              (progn
                (setq desc (apply 'propertize desc link-props))
                (setq text (replace-match desc nil nil text)))
            (setq link (apply 'propertize link link-props))
            (setq text (replace-match link nil nil text)))))
    (when time-str
      (setq text (concat time-str text)))
    (propertize
     (apply 'propertize text props)
     ;; include org filename
     ;; (and buffer (concat " " (buffer-name buffer)))
     'keymap cfw:org-text-keymap
     ;; Delete the display property, since displaying images will break our
     ;; table layout.
     'display nil
     'calfw-blocks-interval (if start-time (cons start-time end-time)))))



;; TODO: This should be shorter than the previous function.
;; (defun calfw-blocks-org-summary-format (old-fn item)
;;   "Version of cfw:org-summary-format that adds time data needed to draw blocks."
;;   (let* ((time-of-day (cfw:org-tp item 'time-of-day))
;;          (start-time (if time-of-day (list (/ time-of-day 100) (mod time-of-day 100))))
;;          (duration (cfw:org-tp item 'duration))
;;          (end-time (if (and start-time duration) (list (+ (nth 0 start-time) (/ duration 60))
;;                                                        (+ (nth 1 start-time) (mod duration 60)))
;;                      start-time)))
;;     (propertize
;;      (funcall old-fn item)
;;      'calfw-blocks-interval (if start-time (cons start-time end-time)))))


(advice-add 'cfw:org-get-timerange
            :override 'calfw-blocks--cfw-org-get-timerange)
(setq cfw:org-schedule-summary-transformer 'calfw-blocks-org-summary-format)

(provide 'calfw-blocks-org)
