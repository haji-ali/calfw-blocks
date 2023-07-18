;;; calfw-transpose.el --- A block view for calfw  -*- lexical-binding: t; -*-

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

(require 'calfw-blocks)

(defcustom calfw-transpose-date-width 17
  "Width (in characters) of date cell in transpose views."
  :type 'number
  :group 'calfw-transpose)

(defcustom calfw-transpose-day-name-length nil
  "Number of characters of day of week to display in transpose views.
Displays full name if nil."
  :type 'number
  :group 'calfw-transpose)


(defun calfw-transpose-render-append-parts (param)
  "[internal] Append rendering parts to PARAM and return a new list."
  (let* ((EOL "\n")
         (date-cell-width (cfw:k 'date-cell-width param))
         (cell-width (cfw:k 'cell-width param))
         (columns (cfw:k 'columns param))
         (num-cell-char
          (/ cell-width (char-width cfw:fchar-horizontal-line)))
         (num-date-cell-char
          (/ date-cell-width (char-width cfw:fchar-horizontal-line))))
    (append
     param
     `((eol . ,EOL) (vl . ,(cfw:rt (make-string 1 cfw:fchar-vertical-line) 'cfw:face-grid))
       (hline . ,(cfw:rt
                  (concat
                   (cl-loop for i from 0 below 2 concat
                         (concat
                          (make-string 1 (if (= i 0) cfw:fchar-top-left-corner cfw:fchar-top-junction))
                          (make-string num-date-cell-char cfw:fchar-horizontal-line)
                          (make-string 1 (if (= i 0) cfw:fchar-top-left-corner cfw:fchar-top-junction))
                          (make-string num-cell-char cfw:fchar-horizontal-line)))
                   (make-string 1 cfw:fchar-top-right-corner) EOL)
                  'cfw:face-grid))
       (cline . ,(cfw:rt
                  (concat
                   (cl-loop for i from 0 below 2 concat
                         (concat
                          (make-string 1 (if (= i 0) cfw:fchar-left-junction cfw:fchar-junction))
                          (make-string num-date-cell-char cfw:fchar-horizontal-line)
                          (make-string 1 (if (= i 0) cfw:fchar-left-junction cfw:fchar-junction))
                          (make-string num-cell-char cfw:fchar-horizontal-line)))
                   (make-string 1 cfw:fchar-right-junction) EOL) 'cfw:face-grid))))))

(defun calfw-transpose-view-nday-week-calc-param (n dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((time-width 5)
       (time-hline (make-string time-width ? ))
       (win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) 5))
       (date-cell-width calfw-transpose-date-width)
       (cell-width (/ (- win-width junctions-width (* 2 date-cell-width)) 2))
       (cell-height (* 5 win-height)) ;; every cell has essentially unlimited height
       (total-width (+ (* date-cell-width 2) (* cell-width 2) junctions-width)))
    `((cell-width . ,cell-width)
      (date-cell-width . ,date-cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,n)
      (time-width . ,time-width)
      (time-hline . ,time-hline))))

(defun calfw-transpose-view-nday-week (n component &optional model)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (calfw-transpose-render-append-parts
                 (calfw-transpose-view-nday-week-calc-param n dest)))
         (total-width (cfw:k 'total-width param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         (hline (cfw:k 'hline param))
         (cline (cfw:k 'cline param))
         (model (or model
                    (calfw-blocks-view-block-nday-week-model
                     n (cfw:component-model component))))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model)))

    ;; update model
    (setf (cfw:component-model component) model)
    (setq header-line-format "")
    ;; ;; header
    (insert
     "\n"
     (cfw:rt
      (cfw:render-title-period begin-date end-date)
      'cfw:face-title)
     EOL (calfw-blocks-render-toolbar total-width 'week
                             (calfw-blocks-navi-previous-nday-week-command n)
                             (calfw-blocks-navi-next-nday-week-command n))
     EOL)
    (insert cline)
    ;; contents
    (calfw-transpose-render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))

(defun calfw-transpose-render-calendar-cells-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (let ((all-days (apply 'nconc (cfw:k 'weeks model))))
    (calfw-transpose-render-calendar-cells-days
     model param title-func all-days
     'calfw-blocks-render-content t)))


(defun calfw-transpose-render-calendar-cells-days (model param title-func &optional
                                             days content-fun do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (calfw-transpose-render-columns
   (cl-loop with cell-width      = (cfw:k 'cell-width param)
         with days            = (or days (cfw:k 'days model))
         with content-fun     = (or content-fun
                                    'cfw:render-event-days-overview-content)
         with holidays        = (cfw:k 'holidays model)
         with annotations     = (cfw:k 'annotations model)
         with headers         = (cfw:k 'headers  model)
         with raw-periods-all = (calfw-blocks-render-periods-stacks model)
         with sorter          = (cfw:model-get-sorter model)

         for date in days ; days columns loop
         for count from 0 below (length days)
         for hday         = (car (cfw:contents-get date holidays))
         for week-day     = (nth (% count 7) headers)
         for ant          = (cfw:rt (cfw:contents-get date annotations)
                                    'cfw:face-annotation)
         for raw-periods  = (cfw:contents-get date raw-periods-all)
         for raw-contents = (cfw:render-sort-contents
                             (funcall content-fun
                                      (cfw:model-get-contents-by-date date model))
                             sorter)
         for prs-contents = (cfw:render-rows-prop
                             (append (calfw-transpose-render-periods-days
                                      date raw-periods cell-width)
                                     (mapcar 'cfw:render-default-content-face
                                             raw-contents)))
         for num-label = (if prs-contents
                             (format "(%s)"
                                     (+ (length raw-contents)
                                        (length raw-periods))) "")
         for tday = (concat
                     ;; " " ; margin
                     (funcall title-func date week-day hday)
                     (if num-label (concat " " num-label)))
         ;; separate holiday from rest of days in transposed view,
         ;; so it can be put on a new line
         for hday-str = (if hday (cfw:rt (substring hday 0)
                                                  'cfw:face-holiday))
         collect
         (cons date (cons (cons tday (cons ant hday-str)) prs-contents)))
   param))

(defun calfw-transpose-render-periods-days (date periods-stack cell-width)
  "[internal] Insert period texts."
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (cl-loop for (row (begin end content props interval)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width 2)
            for begintime = (if interval (calfw-blocks-format-time (car interval)))
            for endtime = (if interval (calfw-blocks-format-time (cdr interval)))
            for beginday = (cfw:strtime begin)
            for endday = (cfw:strtime end)
            for title =
            (concat (if (not (string= beginday endday))
                        (concat beginday "-" endday " "))
                    (if (and begintime
                             (string= (substring content 0 5) begintime))
                        (concat begintime "-" endtime (substring content 5))
                      content))
            collect
            (if content
                (cfw:render-default-content-face title)
              "")))))

(defun calfw-transpose-render-columns (day-columns param)
  "[internal] This function concatenates each rows on the days into a string of a physical line.
DAY-COLUMNS is a list of columns. A column is a list of following form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
  (let* ((date-cell-width  (cfw:k 'date-cell-width  param))
         (cell-width  (cfw:k 'cell-width  param))
         (cell-height (cfw:k 'cell-height param))
         (EOL (cfw:k 'eol param)) (VL (cfw:k 'vl param))
         (hline (cfw:k 'hline param)) (cline (cfw:k 'cline param))
         (num-days (length day-columns))
         (first-half (seq-subseq day-columns 0 (/ num-days 2)))
         (second-half (seq-subseq day-columns (/ num-days 2) num-days)))
    (cl-loop for j from 0 below (/ num-days 2)
          for day1 = (nth j first-half)
          for day2 = (nth j second-half)
          do
          (cl-loop with breaked-day-columns =
                (cl-loop for day-rows in `(,day1 ,day2)
                      for date = (car day-rows)
                      for line = (cddr day-rows)
                      collect
                      (cons date (cfw:render-break-lines
                                  line cell-width cell-height)))
                with breaked-date-columns =
                (cl-loop for day-rows in `(,day1 ,day2)
                      for date = (car day-rows)
                      for dayname = (aref calendar-day-name-array
                                          (calendar-day-of-week date))
                      for (tday . (ant . hday)) = (cadr day-rows)
                      collect
                      (cons date (cfw:render-break-lines
                                  (list
                                   (cfw:tp
                                    (cfw:render-default-content-face
                                     (concat
                                      (substring dayname 0 calfw-transpose-day-name-length)
                                      " "
                                      tday)
                                     'cfw:face-day-title)
                                    'cfw:date date)
                                   hday) date-cell-width cell-height)))
                with max-height = (max 2
                                       (length (cdr (nth 0 breaked-day-columns)))
                                       (length (cdr (nth 1 breaked-day-columns)))
                                       (length (cdr (nth 0 breaked-date-columns)))
                                       (length (cdr (nth 1 breaked-date-columns))))
                for i from 1 to max-height
                do
                (cl-loop for k from 0 to 1
                      for day-rows = (nth k breaked-day-columns)
                      for date-rows = (nth k breaked-date-columns)
                      for date = (car day-rows)
                      for row = (nth i day-rows)
                      for date-row = (nth i date-rows)
                      do
                      (insert
                       VL (cfw:tp
                           (cfw:render-left date-cell-width (and date-row (format "%s" date-row)))
                           'cfw:date date))
                      (insert
                       VL (cfw:tp
                           (cfw:render-separator
                            (cfw:render-left cell-width (and row (format "%s" row))))
                           'cfw:date date)))
                (insert VL EOL))
          (insert cline))
    (insert EOL)))



(defun calfw-transpose-view-8-day (component)
  (calfw-transpose-view-nday-week 8 component))

(defun calfw-transpose-view-10-day (component)
  (calfw-transpose-view-nday-week 10 component))

(defun calfw-transpose-view-12-day (component)
  (calfw-transpose-view-nday-week 12 component))

(defun calfw-transpose-view-14-day (component)
  (calfw-transpose-view-nday-week 14 component))

(defun calfw-transpose-view-two-weeks (component)
  (calfw-transpose-view-nday-week 14 component
                                         (cfw:view-two-weeks-model
                                          (cfw:component-model component))))


(defun calfw-transpose-change-view-two-weeks ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-two-weeks)))

(defun calfw-transpose-change-view-14-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-14-day)))

(defun calfw-transpose-change-view-12-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-12-day)))

(defun calfw-transpose-change-view-10-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-10-day)))

(defun calfw-transpose-change-view-8-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-8-day)))


(setq
 cfw:cp-dipatch-funcs
 (append
  cfw:cp-dipatch-funcs
  '((transpose-8-day   .  calfw-transpose-view-8-day)
    (transpose-10-day  .  calfw-transpose-view-10-day)
    (transpose-12-day  .  calfw-transpose-view-12-day)
    (transpose-14-day  .  calfw-transpose-view-14-day)
    (transpose-two-weeks    . calfw-transpose-view-two-weeks))))

(setq
 calfw-block-toolbar-views
 '(("Day" . block-day)
   ("3-Day" . block-3-day)
   ("Week" . block-week)
   ("Two Week" . two-weeks)
   ("W^T" . transpose-8-day)
   ("2W^T" . transpose-14-day)
   ("Month" . month)))

(provide 'calfw-transpose)
