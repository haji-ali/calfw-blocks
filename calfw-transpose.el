;;; calfw-transpose.el --- A block view for calfw  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, ml729 and Al Haji-Ali

;; Author: ml729
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: Author
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.1") (calfw "2.0"))
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
         (date-cell-width (calfw--k 'date-cell-width param))
         (cell-width (calfw--k 'cell-width param))
         ;; (columns (calfw--k 'columns param))
         (num-cell-char
          (/ cell-width (char-width calfw-fchar-horizontal-line)))
         (num-date-cell-char
          (/ date-cell-width (char-width calfw-fchar-horizontal-line))))
    (append
     param
     `((eol . ,EOL) (vl . ,(calfw--rt (make-string 1 calfw-fchar-vertical-line) 'calfw-grid-face))
       (hline . ,(calfw--rt
                  (concat
                   (cl-loop for i from 0 below 2 concat
                            (concat
                             (make-string 1 (if (= i 0) calfw-fchar-top-left-corner calfw-fchar-top-junction))
                             (make-string num-date-cell-char calfw-fchar-horizontal-line)
                             (make-string 1 (if (= i 0) calfw-fchar-top-left-corner calfw-fchar-top-junction))
                             (make-string num-cell-char calfw-fchar-horizontal-line)))
                   (make-string 1 calfw-fchar-top-right-corner) EOL)
                  'calfw-grid-face))
       (cline . ,(calfw--rt
                  (concat
                   (cl-loop for i from 0 below 2 concat
                            (concat
                             (make-string 1 (if (= i 0) calfw-fchar-left-junction calfw-fchar-junction))
                             (make-string num-date-cell-char calfw-fchar-horizontal-line)
                             (make-string 1 (if (= i 0) calfw-fchar-left-junction calfw-fchar-junction))
                             (make-string num-cell-char calfw-fchar-horizontal-line)))
                   (make-string 1 calfw-fchar-right-junction) EOL) 'calfw-grid-face))))))

(defun calfw-transpose-view-nday-week-calc-param (n dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((time-width 5)
       (time-hline (make-string time-width ? ))
       (win-width (calfw-dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (calfw-dest-height dest) 10)))
       (junctions-width (* (char-width calfw-fchar-junction) 5))
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
  (let* ((dest (calfw-component-dest component))
         (param (calfw-transpose-render-append-parts
                 (calfw-transpose-view-nday-week-calc-param n dest)))
         (total-width (calfw--k 'total-width param))
         ;; (time-width (calfw--k 'time-width param))
         (EOL (calfw--k 'eol param))
         ;; (VL (calfw--k 'vl param))
         ;; (time-hline (calfw--k 'time-hline param))
         ;; (hline (calfw--k 'hline param))
         (cline (calfw--k 'cline param))
         (model (or model
                    (calfw-blocks-view-block-nday-week-model
                     n (calfw-component-model component))))
         (begin-date (calfw--k 'begin-date model))
         (end-date (calfw--k 'end-date model)))

    ;; update model
    (setf (calfw-component-model component) model)
    (setq header-line-format "")
    ;; ;; header
    (insert
     "\n"
     (calfw--rt
      (calfw-render-title-period begin-date end-date)
      'calfw-title-face)
     EOL (calfw-blocks-render-toolbar total-width 'week
                             (calfw-blocks-navi-previous-nday-week-command n)
                             (calfw-blocks-navi-next-nday-week-command n))
     EOL)
    (insert cline)
    ;; contents
    (calfw-transpose-render-calendar-cells-weeks
     model param
     (lambda (date week-day hday)
       (calfw--rt (format "%s" (calendar-extract-day date))
               (if hday 'calfw-sunday-face
                 (calfw--render-get-week-face
                  week-day 'calfw-default-day-face)))))
    ;; footer
    (insert (calfw--render-footer total-width (calfw--model-get-contents-sources model)))))

(defun calfw-transpose-render-calendar-cells-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (let ((all-days (apply 'nconc (calfw--k 'weeks model))))
    (calfw-transpose-render-calendar-cells-days
     model param title-func all-days
     'calfw-blocks-render-content t)))


(defun calfw-transpose-render-calendar-cells-days
    (model param title-func &optional
           days content-fun _do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (calfw-transpose-render-columns
   (cl-loop with cell-width      = (calfw--k 'cell-width param)
         with days            = (or days (calfw--k 'days model))
         with content-fun     = (or content-fun
                                    'calfw--render-event-days-overview-content)
         with holidays        = (calfw--k 'holidays model)
         with annotations     = (calfw--k 'annotations model)
         with headers         = (calfw--k 'headers  model)
         with raw-periods-all = (calfw-blocks-render-periods-stacks model)
         with sorter          = (calfw-model-get-sorter model)

         for date in days ; days columns loop
         for count from 0 below (length days)
         for hday         = (car (calfw--contents-get date holidays))
         for week-day     = (nth (% count 7) headers)
         for ant          = (calfw--rt (calfw--contents-get date annotations)
                                    'calfw-annotation-face)
         for raw-periods  = (calfw--contents-get date raw-periods-all)
         for raw-contents = (calfw--render-sort-contents
                             (funcall content-fun
                                      (calfw-model-get-contents-by-date date model))
                             sorter)
         for prs-contents = (calfw--render-rows-prop
                             (append (calfw-transpose-render-periods-days
                                      date raw-periods cell-width)
                                     (mapcar 'calfw--render-default-content-face
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
         for hday-str = (if hday (calfw--rt (substring hday 0)
                                                  'calfw-holiday-face))
         collect
         (cons date (cons (cons tday (cons ant hday-str)) prs-contents)))
   param))

(defun calfw-transpose-render-periods-days (_date periods-stack _cell-width)
  "[internal] Insert period texts."
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (cl-loop for (_row (begin end content _props interval)) in stack
            ;; for beginp = (equal date begin)
            ;; for endp = (equal date end)
            ;; for width = (- cell-width 2)
            for begintime = (if interval (calfw-blocks-format-time (car interval)))
            for endtime = (if interval (calfw-blocks-format-time (cdr interval)))
            for beginday = (calfw-strtime begin)
            for endday = (calfw-strtime end)
            for title =
            (concat (if (not (string= beginday endday))
                        (concat beginday "-" endday " "))
                    (if (and begintime
                             (string= (substring content 0 5) begintime))
                        (concat begintime "-" endtime (substring content 5))
                      content))
            collect
            (if content
                (calfw--render-default-content-face title)
              "")))))

(defun calfw-transpose-render-columns (day-columns param)
  "Concatenates each row on the days into a string of a physical line.
[internal]

DAY-COLUMNS is a list of columns. A column is a list of following
form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
  (let* ((date-cell-width  (calfw--k 'date-cell-width  param))
         (cell-width  (calfw--k 'cell-width  param))
         (cell-height (calfw--k 'cell-height param))
         (EOL (calfw--k 'eol param)) (VL (calfw--k 'vl param))
         ;; (hline (calfw--k 'hline param))
         (cline (calfw--k 'cline param))
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
                      (cons date (calfw--render-break-lines
                                  line cell-width cell-height)))
                with breaked-date-columns =
                (cl-loop for day-rows in `(,day1 ,day2)
                      for date = (car day-rows)
                      for dayname = (aref calendar-day-name-array
                                          (calendar-day-of-week date))
                      for (tday . (_ant . hday)) = (cadr day-rows)
                      collect
                      (cons date (calfw--render-break-lines
                                  (list
                                   (calfw--tp
                                    (calfw--render-default-content-face
                                     (concat
                                      (substring dayname 0 calfw-transpose-day-name-length)
                                      " "
                                      tday)
                                     'calfw-day-title-face)
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
                       VL (calfw--tp
                           (calfw--render-left date-cell-width (and date-row (format "%s" date-row)))
                           'cfw:date date))
                      (insert
                       VL (calfw--tp
                           (calfw--render-separator
                            (calfw--render-left cell-width (and row (format "%s" row))))
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
                                         (calfw--view-two-weeks-model
                                          (calfw-component-model component))))


(defun calfw-transpose-change-view-two-weeks ()
  "change-view-month"
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'transpose-two-weeks)))

(defun calfw-transpose-change-view-14-day ()
  "change-view-month"
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'transpose-14-day)))

(defun calfw-transpose-change-view-12-day ()
  "change-view-month"
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'transpose-12-day)))

(defun calfw-transpose-change-view-10-day ()
  "change-view-month"
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'transpose-10-day)))

(defun calfw-transpose-change-view-8-day ()
  "change-view-month"
  (interactive)
  (when (calfw-cp-get-component)
    (calfw-cp-set-view (calfw-cp-get-component) 'transpose-8-day)))


(setq
 calfw-cp-dipatch-funcs
 (append
  calfw-cp-dipatch-funcs
  '((transpose-8-day   .  calfw-transpose-view-8-day)
    (transpose-10-day  .  calfw-transpose-view-10-day)
    (transpose-12-day  .  calfw-transpose-view-12-day)
    (transpose-14-day  .  calfw-transpose-view-14-day)
    (transpose-two-weeks    . calfw-transpose-view-two-weeks))))

(setq
 calfw-blocks-toolbar-views
 '(("Day" . block-day)
   ("3-Day" . block-3-day)
   ("Week" . block-week)
   ("Two Week" . two-weeks)
   ("W^T" . transpose-8-day)
   ("2W^T" . transpose-14-day)
   ("Month" . month)))

(provide 'calfw-transpose)
