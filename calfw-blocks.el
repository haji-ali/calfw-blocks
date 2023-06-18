;;; calfw-blocks.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 null
;;
;; Author: ml729
;; Maintainer: ml729 <null>
;; Created: July 06, 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'calfw)
(require 'calfw-org)
;; (require 'posframe)

(defcustom calfw-blocks-initial-visible-time '(8 0)
  "Earliest initial visible time as list (hours minutes)."
  :group 'calfw-blocks
  :type 'list)

(defcustom calfw-blocks-lines-per-hour 4
  "Number of lines per hour in a block."
  :group 'calfw-blocks
  :type 'number)

(defcustom calfw-blocks-default-event-length 1
  "Length in hours of events with same start and end time.
Also used for events with a start time and no end time."
  :group 'calfw-blocks
  :type 'number)
(defcustom calfw-blocks-min-block-width 3
  "Minimum width of blocks in characters."
  :group 'calfw-blocks
  :type 'number)

(defcustom calfw-blocks-show-time-grid t
  "Whether to show horizontal lines for each hour."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-render-multiday-events t
  "Whether to render (nonblock) multiday events.
If \\='cont then render them without splitting into cells."
  :group 'calfw-blocks
  :type 'symbol)

(defcustom calfw-blocks-time-grid-lines-on-top t
  "Whether time grid lines should cut through vertical lines."
  :group 'calfw-blocks
  :type 'boolean)


(defcustom calfw-blocks-show-current-time-indicator t
  "Whether to show a line indicating the current time."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-grid-line-char (propertize " " 'face 'overline)
  "Whether time grid lines should cut through vertical lines."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-display-end-times t
  "Whether or not to display end times in blocks.")

(defcustom calfw-blocks-transpose-date-width 17
  "Width (in characters) of date cell in transpose views.")

(defcustom calfw-blocks-transpose-day-name-length nil
  "Number of characters of day of week to display in transpose views.
Displays full name if nil.")

(defface calfw-blocks-overline
  '((t :overline t))
    "Basic face for overline."
  :group 'basic-faces)


(defvar calfw-blocks-earliest-visible-time '(0 0)
  "Earliest visible time in a day as list (hours minutes).")


(defvar calfw-blocks-nday-views-alist
  '((1 . block-day)
    (2 . block-2-day)
    (3 . block-3-day)
    (4 . block-4-day)
    (5 . block-5-day)
    (10 . block-10-day)))


(defvar calfw-blocks-posframe-buffer " *cfw-calendar-sticky*")
(defvar-local calfw-blocks-header-line-string nil)

;; Faces
(defface calfw-blocks-now-indicator
  '((t (:background "#e0a3ff")))
  "Face for current time indicator."
  :group 'calfw-blocks)

;; Transpose

(defun calfw-blocks-render-append-transpose-parts (param)
  "[internal] Append rendering parts to PARAM and return a new list."
  (let* ((EOL "\n")
         (date-cell-width (cfw:k 'date-cell-width param))
         (cell-width (cfw:k 'cell-width param))
         (columns (cfw:k 'columns param))
         (num-cell-char
          (/ cell-width (char-width cfw:fchar-horizontal-line)))
         (num-date-cell-char
          (/ date-cell-width (char-width cfw:fchar-horizontal-line)))
         )
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
                          (make-string num-cell-char cfw:fchar-horizontal-line)
                          ))
                   (make-string 1 cfw:fchar-top-right-corner) EOL)
                  'cfw:face-grid))
       (cline . ,(cfw:rt
                  (concat
                   (cl-loop for i from 0 below 2 concat
                         (concat
                          (make-string 1 (if (= i 0) cfw:fchar-left-junction cfw:fchar-junction))
                          (make-string num-date-cell-char cfw:fchar-horizontal-line)
                          (make-string 1 (if (= i 0) cfw:fchar-left-junction cfw:fchar-junction))
                          (make-string num-cell-char cfw:fchar-horizontal-line)
                          ))
                   (make-string 1 cfw:fchar-right-junction) EOL) 'cfw:face-grid))))))

(defun calfw-blocks-view-nday-transpose-week-calc-param (n dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((time-width 5)
       (time-hline (make-string time-width ? ))
       (win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) 5))
       (date-cell-width calfw-blocks-transpose-date-width)
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

(defun calfw-blocks-view-transpose-nday-week (n component &optional model)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (calfw-blocks-render-append-transpose-parts (calfw-blocks-view-nday-transpose-week-calc-param n dest)))
         (total-width (cfw:k 'total-width param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         (hline (cfw:k 'hline param))
         (cline (cfw:k 'cline param))
         (model (if model model (calfw-blocks-view-block-nday-week-model n (cfw:component-model component))))
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
    (calfw-blocks-render-calendar-cells-transpose-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))

(defun calfw-blocks-render-calendar-cells-transpose-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (let ((all-days (apply 'nconc (cfw:k 'weeks model))))
    (calfw-blocks-render-calendar-cells-transpose-days model param title-func all-days
                                                       'calfw-blocks-render-content
                                                       t)))


(defun calfw-blocks-render-calendar-cells-transpose-days (model param title-func &optional
                                             days content-fun do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (calfw-blocks-render-columns-transpose
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
                             (append (calfw-blocks-render-transpose-periods-days
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

(defun calfw-blocks-render-transpose-periods-days (date periods-stack cell-width)
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

(defun calfw-blocks-render-columns-transpose (day-columns param)
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
                                      (substring dayname 0 calfw-blocks-transpose-day-name-length)
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

;; Block views

(defun calfw-blocks-view-block-nday-week-model (n model)
  "[internal] Create a logical view model of weekly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:k 'init-date model))
         (begin-date (cfw:date-before init-date
                     (mod (cfw:days-diff (cfw:emacs-to-calendar (current-time)) init-date) n)))
         (end-date (cfw:date-after begin-date (1- n))))
    (calfw-blocks-view-model-make-common-data-for-nday-weeks n model begin-date end-date)))

(defun calfw-blocks-view-model-make-common-data-for-nday-weeks (n model begin-date end-date)
  "[internal] Return a model object for week based views."
  (cfw:model-create-updated-view-data
   model
   (cfw:view-model-make-common-data
    model begin-date end-date
    `((headers . ,(calfw-blocks-view-model-make-day-names-for-nday-week n begin-date)) ; a list of the index of day-of-week
      (weeks . ,(calfw-blocks-view-model-make-nday-weeks ; a matrix of day-of-month, which corresponds to the index of `headers'
                 n
                 begin-date
                 end-date))))))

(defun calfw-blocks-view-model-make-day-names-for-nday-week (n begin-date)
  "[internal] Return a list of index of day of the week."
  (let ((begin-day (calendar-day-of-week begin-date)))
  (cl-loop for i from 0 below n
        collect (% (+ begin-day i) cfw:week-days))))
;;todo replace calendar week start day with day of the week of init date

(defun calfw-blocks-view-model-make-nday-weeks (n begin-date end-date)
  "[internal] Return a list of weeks those have 7 days."
  (let* ((first-day-day (calendar-day-of-week begin-date)) weeks)
    (cl-loop with i = begin-date
          with day = first-day-day
          with week = nil
          do
          ;; flush a week
          (when (and (= 0 (mod (- day first-day-day) n)) week)
            (push (nreverse week) weeks)
            (setq week nil)
            (when (cfw:date-less-equal-p end-date i) (cl-return)))
          ;; add a day
          (push i week)
          ;; increment
          (setq day (% (1+ day) n))
          (setq i (cfw:date-after i 1)))
    (nreverse weeks)))

(defun calfw-blocks-view-nday-week-calc-param (n dest)
  "[internal] Calculate cell size from the reference size and
return an alist of rendering parameters."
  (let*
      ((time-width 5)
       (time-hline (make-string time-width ? ))
       (win-width (cfw:dest-width dest))
       ;; title 2, toolbar 1, header 2, hline 2, footer 1, margin 2 => 10
       (win-height (max 15 (- (cfw:dest-height dest) 10)))
       (junctions-width (* (char-width cfw:fchar-junction) (1+ n)))
       (cell-width  (cfw:round-cell-width
                     (max 5 (/ (- win-width junctions-width time-width) n))))
       (cell-height (* calfw-blocks-lines-per-hour 24))
       (total-width (+ time-width (* cell-width n) junctions-width)))
    `((cell-width . ,cell-width)
      (cell-height . ,cell-height)
      (total-width . ,total-width)
      (columns . ,n)
      (time-width . ,time-width)
      (time-hline . ,time-hline))))

(defun calfw-blocks-view-block-day (component)
  (calfw-blocks-view-block-nday-week 1 component))

(defun calfw-blocks-view-block-2-day (component)
  (calfw-blocks-view-block-nday-week 2 component))

(defun calfw-blocks-view-block-3-day (component)
  (calfw-blocks-view-block-nday-week 3 component))

(defun calfw-blocks-view-block-4-day (component)
  (calfw-blocks-view-block-nday-week 4 component))

(defun calfw-blocks-view-block-5-day (component)
  (calfw-blocks-view-block-nday-week 5 component))

(defun calfw-blocks-view-block-7-day (component)
  (calfw-blocks-view-block-nday-week 7 component))

(defun calfw-blocks-view-transpose-8-day (component)
  (calfw-blocks-view-transpose-nday-week 8 component))

(defun calfw-blocks-view-transpose-10-day (component)
  (calfw-blocks-view-transpose-nday-week 10 component))

(defun calfw-blocks-view-transpose-12-day (component)
  (calfw-blocks-view-transpose-nday-week 12 component))

(defun calfw-blocks-view-transpose-14-day (component)
  (calfw-blocks-view-transpose-nday-week 14 component))

(defun calfw-blocks-view-transpose-two-weeks (component)
  (calfw-blocks-view-transpose-nday-week 14 component
                                         (cfw:view-two-weeks-model
                                          (cfw:component-model component))))

(defun calfw-blocks-view-block-week (component)
  (calfw-blocks-view-block-nday-week 7 component
                                     (cfw:view-week-model
                                      (cfw:component-model component))))

;; Rendering views

(defun calfw-blocks-view-block-nday-week (n component &optional model)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts (calfw-blocks-view-nday-week-calc-param n dest)))
         (total-width (cfw:k 'total-width param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         (hline (concat time-hline (cfw:k 'hline param)))
         (cline (concat time-hline (cfw:k 'cline param)))
         (model (if model model (calfw-blocks-view-block-nday-week-model n (cfw:component-model component))))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model))
         day-of-week-names)

    ;; (print model)
    ;; update model
    (setf (cfw:component-model component) model)
    (setq day-of-week-names
          (cl-loop
           with date = begin-date
           for i in (cfw:k 'headers model)
                                                 with VL = (cfw:k 'vl param) with cell-width = (cfw:k 'cell-width param)
                                                 concat
           (concat VL (cfw:render-center
                       cell-width
                       (concat
                        (when (equal (calendar-current-date) date)
                          (cfw:rt
                           "@"
                           'cfw:face-today-title))
                       (cfw:rt
                        (format "%s (%s%d)"
                                 (calendar-day-name date t)
                                (if (= (calendar-extract-month date)
                                       (calendar-extract-month begin-date))
                                    ""
                                  (format "%d/" (calendar-extract-month date)))
                                (calendar-extract-day date))
                         (cfw:render-get-week-face i 'cfw:face-header)))))
           do
           (setq date  (cfw:date-after date 1))))

    (setq calfw-blocks-header-line-string
          (concat "  "
                  (calendar-month-name (calendar-extract-month begin-date) t)
                  " "
                  day-of-week-names))
    (setq header-line-format '((:eval
                                (if (< (line-number-at-pos (window-start)) 6)
                                    ""
                                  calfw-blocks-header-line-string))))

    ;; header
    (insert
     "\n"
     (cfw:rt
      (cfw:render-title-period begin-date end-date)
      'cfw:face-title)
     EOL (calfw-blocks-render-toolbar total-width 'week
                             (calfw-blocks-navi-previous-nday-week-command n)
                             (calfw-blocks-navi-next-nday-week-command n))
     EOL hline)
    ;; time header
    (insert (cfw:rt (cfw:render-right time-width "Time")
                           'default))
    ;; day names
    (insert day-of-week-names)
    (insert VL EOL cline)
    ;; contents
    (calfw-blocks-render-calendar-cells-block-weeks
     model param
     (lambda (date week-day hday)
       (cfw:rt (format "%s" (calendar-extract-day date))
               (if hday 'cfw:face-sunday
                 (cfw:render-get-week-face
                  week-day 'cfw:face-default-day)))))
    ;; footer
    (insert (cfw:render-footer total-width (cfw:model-get-contents-sources model)))))


(defun calfw-blocks-navi-next-nday-week-command (n)
  "Move the cursor forward NUM weeks. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (lambda (&optional num)
  (interactive "p")
  (cfw:navi-next-day-command (* n (or num 1)))))

(defun calfw-blocks-navi-previous-nday-week-command (n)
  "Move the cursor back NUM weeks. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (lambda (&optional num)
    (interactive "p")
    (cfw:navi-next-day-command (* (- n) (or num 1)))))

(defun calfw-blocks-render-toolbar (width current-view prev-cmd next-cmd)
  "[internal] Return a text of the toolbar.

WIDTH is width of the toolbar. CURRENT-VIEW is a symbol of the
current view type. This symbol is used to select the button faces
on the toolbar. PREV-CMD and NEXT-CMD are the moving view
command, such as `cfw:navi-previous(next)-month-command' and
`cfw:navi-previous(next)-week-command'."
  (let* ((prev (cfw:render-button " < " prev-cmd))
         (today (cfw:render-button "Today" 'cfw:navi-goto-today-command))
         (next (cfw:render-button " > " next-cmd))
         (month (cfw:render-button
                 "Month" 'cfw:change-view-month
                 (eq current-view 'month)))
         (tweek (cfw:render-button
                 "Two Weeks" 'cfw:change-view-two-weeks
                 (eq current-view 'two-weeks)))
         (transpose-two-week (cfw:render-button
                 "2W^T" 'calfw-blocks-change-view-transpose-14-day
                 (eq current-view 'transpose-14-day)))
         (transpose-week (cfw:render-button
                 "W^T" 'calfw-blocks-change-view-transpose-8-day
                 (eq current-view 'transpose-8-day)))
         (week (cfw:render-button
                "Week" 'calfw-blocks-change-view-block-week
                (eq current-view 'block-week)))
         (3day (cfw:render-button
                "3-Day" (lambda  () (interactive) (calfw-blocks-change-view-block-nday 3))
                (eq current-view 'block-3-day)))
         (day (cfw:render-button
               "Day" (lambda () (interactive) (calfw-blocks-change-view-block-nday 1))
               (eq current-view 'block-day)))
         (sp  " ")
         (toolbar-text
          (cfw:render-add-right
           width (concat sp prev sp next sp today sp)
           (concat day sp 3day sp week sp tweek sp transpose-week sp transpose-two-week sp month sp))))
    (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

(defun calfw-blocks-change-view-transpose-two-weeks ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-two-weeks)))

(defun calfw-blocks-change-view-transpose-14-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-14-day)))

(defun calfw-blocks-change-view-transpose-12-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-12-day)))

(defun calfw-blocks-change-view-transpose-10-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-10-day)))

(defun calfw-blocks-change-view-transpose-8-day ()
  "change-view-month"
  (interactive)
  (when (cfw:cp-get-component)
    (cfw:cp-set-view (cfw:cp-get-component) 'transpose-8-day)))


(defun calfw-blocks-change-view-block-nday (n)
  ""
  (interactive)
  (when (cfw:cp-get-component)
    (advice-add 'cfw:dest-ol-today-set :override 'calfw-blocks-dest-ol-today-set)
    (cfw:cp-set-view (cfw:cp-get-component) (alist-get n calfw-blocks-nday-views-alist))
    (advice-remove 'cfw:dest-ol-today-set 'calfw-blocks-dest-ol-today-set)))


(defun calfw-blocks-change-view-block-week ()
  "change-view-week"
  (interactive)
  (when (cfw:cp-get-component)
    (advice-add 'cfw:dest-ol-today-set :override 'calfw-blocks-dest-ol-today-set)
    (cfw:cp-set-view (cfw:cp-get-component) 'block-week)
    (advice-remove 'cfw:dest-ol-today-set 'calfw-blocks-dest-ol-today-set)))

(defun calfw-blocks-render-calendar-cells-block-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (cl-loop for week in (cfw:k 'weeks model) do
        ;; (setq week (list (nth 4 week)))
        ;; (print week)
        (calfw-blocks-render-calendar-cells-days model param title-func week
                                                 'calfw-blocks-render-content
                                                 t)))

(defun calfw-blocks-render-calendar-cells-days (model param title-func &optional
                                             days content-fun do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (calfw-blocks-render-columns
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
         ;; for hday = (if (stringp hday) (list hday) hday)
         ;; for prs-hday = (if hday (mapcar (lambda (h) (cfw:rt h 'cfw:face-holiday)) hday))
         for week-day     = (nth (% count 7) headers)
         for ant          = (cfw:rt (cfw:contents-get date annotations)
                                    'cfw:face-annotation)
         for raw-periods  = (cfw:contents-get date raw-periods-all)
         for raw-contents = (cfw:render-sort-contents
                             (funcall content-fun
                                      (cfw:model-get-contents-by-date date model))
                             sorter)
            for prs-contents = (append (if do-weeks
                                         (calfw-blocks-render-periods
                                          date week-day raw-periods cell-width model)
                                       (calfw-blocks-render-periods-days
                                        date raw-periods cell-width))
                                     (mapcar 'cfw:render-default-content-face
                                               raw-contents))
         for num-label = (if prs-contents
                             (format "(%s)"
                                     (+ (length raw-contents)
                                        (length raw-periods))) "")
         for tday = (concat
                     " " ; margin
                     (funcall title-func date week-day hday)
                     (if num-label (concat " " num-label))
                     (if hday (concat " " (cfw:rt (substring hday 0)
                                                  'cfw:face-holiday)))
                     )
         collect
         (cons date (cons (cons tday ant) prs-contents)))
   param))


(defun calfw-blocks-render-periods-days (date periods-stack cell-width)
  "[internal] Insert period texts.
Modified to not truncate events. TODO"
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (cl-loop for (row (begin end content)) in stack
            for beginp = (equal date begin)
            for endp = (equal date end)
            for width = (- cell-width 2)
            for title = (cfw:render-truncate
                         (concat
                          (cfw:strtime begin) " - "
                          (cfw:strtime end) " : "
                          content) width t)
            collect
            (if content
                (cfw:rt
                 (concat
                  (if beginp "(" " ")
                  (cfw:render-left width title ?-)
                  (if endp ")" " "))
                 (cfw:render-get-face-period content 'cfw:face-periods))
              "")))))


(defun calfw-blocks-render-periods-stacks (model)
  "Modified version of cfw:render-periods-stacks, where the last element of
period is a pair containing the start and end of time of each event.
[internal] Arrange the `periods' records of the model and
create period-stacks on the each days.
period-stack -> ((row-num . period) ... )"
  (let* (periods-each-days)
    (cl-loop for (begin end event) in (cfw:k 'periods model)
          for content = (if (cfw:event-p event)
                            ;; (cfw:event-period-overview event)
                            (cfw:event-period-overview event)
                          event)
          for period = (list begin end content
                             (cfw:extract-text-props content 'face)
                             (if (cfw:event-p event) (calfw-blocks-get-time-interval event) nil))
          for row = (cfw:render-periods-get-min periods-each-days begin end)
          do
          (setq periods-each-days (cfw:render-periods-place
                                   periods-each-days row period)))
    periods-each-days))

(defun calfw-blocks-get-time-interval (event)
  "Return (start-time . end-time) of EVENT, a `cfw:event' struct.
start-time and end-time are both lists (a b) where a is the hour,
b is the minute."
  (when (cfw:event-start-time event)
  (cons (cfw:event-start-time event)
        (cfw:event-end-time event))))

(defun calfw-blocks-render-content (lst)
  "[internal] Apply `cfw:event-overview' on `cfw:event's in `lst'."
  (mapcar (lambda (event)
            (if (cfw:event-p event)
                (progn
                  (propertize
                   (cfw:event-overview event)
                   'calfw-blocks-interval (calfw-blocks-get-time-interval event)))
                event))
          lst))

(defun calfw-blocks-render-periods (date week-day periods-stack cell-width model)
  "[internal] This function translates PERIOD-STACK to display content on the DATE."
  (seq-filter 'identity
  (mapcar (lambda (p)
            (let* ((content (nth 2 (cadr p)))
                  (props (nth 3 (cadr p)))
                  (interval (nth 4 (cadr p)))
                  (begintime (if interval (calfw-blocks-format-time (car interval))))
                  (endtime (if interval (calfw-blocks-format-time (cdr interval)))))
          (if (or interval (get-text-property 0 'calfw-blocks-interval content)
                  (not calfw-blocks-render-multiday-events))
              (apply 'propertize
                     (if (and calfw-blocks-display-end-times
                              begintime
                              (string= (substring content 0 5) begintime))
                         (concat begintime "-" endtime (substring content 5))
                       content)
                         'face (cons
                                'calfw-blocks-overline
                                (cfw:render-get-face-period content 'cfw:face-periods))
                          'font-lock-face (cfw:render-get-face-period content 'cfw:face-periods)
                          'cfw:period t
                          'cfw:row-count (car p)
                          'calfw-blocks-interval interval
                          props)
                            (if (eq calfw-blocks-render-multiday-events 'cont)
                                (let* ((begin (nth 0 (cadr p)))
                                       (begin-date (cfw:k 'begin-date model))
                                       (beginp (or (equal date begin)
                                                   (and
                                                    (equal date begin-date)
                                                    (< (calendar-absolute-from-gregorian begin)
                                                       (calendar-absolute-from-gregorian begin-date))))))
                                  (when beginp
                                    (let* ((end (nth 1 (cadr p)))
                                           (end-date (cfw:k 'end-date model))
                                           (len (1+ (min (cfw:days-diff begin end)
                                                         (cfw:days-diff begin end-date)
                                                         (cfw:days-diff begin-date end)
                                                         (cfw:days-diff begin-date end-date)))))
                                      (apply 'propertize
                                             (cfw:render-left (+ (1- len) (* len cell-width)) content ? )
                                             'face (cons
                                                    'calfw-blocks-overline
                                                    (cfw:render-get-face-period content 'cfw:face-periods))
                                             'font-lock-face (cfw:render-get-face-period content 'cfw:face-periods)
                                             'cfw:period t
                                             'cfw:cell-span len
                                             'help-echo (substring-no-properties content)
                                             'cfw:row-count (car p)
                                             props))))
            (let* ((begin (nth 0 (cadr p)))
                   (end (nth 1 (cadr p)))
                   (beginp (equal date begin))
                   (endp (equal date end))
                                     (width (- cell-width
                                               (if beginp (length cfw:fstring-period-start) 0)
                                               (if endp (length cfw:fstring-period-end) 0)))
                   (title (calfw-blocks-render-periods-title
                               date week-day begin end content
                               (- cell-width
                                  (if beginp 1 0)
                                  (if endp 1 0))
                               model)))
                                (apply 'propertize (concat
                                                    (when beginp cfw:fstring-period-start)
                                             (cfw:render-left width title ? )
                                  (when endp cfw:fstring-period-end))
                         'face (cons
                                'calfw-blocks-overline
                                (cfw:render-get-face-period content 'cfw:face-periods))
                          'font-lock-face (cfw:render-get-face-period content 'cfw:face-periods)
                          'cfw:period t
                                       'help-echo (substring-no-properties content)
                          'cfw:row-count (car p)
                                       props))))))
          (seq-sort
           (lambda (a b) (< (car a) (car b)))
                       periods-stack))))

(defun calfw-blocks-render-periods-title (date week-day begin end content cell-width model)
  "[internal] Return a title string.

Fix erroneous width in last line, should be fixed upstream in calfw."
  (let* ((title-begin-abs
          (max (calendar-absolute-from-gregorian begin)
          (calendar-absolute-from-gregorian (cfw:k 'begin-date model))))
         (title-begin (calendar-gregorian-from-absolute title-begin-abs))
         (num (- (calendar-absolute-from-gregorian date) title-begin-abs)))
    (when content
      (cl-loop with title = (substring content 0)
            for i from 0 below num
            for pdate = (calendar-gregorian-from-absolute (+ title-begin-abs i))
            for chopn = (+ (if (equal begin pdate) 1 0) (if (equal end pdate) 1 0))
            for del = (truncate-string-to-width title (- cell-width chopn))
            do
            (setq title (substring title (length del)))
            finally return
            (cfw:render-truncate title cell-width (equal end date))))))


(defun calfw-blocks-format-time (time-obj)
  (format "%02d:%02d" (car time-obj) (cadr time-obj)))

(defun calfw-blocks-time-column (time-width cell-height)
  (let* ((num-hours (floor (/ cell-height calfw-blocks-lines-per-hour)))
        (start-hour (car calfw-blocks-earliest-visible-time))
        (start-minute (cadr calfw-blocks-earliest-visible-time))
        (times-lst (mapcar (lambda (x) (list (mod (+ x start-hour) 24) start-minute))
                           (number-sequence 0 (1- num-hours)))))
    (mapcan (lambda (x) (append (list (calfw-blocks-format-time x))
                       (mapcar (lambda (x) (make-string time-width ? ))
                               (number-sequence 0 (- calfw-blocks-lines-per-hour 2)))))
     times-lst)))

(defun calfw-blocks-render-columns (day-columns param)
  "[internal] Concatenate rows on the days into a string of a physical line.

DAY-COLUMNS is a list of columns. A column is a list of following
form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...)."
  (let* ((cell-width  (cfw:k 'cell-width  param))
        (cell-height (cfw:k 'cell-height param))
        (time-width (cfw:k 'time-width param))
        (EOL (cfw:k 'eol param))
        (VL (cfw:k 'vl param))
        (time-hline (cfw:k 'time-hline param))
        (hline (concat time-hline (cfw:k 'hline param)))
        (cline (concat time-hline (cfw:k 'cline param)))
        (earliest-date (caar day-columns))
        (curr-time-linum (calfw-blocks--current-time-vertical-position)))
    (cl-loop with breaked-all-day-columns =
        (cl-loop for day-rows in day-columns
                        for (date ants . lines) = day-rows
                        collect
                        (cons date (calfw-blocks-render-all-day-events
                                lines cell-width (1- cell-height))))
        with all-day-columns-height = (seq-max (mapcar 'length breaked-all-day-columns))
             with breaked-all-day-rows-padded =
             (calfw-blocks--pad-and-transpose breaked-all-day-columns)
        for i from 1 below all-day-columns-height do
          (insert (cfw:render-left time-width ""))
             for row = (car breaked-all-day-rows-padded)
             for date = (caar breaked-all-day-columns)
             do
             (setq breaked-all-day-rows-padded (cdr breaked-all-day-rows-padded))
             (setq breaked-all-day-columns (cdr breaked-all-day-columns))
             (cl-loop for cell in row
                      for cell-span = (or (get-text-property 0 'cfw:cell-span cell) 1)
                do
                (insert
                 (if (and calfw-blocks-show-time-grid
                          calfw-blocks-time-grid-lines-on-top
                          (= (mod (1- i) calfw-blocks-lines-per-hour) 0)
                                (string= cell (make-string cell-width ?-))
                          (not (eq date earliest-date)))
                     ?-
                   VL)
                 (cfw:tp
                     (cfw:render-separator
                         (cfw:render-left (+ (1- cell-span)
                                             (* cell-span cell-width))
                                          (and cell (format "%s" cell))))
                     'cfw:date date)))
          (insert VL EOL))

    (cl-loop with breaked-day-columns =
          (cl-loop for day-rows in day-columns
                for (date ants . lines) = day-rows
                collect
                (cons date (calfw-blocks-render-event-blocks
                            lines cell-width (1- cell-height))))
             with curr-time-linum = (calfw-blocks--current-time-vertical-position)
          with time-columns = (calfw-blocks-time-column time-width cell-height)
             for i from 1 below cell-height
             for final-line = (apply #'concat
                                     (append
                                      (list
                                       (let ((time (nth (1- i) time-columns)))
                                         (when (= (1- i) curr-time-linum)
                                           (setq time (calfw-blocks-format-time
                                                       (let ((curr-time (decode-time (current-time))))
                                                         (list (nth 2 curr-time)
                                                               (nth 1 curr-time)))))
                                           (add-face-text-property
                                            0 (length time) 'cfw:face-today-title
                                            t time)
                                           )
                                         (cfw:render-left time-width time))
                                       )
          (cl-loop for day-rows in breaked-day-columns
                for date = (car day-rows)
                for row = (nth i day-rows)
                                               collect
                                               (concat
                 (if (and calfw-blocks-show-time-grid
                          calfw-blocks-time-grid-lines-on-top
                          (= (mod (1- i) calfw-blocks-lines-per-hour) 0)
                          (string= row (make-string cell-width ?-))
                          (not (eq date earliest-date)))
                     ?-
                                                  (if (and
                                                       calfw-blocks-show-current-time-indicator
                                                       (= (1- i) curr-time-linum)
                                                       (equal date (calendar-current-date)))
                                                      (propertize "@"
                                                                  'face
                                                                  'cfw:face-today-title)
                                                    VL))
                 (cfw:tp
                  (cfw:render-separator
                   (cfw:render-left cell-width (and row (format "%s" row))))
                  'cfw:date date)))
                                      (list VL EOL)))
             do
             (when (and calfw-blocks-show-current-time-indicator
                        (= (1- i) curr-time-linum))
               (add-face-text-property
                time-width (length final-line) 'calfw-blocks-now-indicator
                t final-line))
             (insert final-line))
    (insert cline)))

(defun calfw-blocks--pad-and-transpose (columns)
  "Returns the columns as rows, padded with space when needed"
  (let* ((max-len (apply 'max (mapcar 'length columns)))
         (col-count (length columns)))
    (cl-loop for i from 0 below (1- max-len)  ;; Loop over rows
             for filled-cells = 0
             for rows = (cl-loop
                         for c in columns
                         for cols = (cdr c)
                         when (and cols (< filled-cells col-count))
                         collect
                         (let* ((col (car cols))
                                (on-row (eq i
                                            (or (get-text-property
                                                 0 'cfw:row-count col)
                                                i)))
                                (cell-span (or (get-text-property
                                                0 'cfw:cell-span col)
                                               1)))
                           (if on-row
                               (progn
                                 (setq filled-cells (+ filled-cells
                                                       cell-span))
                                 (setcdr c (cdr cols))
                                 col)
                             (setq filled-cells (+ filled-cells 1))
                             " ")))
             collect
             (append rows (make-list (- col-count filled-cells) "")))))


;; Interval helper functions

(defun calfw-blocks--interval-member? (elem a)
  "Return t iff ELEM is within interval A."
  (and (< elem (cadr a)) (>= elem (car a))))

(defun calfw-blocks--interval-intersect? (a b)
  "Return t iff intervals A and B intersect.
Return nil otherwise. Ain interval [a1, a2) is represented as a
list (a1 a2)."
  (or (calfw-blocks--interval-member? (car a) b)
      (calfw-blocks--interval-member? (car b) a)))

(defun calfw-blocks--interval-intersection (a b)
  "Compute intersection of intervals A and B.
An interval [a1, a2) is represented as a list (a1 a2).
Return nil if intersection is empty."
  (if (calfw-blocks--interval-intersect? a b)
      (let ((start (max (car a) (car b)))
            (end (min (cadr a) (cadr b))))
        (list start end))))

(defun calfw-blocks--interval-subtract-many (a lst)
  "Return interval(s) resulting from removing intervals in LST from A.
Assume that all intervals in lst are disjoint and subsets of A."
  (let* ((lst (seq-sort (lambda (x y) (< (car x) (car y))) lst))
        (endpoints (append (list (car a))
                          (cfw:flatten lst)
                          (list (cadr a))))
        (intervals nil))
    (while endpoints
      (let ((start (pop endpoints))
            (end (pop endpoints)))
        (if (< start end)
            (push (list start end) intervals))))
    (reverse intervals)))

(defun calfw-blocks--interval-distribute (lst n)
  "Return N intervals of approx equal size, whose union is the union of LST."
  (let* ((lst (seq-sort (lambda (x y) (> (- (cadr x) (car x))
                                         (- (cadr y) (car y)))) lst))
         (lst-lengths (mapcar (lambda (x) (- (cadr x) (car x))) lst))
         (avg-len (/ (seq-reduce '+ lst-lengths 0) (float n)))
         (intervals nil))
    (while (and lst (< (length intervals) n))
      (let* ((l (pop lst))
             (l-len (float (- (cadr l) (car l))))
             (num-intervals
              (if (not lst)
                  (- n (length intervals))
                (min (round (/ l-len avg-len))
                     (- n (length intervals)))))
             (l-inner-len (/ l-len num-intervals)))
        ;; (print num-intervals)
        (dolist (i (number-sequence 0 (1- num-intervals)))
          (let ((start
                 (+ (car l)
                    (if (= i 0)
                        (floor (* i l-inner-len))
                      (round (* i l-inner-len)))))
                (end
                 (+ (car l)
                    (if (= i (1- num-intervals))
                        (ceiling (* (1+ i) l-inner-len))
                      (round (* (1+ i) l-inner-len))))))
            (push (list start end) intervals)))))
    (seq-sort (lambda (a b) (< (car a) (car b))) intervals)))

;; Calculate block sizes and positions

(defun calfw-blocks--get-intersection-groups (lines-lst)
  "Return a list of groups of overlapping events in LINES-LST.

Each group is a cons pair of the form (intersection . indices).
The first element is the intersection of all vertical position
intervals of the events in the group, represented as a list
containing the start and end of the interval. The second element
is a list of indices (into the original list LINES-LST)
corresponding to the elements of the group."
  (let* ((groups '())
         (prev-line-indices '()))
    (dotimes (i (length lines-lst))
      (let* ((i-interval (nth 1 (nth i lines-lst)))
             (i-intersects-indices '())
             (in-prev-group nil)
             (i-group (cons i-interval (list i))))
        (dolist (g groups)
          (when (and (calfw-blocks--interval-intersect? (car g) i-interval)
                     (not (member i (cdr g))))
            (dolist (elem (cdr g)) (push elem i-intersects-indices))
            (setcdr g (cons i (cdr g)))
            (setcar g (calfw-blocks--interval-intersection (car g) i-interval))))
        (dolist (j prev-line-indices)
          (let ((j-interval (nth 1 (nth j lines-lst))))
            (when (and (not (member j i-intersects-indices))
                       (calfw-blocks--interval-intersect? i-interval j-interval))
              (setcdr i-group (reverse (cons j (reverse (cdr i-group)))))
              (setcar i-group (calfw-blocks--interval-intersection (car i-group) j-interval))
              )))
        (if (not (and i-intersects-indices
                      (= 1 (length (cdr i-group)))))
            (push i-group groups)))
      (push i prev-line-indices))
    (seq-sort (lambda (a b)
                (let ((a-size (length (cdr a)))
                      (b-size (length (cdr b))))
                  (if (= a-size b-size)
                      (< (caar a) (caar b))
                    (> a-size b-size))))
              groups)))


(defun calfw-blocks--get-block-positions (lines cell-width)
  "Return LINES with assigned vertical and horizontal positions.

Each element of the list is a list (event vertical-pos
horizontal-pos). The vertical-pos and horizontal-pos are both
half open intervals represented as two element lists, containing
the start (inclusive) and the end (exclusive). The vertical-pos
is in unit lines, the horizontal-pos is in unit characters. Both
positions are given relative to the calendar cell the event
resides in, which has width CELL-WIDTH.

The vertical positions represent the time of day the event
occurs. The horizontal positions are assigned to display
concurrent events side by side. If there is not enough space for
all blocks to have width at least `calfw-blocks-min-block-width'
then some events are not displayed, and an indicator for how many
events are not displayed is shown."
  (let* ((lines-lst
          (seq-sort (lambda (x y) (>= (car (nth 1 x))
                                      (car (nth 1 y))))
                    (mapcar (lambda (x) (list x (calfw-blocks--get-block-vertical-position x))) lines)))
         (groups (calfw-blocks--get-intersection-groups lines-lst))
         (new-lines-lst nil)
         (added-indices nil))
    (dolist (g groups)
      (let* ((taken-intervals (seq-map (lambda (x) (nth 2 (cdr x)))
                                       (seq-filter (lambda (x)
                                                     (and (member (car x) added-indices)
                                                          (calfw-blocks--interval-intersect? (nth 1 (cdr x)) (car g))))
                                                   new-lines-lst)))
             (lines-left-in-group (- (length (cdr g)) (length taken-intervals)))
             (remaining-intervals (calfw-blocks--interval-subtract-many `(0 ,cell-width) taken-intervals))
             (remaining-intervals-length (cl-reduce '+ (mapcar (lambda (x) (- (cadr x) (car x))) remaining-intervals)))
             (exceeded-cell-width (and (> lines-left-in-group 0)
                                       (< (/ remaining-intervals-length lines-left-in-group) calfw-blocks-min-block-width)))
             (truncated-lines-left-in-group (if exceeded-cell-width (floor (/ remaining-intervals-length calfw-blocks-min-block-width))
                                              lines-left-in-group))
             (lines-left-out (- lines-left-in-group truncated-lines-left-in-group))
             (distributed-intervals (calfw-blocks--interval-distribute remaining-intervals truncated-lines-left-in-group)))
        (dolist (x (cdr g))
          (when (not (member x added-indices))
            (when (and exceeded-cell-width
                       (= (length distributed-intervals) 1))
              (let* ((x-vertical-pos (nth 1 (nth x lines-lst)))
                     (exceeded-indicator (list (propertize (format "+%dmore" lines-left-out) 'calfw-blocks-exceeded-indicator t)
                                               (list (nth 0 x-vertical-pos)
                                                     (max 4 (nth 1 x-vertical-pos)))
                                               (pop distributed-intervals))))
                (push (cons -1 exceeded-indicator) new-lines-lst)))
            (if (= 0 (length distributed-intervals))
                (push x added-indices)
              (let* ((new-line (append (nth x lines-lst) (list (pop distributed-intervals)))))
                (push (cons x new-line) new-lines-lst)
                (push x added-indices)))))))
    (mapcar 'cdr new-lines-lst)))


(defun calfw-blocks-round-start-time (time)
  (floor time))

(defun calfw-blocks-round-end-time (time)
  (ceiling time))

(defun calfw-blocks-hours-per-line ()
  (/ 1.0 calfw-blocks-lines-per-hour))

(defun calfw-blocks--time-pair-to-float (p)
  (+ (car p) (/ (cadr p) 60.0)))

(defun calfw-blocks--get-float-time-interval (line)
  (let* ((interval (get-text-property 0 'calfw-blocks-interval line))
         (start (calfw-blocks--time-pair-to-float (car interval)))
         (end (calfw-blocks--time-pair-to-float (cdr interval))))
    (list start end)))

(defun calfw-blocks--get-block-vertical-position (p)
  "[inclusive, exclusive)"
  (let* ((float-interval (calfw-blocks--get-float-time-interval p))
        (start-time (calfw-blocks--time-pair-to-float calfw-blocks-earliest-visible-time))
        (minutes-per-line (/ 60 calfw-blocks-lines-per-hour))
        (interval-start (car float-interval))
        (interval-end (if (= interval-start (cadr float-interval)) (+ calfw-blocks-default-event-length interval-start) (cadr float-interval))))
  (list (calfw-blocks-round-start-time (* calfw-blocks-lines-per-hour (- interval-start start-time)))
  (calfw-blocks-round-end-time (* calfw-blocks-lines-per-hour (- interval-end start-time))))))

(defun calfw-blocks--current-time-vertical-position ()
  (let* ((start-time (calfw-blocks--time-pair-to-float calfw-blocks-earliest-visible-time))
         (curr-time (decode-time (current-time)))
         (curr-hour (nth 2 curr-time))
         (curr-min (nth 1 curr-time))
         (time-float (+ curr-hour (/ curr-min 60.0))))
  (calfw-blocks-round-start-time (* calfw-blocks-lines-per-hour (- time-float start-time)))))

(defun calfw-blocks-generalized-substring (s start end)
  (cond ((<= end (length s)) (substring s start end))
        ((< start (length s)) (concat (substring s start (length s))
                                      (make-string (- (- end start) (- (length s) start)) ? )))
        (t (make-string (- end start) ? ))))


(defun calfw-blocks--wrap-text (width &optional word-break)
  (goto-char (point-min))
  (while (< (point) (point-max))
    (let* ((forward-by (min width (- (point-max) (point))))
           (counter forward-by)
           (word-break (or word-break "-"))
           prev-point
           is-whitespace
           (whitespaces '(?\s ?\t ?\n))
           c)
      (forward-char forward-by)
      (setq prev-point (point))
      (unless (eobp)
        (if (or (null (setq c (char-after)))
                (member c whitespaces))
            (progn (when c
                     (forward-char))
                   (setq is-whitespace t))
          (while (not (or (= (setq counter (1- counter)) 0)
                          (null (setq c (char-before)))
                          (setq is-whitespace
                                (member c whitespaces))))
            (backward-char)))
        ;; If not whitespace, but the character immediately after is then just
        ;; advance
        (when (and is-whitespace
                   (> (- prev-point (point)) 1))
          ;; Find if next space is within width
          ;; Otherwise, might as well hard-break at this
          ;; point
          (let* ((pt (point))
                 (forward-by (min width (- (point-max) (point))))
                 (counter forward-by)
                 is-white)
            (forward-char)
            (while (not (or (= (setq counter (1- counter)) 0)
                            (null (setq c (char-before)))
                            (setq is-white
                                  (member c whitespaces))))
              (forward-char))
            (if (or (eobp) is-white)
                (goto-char pt) ;; It's fine to break there
              ;; Just break the word
              (goto-char (1- prev-point))
              (setq is-whitespace nil))))
        ;;
        (if is-whitespace
            (delete-char -1)
          ;; hard break
          (goto-char (1- prev-point))
          (insert word-break))
        (insert ?\n)))))

(defun calfw-blocks--wrap-string (text width &optional word-break)
  "Wrap TEXT to a fixed WIDTH without breaking words."
  ;; Doesn't work with \n in text
  (with-temp-buffer
    (insert text)
    (calfw-blocks--wrap-text width word-break)
    (buffer-string)))

(defun calfw-blocks--to-lines (text max-lines)
  "Wrap TEXT to a fixed WIDTH without breaking words."
  ;; Doesn't work with \n in text
  (let ((lines (string-split text "\n"))
        last-line ellips)
    (when (> (length lines) max-lines)
      (setq ellips (truncate-string-ellipsis)
            last-line (nth max-lines lines))
      (setf (nth max-lines lines)
            (concat
             (substring
              last-line
              0
              (- (length last-line) (length ellips)))
             ellips)))
    lines))

(defun calfw-blocks--remove-unicode-chars (str)
  "Remove any Unicode characters from INPUT-STRING."
  (cl-loop
   for char across str
   if (and (>= char 16) (<= char 127))
   concat (string char)))

(defun calfw-blocks-split-single-block (block cell-width)
  "Split event BLOCK into lines of width CELL-WIDTH.

BLOCK is expected to contain elements of the form (event
vertical-pos horizontal-pos). Event is a string, vertical-pos and
horizontal-pos are two element lists representing half open
intervals. See `calfw-blocks--get-block-positions' for more
details about vertical-pos and horizontal-pos.

An overline is added to the first line of an event block. A character
is added at the beginning of a block to indicate it is the beginning."
  (let* ((block-string (car block))
        (block-vertical-pos (cadr block))
        (block-horizontal-pos (caddr block))
        (block-width (- (cadr block-horizontal-pos) (car block-horizontal-pos)))
        (block-height (- (cadr block-vertical-pos) (car block-vertical-pos)))
        (end-of-cell (= (cadr block-horizontal-pos) cell-width))
        (is-beginning-of-cell (= (car block-horizontal-pos) 0))
        (block-width-adjusted (if is-beginning-of-cell block-width (+ -1 block-width)))
         (block-lines (calfw-blocks--to-lines
                       (calfw-blocks--wrap-string (calfw-blocks--remove-unicode-chars
                                                   block-string)
                                                  block-width-adjusted)
                       (- block-height 1)))
        (rendered-block '())
         (is-exceeded-indicator (get-text-property 0 'calfw-blocks-exceeded-indicator block-string))
         (source-clr (cfw:source-color (get-text-property 0 'cfw:source block-string))))
    (dolist (i (number-sequence 0 (- block-height 1)))
      (push (list (+ (car block-vertical-pos) i)
                  (propertize (concat ;;TODO some parts of the string won't inherit the properties of the event
                               ;; might cause issues with org goto/navigation/etc?
                               (when (not is-beginning-of-cell) "|" );;(if (= i 0) "*" "|"))
                               ;; (if (= i 0) ">")
                               ;; (calfw-blocks-generalized-substring block-string (* i block-width-adjusted)
                               ;;                                     (* (1+ i) block-width-adjusted))
                               (calfw-blocks-generalized-substring
                                (car block-lines) 0 block-width-adjusted)
                                                                   ;; (- (* (1+ i) block-width-adjusted)
                                                                   ;;    (if (= i 0) 1 0)))
                               ;; (when (not end-of-cell) "|")
                               ;; (when (not end-of-cell) " " );;(if (= i 0) "*" "|"))
                               )
                              'face
                              (seq-filter
                               'identity ;; filter out nil's
                               (cons
                                (cfw:render-get-face-content
                                 block-string
                                 'cfw:face-default-content)
                                          (list
                                           (when is-exceeded-indicator 'italic)
                                 (when (= i 0) 'calfw-blocks-overline))))
                              'calfw-blocks-horizontal-pos block-horizontal-pos))
            rendered-block)
      (setq block-lines (cdr block-lines)))
    (reverse rendered-block)))

(defun calfw-blocks-render-all-day-events (lines cell-width cell-height)
  (let ((all-day-lines (seq-filter (lambda (line)
                                     (not (car (get-text-property 0 'calfw-blocks-interval
                                                                       line))))
                                     lines))
        (cfw:render-line-breaker (if (eq calfw-blocks-render-multiday-events 'cont)
                                     (lambda (x &rest _) (list x))
                                   'cfw:render-line-breaker-wordwrap)))
        (cfw:render-break-lines all-day-lines cell-width cell-height)))

(defun calfw-blocks-render-event-blocks (lines cell-width cell-height)
  ""
  (let* ((interval-lines (seq-filter (lambda (line) (car (get-text-property 0 'calfw-blocks-interval
                                                                       line)))
                                     lines))
         (all-day-lines (seq-filter (lambda (line) (not (car (get-text-property 0 'calfw-blocks-interval
                                                                       line))))
                                     lines))
         (block-positions (calfw-blocks--get-block-positions interval-lines cell-width))
         (split-blocks (seq-sort (lambda (a b) (< (car a) (car b)))
                                 (mapcan (lambda (bf) (calfw-blocks-split-single-block bf cell-width))
                                         block-positions)))
         (rendered-lines '())
         (curr-time-grid-line (calfw-blocks--current-time-vertical-position)))
    (dolist (i (number-sequence 0 (1- cell-height)))
      (let ((make-time-grid-line (and calfw-blocks-show-time-grid
                                      (= (mod i calfw-blocks-lines-per-hour) 0)))
            (current-line-lst '()))
        (if (or (not split-blocks) (< i (caar split-blocks)))
            (if make-time-grid-line
                (push (propertize (make-string cell-width ? ) 'face 'calfw-blocks-overline) current-line-lst)
              (push (make-string cell-width ? ) current-line-lst))
          (while (and split-blocks (= i (caar split-blocks)))
            (push (cadr (pop split-blocks)) current-line-lst))
          (setq current-line-lst (calfw-blocks--pad-block-line
                                  current-line-lst cell-width make-time-grid-line)))
        (push (apply 'concat current-line-lst)
              rendered-lines)))
    (reverse rendered-lines)))

(defun calfw-blocks--pad-block-line (block-line cell-width make-time-grid-line)
  (let* ((block-line (seq-sort (lambda (a b) (< (car (get-text-property 0 'calfw-blocks-horizontal-pos a))
                                (car (get-text-property 0 'calfw-blocks-horizontal-pos b))))
                              block-line))
         (padded-line '())
         (prev-end 0))
    (dolist (segment block-line (progn
                                  (if make-time-grid-line
                                  (push (calfw-blocks--grid-line (- cell-width prev-end)) padded-line))
                                  (reverse padded-line) ))
      (let* ((horizontal-pos (get-text-property 0 'calfw-blocks-horizontal-pos segment))
            (start (car horizontal-pos))
            (end (cadr horizontal-pos)))
        (if (< prev-end start)
            (push (if make-time-grid-line
                      (calfw-blocks--grid-line (- start prev-end))
                      (make-string (- start prev-end) ? ))
                  padded-line))
        (push segment padded-line)
        (setq prev-end end)))))

(defun calfw-blocks--grid-line (n)
  (propertize (make-string n ? ) 'face 'calfw-blocks-overline))

(defun calfw-blocks-superimpose-face (text face)
  (propertize text 'face (list (get-text-property 0 'face text) face)))

(defun calfw-blocks-dest-ol-today-set (dest)
  "[internal] Put a highlight face on today."
  (let ((ols))
    (cfw:dest-with-region dest
      (cfw:find-all-by-date
       dest (calendar-current-date)
       (lambda (begin end)
         (let ((overlay (make-overlay begin end)))
           ;; This is the only difference
           (if (eq 'cfw:face-day-title
                   (get-text-property begin 'face))
               (overlay-put overlay 'face
                            'cfw:face-today-title))
           (push overlay ols)))))
    (setf (cfw:dest-today-ol dest) ols)))

(defmacro calfw-blocks-perserve-buffer-view (buf &rest body)
  (declare (indent 1) (debug t))
  `(let* ((wind (get-buffer-window buf))
          (prev-point (point))
          (prev-window-start (when wind (window-start))))
     (unwind-protect
         (progn
           ,@body)
       (goto-char prev-point)
       (when wind
         (set-window-start wind prev-window-start)))))

(defun calfw-blocks-cfw:cp-update (component &optional initial-date)
  "[internal] Clear and re-draw the component content."
  (let* ((buf (cfw:cp-get-buffer component))
         (dest (cfw:component-dest component)))
    (with-current-buffer buf
      (cfw:dest-before-update dest)
      (cfw:dest-ol-today-clear dest)
      (let* ((buffer-read-only nil))
        (calfw-blocks-perserve-buffer-view buf
        (cfw:dest-with-region dest
                              (cfw:dest-clear dest)
                              (funcall (cfw:cp-dispatch-view-impl
                                        (cfw:component-view component))
                     component))))
      (if (eq (cfw:component-view component) 'block-week)
          (calfw-blocks-dest-ol-today-set dest)
        (when cfw:highlight-today (cfw:dest-ol-today-set dest)))
      (when initial-date
        (cfw:cp-goto-date component initial-date))
      (cfw:dest-after-update dest)
      (cfw:cp-fire-update-hooks component))))

(cl-defun calfw-blocks-scroll-to-initial-visible-time (&key date buffer custom-map contents-sources annotation-sources view sorter)
  (when (string-match-p "block" (symbol-name view))
    (scroll-up (floor (* calfw-blocks-lines-per-hour
                       (calfw-blocks--time-pair-to-float calfw-blocks-initial-visible-time))))))

(cl-defun calfw-blocks-scroll-to-initial-visible-time-after-update (component)
  (let ((buf (cfw:cp-get-buffer component))
        (view (cfw:component-view component)))
    (when (and buf
               (get-buffer-window buf)
               (string-match-p "block" (symbol-name view)))
      (with-selected-window (get-buffer-window buf)
        (scroll-up (floor (* calfw-blocks-lines-per-hour
                             (calfw-blocks--time-pair-to-float calfw-blocks-initial-visible-time))))))))

;;; calfw-org
(defun calfw-blocks-cfw:org-get-timerange (text)
  "Return a range object (begin end text).
If TEXT does not have a range, return nil."
  ;; TODO: This is exactly the same as `cfw:org-get-timerange' except that
  ;; it fixed cur-day = 0
  (let* ((dotime (cfw:org-tp text 'dotime)))
    (and (stringp dotime) (string-match org-ts-regexp dotime)
         (let ((date-string  (match-string 1 dotime))
               (extra (cfw:org-tp text 'extra)))
           (if (and extra (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra))
               (let* ((cur-day (string-to-number
                                (match-string 1 extra)))
                      (total-days (string-to-number
                                   (match-string 2 extra)))
                      (start-date (org-read-date nil t date-string))
                      (end-date (time-add
                                 start-date
                                 (seconds-to-time (* 3600 24 (- total-days 1))))))
                 ;; (unless (= cur-day total-days)
                 (list (calendar-gregorian-from-absolute (time-to-days start-date))
                       (calendar-gregorian-from-absolute (time-to-days end-date)) text)))
           ))))

(defun calfw-blocks-org-summary-format (item)
  "Version of cfw:org-summary-format that adds time data needed to draw blocks."
  (let* ((time (cfw:org-tp item 'time))
         (time-of-day (cfw:org-tp item 'time-of-day))
         (start-time (if time-of-day (list (/ time-of-day 100) (mod time-of-day 100))))
         (duration (cfw:org-tp item 'duration))
         (end-time (if (and start-time duration) (list (+ (nth 0 start-time) (/ duration 60))
                                                      (+ (nth 1 start-time) (mod duration 60)))
                     start-time))
         (date (cfw:org-tp item 'date))
         (time-str (and time-of-day
                        (format "%02i:%02i " (/ time-of-day 100) (% time-of-day 100))))
         (end-time-str (and end-time
                        (format "%02i:%02i " (nth 0 end-time) (nth 1 end-time))))
         (category (cfw:org-tp item 'org-category))
         (tags (cfw:org-tp item 'tags))
         (marker (cfw:org-tp item 'org-marker))
         (buffer (and marker (marker-buffer marker)))
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
    (if (string-match org-bracket-link-regexp text)
      (let* ((desc (if (match-end 3) (org-match-string-no-properties 3 text)))
             (link (org-link-unescape (org-match-string-no-properties 1 text)))
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
;;                      start-time))
;;          (end-time-str (and end-time
;;                             (format "%02i:%02i " (nth 0 end-time) (nth 1 end-time)))))
;;     (propertize
;;      (funcall old-fn item)
;;      'calfw-blocks-interval (if start-time (cons start-time end-time)))))


;; TODO: This should be done once only
(setq
 cfw:cp-dipatch-funcs
 (append
  cfw:cp-dipatch-funcs
  '((block-week        .  calfw-blocks-view-block-week)
    (block-day         .  calfw-blocks-view-block-day)
    (block-2-day       .  calfw-blocks-view-block-2-day)
    (block-3-day       .  calfw-blocks-view-block-3-day)
    (block-4-day       .  calfw-blocks-view-block-4-day)
    (block-5-day       .  calfw-blocks-view-block-5-day)
    (block-7-day       .  calfw-blocks-view-block-7-day)
    (transpose-8-day   .  calfw-blocks-view-transpose-8-day)
    (transpose-10-day  .  calfw-blocks-view-transpose-10-day)
    (transpose-12-day  .  calfw-blocks-view-transpose-12-day)
    (transpose-14-day  .  calfw-blocks-view-transpose-14-day)
    (transpose-two-weeks    . calfw-blocks-view-transpose-two-weeks))))

(advice-add 'cfw:open-calendar-buffer
            :after 'calfw-blocks-scroll-to-initial-visible-time)
(advice-add 'cfw:render-toolbar
            :override 'calfw-blocks-render-toolbar)
(advice-add 'cfw:cp-update
            :override 'calfw-blocks-cfw:cp-update)

;;; calfw-org
(advice-add 'cfw:org-get-timerange
            :override 'calfw-blocks-cfw:org-get-timerange)
(setq cfw:org-schedule-summary-transformer 'calfw-blocks-org-summary-format)

(provide 'calfw-blocks)
;;; calfw-blocks.el ends here
