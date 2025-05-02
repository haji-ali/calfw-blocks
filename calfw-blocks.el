;;; calfw-blocks.el --- A block view for calfw  -*- lexical-binding: t; -*-

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

(require 'calfw)
(require 'mule-util)  ;; for truncate-string-ellipsis


(defcustom calfw-blocks-fchar-bottom-right-corner cfw:fchar-right-junction
  "The character used for drawing the bottom-right corner."
  :group 'calfw-blocks
  :type 'character)

(defcustom calfw-blocks-fchar-bottom-left-corner cfw:fchar-left-junction
  "The character used for drawing the bottom-left corner."
  :group 'calfw-blocks
  :type 'character)

(defcustom calfw-blocks-fchar-bottom-junction cfw:fchar-junction
  "The character used for drawing junction lines at the bottom side."
  :group 'calfw-blocks
  :type 'character)

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

(defcustom calfw-blocks-render-multiday-events 'cont
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
  "Whether or not to display end times in blocks."
  :group 'calfw-blocks
  :type 'boolean)

(defcustom calfw-blocks-nonshrinking-hours '(9 . 17)
  "Which hours to never shrink. If nil, shrink all hours.
Cons of (START . END), inclusive."
  :group 'calfw-blocks
  :type 'list)

(defcustom calfw-blocks-hour-shrink-size 1
  "How many lines to leave when shrinking an hour.
An integer, up to `calfw-blocks-lines-per-hour'.
If nil or equal `calfw-blocks-lines-per-hour', do not shrink
hours."
  :group 'calfw-blocks
  :type 'list)

(defvar calfw-blocks-event-start (char-to-string cfw:fchar-vertical-line)
  "String to add at beginning of event, if not on cell start.")

(defvar calfw-blocks-event-keymap nil)

(defvar calfw-blocks-earliest-visible-time '(0 0)
  "Earliest visible time in a day as list (hours minutes).")


(defvar calfw-blocks-posframe-buffer " *cfw-calendar-sticky*")
(defvar-local calfw-blocks-header-line-string nil)

(defvar-local calfw-blocks-days-per-view nil
  "Number of days being in a view.")

;; Faces
(defface calfw-blocks-overline
  '((t (:overline t)))
  "Basic face for overline."
  :group 'calfw-blocks)

(defface calfw-blocks-underline
  '((t (:underline t)))
  "Basic face for underline."
  :group 'calfw-blocks)

(defface calfw-blocks-now-indicator
  '((t (:strike-through "red")))
  "Face for current time indicator."
  :group 'calfw-blocks)

(defface calfw-blocks-cancelled-event
  '((t (:strike-through t)))
  "Face for cancelled events."
  :group 'calfw-blocks)

(defface calfw-blocks-cancelled-event-bg
  '((t (:foreground "#777a7d" :background "#393c41")))
  "Face for cancelled events."
  :group 'calfw-blocks)

(defface calfw-blocks-time-column
  '((t))
  "Face for time column. Also used to identify the column."
  :group 'calfw-blocks)

(defface calfw-blocks-time-column-now
  '((t (:inherit cfw:face-today-title)))
  "Face for time column. Also used to identify the column."
  :group 'calfw-blocks)



;; Block views

(defun calfw-blocks-view-block-nday-week-model (n model)
  "[internal] Create a logical view model of weekly calendar.
This function collects and arranges contents.  This function does
not know how to display the contents in the destinations."
  (let* ((init-date (cfw:k 'init-date model))
         (begin-date (cfw:date-before init-date
                                      (mod (cfw:days-diff
                                            (cfw:emacs-to-calendar
                                             (current-time))
                                            init-date)
                                           n)))
         (end-date (cfw:date-after begin-date (1- n))))
    (calfw-blocks-view-model-make-common-data-for-nday-weeks n model
                                                             begin-date
                                                             end-date)))

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
       ;; (win-height (max 15 (- (cfw:dest-height dest) 10)))
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


(defun calfw-blocks-view-block-week (component)
  (calfw-blocks-view-block-nday-week 7 component
                                     (cfw:view-week-model
                                      (cfw:component-model component))))

;; Rendering views

(defun calfw-blocks-view-block-nday-week (n component &optional model)
  "[internal] Render weekly calendar view."
  (let* ((dest (cfw:component-dest component))
         (param (cfw:render-append-parts
                 (calfw-blocks-view-nday-week-calc-param n dest)))
         (total-width (cfw:k 'total-width param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         (hline (concat time-hline (cfw:k 'hline param)))
         (cline (concat time-hline (cfw:k 'cline param)))
         (model (if model
                    model
                  (calfw-blocks-view-block-nday-week-model
                   n (cfw:component-model component))))
         (begin-date (cfw:k 'begin-date model))
         (end-date (cfw:k 'end-date model))
         day-of-week-names)
    (setq-local calfw-blocks-days-per-view n)
    ;; (print model)
    ;; update model
    (setf (cfw:component-model component) model)
    (setq day-of-week-names
          (cl-loop
           with date = begin-date
           for i in (cfw:k 'headers model)
           with VL = (cfw:k 'vl param) with cell-width = (cfw:k 'cell-width param)
           concat
           (concat VL (cfw:tp
                       (cfw:render-center
                        cell-width
                        (concat
                         (cfw:rt
                          (calendar-day-name date t)
                          (cfw:render-get-week-face i 'cfw:face-header))
                         " "
                         (cfw:rt
                          (format "%s%d"
                                  (if (= (calendar-extract-month date)
                                         (calendar-extract-month begin-date))
                                      ""
                                    (format "%d/" (calendar-extract-month date)))
                                  (calendar-extract-day date))
                          (if (equal (calendar-current-date) date)
                              'cfw:face-today-title
                            (cfw:render-get-week-face i 'cfw:face-header)))))
                       'cfw:date date))
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
     EOL (calfw-blocks-render-toolbar total-width
                                      (cfw:component-view component)
                                      (calfw-blocks-navi-previous-nday-week-command n)
                                      (calfw-blocks-navi-next-nday-week-command n))
     EOL hline)
    ;; time header
    (insert (cfw:rt (cfw:render-right time-width "    ")
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


(defun calfw-blocks-compare-time (t1 t2)
  "Return +1 if T2>T1, -1 if not and 0 if equal."
  (if (equal t1 t2) 0
    (cl-destructuring-bind ((a1 b1) (a2 b2)) (list t1 t2)
      (if (or (< a1 a2)
              (and (= a1 a2) (< b1 b2)))
          +1
        -1))))

(defun calfw-blocks--next-prop (pt prop dir)
  "Get the next position where PROP is set.
Start at PT and go in DIR. Returns a cons of the point and the
value of the property."
  (let ((search-fn (if (> dir 0)
                       #'next-single-property-change
                     #'previous-single-property-change))
        prop-value)
    (while (and pt
                (null
                 (setq prop-value
                       (get-text-property pt prop))))
      (setq pt (funcall search-fn pt prop)))
    (when pt
      (cons pt prop-value))))

(defun calfw-blocks-navi-goto-next-event (dir)
  "Move the cursor forward to next event if visible.
DIR has to be either 1 or -1 (for previous instead of next)."
  (interactive (list 1))
  ;; Get current date and time from cursor.
  ;; Search for next block with cfw:event
  ;; If have the same date and future start time, stop
  ;; Otherwise continue until next.
  ;; If none found, then allow for future day.
  (cl-flet ((col-at-pt (lambda (pos) (save-excursion (goto-char pos)
                                                     (current-column)))))
    (let* ((search-fn (if (> dir 0)
                          #'next-single-property-change
                        #'previous-single-property-change))
           (reverse-search-fn (if (< dir 0)
                                  #'next-single-property-change
                                #'previous-single-property-change))
           (cmp-fn (if (> dir 0) #'> #'<))
           (time-fn (if (> dir 0) #'cfw:event-start-time #'cfw:event-end-time))
           (component (cfw:cp-get-component))
           (dest (cfw:component-dest component))
           (model (cfw:component-model component))
           ;;;
           (cur-pt (point))
           (point-ev (get-text-property cur-pt 'cfw:event))
           (point-date (when point-ev
                         (cfw:event-start-date point-ev)))
           search-for-periods
           ;;
           (continue t)
           time-cmp cur-ev point-col)
      ;; First move back to beginning of event
      (when-let (tmp (calfw-blocks--next-prop cur-pt 'cfw:event dir))
        (setq search-for-periods (get-text-property (car tmp) 'cfw:period)))

      (when (and (not search-for-periods)
                 point-ev
                 (eq point-ev (get-text-property (- cur-pt dir) 'cfw:event)))
        (setq cur-pt (funcall reverse-search-fn cur-pt 'cfw:event))
        (if (< dir 1)
            ;; Go back a smidge to have the same event
            (setq cur-pt (1- cur-pt))))

      (setq point-date
            (or point-date
                (cdr (calfw-blocks--next-prop cur-pt 'cfw:date dir))))

      (when (and point-date cur-pt)
        (setq point-col (col-at-pt cur-pt))
        (while continue
          (if (setq cur-pt
                    (funcall search-fn cur-pt 'cfw:event))
              (when (and (setq cur-ev (get-text-property cur-pt 'cfw:event))
                         (not (eq point-ev cur-ev))
                         (equal
                          point-date
                          (cfw:event-start-date cur-ev))
                         (eq (get-text-property cur-pt 'cfw:period)
                             search-for-periods))
                (when (or
                       search-for-periods
                       (null point-ev)
                       (funcall cmp-fn
                                (setq time-cmp (calfw-blocks-compare-time
                                                (funcall time-fn point-ev)
                                                (funcall time-fn cur-ev)))
                                0) ;; POINT is before CUR
                       (and (eq time-cmp 0)
                            (funcall cmp-fn (col-at-pt cur-pt) point-col)))
                  ;; Future and different event on the same day
                  ;; stop search.
                  (setq continue nil)))
            (setq point-date (cfw:date-after point-date dir))
            (if (cfw:cp-displayed-date-p component point-date)
                (progn
                  (setq cur-pt (calfw-blocks-find-by-date dest point-date dir)
                        point-ev nil
                        point-col (col-at-pt cur-pt))
                  ;; If already on event, then stop search
                  (when (and (get-text-property cur-pt 'cfw:event)
                             (eq search-for-periods
                                 (get-text-property cur-pt 'cfw:period)))
                    (setq continue nil)))
              ;; Next day is not visible, stop search?
              (cond
               ((and (< dir 0) (not search-for-periods))
                (setq search-for-periods t
                      point-date (cfw:k 'end-date model)
                      cur-pt (cfw:dest-point-max dest)
                      point-col (col-at-pt cur-pt)
                      point-ev nil))
               ((and (> dir 0) search-for-periods)
                (setq search-for-periods nil
                      point-date (cfw:k 'begin-date model)
                      cur-pt (cfw:dest-point-min dest)
                      point-col (col-at-pt cur-pt)
                      point-ev nil))
               (t
                (setq continue nil
                      cur-pt nil)))))))
      ;; If we have a pt, move to it
      (if (and cur-pt
               (get-text-property cur-pt 'cfw:event))
          (progn
            (goto-char cur-pt)
            (recenter))
        (user-error "No more events in view")))))

(defun calfw-blocks-find-by-date (dest date dir)
  "[internal] Return a point where the text property `cfw:date'
is equal to DATE in the current calender view. If DATE is not
found in the current view, return nil."
  (cl-loop
   with rev = (< dir 0)
   with pos = (funcall (if rev 'cfw:dest-point-max 'cfw:dest-point-min) dest)
   with end = (funcall (if rev 'cfw:dest-point-min 'cfw:dest-point-max) dest)
   for next = (funcall
               (if rev 'previous-single-property-change
                 'next-single-property-change)
               pos 'cfw:date nil end)
   for text-date = (and next (cfw:cursor-to-date next))
   while next do
   (if (and text-date (equal date text-date))
       (cl-return next))
   (setq pos next)))

(defun calfw-blocks-navi-goto-previous-event ()
  "Move the cursor forward to previous event if visible."
  (interactive)
  (calfw-blocks-navi-goto-next-event -1))

(defmacro calfw-blocks-perserve-buffer-view (&rest body)
  "Preserve view in current buffer before executing BODY.
View includes the point, the scroll position. If
`calfw-blocks-perserve-buffer-view' is bound and nil, then view
is not perserved."
  (declare (indent 1) (debug t))
  `(let* ((wind (get-buffer-window (current-buffer)))
          (prev-point (point))
          (prev-window-start (when wind (window-start wind))))
     (unwind-protect
         (progn
           ,@body)
       (goto-char prev-point)
       (when wind
         (set-window-start wind prev-window-start)))))

(defun calfw-blocks-perserve-buffer-view-advice (old-fn &rest args)
  (calfw-blocks-perserve-buffer-view
      (apply old-fn args)))

(defun calfw-blocks-navi-next-view-command (&optional num)
  "Move the cursor forward NUM of views. If NUM is nil, 1 is used.
Moves backward if NUM is negative."
  (interactive "p")
  (calfw-blocks-perserve-buffer-view
      (cfw:navi-next-day-command (* calfw-blocks-days-per-view
                                    (or num 1)))))

(defun calfw-blocks-navi-previous-view-command (&optional num)
  "Move the cursor back NUM of views. If NUM is nil, 1 is used.
Moves forward if NUM is negative."
  (interactive "p")
  (calfw-blocks-perserve-buffer-view
      (cfw:navi-next-day-command (* (- calfw-blocks-days-per-view)
                                    (or num 1)))))

(defun calfw-blocks--cfw-refresh-calendar-buffer (no-resize)
  "Clear the calendar and render again.
With prefix arg NO-RESIZE, don't fit calendar to window size."
  (interactive "P")
  (calfw-blocks-perserve-buffer-view
      (let ((cp (cfw:cp-get-component)))
        (when cp
          (unless no-resize
            (cfw:cp-resize cp (window-width) (window-height)))
          (cl-loop for s in (cfw:cp-get-contents-sources cp t)
                   for f = (cfw:source-update s)
                   if f do (funcall f))
          (cl-loop for s in (cfw:cp-get-annotation-sources cp)
                   for f = (cfw:source-update s)
                   if f do (funcall f))
          (cfw:cp-update cp)))))


(defun calfw-blocks-navi-goto-now ()
  "Move the cursor to today."
  (interactive)
  (cfw:navi-goto-date (cfw:emacs-to-calendar (current-time)))
  (goto-char (point-min))
  (text-property-search-forward 'cfw:current-time-marker t)
  (recenter))

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

(defvar calfw-blocks-toolbar-views
  '(("Day" . block-day)
    ("3-Day" . block-3-day)
    ("Week" . block-week)
    ("Two Week" . two-weeks)
    ("Month" . month)))

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
         (sp  " ")
         (toolbar-text
          (cfw:render-add-right
           width (concat sp prev sp next sp today sp)
           (cl-loop for (title . view) in calfw-blocks-toolbar-views
                    for fn = (let ((view view)) ;; Enable lexical-binding
                               (lambda ()
                                 (interactive)
                                 (cfw:cp-set-view (cfw:cp-get-component)
                                                  view)))
                    concat
                    (cfw:render-button
                     title
                     fn
                     (eq current-view view))
                    concat
                    sp))))
    (cfw:render-default-content-face toolbar-text 'cfw:face-toolbar)))

(let ((lexical-binding t))
  (cl-loop for a from 0 to 5
           collect (let ((a a))
                     (lambda nil a))))

(defun calfw-blocks-render-calendar-cells-block-weeks (model param title-func)
  "[internal] Insert calendar cells for week based views."
  (cl-loop for week in (cfw:k 'weeks model) do
           ;; (setq week (list (nth 4 week)))
           ;; (print week)
           (calfw-blocks-render-calendar-cells-days model param title-func week
                                                    'calfw-blocks-render-content
                                                    t)))

(defun calfw-blocks-default-sorter (x y)
  "Default sorter for events in calfw-blocks.
Using this function to sort by start time first."
  (if-let ((ev-x (get-text-property 0 'cfw:event x))
           (ev-y (get-text-property 0 'cfw:event y)))
      (> (calfw-blocks-compare-time (cfw:event-start-time ev-x)
                                    (cfw:event-start-time ev-y))
         0)
    (string-lessp x y)))

(defun calfw-blocks-render-default-content-face (str &optional default-face)
  "[internal] Put the default content face. If STR has some
faces, the faces are remained."
  (cl-loop
           with ret = (substring str 0)
   for i from 0 below (length str)
   for face = (or default-face
                           (calfw-blocks--status-face str 'background)
                           (cfw:render-get-face-content
                            str 'cfw:face-default-content))
           unless (get-text-property i 'face ret)
           do
           (put-text-property i (1+ i) 'face face ret)
           (put-text-property i (1+ i) 'font-lock-face face ret)
           finally return ret))

(defun calfw-blocks-model-get-contents-by-date (date model)
  "Return a list of contents on the DATE."
  ;; This is the same as `cfw:model-get-contents-by-date'
  ;; excepts that it returns content if they are overlap with DATE
  ;; rather than being precisely on DATE
  (let ((contents (cfw:k 'contents model)))
    (cdr
     (cond
      ((or (null date) (null contents)) nil)
      (t (cl-loop for i in contents
                  for start-date = (cfw:event-start-date (cadr i))
                  for end-date = (cfw:event-end-date (cadr i))
                  if (or
                      (and (null end-date) (equal date (car i)))
                      (cfw:date-between start-date end-date date))
                  return i
                  finally return nil))))))

(defun calfw-blocks-render-calendar-cells-days (model param title-func &optional
                                                      days content-fun do-weeks)
  "[internal] Insert calendar cells for the linear views."
  (let*
      ((min-hour (or (car-safe calfw-blocks-nonshrinking-hours) 25))
       (max-hour (or (cdr-safe calfw-blocks-nonshrinking-hours) -1))
       (day-columns
   (cl-loop with cell-width      = (cfw:k 'cell-width param)
                 ;; with show-times      = (make-vector 24 nil)
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
                 for cfw-contents = (calfw-blocks-model-get-contents-by-date
                                     ;; cfw:model-get-contents-by-date
                                     date model)
                 do (cl-loop for evnt in cfw-contents
                             for interval = (calfw-blocks-get-time-interval evnt)
                             ;; for start-hour = (caar interval)
                             ;; for end-hour = (cadr interval)
                             ;; for end-minute = (cddr interval)
                             do
                             (setq
                              min-hour (min min-hour (caar interval))
                              max-hour (max max-hour
                                            ;; If the minute is 0, substrct
                                            ;; from hour.
                                            (- (cadr interval)
                                               (if (= (caddr interval) 0) 1
                                                 0)))))
                 for raw-contents = (sort (funcall content-fun cfw-contents) sorter)
            for prs-contents = (append (if do-weeks
                                           (calfw-blocks-render-periods
                                            date raw-periods cell-width model)
                                         (calfw-blocks-render-periods-days
                                          date raw-periods cell-width))
                                       (mapcar 'calfw-blocks-render-default-content-face
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
                                                     'cfw:face-holiday))))
            collect
                 (cons date (cons (cons tday ant) prs-contents)))))
    (calfw-blocks-render-columns day-columns (cons min-hour max-hour) param)))


(defun calfw-blocks-render-periods-days (date periods-stack cell-width)
  "[internal] Insert period texts.
Modified to not truncate events. TODO"
  (when periods-stack
    (let ((stack (sort (copy-sequence periods-stack)
                       (lambda (a b) (< (car a) (car b))))))
      (cl-loop for (_ (begin end content)) in stack
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
                               (propertize
                                (cfw:event-period-overview event)
                                'cfw:event event)
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
                   'calfw-blocks-interval (calfw-blocks-get-time-interval event)
                   'cfw:event event))
              event))
          lst))

(defun calfw-blocks-render-periods (date periods-stack cell-width model)
  "[internal] This function translates PERIOD-STACK to display content on the DATE."
  (seq-filter
   'identity
   (mapcar (lambda (p)
             (let* ((content (nth 2 (cadr p)))
                    (face (cfw:render-get-face-period content 'cfw:face-periods))
                    (props (append
                            (list
                             'keymap calfw-blocks-event-keymap
                             'cfw:period t
                             'cfw:row-count (car p)
                             'face (cons
                                    'calfw-blocks-overline
                                    face)
                             'font-lock-face face)
                            (nth 3 (cadr p))))
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
                                  'cfw:cell-span len
                                  props))))
                   (let* ((begin (nth 0 (cadr p)))
                          (end (nth 1 (cadr p)))
                          (beginp (equal date begin))
                          (endp (equal date end))
                          (width (- cell-width
                                    (if beginp (length cfw:fstring-period-start) 0)
                                    (if endp (length cfw:fstring-period-end) 0)))
                          (title (calfw-blocks-render-periods-title
                                  date begin end content
                                  (- cell-width
                                     (if beginp 1 0)
                                     (if endp 1 0))
                                  model)))
                     (apply 'propertize (concat
                                         (when beginp cfw:fstring-period-start)
                                         (cfw:render-left width title ? )
                                         (when endp cfw:fstring-period-end))
                            props))))))
           (seq-sort
            (lambda (a b) (< (car a) (car b)))
            periods-stack))))

(defun calfw-blocks-render-periods-title (date begin end content
                                               cell-width model)
  "[internal] Return a title string.

Fix erroneous width in last line, should be fixed upstream in calfw."
  (let* ((title-begin-abs
          (max (calendar-absolute-from-gregorian begin)
               (calendar-absolute-from-gregorian (cfw:k 'begin-date model))))
         ;;(title-begin (calendar-gregorian-from-absolute title-begin-abs))
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

(defun calfw-blocks-time-column (cell-height)
  (let* ((num-hours (floor (/ cell-height calfw-blocks-lines-per-hour)))
         (start-hour (car calfw-blocks-earliest-visible-time))
         (start-minute (cadr calfw-blocks-earliest-visible-time)))
    (cl-loop for x from 0 below num-hours
             collect
             (propertize
              (calfw-blocks-format-time
               (list (mod (+ x start-hour) 24) start-minute))
              'face 'calfw-blocks-time-column)
             append
             (make-list (- calfw-blocks-lines-per-hour 1) nil))))

(defun calfw-blocks-render-columns (day-columns hour-interval param)
  "[internal] Concatenate rows on the days into a string of a physical line.

DAY-COLUMNS is a list of columns. A column is a list of following
form: (DATE (DAY-TITLE . ANNOTATION-TITLE) STRING STRING...).
HOUR-INTERVAL is a cons (START . END). Hours outside this
interval are hidden."
  (let* ((cell-width  (cfw:k 'cell-width  param))
         (cell-height (cfw:k 'cell-height param))
         (time-width (cfw:k 'time-width param))
         (EOL (cfw:k 'eol param))
         (VL (cfw:k 'vl param))
         (time-hline (cfw:k 'time-hline param))
         ;;(hline (concat time-hline (cfw:k 'hline param)))
         ;;(cline (concat time-hline (cfw:k 'cline param)))
         (bline (concat time-hline
                        (let* ((EOL "\n")
                               (cell-width (cfw:k 'cell-width param))
                               (columns (cfw:k 'columns param))
                               (num-cell-char
                                (/ cell-width (char-width cfw:fchar-horizontal-line))))
                          (cfw:rt
                           (concat
                            (cl-loop
                             for i from 0 below columns
                             concat
                             (concat
                              (make-string
                               1 (if (= i 0)
                                     calfw-blocks-fchar-bottom-left-corner
                                   calfw-blocks-fchar-bottom-junction))
                              (make-string
                               num-cell-char
                               cfw:fchar-horizontal-line)))
                            (make-string
                             1 calfw-blocks-fchar-bottom-right-corner)
                            EOL)
                           'cfw:face-grid))))
         (earliest-date (caar day-columns))
         ;; (curr-time-linum (calfw-blocks--current-time-vertical-position))
         )
    (cl-loop with breaked-all-day-columns =
             (cl-loop for day-rows in day-columns
                      for (date _ . lines) = day-rows
                      collect
                      (cons date (calfw-blocks-render-all-day-events
                                  lines cell-width (1- cell-height))))
             with all-day-columns-height = ;;(seq-max (mapcar 'length
             (seq-max (mapcar (lambda (x)
                                (max
                                 (1- (length x))
                                 (seq-max
                                  (or (mapcar
                                       (lambda (y)
                                         (1+
                                          (or (get-text-property
                                               0 'cfw:row-count y)
                                              -1)))
                                       (cdr x))
                                      '(0)))))
                              breaked-all-day-columns))
             with breaked-all-day-rows-padded =
             (calfw-blocks--pad-and-transpose all-day-columns-height
                                              breaked-all-day-columns)
             for dates = (mapcar 'car breaked-all-day-columns)
             for i from 0 below all-day-columns-height do
             (insert (cfw:render-left time-width ""))
             for row = (car breaked-all-day-rows-padded)
             do
             (setq breaked-all-day-rows-padded (cdr breaked-all-day-rows-padded))
             (cl-loop
              with cur-cell = 0
              for cell in row
              for cell-span = (or (get-text-property 0 'cfw:cell-span cell) 1)
              for date = (nth cur-cell dates)
              do
              (setq cur-cell (+ cur-cell cell-span))
              (insert
               (if (and calfw-blocks-show-time-grid
                        calfw-blocks-time-grid-lines-on-top
                        (= (mod i calfw-blocks-lines-per-hour) 0)
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
    ;; Need to figure out which times should be hidden and which should not
    ;; here.
    (cl-loop with breaked-day-columns =
             (cl-loop for day-rows in day-columns
                      for (date _ . lines) = day-rows
                      collect
                      (cons date (calfw-blocks-render-event-blocks
                                  lines cell-width (1- cell-height))))
             with curr-time-linum = (calfw-blocks--current-time-vertical-position)
             with time-columns = (calfw-blocks-time-column cell-height)
             for i from 1 to cell-height
             for time = (prog1 (car time-columns)
                          (setq time-columns (cdr time-columns)))
             for curVL = (if time
                             (let ((VL-copy (substring VL)))
                               (add-face-text-property
                                0 (length VL)
                                'calfw-blocks-overline
                                t
                                VL-copy)
                               VL-copy)
                           VL)
             for today = (calendar-current-date)
             for today-shown = (cl-some
                                'identity
                                (cl-loop for day-rows in breaked-day-columns
                                         for date = (car day-rows)
                                         collect
                                         (equal date today)))
             for cur-hour = (/ (1- i) calfw-blocks-lines-per-hour)
             for shrunk-hour = (and calfw-blocks-hour-shrink-size
                                    (or (< cur-hour (car hour-interval))
                                        (> cur-hour (cdr hour-interval))))
             for show-cur-time = (and
                                  calfw-blocks-show-current-time-indicator
                                  today-shown
                                  (= (1- i)
                                     (if shrunk-hour
                                         ;; If hour is shrunk, make sure
                                         ;; current time is displayed on the
                                         ;; an visible appropriate
                                         (let ((start (* (/ curr-time-linum
                                            calfw-blocks-lines-per-hour)
                                                         calfw-blocks-lines-per-hour)))
                                           (+ start
                                              (floor
                                               (* (/ (- curr-time-linum start)
                                                     (float calfw-blocks-lines-per-hour))
                                                  calfw-blocks-hour-shrink-size))))
                                       curr-time-linum)))
             for final-line
             = (apply #'concat
                      (append
                       (list
                        (progn
                          (when show-cur-time
                            (setq time (calfw-blocks-format-time
                                        (let ((curr-time (decode-time (current-time))))
                                          (list (nth 2 curr-time)
                                                (nth 1 curr-time)))))
                            (add-face-text-property
                             0 (length time) 'calfw-blocks-time-column-now
                             t time))
                          (cfw:render-left time-width time)))
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
                                   (if (and show-cur-time (equal date today))
                                       (propertize "@"
                                                   'face
                                                   'cfw:face-today-title
                                                   'cfw:current-time-marker t)
                                     curVL))
                                 (cfw:tp
                                  (cfw:render-separator
                                   (cfw:render-left cell-width (and row (format "%s" row))))
                                  'cfw:date date)))
                       (list curVL EOL)))
             do
             (when show-cur-time
               (add-face-text-property
                time-width (length final-line) 'calfw-blocks-now-indicator
                t final-line))
             (when (and shrunk-hour
                        (> (mod (1- i) calfw-blocks-lines-per-hour)
                           (1- calfw-blocks-hour-shrink-size)))
               (add-text-properties 0 (length final-line) '(invisible t)
                                    final-line))
             (insert final-line))
    (insert bline)))

(defun calfw-blocks--pad-and-transpose (max-len columns)
  "Return the columns as rows, padded with space when needed"
  (let* ((col-count (length columns)))
    (prog1
        (cl-loop for i from 0 below max-len  ;; Loop over rows
                 for filled-cells = 0
                 for rows = (cl-loop
                             for c being the elements of columns using (index j)
                             for cols = (cdr c)
                             when (<= filled-cells j)
                             if cols
                             collect
                             (let* ((col (car cols))
                                    (on-row (>= i
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
                                 ;; If (< filled-cells col-count)
                                 ;; then we need to skip
                                 (setq filled-cells (+ filled-cells 1))
                                 ""))
                             else
                             collect
                             (progn
                               (setq filled-cells (+ filled-cells 1))
                               "")
                             end)
                 collect
                 (append rows (make-list (- col-count filled-cells) "")))
      ;; Assert that all columns are moved into rows
      (cl-assert (not (cl-some (lambda (x) (cdr x)) columns))))))


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
              (setcar i-group (calfw-blocks--interval-intersection (car i-group) j-interval)))))
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
  (round time))

(defun calfw-blocks-round-end-time (time)
  (round time))

(defun calfw-blocks-hours-per-line ()
  (/ 1.0 calfw-blocks-lines-per-hour))

(defun calfw-blocks--time-pair-to-float (p)
  (+ (car p) (/ (cadr p) 60.0)))

(defun calfw-blocks--get-float-time-interval (line)
  (let* ((interval (get-text-property 0 'calfw-blocks-interval line))
         (start (calfw-blocks--time-pair-to-float (car interval)))
         (end (calfw-blocks--time-pair-to-float (cdr interval))))
    (if (<= start end)
        (list start end)
      ;; TODO: isssue#108
      ;; If start is before end, it means this event extends to next day
      ;; end it at the end of the current day for now, but we really need a
      ;; better way so that we can also make it start from the beginning.
      (list start
            (calfw-blocks--time-pair-to-float '(23 59))))))

(defun calfw-blocks--get-block-vertical-position (p)
  "[inclusive, exclusive)"
  (let* ((float-interval (calfw-blocks--get-float-time-interval p))
         (start-time (calfw-blocks--time-pair-to-float
                      calfw-blocks-earliest-visible-time))
         ;; (minutes-per-line (/ 60 calfw-blocks-lines-per-hour))
         (interval-start (car float-interval))
         (interval-end (if (= interval-start (cadr float-interval))
                           (+ calfw-blocks-default-event-length interval-start)
                         (cadr float-interval))))
    (list (calfw-blocks-round-start-time (* calfw-blocks-lines-per-hour
                                            (- interval-start start-time)))
          (calfw-blocks-round-end-time (* calfw-blocks-lines-per-hour
                                          (- interval-end start-time))))))

(defun calfw-blocks--current-time-vertical-position ()
  (let* ((start-time (calfw-blocks--time-pair-to-float
                      calfw-blocks-earliest-visible-time))
         (curr-time (decode-time (current-time)))
         (curr-hour (nth 2 curr-time))
         (curr-min (nth 1 curr-time))
         (time-float (+ curr-hour (/ curr-min 60.0))))
    (calfw-blocks-round-start-time (* calfw-blocks-lines-per-hour
                                      (- time-float start-time)))))

(defun calfw-blocks-point-to-time (&optional end-time)
  "Return the timestamp corresponding to point.
If in time row, return (MONTH DAY YEAR HOUR MINUTE). If not in a
time row, return (MONTH DAY YEAR). Return nil if no date on point."
  (let* ((line-start (save-excursion
                       (min
                        (progn (goto-char (point-min))
                               (text-property-search-forward
                                'face 'calfw-blocks-time-column)
                               (line-number-at-pos))
                        (progn (goto-char (point-min))
                               (text-property-search-forward
                                'face 'calfw-blocks-time-column-now)
                               (line-number-at-pos)))))
         (cur-line (and line-start
                        (- (line-number-at-pos) line-start)))
         (date (cfw:cursor-to-date))
         (min-per-line (/ 60 calfw-blocks-lines-per-hour))
         (starting-min (+ (* 60 (car calfw-blocks-earliest-visible-time))
                          (cadr calfw-blocks-earliest-visible-time)))
         (minutes (if (> cur-line 0)
                      (+ starting-min
                         (* min-per-line
                            (+ (if end-time 1 0) cur-line)))
                    0)))
    (when date
      (if (>= cur-line 0)
          (append date
                  (list (/ minutes 60)
                        (% minutes 60)))
        date))))

(defun calfw-blocks-time-to-line-number (hour minute)
  "Return the line number corresponding to hour/minute."
  (let* ((line-start (save-excursion
                       (min
                        (progn (goto-char (point-min))
                               (text-property-search-forward
                                'face 'calfw-blocks-time-column)
                               (line-number-at-pos))
                        (progn (goto-char (point-min))
                               (text-property-search-forward
                                'face 'calfw-blocks-time-column-now)
                               (line-number-at-pos)))))
         (min-per-line (/ 60 calfw-blocks-lines-per-hour))
         ;; (starting-min (+ (* 60 (car calfw-blocks-earliest-visible-time))
         ;;                  (cadr calfw-blocks-earliest-visible-time)))
         (minutes (+ (* hour 60) minute)))
    (+ line-start
       (floor (/ minutes min-per-line)))))

(defun calfw-blocks-region-to-time ()
  "Return time corresponding to region.
Return a list \\=(START END ALL-DAY-P)
where START is the encoded time corresponding to
`region-beginning', END is encoded time corresponding to
`region-end' or nil if region is inactive and ALL-DAY-P is t if
the region is not in the time section of the calendar."
  (save-excursion
    (cl-destructuring-bind
        (reg-begin &optional reg-end)
        (if (region-active-p) (list (region-beginning) (region-end))
          (list (point)))
      (let* ((time-start
              (or (progn (goto-char reg-begin)
                         (unless (cfw:cursor-to-date)
                           ;; If not on date, go to next one
                           (goto-char
                            (or (next-single-property-change
                                 (point)
                                 'cfw:date
                                 nil
                                 (line-end-position))
                                (point-max))))
                         (calfw-blocks-point-to-time))
                  (user-error "No date around point")))
             (time-end (when reg-end
                         (progn (goto-char reg-end)
                                (unless (cfw:cursor-to-date)
                                  (goto-char
                                   ;; If not on date, go to previous one
                                   (or (previous-single-property-change
                                        (point)
                                        'cfw:date
                                        (line-beginning-position))
                                       (point-min))))
                                (calfw-blocks-point-to-time t))))
             (all-day-p (or (<= (length time-start) 3)
                            (and time-end (<= (length time-end) 3)))))
        (list (cl-destructuring-bind
                  (month day year &optional (hour 0) (minute 0)) time-start
                (encode-time (list 0 minute hour day month year)))
              (when time-end
                (cl-destructuring-bind
                    (month day year &optional (hour 0) (minute 0)) time-end
                  (encode-time (list 0 minute hour day month year))))
              all-day-p)))))

(defun calfw-blocks-generalized-substring (s start end props)
  (cond ((<= end (length s)) (substring s start end))
        ((< start (length s)) (concat (substring s start (length s))
                                      (apply
                                       #'propertize
                                       (make-string (- (- end start) (- (length s) start)) ? )
                                       props)))
        (t (apply
            #'propertize
            (make-string (- end start) ? )
            props))))


(defun calfw-blocks--wrap-text (width max-lines &optional word-break)
  (goto-char (point-min))
  (while (and (< (point) (point-max))
              (> max-lines 1))
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
        (insert ?\n)
        (setq max-lines (1- max-lines))))))

(defun calfw-blocks--wrap-string (text width max-lines &optional word-break)
  "Wrap TEXT to a fixed WIDTH without breaking words."
  ;; Doesn't work with \n in text
  (with-temp-buffer
    (insert text)
    (calfw-blocks--wrap-text width max-lines word-break)
    (buffer-string)))

(defun calfw-blocks--to-lines (text width max-lines help-text)
  "Wrap TEXT to a fixed WIDTH without breaking words.
Add HELP-TEXT in case the string is truncated."
  ;; Doesn't work with \n in text
  (let* ((lines (split-string text "\n"))
         (line-n (1- (min max-lines (length lines))))
         (last-line (nth line-n lines))
         ellips)
    (when (or
           (> (length lines) max-lines)
           (> (length last-line) width))
      ;; The concat is there to avoid adding properties to
      ;; (truncate-string-ellipsis) !!
      (setq ellips (concat (truncate-string-ellipsis) ""))
      (add-text-properties
       0 (length ellips)
       (text-properties-at (1- (length last-line))
                           last-line)
       ellips)
      (setf (nth line-n lines)
            (concat
             (substring
              last-line
              0
              (- (min (length last-line) width)
                 (length ellips)))
             ellips))
      (cl-loop for l in lines do
               (add-text-properties
                0 (length l)
                (list 'help-echo help-text)
                l)))
    lines))

(defun calfw-blocks--remove-unicode-chars (str)
  "Remove any Unicode characters from INPUT-STRING."
  (cl-loop
   ;; TODO: Make sure that str maintains its properties.
   for char across str
   if (and (>= char 16) (<= char 127))
   concat (string char)))

(defun calfw-blocks--status-face (text &optional background)
  (let* ((event (get-text-property 0 'cfw:event text)))
    (when event
    (cl-case (cfw:event-status event)
      (cancelled (if background
                     'calfw-blocks-cancelled-event-bg
                     'calfw-blocks-cancelled-event))))))

(defun calfw-blocks-split-single-block (block)
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
         (text-height (or
                       (car-safe (cdddr block))
                       block-height))
         ;; (end-of-cell (= (cadr block-horizontal-pos) cell-width))
         (is-beginning-of-cell (= (car block-horizontal-pos) 0))
         (block-width-adjusted (if is-beginning-of-cell block-width
                                 (- block-width
                                    (string-width calfw-blocks-event-start))))
         (props (cl-copy-seq (text-properties-at (1- (length block-string)) block-string)))
         (block-lines (calfw-blocks--to-lines
                       (calfw-blocks--wrap-string
                        ;; TODO: Figure out a way to remove unicode characters
                        ;; while preserving text properties
                        ;;
                        ;; (calfw-blocks--remove-unicode-chars block-string)
                        ;; TODO: Adjust text/faces based on status, state (if
                        ;; tentative) and if recurring (add a symbol)
                        (prog1
                            block-string
                          (add-face-text-property 0 (length block-string)
                                                  (calfw-blocks--status-face
                                                   block-string)
                                                  t block-string))
                        block-width-adjusted
                        text-height ;;block-height
                        (apply 'propertize "-" props))
                       block-width-adjusted
                       block-height
                       (substring-no-properties block-string)))
         (rendered-block '())
         (is-exceeded-indicator (get-text-property
                                 0 'calfw-blocks-exceeded-indicator
                                 block-string))
         tmp)
    (dolist (i (number-sequence 0 (- block-height 1)))
      (push (list (+ (car block-vertical-pos) i)
                  (prog1
                      (setq tmp
                            (propertize
                             (concat
                              ;;TODO some parts of the string won't inherit
                              ;; the properties of the event might cause
                              ;; issues with org goto/navigation/etc?
                              (when (not is-beginning-of-cell)
                                (apply 'propertize calfw-blocks-event-start
                                       props)) ;;(if (= i 0) "*" "|"))
                              (calfw-blocks-generalized-substring
                               (car block-lines) 0 block-width-adjusted
                               props))
                             'keymap calfw-blocks-event-keymap
                             'calfw-blocks-horizontal-pos block-horizontal-pos))
                    ;; (add-face-text-property 0 (length tmp)
                    ;;                         (cfw:render-get-face-content
                    ;;                          block-string
                    ;;                          'cfw:face-default-content)
                    ;;                         t tmp)
                    (when (= i 0)
                      (add-face-text-property 0 (length tmp)
                                              'calfw-blocks-overline t tmp))
                    (when (= i (- block-height 1))
                      (add-face-text-property 0 (length tmp)
                                              'calfw-blocks-underline t tmp))
                    (when is-exceeded-indicator
                      (add-face-text-property 0 (length tmp)
                                              'italic t tmp))
                    ;; (when (bound-and-true-p 'calfw-blocks-event-box)
                    ;;   ;; Essentially, we need to put a box on the first character
                    ;;   ;; and a no-box on the second character.
                    ;;   (add-face-text-property 0 1 '(:box (:line-width (-1 . 0))) t tmp)
                    ;;   (add-face-text-property 1 2 '(:box (:line-width (0 . 0))) t tmp))
                    ))
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
         ;; (all-day-lines (seq-filter (lambda (line) (not (car (get-text-property 0 'calfw-blocks-interval
         ;;                                                                        line))))
         ;;                            lines))
         (block-positions (calfw-blocks--get-block-positions interval-lines cell-width))
         (split-blocks (seq-sort (lambda (a b) (< (car a) (car b)))
                                 (mapcan #'calfw-blocks-split-single-block
                                         block-positions)))
         (rendered-lines '())
         ;; (curr-time-grid-line (calfw-blocks--current-time-vertical-position))
         )
    (dolist (i (number-sequence 0 (1- cell-height)))
      (let ((make-time-grid-line (and calfw-blocks-show-time-grid
                                      (= (mod i calfw-blocks-lines-per-hour) 0)))
            (current-line-lst '()))
        (if (or (not split-blocks) (< i (caar split-blocks)))
            (if make-time-grid-line
                (push (propertize (make-string cell-width ? )
                                  'face (list 'cfw:face-grid
                                              'calfw-blocks-overline))
                      current-line-lst)
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
  (propertize (make-string n ? ) 'face (list 'cfw:face-grid
                                             'calfw-blocks-overline)))

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

(defun calfw-blocks--cfw-cp-update (component &optional initial-date)
  "[internal] Clear and re-draw the component content."
  (let* ((buf (cfw:cp-get-buffer component))
         (dest (cfw:component-dest component)))
    (with-current-buffer buf
      (cfw:dest-before-update dest)
      (cfw:dest-ol-today-clear dest)
      (let* ((buffer-read-only nil))
        (cfw:dest-with-region dest
          (cfw:dest-clear dest)
          (funcall (cfw:cp-dispatch-view-impl
                    (cfw:component-view component))
                   component)))
      (if (eq (cfw:component-view component) 'block-week)
          (calfw-blocks-dest-ol-today-set dest)
        (when cfw:highlight-today (cfw:dest-ol-today-set dest)))
      (when initial-date
        (cfw:cp-goto-date component initial-date))
      (cfw:dest-after-update dest)
      (cfw:cp-fire-update-hooks component))))

(defun calfw-blocks-get-displayed-events ()
  "Return a list of displayed events in the current buffer.
Each item in the list is a cons containing the first position the
event appears and the cfw:event structure."
  (let ((cur-pt (point-min))
        ev evs)
    (while (setq cur-pt
                 (next-single-property-change cur-pt 'cfw:event))
      (when (setq ev (get-text-property cur-pt 'cfw:event))
        (unless (cl-member ev evs :key 'cdr)
          (setq evs (append evs (list (cons cur-pt ev)))))))
    evs))


(defun calfw-blocks--get-overlapping-block-positions (lines cell-width)
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
              (mapcar
               (lambda (x)
                 (list x (calfw-blocks--get-block-vertical-position x)))
           lines))
         ;; Group by vertical start, sorting the groups in ascending order
         (groups (cl-sort (seq-group-by 'caadr lines-lst)
                          '< :key 'car))
         ;; Sort within each group by descending height
         (groups (cl-loop for g in groups
                          ;; sort within groups
                          collect (cons
                                   (car g)
                                   (seq-sort
                                    (lambda (x y)
                                      (> (- (cadadr x) (caadr x))
                                         (- (cadadr y) (caadr y))))
                                    (cdr g))))))
    (cl-loop
     with new-lines-lst = nil
     for g in groups
     for rem = (length (cdr g)) ;; Remaining segments in this group
     do (cl-loop for l in (cdr g)
                 ;; Go over any added indices that vertically intersect with
                 ;; the current lines. intervals will be a list of (start end
                 ;; event) which specifies the interval and which event it is
                 ;; intersection (one at most).
                 when (> rem 0)
                 do
                 (let* ((intervals
                         (cl-loop
                                  with intervals = nil
                                  with start = 0
                                  with cur-intersection = nil
                                  for x in
                                  (cl-loop for y in new-lines-lst
                                           if (calfw-blocks--interval-intersect?
                                               (cadr y) (cadr l))
                                           collect y into intersection
                                           finally return
                                           (cl-sort intersection '<
                                                    :key 'caaddr))
                                  for x-horz = (caddr x)
                                  for sx = (car x-horz)
                                  do (progn
                               (when (> sx start)
                                           (push (list start sx
                                                       cur-intersection)
                                                 intervals))
                                       (if (= (caadr x) (car g))
                                           ;; Same group, take
                                           ;; out the whole interval
                                           (setq start (cadr x-horz)
                                                 cur-intersection nil)
                                         (setq cur-intersection x
                                               start (1+ sx))))
                          finally do (when (> cell-width start)
                                       (push
                                              (list start cell-width
                                                    cur-intersection)
                                        intervals))
                                  finally return
                                  (cl-sort intervals
                                           '> ;; Get largest intervals first
                                           :key
                                           (lambda (int) ;; Gets width
                                     (- (cadr int) (car int))))))
                        (int (car-safe intervals))
                        (valid-intervals (cl-count-if
                                          (lambda (int) ;; Gets width
                                            (>= (- (cadr int) (car int))
                                                calfw-blocks-min-block-width))
                                          intervals)))
                   (cond
                    ((null int)
                     ;; If we have zero intervals, we're screwed. Need to
                     ;; issue a warning and eventually come up with a solution
                     ;; when we run into it.
                     (warn "Not enough space to show all events in one or more cells!"))
                    ((and (>= rem 1)
                          (< (- (cadr int) (car int))
                             (* rem calfw-blocks-min-block-width)))
                     ;; If we have one interval of insufficient size, and more
                     ;; than one event remaining, need to add an +more
                     ;; indicator.
                     ;;
                     ;; TODO: This is problematic if previous blocks have used
                     ;; up all the remaining space. We should have really
                     ;; replace the one before the current.
                     (let* ((x-vertical-pos (nth 1 l))
                            (exceeded-indicator
                             (list (propertize
                                    (format "+%dmore" rem)
                                    'calfw-blocks-exceeded-indicator t)
                                   (list (nth 0 x-vertical-pos)
                                         (max 4 (nth 1 x-vertical-pos)))
                                   (butlast int))))
                       (push exceeded-indicator new-lines-lst)
                       ;; Ignore the rest
                       (setq rem 0)))
                    (t (push
                        (append l
                                (list
                                 (if (or (= rem 1) (> valid-intervals 1))
                                     (butlast int)
                                   ;; Split the interval into equal parts
                                   (let* ((s (car int))
                                          (e (cadr int))
                                          (w (- e s)))
                                     (list s (+ s (/ w rem)))))))
                        new-lines-lst)))
                   (when-let ((intersector (and int (nth 2 int))))
                     ;; Update the text height of the event that intersects
                     ;; this interval
                     (let* ((int-start (caadr intersector))
                            (int-end (cadadr intersector))
                            (cur-end (caadr l))
                            (max-height (- cur-end int-start))
                            (old-height (nth 3 intersector)))
                       (if old-height
                           (setcar (nthcdr 3 intersector)
                                   (min old-height max-height))
                         (setcdr intersector
                                 (append (cdr intersector)
                                         (list (min max-height
                                                    (- int-end
                                                       int-start))))))))
                   (setq rem (1- rem))))
     finally return new-lines-lst)))

(defun calfw-blocks--pad-overlapping-block-line (block-line
                                                 cell-width make-time-grid-line)
  (let* ((block-line
          (seq-sort (lambda (a b)
                      (> (car (get-text-property
                               0 'calfw-blocks-horizontal-pos a))
                         (car (get-text-property
                               0 'calfw-blocks-horizontal-pos b))))
                    block-line))
         padded-line (prev-start cell-width))
    (dolist (segment block-line
                     (progn
                       (push (if make-time-grid-line
                                 (calfw-blocks--grid-line prev-start)
                               (make-string prev-start ? ))
                             padded-line)
                       padded-line))
      (let* ((horizontal-pos (get-text-property 0 'calfw-blocks-horizontal-pos
                                                segment))
             (start (car horizontal-pos))
             (end (cadr horizontal-pos)))
        (if (< end prev-start)
            (push (if make-time-grid-line
                      (calfw-blocks--grid-line (- prev-start end))
                    (make-string (- prev-start end) ? ))
                  padded-line))
        (push (substring segment 0 (min
                                    (- end start)
                                    (- prev-start start)))
              padded-line)
        (setq prev-start start)))))

(define-minor-mode calfw-blocks-overlapping-mode
  "Allow blocks to overlap in calfw-blocks."
  :global t
  :group 'calfw-blocks
  :init-value nil
  (dolist (ad '((calfw-blocks--get-block-positions
                 . calfw-blocks--get-overlapping-block-positions)
                (calfw-blocks--pad-block-line
                 . calfw-blocks--pad-overlapping-block-line)))
    (if calfw-blocks-overlapping-mode
        (advice-add (car ad) :override (cdr ad))
      (advice-remove (car ad) (cdr ad))))
  (when-let (buf (get-buffer cfw:calendar-buffer-name))
    (with-current-buffer buf
      (cfw:refresh-calendar-buffer nil))))

(define-minor-mode calfw-blocks-mode
  "Enable calfw view as blocks."
  :global t
  :group 'calfw-blocks
  :init-value nil
  (let ((fn-ad (if calfw-blocks-mode
                   'advice-add
                 (lambda (symbol _ fn &rest _)
                   (advice-remove symbol fn))))
        (fn-list (if calfw-blocks-mode
                     'add-to-list
                   (lambda (qlst item)
                     (set qlst (cl-delete
                                item
                                (symbol-value qlst)
                                :test #'equal))))))
    (dolist (dispatch '((block-week        .  calfw-blocks-view-block-week)
                    (block-day         .  calfw-blocks-view-block-day)
                    (block-2-day       .  calfw-blocks-view-block-2-day)
                    (block-3-day       .  calfw-blocks-view-block-3-day)
                    (block-4-day       .  calfw-blocks-view-block-4-day)
                    (block-5-day       .  calfw-blocks-view-block-5-day)
                    (block-7-day       .  calfw-blocks-view-block-7-day)))
      (funcall fn-list 'cfw:cp-dipatch-funcs dispatch))

    (funcall fn-ad 'cfw:render-toolbar
            :override 'calfw-blocks-render-toolbar)
    (funcall fn-ad 'cfw:cp-update :override 'calfw-blocks--cfw-cp-update)
    (funcall fn-ad 'cfw:refresh-calendar-buffer
            :override 'calfw-blocks--cfw-refresh-calendar-buffer)

    (dolist (fn '(cfw:navi-next-month-command
              cfw:navi-previous-month-command
                  cfw:refresh-calendar-buffer
                  cfw:event-toggle-calendar))
      (funcall fn-ad fn :around
               #'calfw-blocks-perserve-buffer-view-advice))))

(calfw-blocks-mode) ;; Enable mode by default

(provide 'calfw-blocks)
;;; calfw-blocks.el ends here
