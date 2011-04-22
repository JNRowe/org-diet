;;; color-theme.el --- install color themes

;; Copyright (C) 2010  Christopher Allan Webber <cwebber@dustycloud.org>

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(require 'org)
(require 'org-table)
(require 'ob-gnuplot)


(defvar org-diet-gathering-funcs
  '(("weight" . org-diet-gather-weight))
;    org-diet-gather-calories)
  "Functions to run to gather data for analysis.

Functions should return a cons of ('Varname' . 'value') for every
piece of information gathered, or nil if nothing is to be
gathered.  Functions should expect to gather information from the
entry at point.  (maybe this will change later)")

(defcustom org-diet-file "~/org/diet.org"
  "Location of your diet file.")


(defun org-diet-gather-weight ()
  (org-entry-get (point) "Weight"))


(defun org-diet-entry-timestamp-fast ()
  "Get the entry date from this item if in the header.
Kind of like (org-entry-get (point) \"TIMESTAMP\") but faster and
only for timestamps on headers."
  (let* ((entry-header (nth 4 (org-heading-components)))
         (date-match-start (string-match org-ts-regexp3
                                         entry-header)))
    (if date-match-start
        ; Return the date, but without the <>/[] chars
        (substring entry-header (+ date-match-start 1) (- (match-end 0) 1)))))


(defun org-diet-gather-properties-at-point ()
  "Return an alist full of all properties at point.

Properties gathered from funcs specified in org-diet-gathering-funcs"
  (mapcar
   (lambda (gather-cons) 
     (let ((gather-property (car gather-cons))
           (gather-func (cdr gather-cons)))
       (cons gather-property
             (apply gather-func nil))))
   org-diet-gathering-funcs))

(defun org-diet-weight-subtree-entries-at-point ()
  (cdr (org-map-entries
        '(cons (org-diet-entry-timestamp-fast)
               (org-diet-gather-properties-at-point))
        nil 'tree)))

;; This is the slowish version
;;
;; (defun org-diet-weight-subtree-entries-at-point ()
;;   (cdr (org-map-entries
;;         '(cons (org-entry-get (point) "TIMESTAMP")
;;                (org-entry-get (point) "Weight"))
;;         nil 'tree)))

(defun org-diet-get-date-range (start-date date-length)
  "Return a range of dates starting with start-date and ending at date-length"
  (let ((new-dates nil)
        (first-date-absolute (org-time-string-to-absolute start-date)))
    (dotimes (i date-length)
      (push
       (format-time-string
        "%Y-%m-%d %a"
        (org-time-from-absolute
         (- (org-time-string-to-absolute start-date) i)))
       new-dates))
    (reverse new-dates)))


(defun org-diet-date-range-property-intersection (date-list date-range property)
  "Extract the dates from DATE-LIST that are within the range DATE-RANGE

The range DATE-RANGE is likely generaged from org-diet-get-date-range."
  (let ((intersected-dates nil))
    (mapcar
     (lambda (date)
       (let ((listed-date (assoc date date-list)))
         (if (and listed-date
                  (assoc property (assoc date date-list))) ; If it has this property
             (push listed-date intersected-dates))))
     date-range)
    intersected-dates))

(defun org-diet-average-weight-for-date (date-list start-date &optional average-length)
  "Returns a list of (averaged-weight accuracy-percent)"
  (let* ((average-length (or average-length 10))
         (date-range (org-diet-get-date-range start-date average-length))
         (date-intersect (org-diet-date-range-property-intersection
                          date-list date-range "weight"))
         (weights (delq nil
                        (mapcar
                         (lambda (date-entry)
                           (let ((weight
                                  (cdr (assoc "weight" (cdr date-entry)))))
                             (if weight
                                 (string-to-number weight))))
                         date-intersect)))
         (date-length (safe-length weights))
         (averaged-weight (/ (apply '+ weights) date-length))
         (weight-accuracy (/ (float date-length) average-length)))
    (list averaged-weight weight-accuracy)))

(defun org-diet-average-weight-prettify (weight-results)
  (list (format "%.2f" (car weight-results))
        (format "%d%%" (* (nth 1 weight-results) 100))))

(defun org-diet-weight-summary ()
  (with-current-buffer (get-file-buffer org-diet-file)
    (save-excursion
      (beginning-of-buffer)
      (search-forward-regexp "^\\* Daily Logs[ \t]*$")
      (let ((date-list (org-diet-weight-subtree-entries-at-point))
            (weight-averages nil))
        (dolist (date-cons date-list)
          (push
           (append (list (car (split-string (car date-cons)))
                         (cdr (assoc "weight" (cdr date-cons))))
                   (org-diet-average-weight-prettify
                    (org-diet-average-weight-for-date
                     date-list (car date-cons) 10)))
                 weight-averages))
        weight-averages))))


(provide 'org-diet)
