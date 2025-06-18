;;; agenda.app --- icalendar url to org files with IDs
;;; Commentary:
;;;
;;; Let's go. this is mostly ripped off from the icalendar.el code
;;; A lot of the code below is lifted straight out of from icalendar.el.
;;;
;;; Copyright (C) 2002-2023 Free Software Foundation, Inc.
;;;
;;; The code is included here because they are private functions subject to
;;; change.
;;; Code:

(defgroup agenda.app nil
	 "Agenda.app"
	 :group 'calendar
	 :prefix "agenda.app/"
	 :link '(url-link :tag "Homepage" "https://chee.party/notebook/docfiles/emacs/packages/agenda-app.html"))

(defcustom agenda.app/ical-url
	 "https://calendar.google.com/calendar/ical/chee.rabbits%40ft.com/public/basic.ics"
	 "The url to fetch your calendar events from."
	 :type 'string
	 :group 'agenda.app)

(defcustom agenda.app/filename
	 (expand-file-name "ft/calendar.org" org-directory)
	 "The directory to store your calendar orgs in."
	 :type 'string
	 :group 'agenda.app)

(defcustom agenda.app/context-days 7
	 "The number of days around today to create entries for.
Anything beyond this range with be archived on the next
`agenda.app/pull'."
	 :type 'number)

(require 'calendar)
(require 'org)
(require 'org-id)
(require 'shr)

(defun agenda.app--dom-to-org (wrap)
	 (lambda (dom)
		 (shr-insert " ")
		 (shr-insert wrap)
		 (shr-generic dom)
		 (shr-insert wrap)
		 (shr-insert " ")))

(defun agenda.app--dom-link-to-org-link (dom)
	 (let ((href (dom-attr dom 'href))
				(text (dom-texts dom))
				(start (point)))
		 (if (string-empty-p (string-trim text))
			(shr-insert (format "[[%s]]" href))
			 (shr-insert (format "[[%s][" href))
			 (shr-generic dom)
			 (shr-insert "]]"))))

(defun agenda.app--dom-li-to-org-li (dom)
	 (shr-ensure-newline)
	 (let ((start (point)))
		 (let* ((bullet
					  (if (numberp shr-list-mode)
						 (prog1
							 (format "%d. " shr-list-mode)
							(setq shr-list-mode (1+ shr-list-mode)))
						  (car shr-internal-bullet)))
					 (width (if (numberp shr-list-mode)
									(shr-string-pixel-width bullet)
								  (cdr shr-internal-bullet))))
			 (insert bullet)
			 (shr-mark-fill start)
			 (let ((shr-indentation (+ shr-indentation width)))
			  (put-text-property start (1+ start)
					 'shr-continuation-indentation shr-indentation)
			  (put-text-property start (1+ start) 'shr-prefix-length (length bullet))
			  (shr-generic dom))))
	 (unless (bolp)
		 (insert "\n")))

(defun agenda.app--strip-html-in-region (start end)
	 (let ((dom (libxml-parse-html-region start end))
				(shr-max-width 9999)
				(shr-bullet "- ")
				(shr-external-rendering-functions
				  '((span . shr-generic)
						(li . agenda.app--dom-li-to-org-li)
						(html-blob . shr-generic)
						(b . (agenda.app--dom-to-org "*"))
						(strong . (agenda.app--dom-to-org "*"))
						(i . (agenda.app--dom-to-org "/"))
						(em . (agenda.app--dom-to-org "/"))
						(u . (agenda.app--dom-to-org "_"))
						(a . agenda.app--dom-link-to-org-link))))
		 (delete-region start end)
		 (let ((start (point)))
			 (shr-insert-document dom)
			 (replace-regexp-in-region "\\(\\\\n\\)+" "\n" start (point)))))

(defun agenda.app/pull nil
	 (interactive)
	 (let* ((ical-buffer (url-retrieve-synchronously agenda.app/ical-url t)))
		 (with-current-buffer ical-buffer
			 (goto-char (point-min))
			 ;; Remove unprintable characters that are in all our google calendar
			 ;; descriptions for some reason
			 ;; FIXME i don't know how to use replace-match
			 (replace-regexp "[^\t\n\r\f -~]" "-")
			 (agenda.app/import-buffer))))

(defconst agenda.app--weekday-array ["SU" "MO" "TU" "WE" "TH" "FR" "SA"])

(defun agenda.app--get-unfolded-buffer (folded-ical-buffer)
	 "Return a new buffer containing the unfolded contents of a buffer.
Folding is the iCalendar way of wrapping long lines.  In the
created buffer all occurrences of CR LF BLANK are replaced by the
empty string.  Argument FOLDED-ICAL-BUFFER is the folded input
buffer."
	 (let ((unfolded-buffer (get-buffer-create " *agenda.app-work*")))
		 (save-current-buffer
			 (set-buffer unfolded-buffer)
			 (erase-buffer)
			 (insert-buffer-substring folded-ical-buffer)
			 (save-excursion
			  (goto-char (point-min))
			  (while (re-search-forward "\r\n\\|\n\r" nil t)
				 (replace-match "\n" nil nil)))
			 (goto-char (point-min))
			 (while (re-search-forward "\r?\n[ \t]" nil t)
			  (replace-match "" nil nil)))
		 unfolded-buffer))

(defun agenda.app--read-element (invalue inparams)
	 "Recursively read the next iCalendar element in the current buffer.
INVALUE gives the current iCalendar element we are reading.
INPARAMS gives the current parameters.....
This function calls itself recursively for each nested calendar element
it finds.  The current buffer should be an unfolded buffer as returned
from `agenda.app--get-unfolded-buffer'."
	 (let (element children line name params param param-name param-value
				 value
				 (continue t))
		 (setq children '())
		 (while (and continue
					  (re-search-forward "^\\([A-Za-z0-9-]+\\)[;:]" nil t))
			 (setq name (intern (match-string 1)))
			 (backward-char 1)
			 (setq params '())
			 (setq line '())
			 (while (looking-at ";")
			  (re-search-forward ";\\([A-Za-z0-9-]+\\)=" nil nil)
			  (setq param-name (intern (match-string 1)))
			  (re-search-forward "\\(\\([^;,:\"]+\\)\\|\"\\([^\"]+\\)\"\\)[;:]"
					 nil t)
			  (backward-char 1)
			  (setq param-value (or (match-string 2) (match-string 3)))
			  (setq param (list param-name param-value))
			  (while (looking-at ",")
				 (re-search-forward "\\(\\([^;,:]+\\)\\|\"\\([^\"]+\\)\"\\)"
						nil t)
				 (if (match-string 2)
					  (setq param-value (match-string 2))
					 (setq param-value (match-string 3)))
				 (setq param (append param param-value)))
			  (setq params (append params param)))
			 (unless (looking-at ":")
			  (error "sorry"))
			 (forward-char 1)
			 (let ((start (point)))
			  (end-of-line)
			  (setq value (buffer-substring start (point))))
			 (setq line (list name params value))
			 (cond ((eq name 'BEGIN)
						(setq children
							(append children
								(list (agenda.app--read-element (intern value)
												params)))))
				((eq name 'END)
				  (setq continue nil))
				(t
				  (setq element (append element (list line))))))
		 (if invalue
			(list invalue inparams element children)
			 children)))

(defun agenda.app--get-event-property (event prop)
	 "For the given EVENT return the value of the first occurrence of PROP."
	 (catch 'found
		 (let ((props (car (cddr event))) pp)
			 (while props
			  (setq pp (car props))
			  (if (eq (car pp) prop)
				  (throw 'found (car (cddr pp))))
			  (setq props (cdr props))))
		 nil))

(defun agenda.app--get-event-property-attributes (event prop)
	 "For the given EVENT return attributes of the first occurrence of PROP."
	 (catch 'found
		 (let ((props (car (cddr event))) pp)
			 (while props
			  (setq pp (car props))
			  (if (eq (car pp) prop)
				  (throw 'found (cadr pp)))
			  (setq props (cdr props))))
		 nil))

(defun agenda.app--get-children (node name)
	 "Return all children of the given NODE which have a name NAME.
For instance the VCALENDAR node can have VEVENT children as well as VTODO
children."
	 (let ((result nil)
				(children (cadr (cddr node))))
		 (when (eq (car node) name)
			 (setq result node))
		 (when children
			 (let ((subresult
						(delq nil
							(mapcar (lambda (n)
										  (agenda.app--get-children n name))
								children))))
			  (if subresult
				  (if result
					 (setq result (append result subresult))
					  (setq result subresult)))))
		 result))

(defun agenda.app--all-events (icalendar)
	 "Return the list of all existing events in the given ICALENDAR."
	 (let ((result '()))
		 (mapc (lambda (elt)
					 (setq result (append (agenda.app--get-children elt 'VEVENT)
											result)))
			(nreverse icalendar))
		 result))

(defun agenda.app--split-value (value-string)
	 "Split VALUE-STRING at `;='."
	 (let ((result '())
				param-name param-value)
		 (when value-string
			 (save-current-buffer
			  (set-buffer (get-buffer-create " *agenda.app-work*"))
			  (set-buffer-modified-p nil)
			  (erase-buffer)
			  (insert value-string)
			  (goto-char (point-min))
			  (while
				  (re-search-forward
					 "\\([A-Za-z0-9-]+\\)=\\(\\([^;:]+\\)\\|\"\\([^\"]+\\)\"\\);?"
					 nil t)
				 (setq param-name (intern (match-string 1)))
				 (setq param-value (match-string 2))
				 (setq result
					 (append result (list (list param-name param-value)))))))
		 result))

(defun agenda.app--convert-tz-offset (alist dst-p)
	 "Return a cons of two strings representing a timezone start.
ALIST is an alist entry from a VTIMEZONE, like STANDARD.
DST-P is non-nil if this is for daylight savings time.
The strings are suitable for assembling into a TZ variable."
	 (let* ((offsetto (car (cddr (assq 'TZOFFSETTO alist))))
				 (offsetfrom (car (cddr (assq 'TZOFFSETFROM alist))))
				 (rrule-value (car (cddr (assq 'RRULE alist))))
				 (rdate-p (and (assq 'RDATE alist) t))
				 (dtstart (car (cddr (assq 'DTSTART alist))))
				 (no-dst (or rdate-p (equal offsetto offsetfrom))))
		 ;; FIXME: the presence of an RDATE is assumed to denote the first day of the year
		 (when (and offsetto dtstart (or rrule-value no-dst))
			 (let* ((rrule (agenda.app--split-value rrule-value))
						(freq (cadr (assq 'FREQ rrule)))
						(bymonth (cadr (assq 'BYMONTH rrule)))
						(byday (cadr (assq 'BYDAY rrule))))
			  ;; FIXME: we don't correctly handle WKST here.
			  (if (or no-dst (and (string= freq "YEARLY") bymonth))
				  (cons
					 (concat
						 ;; Fake a name.
						 (if dst-p "DST" "STD")
						 ;; For TZ, OFFSET is added to the local time.  So,
						 ;; invert the values.
						 (if (eq (aref offsetto 0) ?-) "+" "-")
						 (substring offsetto 1 3)
						 ":"
						 (substring offsetto 3 5))
					 ;; The start time.
					 (let* ((day (if no-dst
										  1
										  (agenda.app--get-weekday-number (substring byday -2))))
								 (week (if no-dst
											  "1"
											 (if (eq day -1)
												byday
												 (substring byday 0 -2)))))
						 ;; "Translate" the iCalendar way to specify the last
						 ;; (sun|mon|...)day in month to the tzset way.
						 (if (string= week "-1")  ; last day as iCalendar calls it
							(setq week "5"))     ; last day as tzset calls it
						 (when no-dst (setq bymonth "1"))
						 (concat "M" bymonth "." week "." (if (eq day -1) "0"
																			(int-to-string day))
							 ;; Start time.
							 "/"
							 (substring dtstart -6 -4)
							 ":"
							 (substring dtstart -4 -2)
							 ":"
							 (substring dtstart -2)))))))))

(defun agenda.app--parse-vtimezone (alist)
	 "Turn a VTIMEZONE ALIST into a cons (ID . TZ-STRING).
Consider only the most recent date specification.
Return nil if timezone cannot be parsed."
	 (let* ((tz-id (agenda.app--convert-string-for-import
							 (agenda.app--get-event-property alist 'TZID)))
				 (daylight (cadr (cdar (agenda.app--get-most-recent-observance alist 'DAYLIGHT))))
				 (day (and daylight (agenda.app--convert-tz-offset daylight t)))
				 (standard (cadr (cdar (agenda.app--get-most-recent-observance alist 'STANDARD))))
				 (std (and standard (agenda.app--convert-tz-offset standard nil))))
		 (if (and tz-id std)
			(cons tz-id
				 (if day
				  (concat (car std) (car day)
					  "," (cdr day) "," (cdr std))
				  (car std))))))

(defun agenda.app--get-most-recent-observance (alist sub-comp)
	 "Return the latest observance for SUB-COMP DAYLIGHT or STANDARD.
ALIST is a VTIMEZONE potentially containing historical records."
														;FIXME?: "most recent" should be relative to a given date
	 (let ((components (agenda.app--get-children alist sub-comp)))
		 (list
			(car
				(sort components
				  (lambda (a b)
					  (let* ((get-recent (lambda (n)
													 (car
														(sort
														  (delq nil
															  (mapcar (lambda (p)
																			 (and (memq (car p) '(DTSTART RDATE))
																				(car (cddr p))))
																  n))
														  'string-greaterp))))
								 (a-recent (funcall get-recent (car (cddr a))))
								 (b-recent (funcall get-recent (car (cddr b)))))
						(string-greaterp a-recent b-recent))))))))

(defun agenda.app--convert-all-timezones (icalendar)
	 "Convert all timezones in the ICALENDAR into an alist.
Each element of the alist is a cons (ID . TZ-STRING),
like `agenda.app--parse-vtimezone'."
	 (let (result)
		 (dolist (zone (agenda.app--get-children (car icalendar) 'VTIMEZONE))
			 (setq zone (agenda.app--parse-vtimezone zone))
			 (if zone
			  (setq result (cons zone result))))
		 result))

(defun agenda.app--find-time-zone (prop-list zone-map)
	 "Return a timezone string for the time zone in PROP-LIST, or nil if none.
ZONE-MAP is a timezone alist as returned by `agenda.app--convert-all-timezones'."
	 (let ((id (plist-get prop-list 'TZID)))
		 (if id
			(cdr (assoc id zone-map)))))


(defvar agenda.app--debug-mode-p nil
	 "Enable agenda.app debug messages.")

(defun agenda.app--dmsg (&rest args)
	 "Print message ARGS if `agenda.app-debug' is non-nil."
	 (if agenda.app--debug-mode-p
		  (apply 'message args)))

(defun agenda.app--decode-isodatetime
	  (isodatetimestring &optional day-shift
		  source-zone
		  result-zone)
	 "Return ISODATETIMESTRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.  If
ISODATETIMESTRING specifies UTC time (trailing letter Z) the
decoded time is given in the local time zone!  If optional
parameter DAY-SHIFT is non-nil the result is shifted by DAY-SHIFT
days.
SOURCE-ZONE, if provided, is the timezone for decoding the time,
in any format understood by `encode-time'.
RESULT-ZONE, if provided, is the timezone for encoding the result
in any format understood by `decode-time'.
FIXME: multiple comma-separated values should be allowed!"
	 (agenda.app--dmsg isodatetimestring)
	 (if isodatetimestring
		  ;; day/month/year must be present
		  (let ((year  (read (substring isodatetimestring 0 4)))
					(month (read (substring isodatetimestring 4 6)))
					(day   (read (substring isodatetimestring 6 8)))
					(hour 0)
					(minute 0)
					(second 0))
			(when (> (length isodatetimestring) 12)
			  ;; hour/minute present
			  (setq hour (read (substring isodatetimestring 9 11)))
			  (setq minute (read (substring isodatetimestring 11 13))))
			(when (> (length isodatetimestring) 14)
			  ;; seconds present
			  (setq second (read (substring isodatetimestring 13 15))))
			;; FIXME: Support subseconds.
			(when (> (length isodatetimestring) 15)
			  (pcase (aref isodatetimestring 15)
				  (?Z
					 (setq source-zone t))
				  ((or ?- ?+)
					 (setq source-zone
						 (concat "UTC" (substring isodatetimestring 15))))))
			;; shift if necessary
			(if day-shift
				(let ((mdy (calendar-gregorian-from-absolute
								  (+ (calendar-absolute-from-gregorian
										  (list month day year))
									  day-shift))))
					(setq month (nth 0 mdy))
					(setq day   (nth 1 mdy))
					(setq year  (nth 2 mdy))))
			;; create the decoded date-time
			;; FIXME!?!
			(let ((decoded-time (list second minute hour day month year
											 nil -1 source-zone)))
			  (condition-case nil
					(decode-time (encode-time decoded-time) result-zone)
				  (error
					 (message "Cannot decode \"%s\"" isodatetimestring)
					 ;; Hope for the best....
					 decoded-time))))
		 ;; isodatetimestring == nil
		 nil))

(defun agenda.app--decode-isoduration (isodurationstring
																&optional duration-correction)
	 "Convert ISODURATIONSTRING into format provided by `decode-time'.
Converts from ISO-8601 to Emacs representation.  If ISODURATIONSTRING
specifies UTC time (trailing letter Z) the decoded time is given in
the local time zone!

Optional argument DURATION-CORRECTION shortens result by one day.

FIXME: TZID-attributes are ignored....!
FIXME: multiple comma-separated values should be allowed!"
	 (if isodurationstring
		  (save-match-data
			(string-match
			  (concat
				 "^P[+-]?\\("
				 "\\(\\([0-9]+\\)D\\)"         ; days only
				 "\\|"
				 "\\(\\(\\([0-9]+\\)D\\)?T\\(\\([0-9]+\\)H\\)?" ; opt days
				 "\\(\\([0-9]+\\)M\\)?\\(\\([0-9]+\\)S\\)?\\)"  ; mand. time
				 "\\|"
				 "\\(\\([0-9]+\\)W\\)"         ; weeks only
				 "\\)$") isodurationstring)
			(let ((seconds 0)
						(minutes 0)
						(hours 0)
						(days 0)
						(months 0)
						(years 0))
			  (cond
				  ((match-beginning 2)         ;days only
					 (setq days (read (substring isodurationstring
												  (match-beginning 3)
												  (match-end 3))))
					 (when duration-correction
						 (setq days (1- days))))
				  ((match-beginning 4)         ;days and time
					 (if (match-beginning 5)
						(setq days (read (substring isodurationstring
													  (match-beginning 6)
													  (match-end 6)))))
					 (if (match-beginning 7)
						(setq hours (read (substring isodurationstring
														(match-beginning 8)
														(match-end 8)))))
					 (if (match-beginning 9)
						(setq minutes (read (substring isodurationstring
															(match-beginning 10)
															(match-end 10)))))
					 ;; FIXME: Support subseconds.
					 (if (match-beginning 11)
						(setq seconds (read (substring isodurationstring
															(match-beginning 12)
															(match-end 12))))))
				  ((match-beginning 13)        ;weeks only
					 (setq days (* 7 (read (substring isodurationstring
														  (match-beginning 14)
														  (match-end 14)))))))
			  (list seconds minutes hours days months years)))
		 ;; isodatetimestring == nil
		 nil))

(defun agenda.app--add-decoded-times (time1 time2)
	 "Add TIME1 to TIME2.
Both times must be given in decoded form.  One of these times must be
valid (year > 1900 or something)."
	 ;; FIXME: does this function exist already?  Can we use decoded-time-add?
	 (decode-time (encode-time
							;; FIXME: Support subseconds.
							(time-convert (time-add (decoded-time-second time1)
													 (decoded-time-second time2))
								  'integer)
							(+ (decoded-time-minute time1) (decoded-time-minute time2))
							(+ (decoded-time-hour time1) (decoded-time-hour time2))
							(+ (decoded-time-day time1) (decoded-time-day time2))
							(+ (decoded-time-month time1) (decoded-time-month time2))
							(+ (decoded-time-year time1) (decoded-time-year time2))
							nil
							nil
							;;(or (nth 6 time1) (nth 6 time2)) ;; FIXME?
							)))

(defun agenda.app--datetime-to-org-date (datetime)
	 "Convert the decoded DATETIME to ISO format.
Optional argument SEPARATOR gives the separator between month,
day, and year.  If nil a blank character is used as separator.
ISO format: (year month day)."
	 (if datetime
		  (format "%04d-%02d-%02d" (nth 5 datetime) ;year
			  (nth 4 datetime)            ;month
			  (nth 3 datetime))           ;day
		 ;; datetime == nil
		 nil))

(defun agenda.app--datetime-to-colontime (datetime)
	 "Extract the time part of a decoded DATETIME into 24-hour format.
Note that this silently ignores seconds."
	 (format "%02d:%02d" (nth 2 datetime) (nth 1 datetime)))


(defun agenda.app--get-month-number (monthname)
	 "Return the month number for the given MONTHNAME."
	 (catch 'found
		 (let ((num 1)
				  (m (downcase monthname)))
			 (mapc (lambda (month)
						 (let ((mm (downcase month)))
						  (if (or (string-equal mm m)
									(string-equal (substring mm 0 3) m))
							  (throw 'found num))
						  (setq num (1+ num))))
				calendar-month-name-array))
		 ;; Error:
		 -1))

(defun agenda.app--get-weekday-number (abbrevweekday)
	 "Return the number for the ABBREVWEEKDAY."
	 (if abbrevweekday
		  (catch 'found
			(let ((num 0)
						(aw (downcase abbrevweekday)))
			  (mapc (lambda (day)
						  (let ((d (downcase day)))
							  (if (string-equal d aw)
								 (throw 'found num))
							  (setq num (1+ num))))
				  agenda.app--weekday-array)))
		 ;; Error:
		 -1))

(defun agenda.app--get-weekday-numbers (abbrevweekdays)
	 "Return the list of numbers for the comma-separated ABBREVWEEKDAYS."
	 (when abbrevweekdays
		 (let* ((num -1)
					 (weekday-alist (mapcar (lambda (day)
														  (progn
															  (setq num (1+ num))
															  (cons (downcase day) num)))
											  agenda.app--weekday-array)))
			 (delq nil
				(mapcar (lambda (abbrevday)
								(cdr (assoc abbrevday weekday-alist)))
					(split-string (downcase abbrevweekdays) ","))))))

(defun agenda.app--get-weekday-abbrev (weekday)
	 "Return the abbreviated WEEKDAY."
	 (catch 'found
		 (let ((num 0)
				  (w (downcase weekday)))
			 (mapc (lambda (day)
						 (let ((d (downcase day)))
						  (if (or (string-equal d w)
									(string-equal (substring d 0 3) w))
							  (throw 'found (aref agenda.app--weekday-array num)))
						  (setq num (1+ num))))
				calendar-day-name-array))
		 ;; Error:
		 nil))

(defun agenda.app--date-to-isodate (date &optional day-shift)
	 "Convert DATE to iso-style date.
DATE must be a list of the form (month day year).
If DAY-SHIFT is non-nil, the result is shifted by DAY-SHIFT days."
	 (let ((mdy (calendar-gregorian-from-absolute
						  (+ (calendar-absolute-from-gregorian date)
							(or day-shift 0)))))
		 (format "%04d%02d%02d" (nth 2 mdy) (nth 0 mdy) (nth 1 mdy))))

(defun agenda.app--convert-string-for-import (string)
	 "Remove escape chars for comma, semicolon etc. from STRING."
	 (string-replace
		 "\\n" "\n " (string-replace
							 "\\\"" "\"" (string-replace
												  "\\;" ";" (string-replace
																	"\\," "," string)))))

(defun agenda.app--🇺🇸 (date)
	 (let ((date-parts (split-string-and-unquote date "-")))
		 (mapcar #'string-to-number
			 `(,(nth 1 date-parts) ,(nth 2 date-parts) ,(nth 0 date-parts)))))

;;;###autoload
(defun agenda.app/import-buffer nil
	 "Extract iCalendar events from current buffer.

Return code t means that importing worked well, return code nil
means that an error has occurred.  Error messages will be in the
buffer `*agenda.app-errors*'."
	 (interactive)
	 (save-current-buffer
		 ;; prepare ical
		 (message "preparing remote calendar file...")
		 (set-buffer (agenda.app--get-unfolded-buffer (current-buffer)))
		 (goto-char (point-min))
		 (message "preparing remote calendar file...done!")
		 (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
			(let (ical-contents ical-errors)
			  ;; read ical
			  (message "parsing icalendar file...")
			  (beginning-of-line)
			  (setq ical-contents (agenda.app--read-element nil nil))
			  (message "parsing icalendar file...done!")
			  ;; convert ical
			  (message "converting icalendar file to org...")
			  (save-excursion
				  (setq ical-errors (agenda.app--convert-ical-to-org ical-contents)))
			  ;; save the diary file if it is visited already
			  (let ((b (find-buffer-visiting agenda.app/filename)))
				  (when b
					  (save-current-buffer
						(set-buffer b)
						(save-buffer))))
			  (message "converting icalendar file to org...done!")
			  ;; return t if no error occurred
			  (not ical-errors))
			 (message
				"hey you fucker this isn't an icalendar buffer!")
			 ;; return nil, i.e. import did not work
			 nil)))

(defun agenda.app--gather-ids-in-org-file nil
	 (let (ids)
		 (with-current-buffer (find-file-noselect agenda.app/filename)
			 (org-map-entries
				(lambda nil
				 (push (substring-no-properties (or (org-id-get) "")) ids))
				"LEVEL=1" 'file)
			 ids)))

(defun agenda.app--convert-ical-to-org (ical-list)
	 "Convert iCalendar data to an Emacs diary file.
Import VEVENTS from the iCalendar object ICAL-LIST and saves them
to the configured org file.

This function attempts to return t if something goes wrong.  In this
case an error string which describes all the errors and problems is
written into the buffer `*agenda.app-errors*'."
	 (let* ((events (agenda.app--all-events ical-list))
				 (org-entry-ids (agenda.app--gather-ids-in-org-file))
				 (error-string "")
				 (event-ok-p t)
				 (found-error nil)
				 (zone-map (agenda.app--convert-all-timezones ical-list))
				 (inhibit-redisplay t)
				 (event-ids '())
				 event org-date-string)
		 ;; step through all events/appointments
		 (while events
			 (setq event (car events))
			 (setq events (cdr events))
			 (setq event-ok-p nil)
			 (condition-case-unless-debug error-val
			  (let* ((dtstart (agenda.app--get-event-property event 'DTSTART))
						  (dtstart-zone (agenda.app--find-time-zone
												  (agenda.app--get-event-property-attributes
													 event 'DTSTART)
												  zone-map))
						  (dtstart-dec (agenda.app--decode-isodatetime dtstart nil
														dtstart-zone))
						  (start-d (agenda.app--datetime-to-org-date
											dtstart-dec))
						  (start-t (and dtstart
											 (> (length dtstart) 8)
											 (agenda.app--datetime-to-colontime dtstart-dec)))
						  (dtend (agenda.app--get-event-property event 'DTEND))
						  (dtend-zone (agenda.app--find-time-zone
												 (agenda.app--get-event-property-attributes
													event 'DTEND)
												 zone-map))
						  (dtend-dec (agenda.app--decode-isodatetime dtend
													nil dtend-zone))
						  (dtend-1-dec (agenda.app--decode-isodatetime dtend -1
														dtend-zone))
						  end-d
						  end-1-d
						  end-t
						  (summary (agenda.app--convert-string-for-import
											(or (agenda.app--get-event-property event 'SUMMARY)
												"No summary")))
						  (rrule (agenda.app--get-event-property event 'RRULE))
						  (rdate (agenda.app--get-event-property event 'RDATE))
						  (duration (agenda.app--get-event-property event 'DURATION)))
				  (agenda.app--dmsg "%s: `%s'" start-d summary)
				  ;; check whether start-time is missing
				  (if  (and dtstart
							  (string=
								 (cadr (agenda.app--get-event-property-attributes
											 event 'DTSTART))
								 "DATE"))
					 (setq start-t nil))
				  (when duration
					  (let ((dtend-dec-d (agenda.app--add-decoded-times
													dtstart-dec
													(agenda.app--decode-isoduration duration)))
								(dtend-1-dec-d (agenda.app--add-decoded-times
														 dtstart-dec
														 (agenda.app--decode-isoduration duration
																 t))))
						(if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
							(message "Inconsistent endtime and duration for %s"
								summary))
						(setq dtend-dec dtend-dec-d)
						(setq dtend-1-dec dtend-1-dec-d)))
				  (setq end-d (if dtend-dec
										(agenda.app--datetime-to-org-date dtend-dec)
									  start-d))
				  (setq end-1-d (if dtend-1-dec
											(agenda.app--datetime-to-org-date dtend-1-dec)
										  start-d))
				  (setq end-t (if (and
											 dtend-dec
											 (not (string=
														(cadr
														  (agenda.app--get-event-property-attributes
															 event 'DTEND))
														"DATE")))
										(agenda.app--datetime-to-colontime dtend-dec)))
				  (agenda.app--dmsg "start-d: %s, end-d: %s" start-d end-d)
				  (cond
					 ;; recurring event
					 (rrule

						 ;; This is a nightmare to express in `org-mode'.  `org-mode' has no concept of
						 ;; "until" but, `org-mode' understands diary sexps, so hopefully we can use a
						 ;; diary sexp here.
						 ;;
						 ;; however: i _think_ google calendar actually gives you the cyclic info as well
						 ;; as giving you one event for every occurance... for some reason? this might be
						 ;; specifc to this one calendar. quién sabe.
						 ;; i'm wrong i'll take icalendar's code for the cyclic diary sexp.
						 ;; (setq org-date-string
						 ;; 	    (agenda.app--convert-recurring-to-org-date-string
						 ;; 	     event dtstart-dec start-t end-t))
						 ;;(setq event-ok-p t)
						 (agenda.app--dmsg "skipping recurring event"))
					 (rdate
						 (agenda.app--dmsg "rdate event")
						 (setq org-date-string "")
						 (mapc (lambda (_datestring)
									 (setq org-date-string
										(concat org-date-string "......")))
							(agenda.app--split-value rdate)))
					 ;; non-recurring event
					 ;; all-day event
					 ((not (string= start-d end-d))
						 (setq org-date-string
							(agenda.app--convert-non-recurring-all-day-to-org-date-string
							  start-d end-1-d))
						 (setq event-ok-p t))
					 ;; not all-day
					 ((and start-t (or (not end-t)
											  (not (string= start-t end-t))))
						 (setq org-date-string
							(agenda.app--convert-non-recurring-not-all-day-to-org-date-string
							  dtstart-dec start-t end-t))
						 (setq event-ok-p t))
					 ;; all-day event
					 (t
						 (agenda.app--dmsg "all day event")
						 (setq org-date-string
							(concat "<"
								(agenda.app--datetime-to-org-date
								  dtstart-dec)
								">"))
						 (setq event-ok-p t)))
				  (when (and
							  event-ok-p
							  (not (null org-date-string))
							  (not (string-empty-p org-date-string))
							  (< (abs (-
											 (calendar-absolute-from-gregorian
												(calendar-current-date))
											 (calendar-absolute-from-gregorian
												(agenda.app--🇺🇸 start-d))))
								  agenda.app/context-days))
					  (push (agenda.app--add-or-update-entry event org-date-string)
						 event-ids)))
			  ;; event was not ok
			  ;;(setq found-error t)
			  ;; FIXME: inform user about ignored event properties
			  ;; handle errors
			  (error
				 (setq found-error t)
				 (save-current-buffer
					 (set-buffer (get-buffer-create "*agenda.app-errors*"))
					 (insert (format-message "Error in line %d -- %s\n"
									  (count-lines (point-min) (point))
									  error-val))))))

		 (mapc
			(lambda (id)
				(let* ((loc (org-id-find-id-in-file id agenda.app/filename))
							(char (cdr loc)))
				 (with-current-buffer (find-file-noselect agenda.app/filename)
					 (goto-char char)
					 (message (format "Archiving outdated event: %s" (org-get-heading)))
					 (org-archive-subtree))))
			(cl-set-difference org-entry-ids event-ids :test 'string=))

		 ;; insert final newline
		 (let ((b (find-buffer-visiting agenda.app/filename)))
			 (when b
			  (save-current-buffer
				 (set-buffer b)
				 (goto-char (point-max))
				 (insert "\n")
				 (save-buffer))))
		 (when found-error
			 (save-current-buffer
			  (set-buffer (get-buffer-create "*agenda.app-errors*"))
			  (erase-buffer)
			  (insert error-string)))
		 (message "converting icalendar...done!")
		 found-error))

(defun agenda.app--convert-non-recurring-all-day-to-org-date-string
	  (start-d end-d)
	 "Convert non-recurring iCalendar EVENT to org date format.

DTSTART is the decoded DTSTART property of E.
Argument START-D gives the first day.
Argument END-D gives the last day."
	 (agenda.app--dmsg "non-recurring all-day event")
	 (format "<%s>--<%s>" start-d end-d))

(defun agenda.app--convert-non-recurring-not-all-day-to-org-date-string
	  (dtstart-dec
		 start-t
		 end-t)
	 "Convert recurring icalendar EVENT to org date format.

DTSTART-DEC is the decoded DTSTART property of E.
START-T is the event's start time in diary format.
END-T is the event's end time in diary format."
	 (agenda.app--dmsg "not all day event")
	 (cond (end-t
				 (format "<%s %s-%s>"
					 (agenda.app--datetime-to-org-date dtstart-dec)
					 start-t end-t))
		 (t
			(format "<%s %s>"
				(agenda.app--datetime-to-org-date dtstart-dec)
				start-t))))

(defun agenda.app--add-or-update-entry (event date-string)
	 "Add EVENT at DATE-STRING to the agenda.app file.
DATE-STRING must be a properly formatted valid org string."
	 (let ((id (agenda.app--get-event-property event 'UID))
				(status (agenda.app--get-event-property event 'STATUS))
				(summary (agenda.app--get-event-property event 'SUMMARY))
				(location
				  (agenda.app--get-event-property event 'LOCATION))
				(video-call
				  (agenda.app--get-event-property event 'X-GOOGLE-CONFERENCE))
				(description
				  (agenda.app--get-event-property event 'DESCRIPTION))
				(organizer
				  (or (agenda.app--get-event-property event 'ORGANIZER) "me"))
				new)
		 (with-current-buffer (find-file-noselect agenda.app/filename)
			 ;; TODO find a previous version and update
			 (let ((existing-entry (org-id-find-id-in-file id agenda.app/filename)))
			  (if existing-entry
				  (progn
					  (goto-char (cdr existing-entry))
					  (org-edit-headline summary))
				 (progn
					 (goto-char (point-min))
					 (insert "* " summary "\n")
					 (forward-line -1)
					 (setq new t)))
			  (org-remove-timestamp-with-keyword "SCHEDULED:")
			  (org-insert-property-drawer)
			  (forward-line)
			  (let ((start-of-property-drawer (point)))
				 (org-end-of-meta-data)
				 (delete-region start-of-property-drawer (point)))
			  (insert ":PROPERTIES:\n")
			  (mapc (lambda (prop)
						  (let ((value (symbol-value prop)))
							 (unless (string-empty-p value)
								 (insert (format ":%s: %s\n" (upcase (symbol-name prop)) value)))))
					'(id organizer status location video-call))
			  (insert ":END:\n")
			  (insert "SCHEDULED: " date-string "\n")
			  ;; TODO what if someone updates the meeting invite description? i should
			  ;; delete the quote if it doesn't match the new one
			  (when new
				 (insert "#+begin_quote\n\n#+end_quote\n\n")
				 (forward-line -3)
				 (let ((description-start (point)) description-end org e)
					 (unless (string-empty-p description)
						 (insert (string-replace "\\," "," description) "\n")
						 (message description)
						 (setq description-end (point))
						 (agenda.app--strip-html-in-region
							description-start
							description-end))))))
		 (substring-no-properties id)))

(provide 'agenda-app)
;;; agenda-app ends here
