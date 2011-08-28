(in-package #:holly)

(defvar *google-calendar-url* (with-open-file (f "/home/ryan/.holly.calendar-url")
				(read-line f))
  "the private URL from google calender, lets me bypass authorization")

(defmacro cdrassoc (item alist)
  `(cdr (assoc ,item ,alist)))

(defclass calendar-entry ()
  ((title :accessor title :initarg :title)
   (times :accessor times :initarg :times :initform nil)))

(defmethod print-object ((obj calendar-entry) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Title:~a, ~d times"
	    (title obj)
	    (length (times obj)))))

(defun make-device-timers ()
  (iter
    (with entries = (get-calendar-entries))
    (for dev in *x10-devices*)
    ;;find the entries where device name is a subseq of entry
    (for next-time = (find (x10-device-name dev) entries
			   :key #'title
			   :test #'(lambda (x10-dev title)
				     (search x10-dev title))))
    (when next-time
      ;;find the next on/off time for the device
      (destructuring-bind (start end) (first (times next-time))
	(collect (make-x10-timer dev T start))
	(collect (make-x10-timer dev nil end))))))

;; need to be sure drakma knows how to convert this to a string
(push (cons "application" "json") drakma:*text-content-types*)

(defun calendar-url (&optional (base-url *google-calendar-url*))
  (format nil "~a?start-min=~a&start-max=~a&alt=jsonc"
	  base-url
	  (to-rfc3339-timestring (local-time:now))
	  (to-rfc3339-timestring
	   (local-time:timestamp+ (local-time:now) 1 :month))))

(defun %get-calendar-data ()
 (labels ((lookup (tree &rest keys)
	    (if keys
		(apply #'lookup
		       (cdrassoc (first keys) tree)
		       (rest keys))
		tree)))
   (iterate
     (with json = (json:decode-json-from-string
		   (drakma:http-request
		    (calendar-url)
		    :additional-headers '(("GData-Version" . "2")))))
     (for entry in (lookup json :data :items))
     (collect (list (cdrassoc :title entry)
		    (cdrassoc :when entry))))))

(defun get-calendar-entries ()
  "creates calendar-entry objects based on the xml returned by the calendar url"
  (iterate
    (for (title times) in (%get-calendar-data))
    (when (starts-with-subseq "x10" title)
      (collect
	  (make-instance
	   'calendar-entry
	   :title title
	   :times (sort (iter
			  (for start-end in times)
			  (collect (list
				    (parse-timestring (cdrassoc :start start-end))
				    (parse-timestring (cdrassoc :end start-end)))))
			#'local-time:timestamp<
			:key #'first))))))

;;now to trigger and schedule the x10 commands
(defun reschedule-timers ()
  (iter (for x10-timer = (pop *x10-timers*))
	(while x10-timer)
	(trivial-timers:unschedule-timer (timer x10-timer)))
  (iter (for x10-timer in (make-device-timers))
	(trivial-timers:schedule-timer (timer x10-timer)
				       (local-time:timestamp-to-universal (timestamp x10-timer))
				       :absolute-p T)))