(in-package #:holly)

(defvar *google-calendar-url* (with-open-file (f "/home/ryan/.holly.calendar-url")
				(read-line f))
  "the private URL from google calender, lets me bypass authorization")

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
	(collect (list (make-x10-timer dev T start) start dev))
	(collect (list (make-x10-timer dev nil end) end dev))))))

(defun calendar-url (&optional (base-url *google-calendar-url*))
  (format nil "~astart-min=~a&start-max=~a"
	  base-url
	  (local-time:to-rfc3339-timestring (local-time:now))
	  (local-time:to-rfc3339-timestring
	   (local-time:timestamp+ (local-time:now)
								      1 :month))))

(defun get-calendar-entries ()
  "creates calendar-entry objects based on the xml returned by the calendar url"
  (iterate
    (with xml = (cxml:make-source
		 (drakma:http-request (calendar-url))))
    (while (klacks:find-element xml "entry"))
    (for title = (progn
		   (klacks:find-element xml "title")
		   (klacks:consume xml)
		   (klacks:consume-characters xml)))
    (when (starts-with-subseq "x10" title)
      (collect
	  (make-instance
	   'calendar-entry
	   :title title
	   :times ;;find gp:when blocks
	   (sort (iter (for next = (multiple-value-list (klacks:peek xml)))
		       ;;stop search when we're peeking at the end of entry
		       (until (and (eq (first next) :end-element)
				   (string-equal (nth 2 next) "entry")))
		       (when (and (eq (first next) :start-element)
				  (string-equal (nth 2 next) "when"))
			 (let ((start-end (list "-" "-")))
			   (klacks:map-attributes
			    #'(lambda (ns lname qname val default-p)
				(declare (ignore ns qname default-p))
				(when (string= lname "startTime")
				  (setf (first start-end)
					(local-time:parse-timestring val)))
				(when (string= lname "endTime")
				  (setf (second start-end)
					(local-time:parse-timestring val))))
			    xml)
			   (when (local-time:timestamp> (first start-end) (local-time:now))
			     (collect start-end))))
		       (klacks:consume xml))
		 #'local-time:timestamp<
		 :key #'first))))))

;;now to trigger and schedule the x10 commands
(defun reschedule-timers ()
  (mapcar #'trivial-timers:unschedule-timer *x10-timers*)
  (setf *x10-timers* nil)
  (iter (for (timer lt dev) in (make-device-timers))
	(trivial-timers:schedule-timer timer
				       (local-time:timestamp-to-universal lt)
				       :absolute-p T)))