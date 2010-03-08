(in-package #:holly)

(defvar *google-calendar-url* (with-open-file (f "/home/ryan/.holly.calender-url")
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

(defun get-calendar-entries (&optional (calendar-url *google-calendar-url*))
  "creates calendar-entry objects based on the xml returned by the calendar url"
  (iterate
    (with xml = (cxml:make-source
		 (drakma:http-request calendar-url)))
    (while (klacks:find-element xml "entry"))
    (collect
	(make-instance
	 'calendar-entry
	 :title (progn
		  (klacks:find-element xml "title")
		  (klacks:consume xml)
		  (klacks:consume-characters xml))

	 :times ;;find gp:when blocks
	 (iter (for next = (multiple-value-list (klacks:peek xml)))
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
			  (setf (first start-end) val))
			(when (string= lname "endTime")
			  (setf (second start-end) val)))
		    xml)
		   (collect start-end)))
	       (klacks:consume xml))))))