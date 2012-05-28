(in-package #:holly)
(defcategory x10)

(defstruct x10-device
  (name "" :type string :read-only T )
  (code "" :type string :read-only T )
  (state 0 :type bit))

(defvar *x10-devices*
  (iter (for (name code) in '(("TV Lamp" "a1")
			      ("Bar Lights" "a3")
			      ("Chandalier" "a5")
			      ("Porch" "a2")))
	(collect (make-x10-device :name name :code code))))

(defvar *x10-processor* nil "thread for serializing x10 req.")
(defvar *x10-channel* (make-instance 'chanl:bounded-channel :size 50)
  "channel to convey x10 commands, max queue length 5")
(defvar *x10-timers* nil "list of active timers")


(defun process-x10 (channel-msg)
  (destructuring-bind (code dir) channel-msg
    (assert (member dir (list "fon" "foff") :test #'string=)
	    () "~a must be fon or foff" dir)
    (assert (member code *x10-devices* :key #'x10-device-code :test #'string=)
	    () "~a must be in the list of devices: ~a" code *x10-devices*)
    (let ((cmd (format nil "heyu ~a ~a" dir code)))
      (log-for x10 cmd)
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd)))
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd)))
      (setf (x10-device-state (find code *x10-devices* :key #'x10-device-code
				    :test #'string=))
	    (if (string= dir "fon") 1 0)))))

(defun make-processor ()
  (setf *x10-processor*
	(chanl:pexec (:name "X10 Processor")
	  (iterate (for x = (chanl:recv *x10-channel*))
		   (until (eql x :shutdown))
		   (process-x10 x)))))

(defgeneric change-x10-device (device direction)
  (:method (device (direction T))
    (change-x10-device device (if direction "fon" "foff")))
  (:method ((dev x10-device) direction)
    (change-x10-device (x10-device-code dev) direction))
  (:method ((code string) (direction string))
    (chanl:send *x10-channel* (list code direction))))

(defun make-x10-timer (dev on-p timestamp)
  (let ((timer (make-instance 'x10-device-timer
			      :device dev
			      :timer (trivial-timers:make-timer
				      #'(lambda () (change-x10-device dev on-p))
				      :name (format nil "Turn ~a ~a at ~a"
						    (x10-device-name dev)
						    (if on-p "on" "off")
						    (local-time:to-rfc1123-timestring timestamp)))
			      :timestamp timestamp)))
    (push timer *x10-timers*)
    timer))

(defclass x10-device-timer ()
  ((device :accessor device :initarg :device)
   (timer :accessor timer :initarg :timer)
   (timestamp :accessor timestamp :initarg :timestamp)))

