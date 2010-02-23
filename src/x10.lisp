(in-package #:holly)

(defstruct x10-device
  (name "" :type string :read-only T )
  (code "" :type string :read-only T )
  (state 0 :type bit))

(defvar *x10-devices* (list (make-x10-device :name "TV Lamp" :code "a1")
			    (make-x10-device :name "Bar Lights" :code "a3")
			    (make-x10-device :name "Chandelier" :code "a5")))

(defvar *x10-processor* nil "thread for serializing x10 req.")
(defvar *x10-channel* (make-instance 'chanl:bounded-channel :size 5)
  "channel to convey x10 commands, max queue length 5")

(defcategory x10)

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
