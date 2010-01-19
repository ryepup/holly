(in-package #:holly)

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080))
  )
(defun entry-points ()
  (push 
   (hunchentoot:create-folder-dispatcher-and-handler "/" "/home/ryan/clbuild/source/holly/www/")
   hunchentoot:*dispatch-table*))

(defvar *x10-devices* '(("TV Lamp" "a1")
			("Bar Lights" "a3")))
(defun x10-table (stream)
  (cl-who:with-html-output (stream)
    (:table :border 1 :padding 3 :spacing 5 :width "100%"
     (:tbody
      (iterate (for (name code) in *x10-devices*)
	       (collect
		   (cl-who:htm (:tr (:td (cl-who:str name))
				    (:td :align "center" (x10-device-link code T stream))
				    (:td :align "center" (x10-device-link code nil stream))))))))))

(defun x10-device-link (code on-p s)
  (cl-who:with-html-output (s)
    (:a :href (x10-device-href code on-p)
	(cl-who:str (if on-p "ON" "OFF")))
    )
  )

(hunchentoot:define-easy-handler (x10 :uri "/x10") ()
  (cl-who:with-html-output-to-string (s)
    (:html
     (:body
      (:div (cl-who:str "Controls x10 devices"))
      (x10-table s)
      (alexandria:when-let ((code-dir (hunchentoot:session-value 'x10-switch)))
	(cl-who:htm (:div (cl-who:fmt "Set ~a to ~a" (car code-dir) (cadr code-dir)))))))))

(defun x10-device-href (code on-p)
  (format nil "/x10-switch?code=~a&dir=~a" code (if on-p "fon" "foff")))

(hunchentoot:define-easy-handler (x10-switch :uri "/x10-switch") (code dir)
  (x10-command code dir)
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value 'x10-switch) (list code dir))
  (hunchentoot:redirect "/x10?"))

(defun x10-command (code dir)
  (assert (member dir (list "fon" "foff") :test #'string=)
	  () "~a must be fon or foff" dir)
  (assert (member code *x10-devices* :key #'cadr :test #'string=)
	  () "~a must be in the list of devices: ~a" code *x10-devices*)
  (trivial-shell:shell-command (format nil "heyu ~a ~a" dir code)))