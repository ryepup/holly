(in-package :holly)

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8082))
  (make-processor)
  (start-stream-sender 'all "/tmp/holly.log"
		       :category-spec '(dribble+ x10)
		       :output-spec '(time category
				      context message))
  (entry-points)
  (timer:schedule-timer-relative
   (timer:make-timer #'reschedule-timers)
   5 (* 60 30))) ;;reschedule every 30 minutes