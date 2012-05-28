(in-package :holly)

(defparameter *chore-file* (reso))


(defun chores ()
  "returns the list of chores, when it was last done")

(defun make-chore (name warn-seconds crit-seconds))

(defun chore-done (name)
  (setf (last-done (find-chore name))
	 (get-univeral-time))
  )
