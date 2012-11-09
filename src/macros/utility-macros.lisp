(in-package :clunit)

(defmacro with-gensyms (syms &body body)
	"WITH-GENSYMS takes a list of symbols SYM and a list of body forms. It returns a LET clause with each symbol in SYM bound to a gensymbol and the body forms as the body of the let clause."
	`(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
		,@body))

(defmacro with-assert-restart (&body body)
	"WITH-ASSERT-RESTART places a restart called SKIP-ASSERTION around an assertion form."
	`(restart-case (progn ,@body)
		;; Strictly speaking, there is no actual difference between the SKIP and CONTINUE restarts.
		;; Their naming is only for conveying a semantic message when displayed in the debugger.
		(continue () 
			:report (lambda (s) (format s "Continue with ~S test." *test-name*))
			nil)
		(continue-without-debugging ()
			:report (lambda (s) (format s "Continue unit test without interactive debugging."))
			(setf *use-debugger* nil))
		(skip-assertion () 
			:report (lambda (s) (format s "Skip assertion in test ~S." *test-name*))
			nil)))

(defmacro with-test-restart (&body body)
	`(restart-case (progn ,@body)
		(continue-without-debugging ()
			:report (lambda (s) (format s "Continue unit test without interactive debugging."))
			(setf *use-debugger* nil))
		(skip-test ()
			:report (lambda (s) (format s "Skip test ~S." *test-name*))
			nil)))
