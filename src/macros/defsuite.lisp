(in-package :clunit)

;; DEFSUITE Algorithm:
;; 1. Test if all parent suites are defined, if not signal an error.
;; 2. Test for circularity in test suite hierarchy.
;; 3. Add child reference in each of its parent's CHILD-SUITES slot.
;; 4. Create new test suite instance and add it to lookup table.
;;
(defmacro defsuite (name parents)
	"Defines a test suite called NAME. If PARENTS is non-NIL the test suite is defined as a sub-suite of each of the test suites in PARENTS."
	(with-gensyms (parent-list)
		`(let ((,parent-list ',parents))

			(unless (symbolp ',name)
				(error "In (defsuite name parents . body), NAME should be a symbol not ~S." ',name))
		
			(unless (listp ,parent-list)
				(error "In (defsuite name parents . body), PARENTS should be a list not ~S." ,parent-list))

			(unless (every #'symbolp ,parent-list)
				(error "In ~S, every name should be a symbol." ,parent-list))

			;; Make sure that all the parents are test suites.
			(loop
				:for parent :in ,parent-list
				:do (unless (get-test-suite parent)
						(error "Trying to define test suite ~S, but one of its parents ~S is not a test suite." ',name parent)))

			;; Test for circularity in test hierarchy.

			;; Add test suite reference to each of its parent's CHILD-SUITES slot.
			(loop
				:for parent :in ,parent-list
				:do	(pushnew ',name (slot-value (get-test-suite parent) 'child-suites)))

			;; Create new test suite instance and add it to lookup table.
			(setf (get-test-suite ',name) (make-instance 'clunit-test-suite :name ',name)))))



;; UNDEFSUITE Algorithm:
;; 1. Check if test suite is defined, if its not throw an error.
;; 2. Undefine test suite
;;
;; N.B. Remember we do not bother tracing the parent suites because they will update their references when they realise this test suite no longer exists.
;;
(defmacro undefsuite (name)
	`(progn
		(unless (symbolp ',name)
				(error "In (undefsuite name), NAME should be a symbol not ~S." ',name))

		(unless (get-test-suite ',name)
			(error "In (undefsuite name), test suite ~S is not defined. " ',name))

		(delete-test-suite ',name)))

