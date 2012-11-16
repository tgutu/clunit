(in-package :clunit)

;; DEFSUITE Algorithm:
;; 1. If test suite already exists, undefine it first.
;; 2. Test if all parent suites are defined, if not signal an error.
;; 3. Test for circularity in test suite hierarchy.
;; 4. Add child reference in each of its parent's CHILD-SUITES slot.
;; 5. Create new test suite instance and add it to lookup table.
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

			(setf ,parent-list (remove-duplicates ,parent-list))

			;; Make sure that all the parents are test suites.
			(loop
				:for parent :in ,parent-list
				:do (unless (get-test-suite parent)
						(error "Trying to define test suite ~S, but one of its parents ~S is not a test suite." ',name parent)))

			;; Test for circularity in test hierarchy.

			;; Undefine the test suite if it already exists.
			(if (get-test-suite ',name)
				(undefsuite ,name))

			;; Add test suite reference to each of its parent's CHILD-SUITES slot.
			(loop
				:for parent :in ,parent-list
				:do	(pushnew ',name (slot-value (get-test-suite parent) 'child-suites)))

			;; Create new test suite instance and add it to lookup table.
			(setf (get-test-suite ',name) (make-instance 'clunit-test-suite :name ',name :parent-suites ,parent-list)))))



;; UNDEFSUITE Algorithm:
;; 1. Check if test suite is defined, if its not throw an error.
;; 2. Resolve all the parent references.
;; 3. For those parents that still exist, remove this test suite's reference.
;;
(defmacro undefsuite (name)
	`(progn
		(unless (symbolp ',name)
				(error "In (undefsuite name), NAME should be a symbol not ~S." ',name))

		(unless (get-test-suite ',name)
			(error "In (undefsuite name), test suite ~S is not defined. " ',name))

		(loop
			:for parent :in (slot-value (get-test-suite ',name) 'parent-suites)
			:do 
				(let ((test-suite (get-test-suite parent)))
					(if test-suite
						(setf (slot-value test-suite 'child-suites) (delete ',name (slot-value test-suite 'child-suites))))))

		(delete-test-suite ',name)))

