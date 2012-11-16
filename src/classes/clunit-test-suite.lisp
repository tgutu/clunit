(in-package :clunit)

(defclass CLUNIT-TEST-SUITE ()
	((name			:initarg :name			:initform nil)
	 (test-cases	:initarg :test-cases	:initform (list))
	 (child-suites	:initarg :child-suites	:initform (list))
	 (parent-suites	:initarg :parent-suites	:initform (list))))

;;	The slots TEST-CASES, CHILD-SUITES and PARENT-SUITES hold the symbol names
;;	of test cases and test suites instead of the actual objects. Using an indirect
;;	reference like this allows us to undefine a test case or test suite.
;;
;;	When we execute a test suite and try to resolve the reference for an object.
;;	If the object is not found, it means the reference is now stale so the name
;;	of that test case or suite is removed.