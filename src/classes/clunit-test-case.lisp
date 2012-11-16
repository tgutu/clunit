(in-package :clunit)

(defclass CLUNIT-TEST-CASE ()
	((name			:initarg :name			:initform nil)
	 (dependencies	:initarg :dependencies	:initform (list))
	 (test-function	:initarg :test-function	:initform nil)))

;;	The DEPENDENCIES slot holds the names of the test cases that this test case depends on.
;;	Using an indirect reference like this allows us to easily undefine a test case without
;;	much cleaning up.
;;
;;	When we execute a test case and we resolve the references of the tests it depends on.
;;	If the object is not found it means the reference is now stale, so we remove the name
;;	from the list.