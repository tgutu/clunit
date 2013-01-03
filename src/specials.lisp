(in-package :clunit)


(defvar *test-suite-hashtable*	(make-hash-table :test #'eq)	"Holds CLUNIT-TEST-SUITE instances. The key of each test suite object is the symbol name.")
(defvar *test-case-hashtable*	(make-hash-table :test #'eq)	"Holds CLUNIT-TEST-CASE instances. The key of each test case object is the symbol name.")


(defvar *tab-width*		4		"Number of tab columns to use for indentation (in the reporting functions).")


(defvar *report-progress*	t		"This variable switches on progress reporting if set to true.")
(defvar *use-debugger*		nil		"If set to true, the debugger is invoked whenever an assertion fails.")
(defvar *stop-on-fail*		nil		"If any assertion fails or an error condition occurs, stop the unit test.")


(defvar *test-name*		nil		"Holds the name of the test currently executing.")
(defvar *suite-name*	nil		"Holds a list of suite symbol names in their current calling order. First called suite at the front, currently executing suite at the back.")


(defvar *clunit-report*			nil			"Holds a CLUNIT-REPORT object during a test run, used to store aggregated test reports.")
(defvar *last-clunit-report*	nil			"Holds the last returned CLUNIT-REPORT object. Default optional argument to the function RERUN-FAILED-TESTS.")


(defvar *clunit-test-report*	nil			"Holds a CLUNIT-TEST-REPORT object for each executing test case.")
(defvar *queued-test-reports*	nil			"A list of queued CLUNIT-TEST-REPORT objects whose test case execution has been deferred until their dependencies requirements are met.")
(defvar *clunit-report-format*	:default	"Controls the output format of the unit test results. Possible values are :default, :tap or NIL.")
;;	:DEFAULT	=>	Default reporting format.
;;	:TAP		=>  Prints the unit test output in the most recent version of TAP (Test Anything Protocol).
;;	 NIL		=>	Do not print the test results.
