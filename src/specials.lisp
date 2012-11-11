(in-package :clunit)

(defvar *clunit-report* nil "Holds the CLUNIT-RESULT object during a test/suite execution run.")
(defvar *use-debugger* nil "If set to true, the debugger is invoked whenever an assertion fails.")
(defvar *stop-on-fail* nil "If any assertion fails or an error condition occurs, stop the unit test.")
(defvar *report-progress* t "This variable switches on progress reporting if set to true.")
(defvar *suite-name* nil "Holds a list of suite symbol names in their current calling order. First called suite at the front, currently executing suite at the back.")
(defvar *test-name* nil "Holds the name of the test currently executing.")
(defvar *tab-width* 4 "Number of tab columns to move.")

(defvar *clunit-report-format* :default "Controls the output format of the unit test results. Possible values are :default, :tap or NIL.")
;;	:DEFAULT	=>	Default reporting format.
;;	:TAP		=>  Prints the unit test output in the most recent version of TAP (Test Anything Protocol).
;;	 NIL		=>	Do not print the test results.
