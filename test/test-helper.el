(defun should-color (fn code)
  "Make sure calling FN will return a CODE colored string."
  (should-ansi fn code))

(defun should-style (fn code)
  "Make sure calling FN will return a CODE styled string."
  (should-ansi fn code))

(defun should-ansi (fn code)
  "Make sure calling FN will return a CODE ansied string."
  (let* ((actual (funcall fn "string"))
         (expected (format "\e[%smstring\e[0m" code)))
    (should (equal actual expected))))