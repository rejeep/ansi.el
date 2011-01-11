(defun should-color (fn code)
  "Make sure calling FN will return a CODE colored string."
  (let* ((actual (funcall fn "string"))
         (expected (format "\e[%smstring\e[0m" code)))
    (should (equal actual expected))))