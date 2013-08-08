(defmacro should-style (fn code)
  `(should-ansi ,fn ,code))

(defmacro should-color (fn code)
  `(should-ansi ,fn ,code))

(defmacro should-ansi (fn code)
  `(let ((actual (,fn "string"))
         (expected (format "\e[%dm%s\e[0m" ,code "string")))
     (should (equal expected actual))))
