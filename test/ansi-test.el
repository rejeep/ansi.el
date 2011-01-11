(ert-deftest test-nesting ()
  (let ((actual (ansi-bold (ansi-red (ansi-blink "string"))))
        (expected "\e[1m\e[31m\e[5mstring\e[0m\e[0m\e[0m"))
    (should (equal actual expected))))

(ert-deftest test-nesting-in-dsl ()
  (let ((actual (with-ansi (bold (red (blink "string")))))
        (expected "\e[1m\e[31m\e[5mstring\e[0m\e[0m\e[0m"))
    (should (equal actual expected))))
