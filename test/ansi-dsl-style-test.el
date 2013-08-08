(ert-deftest test-dsl-style-bold ()
  (with-ansi
   (should-style bold 1)))

(ert-deftest test-dsl-style-dark ()
  (with-ansi
   (should-style dark 2)))

(ert-deftest test-dsl-style-italic ()
  (with-ansi
   (should-style italic 3)))

(ert-deftest test-dsl-style-underscore ()
  (with-ansi
   (should-style underscore 4)))

(ert-deftest test-dsl-style-blink ()
  (with-ansi
   (should-style blink 5)))

(ert-deftest test-dsl-style-rapid ()
  (with-ansi
   (should-style rapid 6)))

(ert-deftest test-dsl-style-contrary ()
  (with-ansi
   (should-style contrary 7)))

(ert-deftest test-dsl-style-concealed ()
  (with-ansi
   (should-style concealed 8)))

(ert-deftest test-dsl-style-strike ()
  (with-ansi
   (should-style strike 9)))

(ert-deftest test-dsl-combined-styles ()
  (let ((actual
         (with-ansi
          (bold "foo")
          (italic "bar")))
        (expected "\e[1mfoo\e[0m\e[3mbar\e[0m"))
    (should (equal expected actual))))
