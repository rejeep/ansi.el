(ert-deftest test-dsl-color-black ()
  (with-ansi
   (should-color 'black 30)))

(ert-deftest test-dsl-color-red ()
  (with-ansi
   (should-color 'red 31)))

(ert-deftest test-dsl-color-green ()
  (with-ansi
   (should-color 'green 32)))

(ert-deftest test-dsl-color-yellow ()
  (with-ansi
   (should-color 'yellow 33)))

(ert-deftest test-dsl-color-blue ()
  (with-ansi
   (should-color 'blue 34)))

(ert-deftest test-dsl-color-magenta ()
  (with-ansi
   (should-color 'magenta 35)))

(ert-deftest test-dsl-color-cyan ()
  (with-ansi
   (should-color 'cyan 36)))

(ert-deftest test-dsl-color-white ()
  (with-ansi
   (should-color 'white 37)))

(ert-deftest test-dsl-combined-colors ()
  (let ((actual
         (with-ansi
          (black "foo")
          (red "bar")))
        (expected "\e[30mfoo\e[0m\e[31mbar\e[0m"))
    (should (equal expected actual))))
