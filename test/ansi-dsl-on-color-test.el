(ert-deftest test-dsl-color-black ()
  (with-ansi
   (should-color on-black 40)))

(ert-deftest test-dsl-color-red ()
  (with-ansi
   (should-color on-red 41)))

(ert-deftest test-dsl-color-green ()
  (with-ansi
   (should-color on-green 42)))

(ert-deftest test-dsl-color-yellow ()
  (with-ansi
   (should-color on-yellow 43)))

(ert-deftest test-dsl-color-blue ()
  (with-ansi
   (should-color on-blue 44)))

(ert-deftest test-dsl-color-magenta ()
  (with-ansi
   (should-color on-magenta 45)))

(ert-deftest test-dsl-color-cyan ()
  (with-ansi
   (should-color on-cyan 46)))

(ert-deftest test-dsl-color-white ()
  (with-ansi
   (should-color on-white 47)))

(ert-deftest test-dsl-color-bright-black ()
  (with-ansi
   (should-color on-bright-black 100)))

(ert-deftest test-dsl-color-bright-red ()
  (with-ansi
   (should-color on-bright-red 101)))

(ert-deftest test-dsl-color-bright-green ()
  (with-ansi
   (should-color on-bright-green 102)))

(ert-deftest test-dsl-color-bright-yellow ()
  (with-ansi
   (should-color on-bright-yellow 103)))

(ert-deftest test-dsl-color-bright-blue ()
  (with-ansi
   (should-color on-bright-blue 104)))

(ert-deftest test-dsl-color-bright-magenta ()
  (with-ansi
   (should-color on-bright-magenta 105)))

(ert-deftest test-dsl-color-bright-cyan ()
  (with-ansi
   (should-color on-bright-cyan 106)))

(ert-deftest test-dsl-color-bright-white ()
  (with-ansi
   (should-color on-bright-white 107)))

(ert-deftest test-dsl-combined-colors ()
  (let ((actual
         (with-ansi
          (on-black "foo")
          (on-red "bar")))
        (expected "\e[40mfoo\e[0m\e[41mbar\e[0m"))
    (should (equal expected actual))))
