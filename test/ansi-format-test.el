(ert-deftest test-format-color ()
  (should (equal (ansi-black "<%s>" "foo") "\e[30m<foo>\e[0m")))

(ert-deftest test-format-on-color ()
  (should (equal (ansi-on-black "<%s>" "foo") "\e[40m<foo>\e[0m")))

(ert-deftest test-format-styles ()
  (should (equal (ansi-bold "<%s>" "foo") "\e[1m<foo>\e[0m")))

(ert-deftest test-format-dsl-color ()
  (should (equal (with-ansi (black "<%s>" "foo")) "\e[30m<foo>\e[0m")))

(ert-deftest test-format-dsl-on-color ()
  (should (equal (with-ansi (on-black "<%s>" "foo")) "\e[40m<foo>\e[0m")))

(ert-deftest test-format-dsl-styles ()
  (should (equal (with-ansi (bold "<%s>" "foo")) "\e[1m<foo>\e[0m")))
