(ert-deftest test-color-on-black ()
  (should-color 'ansi-on-black 40))

(ert-deftest test-color-on-red ()
  (should-color 'ansi-on-red 41))

(ert-deftest test-color-on-green ()
  (should-color 'ansi-on-green 42))

(ert-deftest test-color-on-yellow ()
  (should-color 'ansi-on-yellow 43))

(ert-deftest test-color-on-blue ()
  (should-color 'ansi-on-blue 44))

(ert-deftest test-color-on-magenta ()
  (should-color 'ansi-on-magenta 45))

(ert-deftest test-color-on-cyan ()
  (should-color 'ansi-on-cyan 46))

(ert-deftest test-color-on-white ()
  (should-color 'ansi-on-white 47))