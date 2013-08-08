(ert-deftest test-color-black ()
  (should-color ansi-black 30))

(ert-deftest test-color-red ()
  (should-color ansi-red 31))

(ert-deftest test-color-green ()
  (should-color ansi-green 32))

(ert-deftest test-color-yellow ()
  (should-color ansi-yellow 33))

(ert-deftest test-color-blue ()
  (should-color ansi-blue 34))

(ert-deftest test-color-magenta ()
  (should-color ansi-magenta 35))

(ert-deftest test-color-cyan ()
  (should-color ansi-cyan 36))

(ert-deftest test-color-white ()
  (should-color ansi-white 37))
