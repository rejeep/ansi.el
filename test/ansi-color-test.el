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

(ert-deftest test-color-bright-black ()
  (should-color ansi-bright-black 90))

(ert-deftest test-color-bright-red ()
  (should-color ansi-bright-red 91))

(ert-deftest test-color-bright-green ()
  (should-color ansi-bright-green 92))

(ert-deftest test-color-bright-yellow ()
  (should-color ansi-bright-yellow 93))

(ert-deftest test-color-bright-blue ()
  (should-color ansi-bright-blue 94))

(ert-deftest test-color-bright-magenta ()
  (should-color ansi-bright-magenta 95))

(ert-deftest test-color-bright-cyan ()
  (should-color ansi-bright-cyan 96))

(ert-deftest test-color-bright-white ()
  (should-color ansi-bright-white 97))
