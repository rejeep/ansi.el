(ert-deftest test-style-bold ()
  (should-style ansi-bold 1))

(ert-deftest test-style-dark ()
  (should-style ansi-dark 2))

(ert-deftest test-style-italic ()
  (should-style ansi-italic 3))

(ert-deftest test-style-underscore ()
  (should-style ansi-underscore 4))

(ert-deftest test-style-blink ()
  (should-style ansi-blink 5))

(ert-deftest test-style-rapid ()
  (should-style ansi-rapid 6))

(ert-deftest test-style-contrary ()
  (should-style ansi-contrary 7))

(ert-deftest test-style-concealed ()
  (should-style ansi-concealed 8))

(ert-deftest test-style-strike ()
  (should-style ansi-strike 9))
