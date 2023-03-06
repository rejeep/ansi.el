(ert-deftest test-inhibit-ansi-apply ()
  (let ((ansi-inhibit-ansi t))
    (should (equal
             (with-ansi (red "hello %s" "world"))
             "hello world"))))

(ert-deftest test-inhibit-ansi-apply-nested ()
  (let ((ansi-inhibit-ansi t))
    (should (equal
             (with-ansi "some text "
                        (red "hello %s " "world")
                        (bold (black "black bold %d" 42)))
             "some text hello world black bold 42"))))

(ert-deftest test-inhibit-ansi-csi-apply ()
  (let ((ansi-inhibit-ansi t))
    (should (equal
             (with-ansi "hello" (backward))
             "hello"))))
