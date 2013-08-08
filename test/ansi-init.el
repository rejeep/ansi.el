(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)

(defvar ansi-test/test-path
  (f-dirname load-file-name))

(defvar ansi-test/root-path
  (f-parent ansi-test/test-path))

(defvar ansi-test/vendor-path
  (f-expand "vendor" ansi-test/root-path))

(load (f-expand "ansi" ansi-test/root-path))

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" ansi-test/vendor-path)))

(add-to-list 'load-path ansi-test/root-path)
