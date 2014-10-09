(require 'f)
(require 's)
(require 'dash)
(require 'ert)
(require 'el-mock)

(defvar ansi-test/test-path
  (f-dirname (f-this-file)))

(defvar ansi-test/root-path
  (f-parent ansi-test/test-path))

(defvar ansi-test/vendor-path
  (f-expand "vendor" ansi-test/root-path))

;; This project uses ert-runner, which in turn uses ansi so to make
;; sure not those functions are tested, this code unbinds all ansi
;; functions.
(unload-feature 'ansi 'force)

(load (f-expand "ansi" ansi-test/root-path))

(add-to-list 'load-path ansi-test/root-path)
