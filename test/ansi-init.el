(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)
(require 'cask)

(defvar ansi-test/test-path
  (f-dirname (f-this-file)))

(defvar ansi-test/root-path
  (f-parent ansi-test/test-path))

(defvar ansi-test/vendor-path
  (f-expand "vendor" ansi-test/root-path))

;; This project uses ert-runner, which in turn uses ansi so to make
;; sure not those functions are tested, this code unbinds all ansi
;; functions.
(let* ((local-ansi-path
        (car (f-glob "ansi-*/ansi.elc" (cask-elpa-dir))))
       (ansi-history
        (--first (equal local-ansi-path (car it)) load-history))
       (ansi-functions
        (-select
         (lambda (item)
           (and (listp item) (eq (car item) 'defun)))
         ansi-history)))
  (-each (-map 'cdr ansi-functions) 'fmakunbound))

(load (f-expand "ansi" ansi-test/root-path))

(unless (require 'ert nil 'noerror)
  (require 'ert (f-expand "ert" ansi-test/vendor-path)))

(add-to-list 'load-path ansi-test/root-path)
