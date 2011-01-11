;;; ansi.el --- Turn string into ansi colored strings

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: color, ansi
;; URL: http://github.com/rejeep/ansi

;; This file is NOT part of GNU Emacs.


;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; This package defines functions that turn string into ansi colored
;; strings.
;;
;; You can paint string like this (see `ansi-colors' for all possible
;; colors that text can have).
;;
;;   (ansi-red "foo")
;;   (ansi-black "bar")
;;
;; You can also use `with-ansi', which allows for a simplified DSL.
;;
;;   (with-ansi
;;    (red "foo")
;;    (black "bar"))


;;; Code:

(eval-when-compile
  (require 'cl))


(defconst ansi-colors
  '((black   . "\e[30m")
    (red     . "\e[31m")
    (green   . "\e[32m")
    (yellow  . "\e[33m")
    (blue    . "\e[34m")
    (magenta . "\e[35m")
    (cyan    . "\e[36m")
    (white   . "\e[37m"))
  "List of text colors.")

(defconst ansi-reset "\e[0m"
  "Ansi code for reset.")


(defmacro with-ansi (&rest body)
  "Allows using shortcut names of coloring functions."
  `(flet ((<< (colored) (setq result (concat result colored)) colored)
          (black   (string) (<< (ansi-black string)))
          (red     (string) (<< (ansi-red string)))
          (green   (string) (<< (ansi-green string)))
          (yellow  (string) (<< (ansi-yellow string)))
          (blue    (string) (<< (ansi-blue string)))
          (magenta (string) (<< (ansi-magenta string)))
          (cyan    (string) (<< (ansi-cyan string)))
          (white   (string) (<< (ansi-white string))))
     (let (result)
       ,@body
       result)))


(defun ansi-black (string)
  (ansi-color string 'black))

(defun ansi-black (string)
  (ansi-color string 'black))

(defun ansi-red (string)
  (ansi-color string 'red))

(defun ansi-green (string)
  (ansi-color string 'green))

(defun ansi-yellow (string)
  (ansi-color string 'yellow))

(defun ansi-blue (string)
  (ansi-color string 'blue))

(defun ansi-magenta (string)
  (ansi-color string 'magenta))

(defun ansi-cyan (string)
  (ansi-color string 'cyan))

(defun ansi-white (string)
  (ansi-color string 'white))


(defun ansi-color (string color)
  "Paint STRING with COLOR."
  (let ((code (cdr (assoc color ansi-colors))))
    (concat code string ansi-reset)))


(provide 'ansi)

;;; ansi.el ends here
