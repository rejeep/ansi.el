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

;; You can paint strings (see `ansi-colors' for all possible text
;; colors).
;;
;;   (ansi-red "foo")
;;   (ansi-black "bar")
;;
;;
;; You can paint a string backgrounds (see `ansi-on-colors' for all
;; possible background colors).
;;
;;   (ansi-on-blue "foo")
;;   (ansi-on-green "bar")
;;
;;
;; You can add styles to a string (see `ansi-styles' for all possible
;; styles).
;;
;;   (ansi-bold "foo")
;;   (ansi-blink "bar")
;;
;;
;; You can use `with-ansi', which allows for a simplified DSL.
;;
;;   (with-ansi
;;    (red "foo")
;;    (black "bar"))
;;
;;   (with-ansi
;;    (on-blue "foo")
;;    (on-green "bar"))
;;
;;   (with-ansi
;;    (bold "foo")
;;    (blink "bar")
;;    ;; Note that reverse is not available within `with-ansi' because
;;    ;; it conflicts with the builtin function for reversing a
;;    ;; list. However, `ansi-reverse' is still available.
;;    (contrary "foobar"))


;;; Code:

(eval-when-compile
  (require 'cl))


(defconst ansi-colors
  '((black   . 30)
    (red     . 31)
    (green   . 32)
    (yellow  . 33)
    (blue    . 34)
    (magenta . 35)
    (cyan    . 36)
    (white   . 37))
  "List of text colors.")

(defconst ansi-on-colors
  '((black   . 40)
    (red     . 41)
    (green   . 42)
    (yellow  . 43)
    (blue    . 44)
    (magenta . 45)
    (cyan    . 46)
    (white   . 47))
  "List of colors to draw text on.")

(defconst ansi-styles
  '((bold       . 1)
    (dark       . 2)
    (italic     . 3)
    (underscore . 4)
    (blink      . 5)
    (rapid      . 6)
    (contrary   . 7)
    (concealed  . 8)
    (strike     . 9))
  "List of styles.")

(defconst ansi-reset 0
  "Ansi code for reset.")


(defmacro with-ansi (&rest body)
  "Allows using shortcut names of coloring functions."
  `(flet ((<< (colored) (setq result (concat result colored)) colored)
          ;; TEXT COLORS
          (black   (string) (<< (ansi-black string)))
          (red     (string) (<< (ansi-red string)))
          (green   (string) (<< (ansi-green string)))
          (yellow  (string) (<< (ansi-yellow string)))
          (blue    (string) (<< (ansi-blue string)))
          (magenta (string) (<< (ansi-magenta string)))
          (cyan    (string) (<< (ansi-cyan string)))
          (white   (string) (<< (ansi-white string)))

          ;; ON COLORS
          (on-black   (string) (<< (ansi-on-black string)))
          (on-red     (string) (<< (ansi-on-red string)))
          (on-green   (string) (<< (ansi-on-green string)))
          (on-yellow  (string) (<< (ansi-on-yellow string)))
          (on-blue    (string) (<< (ansi-on-blue string)))
          (on-magenta (string) (<< (ansi-on-magenta string)))
          (on-cyan    (string) (<< (ansi-on-cyan string)))
          (on-white   (string) (<< (ansi-on-white string)))

          ;; STYLES
          (bold       (string) (<< (ansi-bold string)))
          (dark       (string) (<< (ansi-dark string)))
          (italic     (string) (<< (ansi-italic string)))
          (underscore (string) (<< (ansi-underscore string)))
          (blink      (string) (<< (ansi-blink string)))
          (rapid      (string) (<< (ansi-rapid string)))
          (contrary   (string) (<< (ansi-contrary string)))
          (concealed  (string) (<< (ansi-concealed string)))
          (strike     (string) (<< (ansi-strike string))))
     (let (result)
       ,@body
       result)))


(defun ansi-color (string color)
  "Paint STRING with COLOR."
  (ansi-effect ansi-colors string color))

(defun ansi-on-color (string color)
  "Paint STRING on COLOR."
  (ansi-effect ansi-on-colors string color))

(defun ansi-style (string style)
  "Style STRING with STYLE."
  (ansi-effect ansi-styles string style))

(defun ansi-effect (list string effect)
  "Add EFFECT to string."
  (let ((code (cdr (assoc effect list))))
    (format "\e[%sm%s\e[%sm" code string ansi-reset)))


;; ON TEXT

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


;; ON COLORS

(defun ansi-on-black (string)
  (ansi-on-color string 'black))

(defun ansi-on-black (string)
  (ansi-on-color string 'black))

(defun ansi-on-red (string)
  (ansi-on-color string 'red))

(defun ansi-on-green (string)
  (ansi-on-color string 'green))

(defun ansi-on-yellow (string)
  (ansi-on-color string 'yellow))

(defun ansi-on-blue (string)
  (ansi-on-color string 'blue))

(defun ansi-on-magenta (string)
  (ansi-on-color string 'magenta))

(defun ansi-on-cyan (string)
  (ansi-on-color string 'cyan))

(defun ansi-on-white (string)
  (ansi-on-color string 'white))


;; STYLES

(defun ansi-bold (string)
  (ansi-style string 'bold))

(defun ansi-dark (string)
  (ansi-style string 'dark))

(defun ansi-italic (string)
  (ansi-style string 'italic))

(defun ansi-underscore (string)
  (ansi-style string 'underscore))

(defun ansi-blink (string)
  (ansi-style string 'blink))

(defun ansi-rapid (string)
  (ansi-style string 'rapid))

(defun ansi-contrary (string)
  (ansi-style string 'contrary))
(defalias 'ansi-reverse 'ansi-contrary)

(defun ansi-concealed (string)
  (ansi-style string 'concealed))

(defun ansi-strike (string)
  (ansi-style string 'strike))


(provide 'ansi)

;;; ansi.el ends here
