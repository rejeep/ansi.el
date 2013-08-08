;;; ansi.el --- Turn string into ansi strings

;; Copyright (C) 2010 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.2
;; Keywords: color, ansi
;; URL: http://github.com/rejeep/ansi
;; Package-Requires: ((s "1.6.1") (dash "1.5.0"))

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
  '((on-black   . 40)
    (on-red     . 41)
    (on-green   . 42)
    (on-yellow  . 43)
    (on-blue    . 44)
    (on-magenta . 45)
    (on-cyan    . 46)
    (on-white   . 47))
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



(defun ansi--concat (&rest sequences)
  (apply 's-concat (-select 'stringp sequences)))

(defmacro ansi--define (scope effect)
  "Define ansi function with EFFECT."
  (let ((fn-name (intern (format "ansi-%s" (symbol-name effect)))))
    `(defun ,fn-name (string &rest objects)
       ,(format "Add '%s' ansi effect to string." effect)
       (let ((code (cdr (assoc ',effect ,scope)))
             (formatted (apply 'format (cons string objects))))
         (format "\e[%dm%s\e[%sm" code formatted ,ansi-reset)))))

(defmacro with-ansi (&rest body)
  "allows using shortcut names of coloring functions."
  `(flet
       ,(-map
         (lambda (alias)
           (let ((fn (intern (format "ansi-%s" (symbol-name alias)))))
             `(,alias (string &rest objects) (apply ',fn (cons string objects)))))
         (-concat
          (-map 'car ansi-colors)
          (-map 'car ansi-on-colors)
          (-map 'car ansi-styles)))
     ,(cons 'ansi--concat body)))



(ansi--define ansi-colors black)
(ansi--define ansi-colors red)
(ansi--define ansi-colors green)
(ansi--define ansi-colors yellow)
(ansi--define ansi-colors blue)
(ansi--define ansi-colors magenta)
(ansi--define ansi-colors cyan)
(ansi--define ansi-colors white)

(ansi--define ansi-on-colors on-black)
(ansi--define ansi-on-colors on-red)
(ansi--define ansi-on-colors on-green)
(ansi--define ansi-on-colors on-yellow)
(ansi--define ansi-on-colors on-blue)
(ansi--define ansi-on-colors on-magenta)
(ansi--define ansi-on-colors on-cyan)
(ansi--define ansi-on-colors on-white)

(ansi--define ansi-styles bold)
(ansi--define ansi-styles dark)
(ansi--define ansi-styles italic)
(ansi--define ansi-styles underscore)
(ansi--define ansi-styles blink)
(ansi--define ansi-styles rapid)
(ansi--define ansi-styles contrary)
(ansi--define ansi-styles concealed)
(ansi--define ansi-styles strike)

(provide 'ansi)

;;; ansi.el ends here
