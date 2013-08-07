# ansi.el [![Build Status](https://api.travis-ci.org/rejeep/ansi.png?branch=master)](http://travis-ci.org/rejeep/ansi)

This package defines functions that turns simple strings to ansi
strings. Turning a string into an ansi string can be to add color to a
text, add color in the background of a text or adding a style, such as
bold, underscore or italic.

## Installation

Add **ansi.el** to Emacs load path and then require it.

    (add-to-list 'load-path "/path/to/ansi")
    (require 'ansi)

## Usage

All colors, background colors and styles can be accessed via a
function with the **ansi-** prefix. For example:

    ;; Color text
    (ansi-red "foo")
    (ansi-blue "bar")
     
    ;; Color background
    (ansi-on-red "foo")
    (ansi-on-blue "bar")
     
    ;; Add style
    (ansi-bold "foo")
    (ansi-blink "bar")

It can become quite cumbersome to use those function names if the
coloring is a little bit more advanced. To simplify this, there's a
DSL, which makes this much more pleasant. If within a **with-ansi**
block, the **ansi-** prefix is not necessary anymore. This is the same
as the above.

    ;; Color text
    (with-ansi
     (red "foo")
     (blue "bar"))
     
    ;; Color background
    (with-ansi
     (on-red "foo")
     (on-blue "bar"))
     
    ;; Add style
    (with-ansi
     (bold "foo")
     (blink "bar"))

### Nesting

Lets say you want to make a text bold, red and blinking. You can do
this with nesting.

    ;; Without DSL
    (ansi-bold
     (ansi-red
      (ansi-blink "foo bar")))
     
    ;; Using DSL
    (with-ansi
     (bold
      (red
       (blink "foo bar"))))
