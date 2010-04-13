;; ----------------------------------------------------------------------
;; File: init-display.el - setup look and feel for my emacs 
;;                         (scrolling, fonts, color theme etc.)
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;            
;; Creation:  08 Jan 2010
;; Time-stamp: <Tue 2010-04-13 21:30 svarrette>
;;
;; Copyright (c) 2010 Sebastien Varrette <Sebastien.Varrette@uni.lu>
;;               http://varrette.gforge.uni.lu
;;
;; More information about Emacs Lisp: 
;;              http://www.emacswiki.org/emacs/EmacsLisp
;; ----------------------------------------------------------------------
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ----------------------------------------------------------------------

;; === Fix the "copy-paste from MS Word" issue on Mac OS X ===
;; prohibit pasting of TIFFs
(defun x-selection-value (type)
  (let ((data-types '(public.file-url
                       public.utf16-plain-text
                       com.apple.traditional-mac-plain-text))
    text)
    (while (and (null text) data-types)
      (setq text (condition-case nil
    	     (x-get-selection type (car data-types))
    	   (error nil)))
      (setq data-types (cdr data-types)))
    (if text
    (remove-text-properties 0 (length text) '(foreign-selection nil) text))
    text))

;; === Default size of the frame ===
(set-frame-width (selected-frame) 120)
(set-frame-height (selected-frame) 40)
 
;; === remove the few annoyance of default emacs ===
;; Use "y or n" answers instead of full words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p) 

;; remove initial message 
(setq inhibit-startup-message t)

;; kill and move region directly 
(delete-selection-mode t)
(pc-selection-mode)

;; no blinking cursor
;(if (>= emacs-major-version 21)
;    (blink-cursor-mode nil))

;; === display current time in the status bar ===
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)

;; === Specify the frame title ===
;; see http://www.emacswiki.org/emacs/FrameTitle 
;; recognize the same special characters as mode-line-format variable, mainly:
;;    %b -- print buffer name.      %f -- print visited file name.
;;    %F -- print frame name.
;;    %* -- print %, * or hyphen.   %+ -- print *, % or hyphen.
;;          %& is like %*, but ignore read-only-ness.
;;          % means buffer is read-only and * means it is modified.
;;          For a modified read-only buffer, %* gives % and %+ gives *.
;;    %m -- print the mode name.
;;    %z -- print mnemonics of buffer, terminal, and keyboard coding systems.
;;    %Z -- like %z, but including the end-of-line format.
;;    %[ -- print one [ for each recursive editing level.  %] similar.
;;    %% -- print %.   %- -- print infinitely many dashes.
;;  Decimal digits after the % specify field width to which to pad.
(setq frame-title-format '(buffer-file-name "emacs: %b (%f)" "emacs: %b"))

;; =================================================================
;; Emacs Color Theme
;; see http://www.emacswiki.org/emacs/ColorTheme
;; see http://code.google.com/p/gnuemacscolorthemetest/ for direct
;; screenshots
;; =================================================================
;; WITH color theme
(require 'color-theme)
(CarbonEmacs 
	(color-theme-initialize))
(setq color-theme-is-global t)


;(color-theme-aalto-light)
(color-theme-vim-colors)
;(require 'color-theme-tango)
;(color-theme-tango)

;; WITHOUT color theme
;(set-background-color "lightyellow")


(setq default-frame-alist
      '((cursor-color . "green")
        (cursor-type . box)))

;; =================================================================
;; Font Lock configuration
;; Note: minor mode, always local to a particular buffer, which
;; highlights (or “fontifies”) the buffer contents according to the
;; syntax of the text you are editing.
;; =================================================================
;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)
(setq font-lock-support-mode 'fast-lock-mode)
;; Under Mac, package lazy-lock id obsolete
;;(setq font-lock-support-mode 'lazy-lock-mode)
 
(setq font-lock-support-mode 'jit-lock-mode)

;; =================================================================
;; Faces: Controlling Text Display Style
;; =================================================================
;; copy from my previous config, probably OBSOLETE now that color
;; themes exists
;(set-face-foreground 'font-lock-comment-face "red")
;(copy-face 'bold 'font-lock-function-name-face)
;(set-face-foreground 'font-lock-function-name-face "blue")
;(copy-face 'italic 'font-lock-type-face)
;(set-face-foreground 'font-lock-type-face "brown")
;(copy-face 'bold 'font-lock-keyword-face)
;(set-face-foreground 'font-lock-keyword-face "purple")
;(set-face-foreground 'default "black")
;(set-face-background 'modeline "blue")
;(set-face-foreground 'modeline "white")
;(set-face-background 'region "lightblue")

;; === translate ANSI escape sequences into faces ===
(GNUEmacs
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; === Auto fit the size of the frame to the buffer content ===
;; see http://www.emacswiki.org/emacs/Shrink-Wrapping_Frames
;; run 'M-x fit-frame' for that
(require 'fit-frame)
(add-hook 'after-make-frame-functions 'fit-frame)




(provide 'init-display)
;; ----------------------------------------------------------------------
;; eof
;;
;; Local Variables: 
;; mode: lisp
;; End: 

 
