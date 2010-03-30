;; -------------------------------------------------------------------------
;; .emacs -- my personnal Emacs Init File
;;           see http://github.com/Falkor/emacs-config
;;
;; Copyright (c) 2000-2010 Sebastien Varrette <Sebastien.Varrette@uni.lu>
;;               http://varrette.gforge.uni.lu
;;
;; Time-stamp: <Mar 2010-03-30 18:04 svarrette>
;; -------------------------------------------------------------------------
;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;
;;
;;
;; -------------------------------------------------------------------------
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
;; -------------------------------------------------------------------------
;;
;;; Resources:
;;    * http://www.gnu.org/software/emacs/
;;    * http://www.emacswiki.org/emacs/
;;    * http://emacsblog.org/
;;    * http://www.io.com/~jimm/emacs_tips.html
;;    * The Emacs Starter Kit by Phil Hagelberg
;;           http://github.com/technomancy/emacs-starter-kit/tree/master
;;    * Carbon Emacs package (Mac OS X port)
;;           http://homepage.mac.com/zenitani/emacs-e.html
;;    * Various .emacs example on the web, including
;;           http://www.mygooglest.com/fni/dot-emacs.html
;;           http://www.dotemacs.de/dotfiles/JohnJGlynn.emacs.html
;;           http://www-verimag.imag.fr/~moy/emacs/.emacs
;;           http://github.com/alexott/emacs-configs
;;    * Emacs Lisp List (ELL) - authorative source of all Emacs packages
;;           http://www.emacswiki.org/emacs/EmacsLispList
;;
;;; Commentary:
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;
;;     "Emacs is like a laser guided missile. It only has to be slightly
;;      mis-configured to ruin your whole day."
;;                                                            [Sean McGrath]
;;
;;     "While any text editor can save your files, only Emacs can save your
;;      soul."
;;                                                          [Per Abrahamsen]
;; -------------------------------------------------------------------------

;; =========================
;; Environment determination
;; =========================
;; Running XEmacs ?
(defvar running-xemacs       (string-match "XEmacs" emacs-version))

;; Emacs version
(cond ((string-match ".*macs *23\..*" (emacs-version))
       (setq emacs-major-version 23))
      ((string-match ".*macs *22\..*" (emacs-version))
       (setq emacs-major-version 22))
      ((string-match ".*macs *21\..*" (emacs-version))
       (setq emacs-major-version 21))
      ((string-match ".*macs *20\..*" (emacs-version))
       (setq emacs-major-version 20))
      ((string-match ".*macs *19\..*" (emacs-version))
       (setq emacs-major-version 19))
      )
(if running-xemacs
    ;; don't offer migration of the init file
    (setq load-home-init-file t))

;; Macro to be used later to differenciate code for GNU Emacs, XEmacs or
;; Carbon Emacs
(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (not running-xemacs) (cons 'progn body)))
(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if running-xemacs (cons 'progn body)))
(defmacro CarbonEmacs (&rest body)
  "Execute any number of forms if running under Mac OS X port Carbon Emacs."
  (list 'if (featurep 'carbon-emacs-package) (cons 'progn body)))
;; To detect Carbon Emacs, use the following:
;; (if (featurep 'carbon-emacs-package)
;;     (progn
;;       (something-to-do)
;;       (something-to-do)
;;       (something-to-do)
;;       ))


;; === Load path etc. ===
(setq load-path (cons "~/.emacs.d/"          load-path)) ; my own config files
(setq load-path (cons "~/.emacs.d/themes"    load-path)) ; emacs display themes
(setq load-path (cons "~/.emacs.d/site-lisp" load-path)) ; external elisp files
(setq load-path (cons "~/.emacs.d/elpa"      load-path)) ; packages installed via ELPA
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/ecb")) ; ECB
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/cedet/common")) ; CEDET 
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nxhtml")) ; NXHTML
(unless (equal emacs-major-version 23)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/epg"))) 
(setq load-path (cons "~/.emacs.d/site-lisp/doxymacs/share/emacs/site-lisp/" load-path)) ; Doxymacs
(CarbonEmacs
 (setenv "PATH" (concat "/sw/bin:/opt/local/bin:usr/local/bin:" (getenv "PATH"))))

;; === Special Mac OS X configuration (Carbon emacs and aquamacs)
;; Enhanced Carbon EMacs (ECE) plugin
;; see http://www.inf.unibz.it/~franconi/mac-emacs/
(CarbonEmacs
 (unless   (or (boundp 'enhanced-carbon-emacs) (boundp 'aquamacs-version))
   (defun load-local-site-start (site-lisp-directory)
     "Load site-start.el from a given site-lisp directory"
     (let ((current-default-directory default-directory))
       (setq default-directory site-lisp-directory)
       (normal-top-level-add-subdirs-to-load-path)
       (setq default-directory current-default-directory)
       (setq load-path (cons site-lisp-directory load-path))
       (load (concat site-lisp-directory "/site-start.el"))
       ))
   (load-local-site-start "/Library/Application Support/emacs/ec-emacs/site-lisp")))

;; =====================
;; General Emacs Options
;; =====================

;; === User authentication ===
(setq user-full-name    "Sebastien Varrette")
(setq user-mail-address "<Sebastien.Varrette@uni.lu>")
;; for the webpage url, see Auto-insert section

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these (except perhaps the menu-bar); trust me.
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode nil))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode nil))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode nil))

(setq truncate-partial-width-windows nil)
(setq line-number-mode         t)
(setq column-number-mode       t)
(setq search-highlight         t)       ; highlight search object
(setq query-replace-highlight  t)       ; highlight query object
(auto-compression-mode         1)       ; transparently edit compressed files
(setq byte-compile-verbose     t)
(setq visible-bell t)
(setq initial-major-mode 'text-mode)   ; to avoid autoloads for lisp mode
(setq require-final-newline t)         ; ensure a file ends in a newline when it
                                        ; is saved

;; === Auto-save and backup files ===
(setq auto-save-list-file-name nil)     ; no .saves files
(setq auto-save-default        t)       ; auto saving
(setq make-backup-files        t)       ; make  backup files
;; see http://www.emacswiki.org/emacs/BackupDirectory
(setq
 backup-by-copying t                    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))                  ; don't litter my fs tree
 delete-old-versions t                  ; delete excess backup versions
                                        ; silently
 kept-new-versions 6
 kept-old-versions 2
 version-control t)                     ; make numeric backup versions
;; TODO: check out backup-dir

;; === Language Environments (ISO 8859 Latin-1) ===
;; see http://www.gnu.org/s/emacs/manual/html_node/emacs/Language-Environments.html
(if (>= emacs-major-version 21)
    (progn
      (set-language-environment   'latin-1)
      (set-terminal-coding-system 'latin-1)
      (set-keyboard-coding-system 'latin-1)
      )
  (progn
    (set-terminal-coding-system   'latin-1)
    (set-keyboard-coding-system   'latin-1)
    (set-language-environment     'latin-1)
    )
  )
(setq iso-accents-mode t)

;; === Auto-fill configuration ===
;; automatic wrapping of lines and insertion of newlines when the cursor
;; goes over the column limit.
(setq-default fill-column 80)
(setq auto-fill-mode t)                 ; activate by default

;; ============================================================
;; These should be loaded on startup rather than autoloaded
;; on demand since they are likely to be used in every session

;; turn on Common Lisp support
(require 'cl)

;; Saving Emacs Sessions (cursor position etc. in a previously visited file)
(require 'saveplace)
(setq-default save-place t)

;; Finding Files (and URLs) At Point (FFAP)
;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/FFAP.html
(require 'ffap)


;; Unique buffer names dependent on file name
(require 'uniquify)
;; style used for uniquifying buffer names with parts of directory name
(setq uniquify-buffer-name-style 'forward)

;; === ELPA, the package manager ===
;; see http://tromey.com/elpa/
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize)
  (require 'init-elpa))

;; Automatically compile .el files as they're loaded.
;; Keeps the compiled files in ~/.emacs.d/byte-cache by default, and will
;; automatically recompile any files that change
;;(require 'byte-code-cache)
;;(setq bcc-cache-directory "~/.emacs.d/.byte-cache")

;; ===================================================
;;  Definition of some custom functions
;; ===================================================
(require 'init-defuns)                  ; see  ~/.emacs.d/init-defuns.el

;; ===================================================
;;  Emacs display (fonts, color theme etc.)
;; ===================================================
(require 'init-display)                 ; see  ~/.emacs.d/init-display.el

;; ===================================================
;;  Configure Emacs modes
;; ===================================================
(require 'init-emodes)                  ; see  ~/.emacs.d/init-emodes.el

;; ===================================================
;;  Emacs bindings
;; ===================================================
(require 'init-bindings)                ; see  ~/.emacs.d/init-bindings.el

;; ===================================================
;;  Remaining (misc) configuration
;; ===================================================

;; === Maintain last change time stamps (via Time-stamp: <>) ===
(require 'time-stamp)
;; format of the string inserted by `M-x time-stamp'
(setq time-stamp-format "%3a %:y-%02m-%02d %02H:%02M %u")
                                        ; `Weekday YYYY-MM-DD HH:MM USER'

;; update time stamps every time you save a buffer
(add-hook 'write-file-hooks 'time-stamp)

;; === Show matching parenthesis ===
(require 'paren)
(GNUEmacs
 (show-paren-mode t)
 (setq show-paren-ring-bell-on-mismatch t))
(XEmacs
 (paren-set-mode 'paren))

;; show matching parenthesis, even if found outside the present screen.
;; see http://www.emacswiki.org/emacs/MicParen
(require 'mic-paren)                    ; loading
(paren-activate)                        ; activating

;; === Indenting configuration ===
(setq-default indent-tabs-mode nil)     ; indentation can't insert tabs

(setq c-brace-offset -2)
;;(setq c-auto-newline t)
(add-hook 'c-mode-common-hook (lambda () (setq c-basic-offset 4)))
(add-hook 'c-mode-common-hook (lambda () (setq c-recognize-knr-p nil)))
(add-hook 'ada-mode-hook (lambda ()      (setq ada-indent 4)))
(add-hook 'perl-mode-hook (lambda ()     (setq perl-basic-offset 4)))
(add-hook 'cperl-mode-hook (lambda ()    (setq cperl-indent-level 4)))

;; =============================================
;; Activate fill adapt
;; see http://www.emacswiki.org/emacs/FillAdapt
;; =============================================
(require 'filladapt)
;; turn on filladapt mode everywhere but in ChangeLog files
(setq-default filladapt-mode nil)
(cond ((equal mode-name "Change Log")
       t)
      (t
       (turn-on-filladapt-mode)))

;; ========================================================
;; Auto-insert: automatic insertion of text into new files
;; ========================================================
(require 'auto-insert-tkld)    ; see ~/.emacs.d/site-lisp/auto-insert-tkld.el
;; doc:  ~/.emacs.d/site-lisp/auto-insert-tkld.pdf
(setq auto-insert-path (cons "~/.emacs.d/insert" auto-insert-path))
;; trick to abstract the personal web page
(setq auto-insert-organisation "http://varrette.gforge.uni.lu")
(setq auto-insert-automatically t)
;; associate file extention to a template name
(setq auto-insert-alist
      '(
        ("\\.tex$"      . "LaTeX")            ; TeX or LaTeX
        ("\\.bib$"      . "BibTeX")           ; BibTeX
        ("\\.sty$"      . "LaTeX Style")      ; LaTeX Style
        ("\\.el$"       . "Emacs Lisp")       ; Emacs Lisp
        ("\\.java$"     . "Java")             ; Java
        ("\\App.java$"  . "JavaSwing")        ; Java Swing app
        ("[Tt]ools.h"   . "Tools C++")        ; Useful functions in C/C++
        ("\\Logs.cpp"   . "Logs C++")         ; Macros for logs/debugging
        ("\\Logs.h[+p]*". "Logs C++ Include") ; " header file
        ("\\.c$"        . "C")                ; C
        ("\\.h$"        . "C Include")        ; C header file
        ("\\.cxx$"      . "C++")              ; C++
        ("\\.c\\+\\+$"  . "C++")              ;
        ("\\.cpp$"      . "C++")              ;
        ("\\.cc$"       . "C++")              ;
        ("\\.C$"        . "C++")              ;
        ("[Mm]akefile$" . "Makefile")         ; Makefile
        ("\\.txt$"      . "Text")             ; Text
        ("\\.gpg$"      . "GPG")              ; GPG 
        ("[Rr]eadme$"   . "Readme")           ; Readme
        ("README$"      . "Readme")           ;
        ("\\.sh$"       . "Shell")            ; Shell
        ("\\.csh$"      . "Shell")            ;
        ("\\.tcsh$"     . "Shell")            ;
        ("\\.html"      . "Html")             ; HTML
        ("\\.wml"       . "WML")              ; WML (Website Meta Language)
        ("\\.php"       . "PHP")              ; PHP
        ("\\.gnuplot"   . "Gnuplot")          ; Gnuplot
        ("\\.pl$"       . "Perl")             ; Perl
        ("\\.pm$"       . "Perl Module")      ; PerlModule
        ("\\.rb$"       . "Ruby")             ; Ruby
        (""             . "Shell") ; Shell (by default: assume a shell template)
        ))
;; now associate a template name to a template file
(setq auto-insert-type-alist
      '(
        ("LaTeX"       . "insert.tex")
        ("BibTeX"      . "insert.bib")
        ("LaTeX Style" . "insert.sty")
        ("Emacs Lisp"  . "insert.el")
        ("Java"        . "insert.java")
        ("JavaSwing"   . "insertApp.java")
        ("C"           . "insert.c")
        ("C Include"   . "insert.h")
        ("C++"         . "insert.cpp")
        ("Tools C++"   . "insert.tools_cpp.h")
        ("Logs C++"    . "insert.logs.cpp")
        ("Logs C++ Include" . "insert.logs.h")
        ("Makefile"    . "insert.makefile")
        ("Text"        . "insert.txt")
        ("GPG"         . "insert.gpg")
        ("Readme"      . "insert.readme")
        ("Shell"       . "insert.shell")
        ("Html"        . "insert.html")
        ("WML"         . "insert.wml")
        ("PHP"         . "insert.php")
        ("Gnuplot"     . "insert.gnuplot")
        ("Perl"        . "insert.pl")
        ("Perl Module" . "insert.pm")
        ("Ruby"        . "insert.rb")
        ))

;; ================================================================
;; Smart-compile
;; figures out the better compile command, based on filename or its
;; major-mode.
;; see http://www.emacswiki.org/emacs/SmartCompile
;; ================================================================
(require 'smart-compile)

;; Switch back to whatever buffer was in your other window if compilation is
;; successful
;; see http://www.emacswiki.org/emacs/ModeCompile
;; Close the compilation window if there was no error at all.
;; version that use the winner mode (see ~/.emacs.d/init-defuns.el)
(setq compilation-finish-functions 'compile-autoclose)




;; ===  Detaching the custom-file ===
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Local Variables:
;; mode: Lisp
;; fill-column: 76
;; End:
;;; .emacs ends here




