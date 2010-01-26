;; ----------------------------------------------------------------------
;; File: init-bindings.el - setup my key bindings in emacs
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Creation:  08 Jan 2010
;; Time-stamp: <Ven 2010-01-22 14:44 svarrette>
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

;; === Buffer switching ===
;; C-x b permits to switch among the buffer by entering a buffer name,
;; with completion.
;; See http://www.emacswiki.org/emacs/IswitchBuffers
(require 'iswitchb)
(iswitchb-mode t)
;; to ignore the *...* special buffers from the list
(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

;; Move from one buffer to another using 'C-<' and 'C->'
;(load "cyclebuffer" nil 't)
;(global-set-key (kbd "C-<") 'cyclebuffer-forward)
;(global-set-key (kbd "C->") 'cyclebuffer-backward)
(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)

;; === Window switching ===
(global-set-key [C-prior] 'other-window)
(global-set-key [C-next]  'other-window)

;; === Font size ===
;; I may prefer C-+ and C-- for window enlarge/schrink
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; === Multi speed mouse scrolling ===
;; scroll:         normal speed
;; Ctrl + scroll:  high speed
;; Shift + scroll: low  speed
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;; === Navigation ===
(global-set-key [kp-home]  'beginning-of-buffer) ; [Home]
(global-set-key [home]     'beginning-of-buffer) ; [Home]
(global-set-key [kp-end]   'end-of-buffer)       ; [End]
(global-set-key [end]      'end-of-buffer)       ; [End]

;; goto next error (raised in the compilation buffer typically)
(global-set-key (kbd "C-x n") 'next-error)
(global-set-key (kbd "C-x p") 'previous-error)

(global-set-key (kbd "M-n") 'goto-line)          ; goto line number

;; ECB
(global-set-key [(f2)]   'ecb-toggle) ; Activate ECB (see ~/.emacs.d/init-cedet)

;; Speedbar
;(global-set-key [(f4)] 'speedbar-get-focus)      ; jump to speedbar frame
(require 'sr-speedbar)
(global-set-key [(f4)] 'sr-speedbar-toggle)       ; jump to speedbar frame

;; find matching parenthesis (% command in vim: Go to the matching parenthesis,
;; if on parenthesis; otherwise, insert '%')
;; see ~/.emacs.d/init-defuns.el
;; in practice, it's annoying when writing a C code with printf format so I
;; decided to rebind it to something different that '%'
(global-set-key (kbd "C-c C-p") 'match-paren)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; === Compilation ===
(global-set-key (kbd "C-x C-e") 'compile)

;; === Kill this buffer ===
(global-set-key (kbd "C-q") 'kill-this-buffer)

;; === Launch a shell ===
(global-set-key (kbd "C-!") 'shell)

;; === Re-indent the full file (quite useful) ===
(global-set-key (kbd "C-c i") 'indent-buffer)  ; see ~/.emacs.d/init-defuns

;; === Search [and replace] ===
                                        ; Use regex searches by default.
(global-set-key (kbd "C-s")   'isearch-forward)
(global-set-key (kbd "\C-r")  'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-q")   'query-replace)


;; === Instant messaging ===
;; see http://www.emacswiki.org/emacs/CategoryChatClient
;; TO BE TRIED LATER
                                        ;(global-set-key (kbd "C-c j") (lambda () (interactive) (switch-or-start 'jabber-connect "*-jabber-*")))
                                        ;(global-set-key (kbd "C-c M-j") 'jabber-disconnect)

                                        ;(global-set-key (kbd "C-c i")
                                        ;               (lambda () (interactive)
                                        ;                 (switch-or-start (lambda () (rcirc-connect "irc.freenode.net"))
                                        ;                                  "*irc.freenode.net*")))


;; === Emacs Org ===
;; An Emacs Mode for Notes, Project Planning, and Authoring
;; see http://www.emacswiki.org/emacs/OrgMode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; === Flyspell ===
(global-set-key (kbd "C-c C-i w")  'ispell-word)
(global-set-key (kbd "C-c C-i b")  'ispell-buffer)




;; ===============================
;;  BINDINGS INDUCED BY SUB-MODES
;; ===============================

;; * comment and uncomment a region in a buffer is done via 'M-;'

;; * LaTeX-mode: see AucTeX manual.
;;   Some additionnal notes:
;;   - make a LaTeX reference (to a label) by pressing `C-c )'
;;   - insert a label by pressing `C-c (' (or `C-('
;;   - insert a citation by pressing `C-c [' (or `C-['
;;   - hit `C-c ='; the buffer will split into 2 and in the top half you
;;     will see a TOC, hitting `l' there will show all the labels and cites.

;; * C-x d    open dired (for directory browsing), see ~/.emacs.d/dired-refcard.gnu.pdf
;;            Note: I bind 'p' once on a file to run the 'open' command on this file
;;            See ~/.emacs.d/init-emodes.el (section Dired)

;; * SVN: see menu Tools/Version Control (C-x v v to commit for instance)

;; * CEDET: see ~/.emacs.d/init-cedet.el






(provide 'init-bindings)
;; ----------------------------------------------------------------------
;; eof
;;
;; Local Variables:
;; mode: lisp
;; End:
