;; ----------------------------------------------------------------------
;; File: init-cedet.el -   configuration of my CEDET environment
;;       Part of my emacs configuration (see ~/.emacs or init.el)
;;
;; Copyright (c) 2010 Sebastien Varrette <Sebastien.Varrette@uni.lu>
;;               http://varrette.gforge.uni.lu
;;
;;          _       _ _                     _      _         _ 
;;         (_)_ __ (_) |_       ___ ___  __| | ___| |_   ___| |
;;         | | '_ \| | __|____ / __/ _ \/ _` |/ _ \ __| / _ \ |
;;         | | | | | | ||_____| (_|  __/ (_| |  __/ |_ |  __/ |
;;         |_|_| |_|_|\__|     \___\___|\__,_|\___|\__(_)___|_|
;;
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

;; ================================================
;; === Integrated Development Environment (IDE) ===
;; ================================================
;; Mainly rely on Collection Of Emacs Development Environment Tools (CEDET)
;; see http://cedet.sourceforge.net/
;; Nice intro to CEDET:
;;    http://xtalk.msk.su/~ott/en/writings/emacs-devenv/EmacsCedet.html

(require 'cedet)

;; ----------------
;; === Semantic ===
;; ----------------
;; see http://cedet.sourceforge.net/semantic.shtml
;; The most critical part as it is the code parser that will latter provide text
;; analysis in Emacs

;; Depending on your requirements, you can use one of the commands, described
;; below, to load corresponding set of features (these commands are listed in
;; increasing order, and each command include features of previous commands):
;;
;;     o   This is the default. Enables the database and idle reparse
;;         engines
;;(semantic-load-enable-minimum-features)

;;     o This enables some tools useful for coding, such as summary mode imenu
;;       support, the semantic navigator i.e prototype help and smart completion
(semantic-load-enable-code-helpers)

;;     o   This enables even more coding tools such as the nascent
;;         intellisense mode decoration mode, and stickyfunc mode (plus
;;         regular code helpers)
;;(semantic-load-enable-guady-code-helpers)

;;     o   This turns on which-func support (plus all other code
;;         helpers)
;;(semantic-load-enable-excessive-code-helpers)

;;     o   This turns on modes that aid in writing grammar and developing
;;         semantic tool.
;;         It does not enable any other features such as code helpers above.
;;(semantic-load-enable-semantic-debugging-helpers)

;; Directory that semantic use to cache its files
(setq semanticdb-default-save-directory "~/.emacs.d/.emacs-semanticdb") ; getting rid of semantic.caches

(require 'semantic-ia)

;; Add a new menu called "TAGS" that provides... Guess it;)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; -----------------------------
;; === Prepare CEDET binding ===
(defun my-cedet-hook ()
  ;; Intellisense menu
  (local-set-key (read-kbd-macro "C-<return>") 'semantic-ia-complete-symbol-menu)
  ;;  jump to declaration of variable or function, whose name is under point
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle) ; swith to/from declaration/implement
  ;;
  (local-set-key "\C-ch" 'semantic-decoration-include-visit) ; visit the header file under point
  ;;
  ;; shows documentation for function or variable, whose names is under point
  (local-set-key "\C-cd" 'semantic-ia-show-doc)     ; in a separate buffer
  (local-set-key "\C-cs" 'semantic-ia-show-summary) ; in the mini-buffer
  )

(add-hook 'c-mode-common-hook   'my-cedet-hook)
(add-hook 'lisp-mode-hook       'my-cedet-hook)
(add-hook 'scheme-mode-hook     'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)


;; ;; === EAssist (Emacs Assist) ===
;; see http://www.emacswiki.org/emacs/EAssist
(require 'eassist)
;; mainly provides the following methods:
;;  * eassist-switch-h-cpp: switch from header to body file and backwards (for
;;                           C++ only)
;;  * eassist-list-methods:  produce method/function list

(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "\C-xz") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; --------------------------------
;; === ECB (Emacs Code Browser) ===
;; see http://ecb.sourceforge.net/
;; or  http://www.emacswiki.org/emacs/EmacsCodeBrowser
;; or  http://www.emacswiki.org/emacs/PracticalECB
(require 'ecb)
(require 'ecb-autoloads)

;; /!\ Caution on ECB variable configuration
;; see http://ecb.sourceforge.net/docs/setq-or-customize.html#setq-or-customize 
;; for the options that shouldn't be configured via setq ;(

;; --- ECB layout
(setq ecb-create-layout-file "~/.emacs.d/.ecb-user-layout.el") ; where my layout are saved
(setq ecb-windows-width 35)

;; create a simple special layout "falkor1"
;; +------+-------+--------------------------------------+
;; |              |                                      |
;; |              |                                      |
;; | Directories  |                                      |
;; |              |          Edit                        |
;; |              |                                      |
;; +------+-------+                                      |
;; |   History    |                                      |
;; |              |                                      |
;; +-----------------------------------------------------+
(ecb-layout-define "falkor1" left nil
                   ;; The frame is already splitted side-by-side and point stays in the
                   ;; left window (= the ECB-tree-window-column)
                   
                   ;; Here is the creation code for the new layout
                   ;; 1. Defining the current window/buffer as ECB-methods buffer
                   (ecb-set-directories-buffer)
                   ;; 2. Splitting the ECB-tree-windows-column in two windows
                   (ecb-split-ver 0.75 t)
                   ;; 3. Go to the second window
                   (other-window 1)
                   ;; 4. Defining the current window/buffer as ECB-history buffer
                   (ecb-set-history-buffer)
                   ;; 5. Make the ECB-edit-window current (see Postcondition above)
                   (select-window (next-window)))

(setq ecb-show-sources-in-directories-buffer "falkor1")

;; see http://ecb.sourceforge.net/docs/Changing-the-ECB-layout.html for default
;; layout alternatives.
;; My personnal ECB layout (falkor), is as follows:
;; +------+-------+--------------------------------------+
;; |      |       |                                      |
;; | Dir  | Src   |                                      |
;; |      |       |                                      |
;; |      |       |                                      |
;; +------+-------+                                      |
;; |   History    |                 Edit                 |
;; |              |                                      |
;; +--------------+                                      |
;; |              |                                      |
;; |  Methods     |                                      |
;; |              |                                      |
;; |              |                                      |
;; +-----------------------------------------------------+
;; You can easily create your own layout using M-x ecv-create-new-layout
;; see ~/emacs.d/custom.el for the configuration of my own layout

(setq ecb-history-sort-method nil)  ; No sorting, means the most recently used
                                        ; buffers are on the top of the history
                                        ; and the seldom used buffers at the bottom
(setq ecb-vc-enable-support t)          ; show versionning status of the files
                                        ; in the sources/hstory (SVN etc.)
;; autostart ECB on emacs startup (put to nil to desactivate)
(setq ecb-auto-activate t)

;; --- Annoyances
;; use the primary button to navigate in the source tree -- middle button otherwise (!?!)
(setq ecb-tip-of-the-day nil)           ; disable tips of the day

(provide 'init-cedet)
;; ----------------------------------------------------------------------
;; eof
;;
;; Local Variables:
;; mode: lisp
;; End:


