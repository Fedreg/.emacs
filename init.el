; increase garbace collector threshold for startup
(setq gc-cons-threshold 800000000)
(setq inhibit-compacting-font-caches t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; List packages you want to install
(setq package-list '(ag
 		     cider
		     company
		     fzf
		     key-chord
		     magit
		     meow
		     rainbow-delimiters
		     smartparens
		     typit
		     xclip))

;; Package Repos
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;;Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(defun edit-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar bg "#162230")
  (load-theme 'tsdh-dark t)
  (set-face-background 'mode-line bg)
  (set-face-foreground 'mode-line "#bbb")
  (set-face-background 'mode-line-inactive bg)
  (set-face-foreground 'mode-line-inactive bg)
  (set-face-background 'default bg)
  ;; (set-face-attribute 'mode-line nil :box nil)
  (set-cursor-color "#f00") 
  (set-face-attribute 'mode-line-buffer-id nil :foreground "DeepSkyBlue1")
  (set-face-background 'vertical-border "#222")
  (set-face-foreground 'vertical-border "#333"))

;; y or n for prompts
(fset 'yes-or-no-p 'y-or-n-p)


;; Gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ns-auto-hide-menu-bar t)

;; No startup banner
(setq inhibit-startup-message t)

(setq initial-major-mode 'fundamental-mode)

;; Font
(add-to-list 'default-frame-alist '(font . "Menlo-16"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ido Mode (default emacs narrowing; comes with Emacs)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; No Sounds
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ag-highlight-search 't)
(setq ag-reuse-buffers 't)
(add-hook 'ag-mode-hook #'next-error-follow-minor-mode)

(setq magit-blame-styles
      '((margin
	 (margin-format " %s%f" " %C %a" " %H")
	 (margin-width . 42)
	 (margin-face . magit-blame-margin)
	 (margin-body-face magit-blame-dimmed))))

;; So that customizations aren't loaaded into this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Auto Save on loss of focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(global-company-mode)

(xclip-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(setq cider-auto-jump-to-error             nil)
(setq cider-auto-select-error-buffer       nil)
(setq cider-auto-select-test-report-buffer t)
(setq cider-repl-display-help-banner       nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-pop-to-buffer-on-connect  nil)
(setq cider-repl-use-pretty-printing       t)
(setq cider-save-file-on-load              t)
(setq cider-show-error-buffer              :only-in-repl)
(setq cider-test-show-report               nil)
(setq nrepl-hide-special-buffers           t)
(setq cider-print-options '(("length" 1500)))

(require 'smartparens-config)
(smartparens-global-mode 1)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument) 
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("b" . ido-switch-buffer)
   '("f" . ag-project)
   '("l" . switch-to-previous-buffer)
   '("o" . other-window)
   '("z" . fzf-find-file)
   '(";" . comment-line)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . isearch-forward)
   '(";" . meow-reverse)
   '("'" . repeat)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-kill)
   '("e" . meow-find)
   '("f" . meow-next-word)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-delete)
   '("V" . meow-line)
    '("<escape>" . mode-line-other-buffer)))

(require 'meow)
(meow-setup)
(meow-global-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define meow-insert-state-keymap "jk" [escape])
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c d") 'cider-debug-defun-at-point)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c G") 'magit-blame-echo)
(global-set-key (kbd "C-c i") 'edit-init-file)
(global-set-key (kbd "C-c l") 'switch-to-previous-buffer)
(global-set-key (kbd "C-o") 'other-window)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup after load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 2 1000 1000))

;; Keybinding Cheatsheet

;; Window
;; C-x o - other window
;; C-x 0 - close current window
;; C-x 1 - one window
;; C-x 2 - horizontal split
;; C-x 3 - vertical split

;; Movement
;; C-f - forward
;; C-b - back
;; M-f - forward word
;; M-b - back word
;; C-n - next line
;; C-p - previous-line
;; C-a - beginning of line
;; C-e - end of line
;; M-a - beginning of sentence
;; M-e - end of sentence
;; C-v - scroll-down fast
;; M-v - scroll-up fast
;; C-x [ - beginning of file
;; C-x ] - end of file
;; M-h - move to beginning of current defun and mark to end

;; Deleting
;; C-k Kill from cursor to end of line
;; M-k kill from cursor to end of sentence
;; C-d kill char after cursor
;; M-d kill word after cursor
;; C-SPC to select text C-w to delete

;; Copy/Paste
;; C-k to kill (copy)
;; C-y to yank (paste)

;; Files
;; C-x C-f - Find file
;; C-x C-s - save file

;; Buffers
;; C-x C-bn - List buffers

;; Search
;; C-s - Search forward
;; C-r - Search backward

;; Comment
;; M-;

;; Shell
;; M-! Quick shell commannd in minibuffer (prefix with C-u to insert output at point)

;; Undo
;; C-/

;; Rectangle
;; M-h to select rectangle
;; M-w to add to kill ring
;; C-x r d - delete

;; Zap
;; M-z CHAR

;; CIDER
;; C-c M-c - cider-connect-clj
;; C-c C-k - cider-load-buffer
;; C-c C-e - cider-eval-last-sexp
;; C-c C-c - Cider-eval-defun-to-point
;; C-c C-p - cider-eval-pprint-last-sexp
;; C-c C-d d - cider-doc
;; C-c C-d j - cider-javadoc
;; C-c C-d c - cider-clojuredoc
;; C-c C-z - cider-switch-to-repl-buffer
;; C-c C-b - cider interrupt
;; C-u C-M-x - instrument debugger
;; M-. - Jump to definition
;; M-, - pop back

;; C-c C-t n - run ns tests
;; C-c C-t p - run all loaded ns tests
;; C-c C-t t - run test under point
