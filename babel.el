
(fset 'org-wrap-elisp
 "#+begin_src emacs-lisp\C-m\C-m#+end_src\C-p")

(setq dotfiles-dir (file-name-directory
                    (or load-file-name (buffer-file-name))))

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "org-mode/lisp/"))
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'org)

(setq inhibit-startup-screen t)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)

(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error
     (condition-case nil
 (fix-frame-horizontal-size width)
       (error
(error "Cannot resize window or frame horizontally"))))))

(defun split-screen-80-char ()
 "Split screen horizontally with specified char width."
 (interactive)
 (split-window-horizontally 86))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(require 'linum)
(global-linum-mode 1)
(setq linum-format "%d ")

(require 'magit)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

(require 'smex)
(smex-initialize)

(require 'ace-jump-mode)

(require 'cedet)

(require 'eassist)

(require 'anything-config)

(require 'autopair)

(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-x3" 'split-screen-80-char)
(global-set-key "\C-xl" 'goto-line)

(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace-regexp)
(defalias 'sm 'eassist-list-methods)

(global-set-key "\C-cj" 'windmove-left)
(global-set-key "\C-ck" 'windmove-right)
(global-set-key "\C-cu" 'windmove-up)
(global-set-key "\C-cm" 'windmove-down)

(require 'yasnippet)

(require 'eclim)
(setq eclim-auto-save t)
(global-eclim-mode)

(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(require 'pydoc-info)

(require 'pymacs)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;; Initialize Rope
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(py-shell-name "ipython")
'(py-which-bufname "IPython"))

(require 'anything)
(require 'anything-ipython)
(when (require 'anything-show-completion nil t)
   (use-anything-show-completion 'anything-ipython-complete
                                 '(length initial-pattern)))

(require 'autopair)

(require 'python-pep8)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'python-pylint)

(defun python-add-breakpoint ()
  (interactive)
  (py-newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
(define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)

(add-hook 'python-mode-hook
          (lambda ()
           (push '(?' . ?')
                  (getf autopair-extra-pairs :code))
           (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))
           (autopair-mode t)
           (ropemacs-mode)
           (setq py-complete-function 'ipython-complete)
           (setq py-shell-complete-function 'ipython-complete)
           (setq py-shell-name "ipython")
           (setq py-which-bufname "IPython")))

(require 'cython-mode)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
