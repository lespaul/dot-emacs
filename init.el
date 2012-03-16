;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org-mode" (expand-file-name
				     "lisp"))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'org-install)
  (require 'ob-tangle))

;; load up the main file
(org-babel-load-file (expand-file-name "babel.org" dotfiles-dir))
;;; init.el ends here

