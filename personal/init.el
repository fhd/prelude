;; Add additional packages
(prelude-require-packages '(gdscript-mode
                            vterm
                            ledger-mode
                            arduino-cli-mode
                            rg
                            csharp-mode
                            plantuml-mode))

;; Start server
(load "server")
(unless (server-running-p) (server-start))

;; Open vterm from crux-visit-term-buffer
(defun crux-vterm (buffer-name)
    ;; TODO: Set crux-shell and buffer-name
    (vterm))
(customize-set-variable 'crux-term-func #'crux-vterm)

;; Make C-a work in *term-mode
(defun c-a-fixing-term-mode-hook ()
    (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
        (set-keymap-parent newmap oldmap)
        (define-key newmap (kbd "C-a") nil)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'term-mode-hook 'c-a-fixing-term-mode-hook)
(add-hook 'vterm-mode-hook 'c-a-fixing-term-mode-hook)

;; Split windows more sensibly
;; From: https://emacs.stackexchange.com/a/33756
(setq split-height-threshold 120
    split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                (with-selected-window window
                    (split-window-right)))
            (and (window-splittable-p window)
                (with-selected-window window
                    (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

;; Additional smartparens key bindings
(define-key smartparens-mode-map
  (kbd "M-S-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map
  (kbd "M-S-<backspace>") 'sp-backward-unwrap-sexp)

;; Automatically save org-mode archive files
(setq org-archive-subtree-save-file-p t)

;; Use c-mode and arduino-cli-mode for Arduino .ino files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . (lambda ()
                                                (progn
                                                  (c-mode)
                                                  (arduino-cli-mode)))))

;; Support plantuml in org-mode
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
(setq org-plantuml-jar-path (expand-file-name "~/Software/plantuml.jar"))

;; Use Stroustrup-style indentation for C/C++
(setq c-default-style "stroustrup")
