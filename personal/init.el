;; Add additional packages
(prelude-require-packages '(gdscript-mode))

;; Make C-a work in term-mode
(defun c-a-fixing-term-mode-hook ()
    (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
        (set-keymap-parent newmap oldmap)
        (define-key newmap (kbd "C-a") nil)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'term-mode-hook 'c-a-fixing-term-mode-hook)
(add-hook 'vterm-mode-hook 'c-a-fixing-term-mode-hook)

;; Start server
(load "server")
(unless (server-running-p) (server-start))

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
