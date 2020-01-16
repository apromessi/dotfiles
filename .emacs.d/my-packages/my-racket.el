(use-package racket-mode
  :config
  ;; explicitly indicate .rkt files are to be opened in racket-mode
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

(defun my-racket-describe-symbol ()
  "Describe symbol at point"
  (interactive)
  (racket-describe nil))

(defun my-racket-eval-symex ()
  "Eval last sexp.

Accounts for different point location in evil vs emacs mode."
  (interactive)
  (save-excursion
      (when (equal evil-state 'normal)
        (forward-char))
      (racket-send-last-sexp)))

(defun my-racket-eval-region ()
  "Eval region"
  (interactive)
  (racket-send-region (region-beginning) (region-end))
  (deactivate-mark))

(defun my-racket-eval-exp-or-region ()
  "Eval region or last sexp"
  (interactive)
  (if mark-active
      (progn (my-racket-eval-region)
             (message "Evaluated region."))
    (my-racket-eval-symex)))

(defun my-racket-eval (what)
  "Evaluate something"
  (interactive "cwhat?")
  (cond ((equal what ?e) (my-racket-eval-exp-or-region))
        ((equal what ?r) (my-racket-eval-exp-or-region))
        ((equal what ?f) (racket-send-definition nil))
        (t nil)))

(defhydra hydra-racket (:timeout my-leader-timeout
                        :columns 2
                        :exit t)
  "Racket menu"
  ("e" my-racket-eval "Eval")
  ("v" my-racket-eval "Eval")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" my-racket-describe-symbol "See documentation on this")
  ("?" my-racket-describe-symbol "See documentation on this")
  ("r" racket-repl "Go to racket REPL"))

(defun register-racket-leader ()
  "Pull up racket hydra with local leader"
  (interactive)
  (general-define-key :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-racket/body))

;; register racket leader in racket mode
(add-hook 'racket-mode-hook 'register-racket-leader)

(provide 'my-racket)
