;; Ruby IDE
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'flycheck-mode))

(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package inf-ruby
  :config
  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(defun my-ruby-send-to-repl ()
  "Send a region or definition to the REPL, depending on context."
  (interactive)
  (condition-case nil
      (inf-ruby-proc)
    (error (progn (inf-ruby "ruby")
                  (other-window 1))))
  (if (region-active-p)
      (ruby-send-region-and-go (region-beginning) (region-end))
    (ruby-send-definition-and-go)))

(defhydra hydra-ruby (:timeout my-leader-timeout
                      :columns 2
                      :exit t)
  "Ruby menu"
  ("c" elpy-check "Run lint checks")
  ("g" evil-jump-to-tag "Go to definition")
  ("i" elpy-doc "See documentation on this")
  ("o" elpy-occur-definitions "See all definitions in current buffer")
  ("p" inf-ruby "Go to Ruby REPL")
  ("r" my-ruby-send-to-repl "Send to REPL")
  ("t" elpy-test "Run test(s)"))

;; pull up ruby hydra with local leader
(add-hook 'ruby-mode-hook
          (lambda () (general-define-key
                      :states '(normal visual motion)
                      :keymaps 'local
                      my-local-leader 'hydra-ruby/body)))

(provide 'my-ruby)
