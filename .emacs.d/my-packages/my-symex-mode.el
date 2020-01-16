;;; -*- lexical-binding: t -*-
;;; TODO: ideally, would be good to have a simple POC of the AST
;;; to operate on, via semantic?
;;; TODO: consider using S for dragging and C for movement (and then across all modes)
;;; TODO: move back/forward through tree "at same level" without going up or down (i.e. switch branches, ideally preserving position index within branch)
;;; TODO: traverse tree with side effect (traversal-method, side-effect-fn), to use for "indent forward" on paste
;;; TODO: incorporate more clear tree-related terminology
;;; TODO: C-j to move in greedily, going forward
;;; TODO: handle "contracts" of each abstraction level, and where conditions should go, rename functions for clarity. legitimate detours vs conditional itineraries, vs conditional motions
;;; TODO: detours should be maneuvers. define a strategy as a higher-level sequence of maneuvers, where each is tried in sequence until all fail, beginning again from the first on success
;;; TODO: take a symex and bring it out and before/after as a peer of the parent
;;; TODO: my-tidy-symex has edge cases in indenting from evil-cp-a-form, where symex begins with : (keyword arg) or #'
(use-package lispy)
(use-package paredit)
(use-package evil-cleverparens)  ;; really only need cp-textobjects here
(require 'cl-lib)


(defun my-make-move (x y)
  (list x y))

(defun my-move-x (move)
  "X (horizontal) component of move."
  (nth 0 move))

(defun my-move-y (move)
  "Y (vertical) component of move."
  (nth 1 move))

(defvar move-zero (my-make-move 0 0))
(defvar move-go-forward (my-make-move 1 0))
(defvar move-go-backward (my-make-move -1 0))
(defvar move-go-in (my-make-move 0 1))
(defvar move-go-out (my-make-move 0 -1))

(defun is-null-move? (move)
  "Checks if the move specifies no movement."
  (are-moves-equal? move move-zero))

(defun move-exists? (move)
  "Checks if the move specifies tangible movement."
  (not (is-null-move? move)))

(defun are-moves-equal? (m1 m2)
  "Check if two moves are identical, including any conditions."
  (equal m1 m2))

(cl-defun my-make-maneuver (phases
                            &key
                            repeating?
                            pre-condition
                            post-condition)
  "Construct a maneuver from the given moves."
  (let ((pre-condition (or pre-condition (lambda () t)))
        (post-condition (or post-condition (lambda () t))))
    (list 'maneuver
          phases
          repeating?
          pre-condition
          post-condition)))

(defun my-maneuver-phases (maneuver)
  "Get the phases of a maneuver (which are themselves maneuvers or moves)."
  (nth 1 maneuver))

(defun my-maneuver-repeating? (maneuver)
  "Whether the maneuver is repeating or not."
  (nth 2 maneuver))

(defun my-maneuver-pre-condition (maneuver)
  "Pre-condition of maneuver"
  (nth 3 maneuver))

(defun my-maneuver-post-condition (maneuver)
  "Post-condition of maneuver"
  (nth 4 maneuver))

(defvar maneuver-zero (my-make-maneuver nil))

(defun is-maneuver? (obj)
  "Checks if the data specifies a maneuver."
  (condition-case nil
      (equal 'maneuver
             (nth 0 obj))
    (error nil)))

(defun is-null-maneuver? (maneuver)
  "Checks if the maneuver specifies no movement."
  (are-maneuvers-equivalent? maneuver
                             maneuver-zero))

(defun maneuver-exists? (maneuver)
  "Checks if the maneuver is non-zero."
  (not (is-null-maneuver? maneuver)))

(defun are-maneuvers-equal? (m1 m2)
  "Check if two maneuvers are identical."
  (equal m1 m2))

(defun naive-maneuver (maneuver)
  "A 'naive' version of the maneuver, not including any conditions, and
with no repetition."
  (my-make-maneuver (my-maneuver-phases maneuver)))  ;; TODO: should recursively transform nested maneuvers to naive ones, leaving moves alone

(defun are-maneuvers-equivalent? (m1 m2)
  "Check if two maneuvers are equal, disregarding any conditions and repetition."
  (are-maneuvers-equal? (naive-maneuver m1)
                        (naive-maneuver m2)))

(defun my-make-detour (reorientation maneuver)
  "Construct a detour.

A detour consists of two components -- a MANEUVER that we wish to execute, and
a REORIENTATION which is a transformation we want to apply prior to attempting
the maneuver. The reorientation could simply be another maneuver, or could itself
be a detour.

The reorientation is applied repeatedly and the maneuver is re-attempted each
time, until it succeeds. If the reorientation itself fails, then the detour fails
as well."
  (list 'detour
        reorientation
        maneuver))

(defun symex--detour-reorientation (detour)
  "Get the reorientation component of the DETOUR."
  (nth 1 detour))

(defun symex--detour-maneuver (detour)
  "Get the maneuver component of the DETOUR."
  (nth 2 detour))

(defun is-detour? (obj)
  "Checks if the data specifies a detour."
  (condition-case nil
      (equal 'detour
             (nth 0 obj))
    (error nil)))

(defun symex--execute-maneuver-with-reorientation (reorientation maneuver)
  "Apply a reorientation and then attempt the maneuver.

If the maneuver fails, then the reorientation is attempted as many times as
necessary until either it succeeds, or the reorientation fails.

Evaluates to a list of maneuvers executed, if any, which could be treated
as phases of a higher-level maneuver by the caller."
  (let ((executed-reorientation (if (is-detour? reorientation)
                                    (my-execute-detour reorientation)
                                  (my-execute-maneuver reorientation))))
    (when (maneuver-exists? executed-reorientation)
      (let ((executed-maneuver (my-execute-maneuver maneuver)))
        (if (maneuver-exists? executed-maneuver)
            (append (list executed-reorientation)
                    executed-maneuver)
          (let ((attempt (symex--execute-maneuver-with-reorientation reorientation
                                                                     maneuver)))
            (when attempt
              (append (list executed-reorientation)
                      attempt))))))))

(defun my-execute-detour (detour)
  "Execute the DETOUR."
  (let ((original-location (point))
        (reorientation (symex--detour-reorientation detour))
        (maneuver (symex--detour-maneuver detour)))
    (let ((result (symex--execute-maneuver-with-reorientation reorientation
                                                              maneuver)))
      (unless result
        (goto-char original-location))
      (my-make-maneuver result))))

(defun my-make-protocol (options)
  "Construct a protocol abstraction for the given OPTIONS.

An option could be either a maneuver, or a protocol itself."
  (list 'protocol
        options))

(defun symex--protocol-options (protocol)
  "Get the set of options that are part of the PROTOCOL."
  (nth 1 protocol))

(defun is-protocol? (obj)
  "Checks if the data specifies a protocol."
  (condition-case nil
      (equal 'protocol
             (nth 0 obj))
    (error nil)))

(defun symex--try-options-in-sequence (options)
  "Try options one at a time until one succeeds."
  (let ((option (car options))
        (remaining-options (cdr options)))
    (let ((executed-option (if (is-protocol? option)
                               (symex-execute-protocol option)
                             (my-execute-maneuver option))))
      (if (maneuver-exists? executed-option)
          executed-option
        (if remaining-options
            (symex--try-options-in-sequence remaining-options)
          maneuver-zero)))))

(defun symex-execute-protocol (protocol)
  "Given a protocol including a set of options, attempt to execute them
in order until one succeeds.

Evaluates to the maneuver actually executed."
  (let ((options (symex--protocol-options protocol)))
    (symex--try-options-in-sequence options)))

(defun symex-repeat-protocol (protocol)
  "Helper to repeatedly choose a maneuver to execute until steady state."
  (let ((maneuver (symex-execute-protocol protocol)))
    (when (maneuver-exists? maneuver)
      (symex-repeat-protocol protocol))))


(defmacro if-stuck (do-what operation &rest body)
  `(let ((orig-pt (point)))
     ,operation
     (if (= orig-pt (point))
         ,do-what
       ,@body)))

(defun point-at-root-symex? ()
  "Check if point is at a root symex."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-out)
              nil)))

(defun point-at-first-symex? ()
  "Check if point is at the first symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-backward)
              nil)))

(defun point-at-last-symex? ()
  "Check if point is at the last symex at some level."
  (interactive)
  (save-excursion
    (if-stuck t
              (symex-go-forward)
              nil)))

(defun point-at-final-symex? ()
  "Check if point is at the last symex in the buffer."
  (interactive)
  (save-excursion
    (if-stuck (progn (if-stuck t
                               (symex-go-out)
                               nil))
              (symex-go-forward)
              nil)))

(defun point-at-initial-symex? ()
  "Check if point is at the first symex in the buffer."
  (interactive)
  (save-excursion
    (condition-case nil
        (progn (backward-sexp 1)
               (not (thing-at-point 'sexp)))
      (error nil))))

(defun symex-comment-line-p ()
  "Checks if we're currently at the start of a comment line."
  (and (lispy-bolp)
       (looking-at-p ";")))

(defun symex-empty-list-p ()
  "Checks if we're looking at an empty list."
  (save-excursion
    (and (lispy-left-p)
         (progn (forward-char 2) ;; need to go forward by 2 for some reason
                (lispy-right-p)))))

(defun my-symex-index ()
  "Get relative (from start of containing symex) index of current symex."
  (interactive)
  (save-excursion
    (my-select-nearest-symex)
    (let ((original-location (point)))
      (let ((current-location (my-goto-first-symex))
            (result 0))
        (while (< current-location original-location)
          (symex-go-forward)
          (setq current-location (point))
          (setq result (+ 1 result)))
        result))))

(defun my--forward-one-symex ()
  "Forward one symex"
  (let ((original-location (point))
        (result 0))
    (if (thing-at-point 'sexp)
        (condition-case nil
            (progn (forward-sexp 2)
                   (setq result 2))
          (error (condition-case nil
                     (progn (forward-sexp 1)
                            (setq result 1))
                   (error (setq result 0)))))
      (condition-case nil
          (progn (forward-sexp 1)
                 (setq result 1))
        (error (setq result 0))))
    (condition-case nil
        (progn (backward-sexp 1)
               (setq result (- result 1)))
      (error nil))
    (let ((current-location (point)))
      (when (= original-location current-location)
        ;; happens at end of buffer
        (setq result 0)))
    (my-refocus-on-symex)
    result))

(defun my-forward-symex (&optional count)
  "Forward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--forward-one-symex)))
        (setq result (+ res result))))
    (my-make-move result 0)))

(defun my--backward-one-symex ()
  "Backward one symex."
  (let ((result 0))
    (when (not (point-at-initial-symex?))
      (condition-case nil
          (progn (backward-sexp 1)
                 (setq result (+ 1 result)))
        (error nil)))
    result))

(defun my-backward-symex (&optional count)
  "Backward symex"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--backward-one-symex)))
        (setq result (+ res result))))
    (my-refocus-on-symex)
    (my-make-move (- 0 result) 0)))

(defun my--enter-one-symex ()
  "Enter one lower symex level."
  (let ((result 1))
    (cond ((symex-comment-line-p)
           (lispy-flow 1))
          ((and (lispy-left-p)
                (not (symex-empty-list-p)))
           (forward-char))
          (t (setq result 0)))
    result))

(defun my-enter-symex (&optional count)
  "Enter lower symex level."
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--enter-one-symex)))
        (setq result (+ res result))))
    (my-make-move 0 result)))

(defun my--exit-one-symex ()
  "Exit one level."
  (condition-case nil
      (progn (paredit-backward-up 1)
             1)
    (error 0)))

(defun my-exit-symex (&optional count)
  "Exit to higher symex level"
  (interactive)
  (let ((count (or count 1))
        (result 0))
    (dotimes (i count)
      (let ((res (my--exit-one-symex)))
        (setq result (+ res result))))
    (my-make-move 0 (- 0 result))))

(defun execute-tree-move (move)
  "Execute the specified MOVE at the current point location in the tree.

Evaluates to the actual move executed."
  (let ((move-x (my-move-x move))
        (move-y (my-move-y move)))
    (cond ((> move-x 0)
           (my-forward-symex move-x))
          ((< move-x 0)
           (my-backward-symex (abs move-x)))
          ((> move-y 0)
           (my-enter-symex move-y))
          ((< move-y 0)
           (my-exit-symex (abs move-y)))
          (t ;; zero move
           move-zero))))

(cl-defun symex-go-forward (&optional (count 1))
  "Move forward COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move count 0)))

(cl-defun symex-go-backward (&optional (count 1))
  "Move backward COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move (- 0 count) 0)))

(cl-defun symex-go-in (&optional (count 1))
  "Move in COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move 0 count)))

(cl-defun symex-go-out (&optional (count 1))
  "Move out COUNT symexes."
  (interactive)
  (execute-tree-move (my-make-move 0 (- 0 count))))

(defun my--execute-maneuver-phases (phases)
  "Execute the phases of a maneuver, stopping if a phase fails.

Evalutes to a list of phases actually executed."
  (let ((current-phase (car phases))
        (remaining-phases (cdr phases)))
    (if (is-maneuver? current-phase)
        (let ((executed-phase (my-execute-maneuver current-phase)))
          (when (maneuver-exists? executed-phase)
            (let ((result (list executed-phase)))
              (if remaining-phases
                  (append result
                          (my--execute-maneuver-phases remaining-phases))
                result))))
      ;; base case -- execute in terms of primitives, i.e. moves
      (let ((executed-phase (execute-tree-move current-phase)))
        (when (move-exists? executed-phase)
          (let ((result (list executed-phase)))
            (if remaining-phases
                (append result
                        (my--execute-maneuver-phases remaining-phases))
              result)))))))

(defun my--execute-a-maneuver (maneuver)
  "Execute a MANEUVER specification once (disregarding any repetition)."
  (let ((original-location (point))
        (phases (my-maneuver-phases maneuver))
        (pre-condition (my-maneuver-pre-condition maneuver))
        (post-condition (my-maneuver-post-condition maneuver)))
    (if (not (funcall pre-condition))
        maneuver-zero
      (let ((executed-phases (my--execute-maneuver-phases phases)))
        (if (not (funcall post-condition))
            (progn (goto-char original-location)
                   maneuver-zero)
          (my-make-maneuver executed-phases))))))

(defun my--execute-maneuver-with-repetition (maneuver)
  "Execute maneuever, repeating it as specified.

Evaluates to a list containing each instance of maneuver execution (corresponding
to repetition of the maneuver), which could be treated as phases of a
higher-level maneuver by the caller.

TODO: instead of these 'unrolled' phases; add support for repeat 'args' to return
rolled ones."
  (let ((executed-maneuver (my--execute-a-maneuver maneuver))
        (repeating? (my-maneuver-repeating? maneuver)))
    (when (maneuver-exists? executed-maneuver)
      (let ((result (list executed-maneuver)))
        (if repeating?
            (append result
                    (my--execute-maneuver-with-repetition maneuver))
          result)))))

(defun my-execute-maneuver (maneuver)
  "Attempt to execute a given MANEUVER.

Attempts the maneuver in the order of its phases, accepting partial completion
of phases. If any phase fails entirely, then the maneuver it is part of is
terminated at that step.

The maneuver is only executed if PRE-CONDITION holds, and is reversed if
POST-CONDITION does not hold after the provisional execution of the maneuver.

If the maneuver is REPEATING, it will be repeated until it fails.

Evaluates to the maneuver actually executed."
  (let ((executed-phases (my--execute-maneuver-with-repetition maneuver)))
    (my-make-maneuver executed-phases)))

(defun my-goto-first-symex ()
  "Select first symex at present level"
  (interactive)
  (let ((maneuver (my-make-maneuver (list move-go-backward)
                                    :repeating? t)))
    (my-execute-maneuver maneuver))
  (my-refocus-on-symex)
  (point))

(defun my-goto-last-symex ()
  "Select last symex at present level"
  (interactive)
  (let ((maneuver (my-make-maneuver (list move-go-forward)
                                    :repeating? t)))
    (my-execute-maneuver maneuver))
  (my-refocus-on-symex)
  (point))

(defun my-goto-outermost-symex ()
  "Select outermost symex."
  (interactive)
  (let ((maneuver (my-make-maneuver (list move-go-out)
                                    :repeating? t)))
    (my-execute-maneuver maneuver))
  (my-refocus-on-symex)
  (point))

(defun my-goto-innermost-symex ()
  "Select innermost symex."
  (interactive)
  (let ((go-deep (my-make-maneuver (list move-go-in)
                                   :repeating? t))
        (go-forward (my-make-maneuver (list move-go-forward))))
    (let ((protocol-go-in (my-make-protocol (list go-deep
                                                  go-forward))))
      (symex-repeat-protocol protocol-go-in)))
  (my-refocus-on-symex)
  (point))

;; TODO: is there a way to "monadically" build the tree data structure
;; (or ideally, do an arbitrary structural computation) as part of this traversal?
;; key is, it has to be inferrable from inputs and outputs alone, i.e. specifically
;; from the result of invocation of e.g. forward-symex
(defun my-traverse-symex-forward (&optional flow)
  "Traverse symex as a tree, using pre-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at end of
current rooted tree."
  (interactive)
  (let ((preorder-in (my-make-maneuver (list move-go-in)))
        (preorder-forward (my-make-maneuver (list move-go-forward)))
        (exit-until-root
         (my-make-maneuver (list move-go-out)
                           :post-condition #'(lambda ()
                                               (not (point-at-root-symex?)))))
        (exit-until-end-of-buffer
         (my-make-maneuver (list move-go-out)
                           :post-condition #'(lambda ()
                                               (not (point-at-final-symex?))))))
    (let ((protocol-preorder-explore (my-make-protocol (list preorder-in
                                                             preorder-forward)))
          (reorientation (if flow
                             exit-until-end-of-buffer
                           exit-until-root)))
      (let ((maneuver (symex-execute-protocol protocol-preorder-explore))
            (detour (my-make-detour reorientation
                                    preorder-forward)))
        (if (maneuver-exists? maneuver)
            t
          (my-execute-detour detour))))))

(defun my-traverse-symex-backward (&optional flow)
  "Traverse symex as a tree, using converse post-order traversal.

If FLOW is true, continue from one tree to another. Otherwise, stop at root of
current tree."
  (interactive)
  (let* ((postorder-in
          (my-make-maneuver (list move-go-in
                                  (my-make-maneuver (list move-go-forward)
                                                    :repeating? t))
                            :repeating? t))
         (postorder-backwards-in
          (my-make-maneuver (list move-go-backward postorder-in)))
         (postorder-backwards-in-tree
          (my-make-maneuver (list move-go-backward postorder-in)
                            :pre-condition #'(lambda ()
                                               (not (point-at-root-symex?)))))
         (postorder-out (my-make-maneuver (list move-go-out))))
    (let ((postorder-explore (list postorder-backwards-in postorder-out))
          (postorder-explore-tree (list postorder-backwards-in-tree postorder-out)))
      (let* ((maneuvers (if flow
                            postorder-explore
                          postorder-explore-tree))
             (protocol-postorder (my-make-protocol maneuvers)))
        (let ((maneuver (symex-execute-protocol protocol-postorder)))
          (if (maneuver-exists? maneuver)
              t
            (error "Not implemented")))))))

(defun my-switch-branch-backward ()
  "Switch branch backward"
  (interactive)
  (let ((symex-index (my-symex-index))
        (closest-index -1)
        (best-branch-position (point)))
    (defun switch-backward ()
      (if (point-at-root-symex?)
          (goto-char best-branch-position)
        (symex-go-out)
        (if-stuck (switch-backward)
                  (symex-go-backward)
                  (if-stuck (switch-backward)
                            (symex-go-in)
                            (symex-go-forward symex-index)
                            (let ((current-index (my-symex-index)))
                              (when (and (< current-index
                                            symex-index)
                                         (> current-index
                                            closest-index))
                                (setq closest-index current-index)
                                (setq best-branch-position (point)))))))))
  (switch-backward))

(defun my-switch-branch-forward ()
  "Switch branch forward"
  (interactive)
  (let ((symex-index (my-symex-index)))
    (symex-go-out)
    (symex-go-forward)
    (symex-go-in)
    (symex-go-forward symex-index)))

(defun my-delete-symex ()
  "Delete symex"
  (interactive)
  (sp-kill-sexp nil)
  (cond ((or (current-line-empty-p)  ;; ^<>$
             (save-excursion (back-to-indentation)  ;; ^(<>
                             (forward-char)
                             (lispy-right-p)))
         (progn (evil-previous-line)
                (my-symex-join-lines)))
        ((save-excursion (evil-last-non-blank)  ;; (<>$
                         (lispy-left-p))
         (sp-next-sexp)
         (save-excursion
           (my-symex-join-lines t)))
        ((looking-at-p "\n")  ;; (abc <>
         (evil-join (line-beginning-position)
                    (line-end-position)))
        (t (fixup-whitespace)))
  (my-select-nearest-symex)
  (my-tidy-symex))

(defun my-change-symex ()
  "Change symex"
  (interactive)
  (kill-sexp 1)
  (evil-insert-state))

(defun my-replace-symex ()
  "Replace contents of symex"
  (interactive)
  (let ((move (symex-go-in)))
    (if (move-exists? move)
        (apply 'evil-change (evil-inner-paren))  ;; TODO: dispatch on paren type
      (sp-kill-sexp nil)
      (evil-insert-state))))

(defun my-spit-backward ()
  "Spit backward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in) ;; need to be inside the symex to spit and slurp
      (paredit-backward-barf-sexp 1))
    (symex-go-forward)
    (when (symex-empty-list-p)
      (fixup-whitespace)
      (re-search-forward lispy-left)
      (symex-go-out))))

(defun my-spit-forward ()
  "Spit forward"
  (interactive)
  (when (and (lispy-left-p)
             (not (symex-empty-list-p)))
    (save-excursion
      (symex-go-in) ;; need to be inside the symex to spit and slurp
      (paredit-forward-barf-sexp 1))
    (when (symex-empty-list-p)
      (symex-go-forward)
      (fixup-whitespace)
      (re-search-backward lispy-left))))

(defun my-slurp-backward ()
  "Slurp from behind"
  (interactive)
  (when (lispy-left-p)
    (if (symex-empty-list-p)
        (forward-char)
      (symex-go-in)) ;; need to be inside the symex to spit and slurp
    (paredit-backward-slurp-sexp 1)
    (fixup-whitespace)
    (symex-go-out)))

(defun my-slurp-forward ()
  "Slurp from the front"
  (interactive)
  (when (lispy-left-p)
    (save-excursion
      (if (symex-empty-list-p)
          (forward-char)
        (symex-go-in))  ;; need to be inside the symex to spit and slurp
      (lispy-forward-slurp-sexp 1))))

(defun my-join-symexes ()
  "Merge symexes at the same level."
  (interactive)
  (save-excursion
    (symex-go-forward)
    (paredit-join-sexps)))

(defun my-symex-join-lines (&optional backwards)
  "Join lines inside symex."
  (interactive)
  (save-excursion
    (if backwards
        (evil-previous-line)
      (forward-sexp))
    (evil-join (line-beginning-position)
               (line-end-position)))
  (when backwards
    (forward-char)))

(defun my-yank-symex ()
  "Yank (copy) symex."
  (interactive)
  (lispy-new-copy))

(defun my-paste-before-symex ()
  "Paste before symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-append "\n"))
        (t (setq extra-to-append " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (evil-paste-before nil nil)
        (forward-char)
        (insert extra-to-append))
      (symex-go-forward)
      (my-tidy-symex))))

(defun my-paste-after-symex ()
  "Paste after symex"
  (interactive)
  (cond ((or (and (point-at-indentation-p)
                  (not (bolp)))
             (save-excursion (forward-sexp)
                             (eolp)))
         (setq extra-to-prepend "\n"))
        (t (setq extra-to-prepend " ")))
  (with-undo-collapse
    (save-excursion
      (save-excursion
        (forward-sexp)
        (insert extra-to-prepend)
        (evil-paste-before nil nil)
        (forward-char))
      (symex-go-forward)
      (my-tidy-symex))
    (symex-go-forward)))

(defun my-open-line-after-symex ()
  "Open new line after symex"
  (interactive)
  (forward-sexp)
  (newline-and-indent)
  (evil-insert-state))

(defun my-open-line-before-symex ()
  "Open new line before symex"
  (interactive)
  (newline-and-indent)
  (evil-previous-line)
  (indent-according-to-mode)
  (evil-append-line 1))

(defun my-append-after-symex ()
  "Append after symex (instead of vim's default of line)."
  (interactive)
  (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
  (evil-insert 1 nil nil))

(defun my-insert-before-symex ()
  "Insert before symex (instead of vim's default at the start of line)."
  (interactive)
  (evil-insert 1 nil nil))

(defun my-insert-at-beginning-of-symex ()
  "Insert at beginning of symex."
  (interactive)
  (if (lispy-left-p)
      (evil-append 1 nil)
    (evil-insert 1 nil nil)))

(defun my-insert-at-end-of-symex ()
  "Insert at end of symex."
  (interactive)
  (if (lispy-left-p)
      (progn (forward-sexp)
             (backward-char)
             (evil-insert 1 nil nil))
    (progn (forward-sexp)
           (evil-insert 1 nil nil))))

(defun my-create-symex (type)
  "Create new symex (list)."
  (interactive)
  (save-excursion
    (cond ((equal type 'round)
           (insert "()"))
          ((equal type 'square)
           (insert "[]"))
          ((equal type 'curly)
           (insert "{}"))
          ((equal type 'angled)
           (insert "<>")))))

(defun my-insert-symex-newline ()
  "Insert newline and reindent symex."
  (interactive)
  (newline-and-indent)
  (my-tidy-symex))

(defun my-swallow-symex ()
  "Swallow symex, putting its contents in the parent symex."
  (interactive)
  (symex-go-in)
  (symex-go-forward)
  (paredit-splice-sexp-killing-backward))

(defun my-symex-wrap-round ()
  "Wrap with ()"
  (interactive)
  (paredit-wrap-round)
  (symex-go-out))

(defun my-symex-wrap-square ()
  "Wrap with []"
  (interactive)
  (paredit-wrap-square)
  (symex-go-out))

(defun my-symex-wrap-curly ()
  "Wrap with {}"
  (interactive)
  (paredit-wrap-curly)
  (evil-find-char-backward nil 123))

(defun my-symex-wrap-angled ()
  "Wrap with <>"
  (interactive)
  (paredit-wrap-angled)
  (evil-find-char-backward nil 60))

(defun my-wrap-symex ()
  "Wrap with containing symex."
  (interactive)
  (my-symex-wrap-round)
  (my-insert-at-beginning-of-symex))

(defun my-move-symex-forward ()
  "Move symex forward in current tree level."
  (interactive)
  (forward-sexp)
  (condition-case nil
      (progn (transpose-sexps 1)
             (backward-sexp))
    (error (backward-sexp))))

(defun my-move-symex-backward ()
  "Move symex backward in current tree level."
  (interactive)
  (let ((move (symex-go-backward)))
    (when (move-exists? move)
      (my-move-symex-forward)
      (symex-go-backward))))

(defun my-evaluate-symex ()
  "Evaluate Symex"
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-eval-symex))
          ((member major-mode elisp-modes)
           (my-elisp-eval-symex))
          ((equal major-mode 'scheme-mode)
           (my-scheme-eval-symex))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun my-evaluate-definition ()
  "Evaluate top-level definition"
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-send-definition nil))
        ((member major-mode elisp-modes)
         (eval-defun nil))
        ((equal major-mode 'scheme-mode)
         (geiser-eval-definition nil))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun my-eval-print-symex ()
  "Eval symex and print result in buffer."
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-print-last-sexp)))

(defun my-symex-repl ()
  "Go to REPL."
  (interactive)
  (cond ((equal major-mode 'racket-mode)
         (racket-repl))
        ((member major-mode elisp-modes)
         (my-lisp-repl))
        ((equal major-mode 'scheme-mode)
         (geiser-mode-switch-to-repl))
        (t (error "Symex mode: Lisp flavor not recognized!"))))

(defun my-switch-to-scratch-buffer ()
  "Switch to scratch buffer."
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))  ;; TODO: create in lisp interaction mode if missing)

(defun my-select-nearest-symex ()
  "Select symex nearest to point"
  (interactive)
  (cond ((and (not (eobp))
              (save-excursion (forward-char) (lispy-right-p))) ;; |)
         (forward-char)
         (lispy-different))
        ((looking-at-p "[[:space:]\n]")  ;; <> |<> or <> |$
         (condition-case nil
             (progn (re-search-forward "[^[:space:]\n]")
                    (backward-char))
           (error (if-stuck (symex-go-backward)
                            (symex-go-forward)))))
        ((thing-at-point 'sexp)  ;; som|ething
         (beginning-of-thing 'sexp))
        (t (if-stuck (symex-go-backward)
                     (symex-go-forward))))
  (my-refocus-on-symex)
  (point))

(defun my-describe-symex ()
  "Lookup doc on symex."
  (interactive)
  (save-excursion
    (forward-sexp)  ;; selected symexes will have the cursor on the starting paren
    (cond ((equal major-mode 'racket-mode)
           (my-racket-describe-symbol))
          ((member major-mode elisp-modes)
           (my-elisp-describe-symbol))
          ((equal major-mode 'scheme-mode)
           (my-scheme-describe-symbol))
          (t (error "Symex mode: Lisp flavor not recognized!")))))

(defun my-refocus-on-symex (&optional smooth-scroll)
  "Move screen to put symex in convenient part of the view."
  (interactive)
  ;; Note: window-text-height is not robust to zooming
  (let* ((window-focus-line-number (/ (window-text-height)
                                       3))
         (current-line-number (line-number-at-pos))
         (top-line-number (save-excursion (evil-window-top)
                                          (line-number-at-pos)))
         (window-current-line-number (- current-line-number
                                        top-line-number))
         (window-scroll-delta (- window-current-line-number
                                 window-focus-line-number))
         (window-upper-view-bound (/ (window-text-height)
                                     9))
         (window-lower-view-bound (* (window-text-height)
                                     (/ 4.0 6))))
    (unless (and (> window-current-line-number
                    window-upper-view-bound)
                 (< window-current-line-number
                    window-lower-view-bound))
      (if smooth-scroll
          (dotimes (i (/ (abs window-scroll-delta)
                         3))
            (condition-case nil
                (evil-scroll-line-down (if (> window-scroll-delta 0)
                                           3
                                         -3))
              (error nil))
            (sit-for 0.0001))
        (recenter window-focus-line-number)))))

(defun my-tidy-symex ()
  "Auto-indent symex and fix any whitespace."
  (interactive)
  (fixup-whitespace)
  (when (save-excursion (looking-at-p "[[:space:]]"))
      (forward-char))
  (save-excursion
    (forward-sexp)
    (fixup-whitespace))
  (save-excursion
    (apply 'evil-indent
           (seq-take (evil-cp-a-form 1)
                     2)))
  (my-select-nearest-symex))


(defhydra hydra-symex (:idle 1.0
                       :columns 5
                       :color pink
                       :body-pre (progn (my-select-nearest-symex)
                                        (evil-symex-state)))
  "Symex mode"
  ("(" (lambda ()
         (interactive)
         (my-create-symex 'round)) "()")
  ("[" (lambda ()
         (interactive)
         (my-create-symex 'square)) "[]")
  ("{" (lambda ()
         (interactive)
         (my-create-symex 'curly)) "{}")
  ("<" (lambda ()
         (interactive)
         (my-create-symex 'angled)) "<>")
  ("h" symex-go-backward "previous")
  ("j" symex-go-in "enter")
  ("k" symex-go-out "exit")
  ("l" symex-go-forward "next")
  ("f" (lambda ()
         (interactive)
         (my-traverse-symex-forward t)) "flow forward")
  ("b" (lambda ()
         (interactive)
         (my-traverse-symex-backward t)) "flow backward")
  ("C-k" my-switch-branch-backward "switch branch backward")
  ("C-j" my-switch-branch-forward "switch branch forward")
  ("y" my-yank-symex "yank (copy)")
  ("p" my-paste-after-symex "paste after")
  ("P" my-paste-before-symex "paste before")
  ("x" my-delete-symex "delete")
  ("c" my-change-symex "change" :exit t)
  ("s" my-replace-symex "replace" :exit t)
  ("H" my-move-symex-backward "move backward")
  ("L" my-move-symex-forward "move forward")
  ("K" paredit-raise-sexp "raise")
  ("s-J" my-slurp-backward "slurp backward")
  ("s-H" my-spit-backward "spit backward")
  ("s-L" my-spit-forward "spit forward")
  ("s-K" my-slurp-forward "slurp forward")
  ("z" my-swallow-symex "swallow")
  ("e" my-evaluate-symex "evaluate")
  ("d" my-evaluate-definition "evaluate definition")
  ("E" eval-expression "eval expression")
  ("t" my-switch-to-scratch-buffer "scratch buffer" :exit t)
  ("r" my-symex-repl "go to REPL" :exit t)
  ("|" lispy-split "split")
  ("m" my-join-symexes "merge (join)")
  ("\\" lispy-splice "splice (join to higher level)")
  (")" my-symex-wrap-round "wrap with ()")
  ("]" my-symex-wrap-square "wrap with []")
  ("}" my-symex-wrap-curly "wrap with {}")
  (">" my-symex-wrap-angled "wrap with <>")
  ("o" my-open-line-after-symex "open line after" :exit t)
  ("O" my-open-line-before-symex "open line before" :exit t)
  ("n" my-insert-symex-newline "newline")
  ("J" my-symex-join-lines "join lines")
  ("N" (lambda ()
         (interactive)
         (my-symex-join-lines t)) "join lines backwards")
  ("0" my-goto-first-symex "go to first")
  ("M-h" my-goto-first-symex "go to first")
  ("$" my-goto-last-symex "go to last")
  ("M-l" my-goto-last-symex "go to last")
  ("M-k" my-goto-outermost-symex "go to outermost")
  ("M-j" my-goto-innermost-symex "go to innermost")
  ("=" my-tidy-symex "tidy/indent")
  ("A" my-append-after-symex "append after symex" :exit t)
  ("a" my-insert-at-end-of-symex "append inside symex" :exit t)
  ("i" my-insert-at-beginning-of-symex "insert inside symex" :exit t)
  ("I" my-insert-before-symex "insert before symex" :exit t)
  ("w" my-wrap-symex "wrap with symex" :exit t)
  (";" my-eval-print-symex "eval + print")
  ;; canonical action
  ("s-;" my-evaluate-symex "evaluate" :exit t)
  ;; escape hatches
  ("R" evil-replace-state nil :exit t)
  ("v" evil-visual-char nil :exit t)
  ("V" evil-visual-line nil :exit t)
  ("C-v" evil-visual-block nil :exit t)
  ;; standard exits
  ("?" my-describe-symex "info")
  ("<return>" eem-enter-lower-level "enter lower level" :exit t)
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t))

(global-set-key (kbd "s-y") 'hydra-symex/body)  ;; since y looks like inverted lambda
(global-set-key (kbd "s-;") 'hydra-symex/body)  ;; since y is hard to reach

(provide 'my-symex-mode)
