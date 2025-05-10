;;; cargo-insta.el --- A tool for managing cargo-insta. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Samuel Thomas

;; Name: cargo-insta
;; Version: 0.1.0
;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (s dash magit-section f)

;;; External packages:
(require 's)
(require 'dash)
(require 'magit-section)
(require 'f)

;;; Code:
(defgroup cargo-insta nil
  "Variables for `cargo-insta'")

(defvar cargo-insta-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (keymap-set map "TAB" #'magit-section-toggle)
    (keymap-set map "a"   #'cargo-insta--accept-snapshot)
    (keymap-set map "r"   #'cargo-insta--reject-snapshot)
    map)
  "Keymap for `cargo-insta-mode'")

(defvar cargo-insta--pending-snapshots '()
  "List of cargo insta pending snapshots")

(defvar cargo-insta--accepted-snapshots '()
  "List of cargo insta accepted snapshots")

(defvar cargo-insta--rejected-snapshots '()
  "List of cargo insta rejected snapshots")

(defface cargo-insta-diff-added-highlight
  '((t . (:foreground "#b8bb26")))
  "Face for lines in a diff that have been added."
  :group 'cargo-insta)

(defface cargo-insta-diff-removed-highlight
  '((t . (:foreground "#fb4933")))
  "Face for lines in a diff that have been added."
  :group 'cargo-insta)

(defun cargo-insta ()
  "Launch cargo insta"
  (interactive)

  ;; TODO: have buffers per project
  (let* ((new-buffer (get-buffer-create "*cargo-insta*")))
    ;; setup new buffer
    (with-current-buffer new-buffer
      ;; TODO: set default directory to project root
      (cargo-insta-mode)
      (revert-buffer))

    ;; display the buffer
    (switch-to-buffer new-buffer)))

(define-derived-mode cargo-insta-mode special-mode "Cargo Insta"
  "Major mode for Cargo insta"

  (buffer-disable-undo)
  (setq-local truncate-lines t
              revert-buffer-function (lambda (&optional _ignore-auto _no-confirm)
                                       (cargo-insta--reload)))
  (when evil-mode
    (evil-make-overriding-map cargo-insta-mode-map nil))
  (use-local-map cargo-insta-mode-map))

(defun cargo-insta--reload ()
  "Refresh the current *cargo-insta* buffer"

  (setq cargo-insta--pending-snapshots (cargo-insta--get-pending-snapshots))
  (setq cargo-insta--accepted-snapshots nil)
  (setq cargo-insta--rejected-snapshots nil)
  (cargo-insta--render))


(defun cargo-insta--render ()
  "Render out the contents of `cargo-insta-snapshots' to the buffer."
  (let ((inhibit-read-only t)
        (pending cargo-insta--pending-snapshots)
        (accepted cargo-insta--accepted-snapshots)
        (rejected cargo-insta--rejected-snapshots))
    (save-excursion
      (erase-buffer)

      ;; create the root section
      (magit-insert-section (magit-section)

        (when pending
          (magit-insert-section (magit-section)
            (magit-insert-heading "Pending Snapshots")
            (-each pending
              #'cargo-insta--insert-snapshot-section)))

        (when accepted
          (magit-insert-section (magit-section)
            (magit-insert-heading "Accepted Snapshots")
            (-each accepted
              #'cargo-insta--insert-snapshot-section)))

        (when rejected
          (magit-insert-section (magit-section)
            (magit-insert-heading "Rejected Snapshots")
            (-each rejected
              #'cargo-insta--insert-snapshot-section)))

        (unless (or pending accepted rejected)
          (insert "No pending snapshots"))))))

(defun cargo-insta--get-pending-snapshots ()
  "Get current list of pending snapshots."

  (let ((raw-snapshots (--> "cargo insta pending-snapshots --as-json"
                            (shell-command-to-string it))))
    (if (not (string-empty-p raw-snapshots))
        (--> raw-snapshots
             (s-trim-right it)
             (s-split "\n" it)
             (--map (json-parse-string it :object-type 'plist) it)
             (--map (plist-put it :diff (cargo-insta--generate-diff (plist-get it :path))) it))
      nil)))

(defun cargo-insta--trim-name (full-path)
  (f-base full-path))

(defun cargo-insta--generate-diff (snapshot)
  "Generate a diff in patch format for `snapshot'"

  (--> (format "diff -Nau %s %s.new" snapshot snapshot)
       (shell-command-to-string it)))

(defun cargo-insta--insert-snapshot-section (snapshot)
  (magit-insert-section (magit-section snapshot)
    (magit-insert-heading (propertize (cargo-insta--trim-name (plist-get snapshot :path))
                                      'face 'bold))
    (magit-insert-section-body
      (let ((diff (plist-get snapshot :diff))
            (i 0))
        (dolist (line (s-split "\n" diff))
          (cond ((< i 3) nil)
                ((s-starts-with? "+" line) (insert (propertize line 'face 'cargo-insta-diff-added-highlight)))
                ((s-starts-with? "-" line) (insert (propertize line 'face 'cargo-insta-diff-removed-highlight)))
                (t (insert line)))
          (setq i (1+ i))
          (when (> i 3)
            (insert "\n")))))))

(defun cargo-insta--current-snapshot ()
  (--> (magit-current-section)
       (magit-section-ident it)
       (cdar it)))

(defun cargo-insta--operation-on-snapshot (operation snapshot)
  (--> snapshot
       (plist-get it :path)
       (format "cargo insta %s --snapshot %s" operation it)
       (shell-command-to-string it)))

(defun cargo-insta--accept-snapshot ()
  (interactive)
  (let ((snapshot (cargo-insta--current-snapshot)))
    (cargo-insta--operation-on-snapshot "accept" snapshot)
    (setq cargo-insta--pending-snapshots (delete snapshot cargo-insta--pending-snapshots))
    (add-to-list 'cargo-insta--accepted-snapshots snapshot))
  
  (cargo-insta--render))

(defun cargo-insta--reject-snapshot ()
  (interactive)
  (let ((snapshot (cargo-insta--current-snapshot)))
    (cargo-insta--operation-on-snapshot "reject" snapshot)
    (setq cargo-insta--pending-snapshots (delete snapshot cargo-insta--pending-snapshots))
    (add-to-list 'cargo-insta--rejected-snapshots snapshot))
  (cargo-insta--render))

(provide 'cargo-insta)

;;; cargo-insta.el ends here
