(require 'typst-ts-mode)
(require 'treesit)
(require 'prog-mode)

(defvar typst-prettify-symbols-alist nil
  "Alist containing all of the Unicode symbols for Typst.")

(defconst cdtypst--get-symbols-script
  (expand-file-name "~/.config/emacs/lisp/cdtypst/symbols.rs")
  "File name for rust-script file fetching the symbols.")

(defvar cdtypst--get-symbols-status nil
  "Indicates whether symbols have been regenerated this Emacs session.")

(defun cdtypst--prettify-symbols-compose-predicate (start end match)
  (treesit-parent-until (treesit-node-at start) "math"))

(defun cdtypst--compute-symbols (&optional force)
  (unless (and cdtypst--get-symbols-status
               (not force))
    (setq typst-prettify-symbols-alist
          (read (shell-command-to-string cdtypst--get-symbols-script)))
    (setq cdtypst--get-symbols-status t)))

(defun turn-on-cdtypst ()
  (interactive)
  (unless (eq major-mode 'typst-ts-mode)
    (user-error "Cannot enable cdtypst in non-Typst buffer."))
  (cdtypst--compute-symbols)
  (setq-local prettify-symbols-alist typst-prettify-symbols-alist)
  (setq-local prettify-symbols-compose-predicate #'cdtypst--prettify-symbols-compose-predicate)
  (prettify-symbols-mode))

(defun turn-off-cdtypst ()
  (interactive)
  (prettify-symbols-mode -1))

(define-minor-mode cdtypst-mode
  "Minor mode for editing Typst documents."
  :ligher "CDT"
  (if cdtypst-mode
      (turn-on-cdtypst)
    (turn-off-cdtypst)))

(provide 'cdtypst)
