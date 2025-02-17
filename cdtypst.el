;; -*- lexical-binding: t; -*-

(require 'typst-ts-mode)
(require 'treesit)
(require 'prog-mode)

(defcustom cdtypst-takeover-sub-superscript t
  "Non-nil means cdtypst is allowed to take over the ^ and _ keys."
  :group 'cdtypst
  :type '(boolean))

(defcustom cdtypst-simplify-sub-superscripts t
  "Non-nil means cdtypst is allowed to simplify single-letter sub- and superscripts."
  :group 'cdtypst
  :type '(boolean))

(defcustom cdtypst-takeover-dollar t
  "Non-nil means cdtypst is allowed to take over the $."
  :group 'cdtypst
  :type '(boolean))

(defcustom cdtypst-math-symbol-letter-alist
  '((?a . "alpha")
    (?b . "beta")
    (?g . "gamma")
    (?d . "delta")
    (?e . "epsilon")
    (?z . "zeta"))
  "Alist consisting of math letters and their typst symbol names."
  :type '(alist :key character :value string))

(defcustom cdtypst-math-symbol-operator-alist
  '((?u . "union")
    (?U . "union.big")
    (?i . "sect")
    (?I . "sect.big"))
  "Alist consisting of math operators and their typst symbol names."
  :type '(alist :key character :value string))

(defcustom cdtypst-math-modify-alist
  '((?s . "sans")
    (?f . "frak")
    (?m . "mono")
    (?b . "bb")
    (?c . "cal"))
  "Alist with math variants."
  :group 'cdtypst
  :type '(alist :key character :value string))

(defcustom cdtypst-math-symbol-entity-alist
  '((?0 . "emptyset"))
  "Alist with math entities."
  :group 'cdtypst
  :type '(alist :key character :value string))

(defcustom cdtypst-tab-hook (list #'cdtypst--tab-indent)
  "A list of functions called by TAB before the default command is
executed."
  :group 'cdtypst
  :type '(repeat (function :tag "Function" :value nil)))

(defvar typst-prettify-symbols-alist nil
  "Alist containing all of the Unicode symbols for typst.")

(defconst cdtypst--get-symbols-script
  (expand-file-name "~/.config/emacs/lisp/cdtypst/symbols.rs")
  "File name for rust-script file fetching the symbols.")

(defvar cdtypst--get-symbols-status nil
  "Indicates whether symbols have been regenerated this Emacs session.")

(defun cdtypst--in-math-p (&optional point)
  (let ((node (treesit-node-at (or point (point)))))
    (treesit-parent-until node "math")))

(defun cdtypst--prettify-p (&optional point)
  (let* ((start (or point (point)))
         (node (treesit-node-at start)))
    (and (= start (treesit-node-start node))   ; Only prettify full symbols (not within),
         (treesit-parent-until node "math")))) ; actually in math mode.

(defun cdtypst--prettify-symbols-compose-predicate (start end match)
  (cdtypst--prettify-p start))

(defun cdtypst--compute-symbols (&optional force)
  (unless (and cdtypst--get-symbols-status
               (not force))
    (setq typst-prettify-symbols-alist
          (read (shell-command-to-string cdtypst--get-symbols-script)))
    (setq cdtypst--get-symbols-status t)))

(defun cdtypst--ensure-math ()
  (unless (cdtypst--in-math-p)
    (insert "$$")
    (backward-char)))

(defun cdtypst-sub-superscript ()
  "Insert ^() or _().
When not in typst math mode, ^() and _() will have dollars."
  (interactive)
  (if (cdtypst--in-math-p)
      (progn (insert (event-basic-type last-command-event))
             (insert "()")
             (backward-char))
    (insert (event-basic-type last-command-event))))

(defun cdtypst-dollar (&optional arg)
  "Insert a pair of dollars; with ARG, insert spaced dollars."
  (interactive "P")
  (cond
   ((region-active-p)
    (pcase-let ((`(,start . ,end) (region-bounds)))
      (goto-char start)
      (insert "$")
      (goto-char (1+ end))
      (insert "$")))
   ((cdtypst--in-math-p)
    (if (char-equal (following-char) ?$)
        (forward-char)
      (message "No dollars inside math mode!")))
   (arg
    (insert "$\n\n$")
    (backward-char 2))
   (t
    (insert "$$")
    (backward-char))))

(defun cdtypst--tab-indent ()
  (let ((point (point)))
    (typst-ts-mode-indent-line-function)
    (not (= point (point)))))

;; Mostly taken as is from cdlatex-tab
(defun cdtypst-tab ()
  "This function is intended to do many cursor movements.

It cleans up short subscripts and superscripts at point. This feature
can be disabled by setting `cdtypst-simplify-sub-superscripts' to nil.

Then it jumps to the next point in a typst text where one would
reasonably expect that more input can be put in."
  (interactive)
  (catch 'stop
    ;; Try hooks.
    (let ((funcs cdtypst-tab-hook))
      (while funcs (if (funcall (pop funcs)) (throw 'stop t))))
    (cond
     ((looking-at (rx ?\)))
      (backward-char 3)
      (if (and (looking-at (rx (| ?_ ?^) ?\( (| ?- ?+ alphanumeric)))
               cdtypst-simplify-sub-superscripts)
          ;; simplify sub/super script
          (progn (forward-char 1)
                 (delete-char 1)
                 (forward-char 1)
                 (delete-char 1))
        (forward-char 4))
      (unless (looking-at (rx (| ?_ ?^ ?\( ?\[)))
        (throw 'stop t)))
     ((char-equal (following-char) ?$)
      (forward-char)
      (throw 'stop t))
     ((char-equal (following-char) ?\ )
      ;; stop after first of many spaces
      (forward-char)
      (re-search-forward (rx (not ?\ )))
      (unless (char-equal (preceding-char) ?\n)
        (backward-char)))
     (t
      (forward-char)))

    (while (re-search-forward (rx (| ?\ ?\) ?\n ?\])) (point-max) t)
      (backward-char)
      (cond
       ((char-equal (following-char) ?\ )
        ;; stop at first space or b-o-l
        (unless (bolp)
          (forward-char))
        (throw 'stop t))
       ((char-equal (following-char) ?\n)
        ;; stop at line end, but not after \
        (if (and (bolp) (not (eobp)))
            (throw 'stop t)
          (if (char-equal (preceding-char) ?\\)
              (forward-char)
            (throw 'stop t))))
       (t
        ;; stop before )] if preceding-char is any parenthesis
        (if (or (char-equal (char-syntax (preceding-char)) ?\()
                (char-equal (char-syntax (preceding-char)) ?\))
                (char-equal (preceding-char) ?-))
            (throw 'stop t)
          (forward-char)
          (unless (looking-at (rx (| ?_ ?^ ?\( ?\[)))
            ;; stop after closing bracket, unless ^_[( follow
            (throw 'stop t))))))))

(defun cdtypst--apply-modifiers (args name)
  name)

(defun cdtypst--transient-parse-suffix (alist &optional insert-fn)
  (mapcar (pcase-lambda (`(,letter . ,name))
            (list (format "%c" letter)
                  (let ((unicode (assoc name typst-prettify-symbols-alist)))
                    (if unicode (format "%c" (cdr unicode)) name))
                  (lambda (args)
                    (interactive (list (transient-args 'cdtypst--math-symbol-letter)))
                    (cdtypst--ensure-math)
                    (funcall (or insert-fn #'insert)
                             (cdtypst--apply-modifiers args name)))))
          alist))

(defun cdtypst--math-symbol-letter-setup (_)
  (transient-parse-suffixes
   'cdtypst--math-symbol-letter
   (cdtypst--transient-parse-suffix cdtypst-math-symbol-letter-alist)))

(defun cdtypst--math-symbol-operator-setup (_)
  (transient-parse-suffixes
   'cdtypst--math-symbol-letter
   (cdtypst--transient-parse-suffix cdtypst-math-symbol-operator-alist)))

(defun cdtypst--math-symbol-entity-setup (_)
  (transient-parse-suffixes
   'cdtypst--math-symbol-entity
   (cdtypst--transient-parse-suffix cdtypst-math-symbol-entity-alist)))

(defun cdtypst--math-modify-insert (variant)
  (cond
   ((region-active-p)
    (pcase-let* ((`((,start . ,end) . ,_) (region-bounds))
                 (start-marker (copy-marker start))
                 (end-marker (copy-marker end)))
      (goto-char start-marker)
      (insert variant)
      (insert "(")
      (goto-char end-marker)
      (insert ")")))
   (t
    (insert variant)
    (insert "()")
    (backward-char))))

(defun cdtypst--math-modify-setup (_)
  (transient-parse-suffixes
   'cdtypst--math-modify
   (append (cdtypst--transient-parse-suffix cdtypst-math-modify-alist
                                            #'cdtypst--math-modify-insert)
           (list (list "'"
                       "literal"
                       (lambda ()
                         (interactive)
                         (insert "'")))))))

(transient-define-prefix cdtypst--math-symbol-entity ()
  ["Choose entity"
   :class transient-column
   :setup-children cdtypst--math-symbol-entity-setup])

(transient-define-prefix cdtypst--math-symbol-operator ()
  ["Choose operator"
   :class transient-column
   :setup-children cdtypst--math-symbol-operator-setup]
  [("`" "Entity" cdtypst--math-symbol-entity
    :transient transient--do-replace)])

(transient-define-prefix cdtypst--math-symbol-letter ()
  ["Modifiers"
   ("-u" "uppercase" "--uppercase")]
  ["Choose letter"
   :class transient-column
   :setup-children cdtypst--math-symbol-letter-setup]
  [("`" "Operator" cdtypst--math-symbol-operator
    :transient transient--do-replace)])

(defun cdtypst-math-symbol ()
  (interactive)
  (cdtypst--math-symbol-letter))

(transient-define-prefix cdtypst--math-modify ()
  ["Choose variant"
   :class transient-column
   :setup-children cdtypst--math-modify-setup])

(defun cdtypst-math-modify ()
  "Modify previous char/group with math variant.
If the character before point is white space, an empty variants call is
inserted and the cursor positioned properly."
  (interactive)
  (cdtypst--math-modify))

(defun cdtypst--check-all-symbols-defined (syms alist)
  "Make sure all names defined in `syms' are actually present in
`typst-prettify-symbols-alist'. `alist' is used for reporting an error
if a name is not found."
  (pcase-dolist (`(,_ . ,name) syms)
    (unless (assoc name typst-prettify-symbols-alist)
      (user-error "Symbol '%s' undefined (%s)" name alist))))

(defun turn-on-cdtypst ()
  (interactive)
  (unless (eq major-mode 'typst-ts-mode)
    (user-error "Cannot enable cdtypst in non-typst buffer."))
  (cdtypst--compute-symbols)
  (setq-local prettify-symbols-alist typst-prettify-symbols-alist)
  (cdtypst--check-all-symbols-defined cdtypst-math-symbol-letter-alist
                                      'letter)
  (cdtypst--check-all-symbols-defined cdtypst-math-symbol-operator-alist
                                      'operator)
  (setq-local prettify-symbols-compose-predicate #'cdtypst--prettify-symbols-compose-predicate)
  (prettify-symbols-mode))

(defun turn-off-cdtypst ()
  (interactive)
  (prettify-symbols-mode -1))

(defvar cdtypst-mode-map
  (let ((map (make-sparse-keymap)))
    (when cdtypst-takeover-sub-superscript
      (keymap-set map "^" #'cdtypst-sub-superscript)
      (keymap-set map "_" #'cdtypst-sub-superscript))
    (when cdtypst-takeover-dollar
      (keymap-set map "$" #'cdtypst-dollar))
    (keymap-set map "<tab>" #'cdtypst-tab)
    (keymap-set map "`" #'cdtypst-math-symbol)
    (keymap-set map "'" #'cdtypst-math-modify)
    map))

(define-minor-mode cdtypst-mode
  "Minor mode for editing Typst documents."
  :ligher "CDT"
  :keymap cdtypst-mode-map
  (if cdtypst-mode
      (turn-on-cdtypst)
    (turn-off-cdtypst)))

(provide 'cdtypst)
