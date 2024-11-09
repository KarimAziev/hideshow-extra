;;; hideshow-extra.el --- Additional commands for hs-minor-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/hideshow-extra
;; Version: 0.2.0
;; Keywords: lisp tools editing comments blocks hiding outlines
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Additional commands for `hs-minor-mode'

;;; Minor mode

;; `hideshow-extra-mode'
;;      Minor mode with additional commands for `hs-minor-mode'.

;;; Commands

;; M-x `hideshow-extra-cycle'
;;      Cycle commands from `hideshow-extra-toggle-all-modes'.

;; M-x `hideshow-extra-narrow-to-docs'
;;      Hide all blocks in except documentation string.

;; M-x `hideshow-extra-toggle-or-indent'
;;      Toggle hiding/showing a of block or invoke `indent-for-tab-command'.
;;      Inside string or comments call `indent-for-tab-command', otherwise
;;      activate `hs-minor-mode' and toggle hiding.

;; M-x `hideshow-extra-toggle-all-down'
;;      Toggle folding for nodes after point.

;; M-x `hideshow-extra-toggle-all-up'
;;      Toggle folding for nodes before point.

;; M-x `hideshow-extra-toggle-all-keep-current'
;;      Toggle folding for all nodes except current.

;; M-x `hideshow-extra-toggle-all'
;;      Toggle folding for all nodes.

;;; Customization

;; `hideshow-extra-recenter-after-toggle'
;;      Whether to recenter window after toggling.

;; `hideshow-extra-cycle-commands'
;;      List of functions to cycle with command `hideshow-extra-cycle'.

;; `hideshow-extra-hide-initial-comment-block-condition'
;;      Function to enable or disable `hs-hide-initial-comment-block'.
;;      It is called without arguments. If returned value is non-nil,
;;      hide initial block silently, otherwise - do nothing.

;;; Code:

(require 'hideshow)

(defcustom hideshow-extra-hide-initial-comment-block-condition 'buffer-file-name
  "Function to enable or disable `hs-hide-initial-comment-block'.
It is called without arguments. If returned value is non-nil,
hide initial block silently, otherwise - do nothing."
  :type '(radio
          (function :tag "Function" buffer-file-name)
          (const :tag "Always hide" nil))
  :group 'hideshow)

(defcustom hideshow-extra-cycle-commands '(hideshow-extra-toggle-all-keep-current
                                           hideshow-extra-narrow-to-docs
                                           hideshow-extra-toggle-all-keep-current)
  "List of functions to cycle with command `hideshow-extra-cycle'."
  :group 'autofix
  :type '(repeat
          (radio
           (function-item :tag "Toggle folding for all nodes except current"
                          hideshow-extra-toggle-all-keep-current)
           (function-item :tag
                          "Hide all blocks in except documentation string"
                          hideshow-extra-narrow-to-docs)
           (function-item :tag "Toggle folding for all nodes"
                          hideshow-extra-toggle-all)
           (function-item :tag "Toggle folding for nodes after point"
                          hideshow-extra-toggle-all-down)
           (function-item :tag "Toggle folding for nodes before point"
                          hideshow-extra-toggle-all-up)
           (function :tag "Custom function"))))

(defcustom hideshow-extra-recenter-after-toggle t
  "Whether to recenter window after toggling."
  :type 'boolean
  :group 'hideshow)

(defvar hideshow-extra-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]
                #'hideshow-extra-toggle-or-indent)
    (define-key map (kbd "TAB")
                #'hideshow-extra-toggle-or-indent)
    (define-key map [(shift tab)]
                'hideshow-extra-cycle)
    (define-key map [backtab]
                'hideshow-extra-cycle)
    map)
  "Keymap for `hideshow-extra-mode'.")

(defvar-local hideshow-extra-toggle-all-state nil)


(defvar-local hideshow-extra-mode-idx 0)

(defun hideshow-extra-index-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))))

(defun hideshow-extra-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

;;;###autoload
(defun hideshow-extra-cycle ()
  "Cycle commands from `hideshow-extra-toggle-all-modes'."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (setq hideshow-extra-mode-idx (hideshow-extra-index-switcher
                                 1
                                 hideshow-extra-mode-idx
                                 hideshow-extra-cycle-commands))
  (funcall (nth hideshow-extra-mode-idx hideshow-extra-cycle-commands)))

;;;###autoload
(defun hideshow-extra-toggle-all ()
  "Toggle folding for all nodes."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (setq hideshow-extra-toggle-all-state (not hideshow-extra-toggle-all-state))
  (save-excursion
    (if hideshow-extra-toggle-all-state
        (hs-hide-all)
      (hs-show-all))))

;;;###autoload
(defun hideshow-extra-toggle-all-keep-current ()
  "Toggle folding for all nodes except current."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (setq hideshow-extra-toggle-all-state (not hideshow-extra-toggle-all-state))
  (hideshow-extra-toggle-all-up)
  (hideshow-extra-toggle-all-down)
  (when hideshow-extra-recenter-after-toggle
    (recenter)))

;;;###autoload
(defun hideshow-extra-toggle-all-up ()
  "Toggle folding for nodes before point."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (let ((toggled)
        (fn (cond ((called-interactively-p 'any)
                   'hs-toggle-hiding)
                  (hideshow-extra-toggle-all-state 'hs-show-block)
                  (t 'hs-hide-block))))
    (save-excursion
      (save-restriction
        (widen)
        (ignore-errors (backward-up-list (car (syntax-ppss (point)))))
        (while (and
                (hideshow-extra-backward-list)
                (hs-looking-at-block-start-p))
          (save-excursion
            (funcall fn)
            (setq toggled t)))))
    (when (and toggled
               hideshow-extra-recenter-after-toggle)
      (recenter))))

;;;###autoload
(defun hideshow-extra-toggle-all-down ()
  "Toggle folding for nodes after point."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (let ((toggled)
        (fn (cond ((called-interactively-p 'any)
                   'hs-toggle-hiding)
                  (hideshow-extra-toggle-all-state 'hs-show-block)
                  (t 'hs-hide-block))))
    (save-excursion
      (save-restriction
        (widen)
        (let ((depth (car (syntax-ppss (point)))))
          (when (> depth 0)
            (ignore-errors (backward-up-list depth)
                           (forward-sexp 2)
                           (forward-sexp -1)))
          (let ((pos (point)))
            (goto-char (point-max))
            (while (and
                    (> (point) pos)
                    (hideshow-extra-backward-list)
                    (hs-looking-at-block-start-p))
              (save-excursion
                (funcall fn)
                (setq toggled t)))))))
    (when (and toggled
               hideshow-extra-recenter-after-toggle)
      (recenter))))

;;;###autoload
(defun hideshow-extra-show-initial-comment-block ()
  "Hide initial comment block without message."
  (unless hs-minor-mode
    (let ((inhibit-message t))
      (hs-minor-mode 1)))
  (let ((inhibit-message t))
    (save-excursion
      (let ((c-reg (save-excursion
                     (goto-char (point-min))
                     (skip-chars-forward " \t\n\f")
                     (hs-inside-comment-p))))
        (when c-reg
          (hs-show-block))))))

;;;###autoload
(defun hideshow-extra-hide-initial-comment-block ()
  "Hide initial comment block without message."
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (when (and (functionp hideshow-extra-hide-initial-comment-block-condition)
             (funcall hideshow-extra-hide-initial-comment-block-condition))
    (unless hs-minor-mode
      (let ((inhibit-message t))
        (hs-minor-mode 1)))
    (let ((inhibit-message t))
      (save-excursion
        (hs-hide-initial-comment-block)))))

;;;###autoload
(defun hideshow-extra-toggle-or-indent ()
  "Toggle hiding/showing a of block or invoke `indent-for-tab-command'.
Inside string or comments call `indent-for-tab-command', otherwise
activate `hs-minor-mode' and toggle hiding."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (let ((stx (syntax-ppss (point))))
    (cond ((or (nth 3 stx)
               (nth 4 stx)
               (looking-at "[\s\t\n]")
               (and (looking-at (regexp-quote comment-start))
                    (looking-back "\n" 0)))
           (indent-for-tab-command))
          (t
           (unless (bound-and-true-p hs-minor-mode)
             (hs-minor-mode 1))
           (save-excursion
             (funcall-interactively #'hs-toggle-hiding))))))

(defun hideshow-extra-narrow-to-doc ()
  "Hide all blocks in list at point except documentation string."
  (save-excursion
    (let ((doc-pos))
      (while (and (not (setq doc-pos (pcase (save-excursion
                                              (ignore-errors (down-list)
                                                             (symbol-at-point)))
                                       ('define-skeleton  2)
                                       ('ert-deftest  3)
                                       ('define-widget  3)
                                       ('easy-mmode-define-minor-mode  2)
                                       ('defclass 4)
                                       ('cl-defstruct  3)
                                       ('defvar 3)
                                       ('defconst  3)
                                       ('defvar-local 3)
                                       ('defface  3)
                                       ('defcustom 3)
                                       ('defgroup 3)
                                       ('deftheme  3)
                                       ('defun 3)
                                       ('defmacro 3)
                                       ('defsubst  3)
                                       ('defalias  4)
                                       ('defhydra 3)
                                       ('transient-define-prefix  3)
                                       ('transient-define-suffix  3)
                                       ('transient-define-argument  3)
                                       ('transient-define-infix  3)
                                       ('cl-defun 3)
                                       ('cl-defsubst  3)
                                       ('cl-defmacro  3)
                                       ('cl-defgeneric  3)
                                       ('cl-defmethod 3)
                                       ('define-minor-mode 2)
                                       ('define-derived-mode  4)
                                       ('define-generic-mode  8)
                                       ('define-compilation-mode  3))))
                  (when-let* ((list-start (nth 1 (syntax-ppss (point)))))
                    (goto-char list-start)
                    t)))
      (when-let* ((beg (and doc-pos (point)))
                  (doc-end (ignore-errors
                             (down-list)
                             (forward-sexp (1+ doc-pos))
                             (save-excursion
                               (dotimes (_i (1+ doc-pos))
                                 (forward-sexp -1)
                                 (when
                                     (hs-looking-at-block-start-p)
                                   (save-excursion
                                     (hs-hide-block)))))
                             (when (nth 3 (syntax-ppss (1- (point))))
                               (forward-char 1)
                               (point)))))
        (goto-char beg)
        (forward-sexp)
        (forward-char -1)
        (while (and
                (> (point) doc-end)
                (hideshow-extra-backward-list)
                (hs-looking-at-block-start-p))
          (save-excursion
            (hs-hide-block)))))))

;;;###autoload
(defun hideshow-extra-narrow-to-docs ()
  "Hide all blocks in except documentation string."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (let ((toggled)
        (fn (cond ((called-interactively-p 'any)
                   'hideshow-extra-narrow-to-doc)
                  (hideshow-extra-toggle-all-state
                   'hideshow-extra-narrow-to-doc)
                  (t 'hideshow-extra-narrow-to-doc))))
    (funcall fn)
    (save-excursion
      (save-restriction
        (widen)
        (ignore-errors (backward-up-list (car (syntax-ppss (point)))))
        (goto-char (point-max))
        (while (and
                (hideshow-extra-backward-list)
                (hs-looking-at-block-start-p))
          (save-excursion
            (funcall fn)
            (setq toggled t)))))
    (when (and toggled
               hideshow-extra-recenter-after-toggle)
      (recenter))))

;;;###autoload
(define-minor-mode hideshow-extra-mode
  "Minor mode with additional commands for `hs-minor-mode'.
Key bindings:
\\{hideshow-extra-mode-map}."
  :group 'hideshow
  :lighter " hs+"
  :keymap hideshow-extra-mode-map
  :global nil
  (if hideshow-extra-mode
      (easy-menu-add-item
       hs-minor-mode-menu nil '("Hide/show extra"
                                ["Toggle all except current"
                                 hideshow-extra-toggle-all-keep-current
                                 t]
                                ["Toggle or indent"
                                 hideshow-extra-toggle-or-indent
                                 t]
                                ["Toggle all before"
                                 hideshow-extra-toggle-all-up t]
                                ["Toggle all down"
                                 hideshow-extra-toggle-all-down t]
                                ["Toggle all"
                                 hideshow-extra-toggle-all t]))
    (easy-menu-remove-item
     hs-minor-mode-menu nil '("Hide/show extra"
                              ["Toggle all except current"
                               hideshow-extra-toggle-all-keep-current
                               t]
                              ["Toggle or indent"
                               hideshow-extra-toggle-or-indent
                               t]
                              ["Toggle all before"
                               hideshow-extra-toggle-all-up t]
                              ["Toggle all down"
                               hideshow-extra-toggle-all-down t]
                              ["Toggle all"
                               hideshow-extra-toggle-all t]))))

(provide 'hideshow-extra)
;;; hideshow-extra.el ends here