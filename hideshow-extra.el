;;; hideshow-extra.el --- Additional hideshow commands -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/hideshow-extra
;; Version: 0.1.0
;; Keywords: lisp tools editing comments blocks hiding outlines
;; Package-Requires: ((emacs "25.1"))

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

;; Additional hideshow commands and minor mode.

;;; Minor mode

;; `hideshow-extra-mode'
;;      Minor mode to selectively hide/show code and comment blocks.
;;      The main commands are: `hideshow-extra-toggle-or-indent' and
;;      `hideshow-extra-toggle-all'.
;;      Key bindings:
;;      \{hideshow-extra-mode-map}.

;;; Keymaps

;; `hideshow-extra-mode-map'
;;      Keymap for `hideshow-extra-mode'.

;;; Commands

;; M-x `hideshow-extra-toggle-or-indent'
;;      Toggle hiding/showing a of block or invoke `indent-for-tab-command'.
;;      Inside string or comments call `indent-for-tab-command', otherwise
;;      activate `hs-minor-mode' and toggle hiding.

;; M-x `hideshow-extra-toggle-all'
;;      Activate `hs-minor-mode' and toggle folding for all nodes.

;;; Customization

;; `hideshow-extra-hide-initial-comment-block-condition'
;;      Function to enable or disable `hs-hide-initial-comment-block'.
;;      It is called without arguments. If returned value is non-nil,
;;      hide initial block silencly, otherwise - do nothing.


;;; Code:

(require 'hideshow)

(defcustom hideshow-extra-hide-initial-comment-block-condition 'buffer-file-name
  "Function to enable or disable `hs-hide-initial-comment-block'.
It is called without arguments. If returned value is non-nil,
hide initial block silencly, otherwise - do nothing."
  :type 'function
  :type '(radio
          (function :tag "Function" buffer-file-name)
          (const :tag "Always hide" nil))
  :group 'hideshow)


(defvar hideshow-extra-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(tab)]
                #'hideshow-extra-toggle-or-indent)
    (define-key map (kbd "TAB")
                #'hideshow-extra-toggle-or-indent)
    (define-key map [(shift tab)]
                'hideshow-extra-toggle-all)
    (define-key map [backtab]
                'hideshow-extra-toggle-all)
    map)
  "Keymap for `hideshow-extra-mode'.")

(defvar-local hideshow-extra-toggle-all-state nil)

;;;###autoload
(defun hideshow-extra-toggle-all ()
  "Activate `hs-minor-mode' and toggle folding for all nodes."
  (interactive)
  (unless hs-minor-mode
    (hs-minor-mode 1))
  (setq hideshow-extra-toggle-all-state (not hideshow-extra-toggle-all-state))
  (if hideshow-extra-toggle-all-state
      (hs-hide-all)
    (hs-show-all)))

;;;###autoload
(defun hideshow-extra-hide-initial-comment-block ()
  "Hide initial comment block without message."
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
  (let ((stx (syntax-ppss (point))))
    (cond ((or (nth 3 stx)
               (nth 4 stx)
               (looking-at "[\s\t]")
               (and (looking-at (regexp-quote comment-start))
                    (looking-back "\n" 0)))
           (indent-for-tab-command))
          (t
           (unless (bound-and-true-p hs-minor-mode)
             (hs-minor-mode 1))
           (funcall-interactively #'hs-toggle-hiding)))))

;;;###autoload
(define-minor-mode hideshow-extra-mode
  "Minor mode to selectively hide/show code and comment blocks.
The main commands are: `hideshow-extra-toggle-or-indent' and
`hideshow-extra-toggle-all'.
Key bindings:
\\{hideshow-extra-mode-map}."
  :group 'hideshow
  :lighter " hs+"
  :keymap hideshow-extra-mode-map
  :global nil)

(provide 'hideshow-extra)
;;; hideshow-extra.el ends here