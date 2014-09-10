;;; highlight-stages.el --- highlight staged (quasi-quoted) expressions

;; Copyright (C) 2014 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.0

;;; Commentary:

;; Require this script and call function
;; "highlight-stages-lisp-initialize".
;;
;;   (require 'highlight-stages)
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-stages-lisp-initialize)

;; For more informations, see Readme.

;;; Change Log:

;; 1.0.0 first released

;;; Code:

(require 'color)

(defconst highlight-stages-version "1.0.0")

;; + customs

(defgroup highlight-stages nil
  "highlight staged (quasi-quoted) expressions"
  :group 'emacs)

(defcustom highlight-stages-lighten-step -2.5
  "Staged expressions get STEP percent lighter."
  :group 'highlight-stages)

(defcustom highlight-stages-highlight-real-quote t
  "If non-nil, \"real\" (not escapable) quotes are also
  highlighted."
  :group 'highlight-stages)

(defcustom highlight-stages-highlight-priority 1
  "Priority which highlight overlays get."
  :group 'highlight-stages)

;; + vars

(defvar highlight-stages-quote-matcher nil
  "A function with 1 parameter, LIMIT, which searches the next
  quoted expression. The function must return non-nil if
  succeeded, or nil otherwise. A special value 'real also can be
  returned by the function, when the quote is \"real\" (not
  escapable) quote. This may be useful for lisp-like
  languages. When the function returns non-nil, (match-string 0)
  must be the expression matched.")

(defvar highlight-stages-escape-matcher nil
  "A function with 1 parameter, LIMIT, which searches the next
  escaped expression. The function must return non-nil if
  succeeded, or nil otherwise. When the function returns
  non-nil, (match-string 0) must be the expression matched.")

;; + utils

(defun highlight-stages--face (level)
  "Make a face suitable for λf.(overlay-put ov 'face f)."
  (let ((cache (make-hash-table :test 'eql)))
    (or (gethash level cache)
        (puthash level
                 (cons 'background-color
                       (color-lighten-name
                        (face-background 'default)
                        (* level highlight-stages-lighten-step)))
                 cache))))

(defun highlight-stages--make-overlay (beg end level)
  "Make a overlay. Trims existing overlays if necessary."
  (dolist (ov (overlays-in beg end))
    (when (eq (overlay-get ov 'category) 'highlight-stages)
      (let ((ov-beg (overlay-start ov))
            (ov-end (overlay-end ov)))
        (cond ((and (< ov-beg beg) (< end ov-end))
               (move-overlay ov ov-beg beg)
               (move-overlay (copy-overlay ov) end ov-end))
              ((< ov-beg beg)
               (move-overlay ov ov-beg beg))
              ((< end ov-beg)
               (move-overlay ov end ov-end))
              (t
               (delete-overlay ov))))))
  ;; we don't need to make an overlay if (level = 0)
  (unless (zerop level)
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face (highlight-stages--face level))
      (overlay-put ov 'category 'highlight-stages)
      (overlay-put ov 'priority highlight-stages-highlight-priority))))

;; + the highlighter

(defun highlight-stages-jit-highlighter (beg end)
  "The jit highlighter of highlight-stages."
  (setq beg (progn (goto-char beg)
                   (beginning-of-defun)
                   (skip-syntax-backward "'-") ; skip newlines?
                   (point))
        end (progn (goto-char end)
                   (end-of-defun)
                   (skip-syntax-forward "'-") ; skip newlines?
                   (point)))
  (remove-overlays beg end 'category 'highlight-stages)
  (highlight-stages--jit-highlighter-1 beg end 0))

(defun highlight-stages--jit-highlighter-1 (beg end base-level)
  "Scan and highlight this level."
  (highlight-stages--make-overlay beg end base-level)
  (goto-char beg)
  (let (quote escape)
    (while (progn
             (setq quote (save-excursion
                           ;; 'real means "real" (non-"quasi") quote
                           (let ((res (funcall highlight-stages-quote-matcher end)))
                             (cond ((eq res 'real)
                                    (cons (match-beginning 0) (cons (match-end 0) t)))
                                   (res
                                    (list (match-beginning 0) (match-end 0))))))
                   escape (save-excursion
                            (when (funcall highlight-stages-escape-matcher end)
                              (list (match-beginning 0) (match-end 0)))))
             (or quote escape))
      (cond ((or (null escape)
                 (and quote (< (car quote) (car escape))))
             (save-excursion
               (cond ((not (cddr quote))
                      ;; "quasi"-quote -> a staging operator (increment level)
                      (highlight-stages--jit-highlighter-1
                       (car quote) (cadr quote) (1+ base-level)))
                     ((not (zerop base-level))
                      ;; "real"-quote inside "quasi"-quote -> an ordinary symbol
                      (highlight-stages--jit-highlighter-1
                       (car quote) (cadr quote) base-level))
                     (t
                      ;; "real"-quote outside "quasi"-quote
                      (when highlight-stages-highlight-real-quote
                        (highlight-stages--make-overlay (car quote) (cadr quote) 1)))))
             (goto-char (cadr quote)))
            (t
             (save-excursion
               (highlight-stages--jit-highlighter-1
                (car escape) (cadr escape) (1- base-level)))
             (goto-char (cadr escape)))))))

;; + settings for lisp

(defun highlight-stages--lisp-quote-matcher (&optional limit)
  (let ((original-pos (point)) syntax realp res)
    (when (search-forward-regexp "`\\|\\(#?'\\)" limit t)
      (setq realp (match-beginning 1))  ; must be done before (syntax-ppss)
      (setq syntax (syntax-ppss))
      (cond ((and (not (nth 3 syntax))  ; NOT in string
                  (not (nth 4 syntax))) ; NOT in comment
             (let ((beg (point))
                   (end (progn (ignore-errors (forward-sexp 1))
                               (point))))
               (set-match-data (list beg end))
               ;; "real" quote or "quasi" quote
               (if realp 'real t)))
            ((setq res (highlight-stages--lisp-quote-matcher limit))
             res)
            (t                          ; not found
             (goto-char original-pos)
             nil)))))

(defun highlight-stages--lisp-escape-matcher (&optional limit)
  (let ((original-pos (point)) syntax res)
    (when (search-forward-regexp ",@?" limit t)
      (setq syntax (syntax-ppss))
      (cond ((and (not (nth 3 syntax))  ; NOT in string
                  (not (nth 4 syntax))) ; NOT in comment
             (let ((beg (point))
                   (end (progn (ignore-errors (forward-sexp 1))
                               (point))))
               (set-match-data (list beg end))
               t))
            ((setq res (highlight-stages--lisp-escape-matcher limit))
             res)
            (t
             (goto-char original-pos)
             nil)))))

;;;###autoload
(defun highlight-stages-lisp-initialize ()
  "Setup highlight-stages for Lisp buffers."
  (interactive)
  (jit-lock-mode 1)
  (set (make-local-variable 'highlight-stages-quote-matcher)
       'highlight-stages--lisp-quote-matcher)
  (set (make-local-variable 'highlight-stages-escape-matcher)
       'highlight-stages--lisp-escape-matcher)
  (jit-lock-register 'highlight-stages-jit-highlighter))

;; + provide

(provide 'highlight-stages)

;;; highlight-stages.el ends here
