;;; org-ls.el --- Org Life Support

;; Copyright (C) 2017 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1
;; Package-Requires: (cl-lib f s)
;; Keywords: org babel
;; URL: http://github.com/dustinlacewell/org-ls

(require 'cl-lib)
(require 'f)
(require 's)

(defvar org-ls--support-file-name nil)

(defun org-ls--keyword-name (keyword)
  (s-chop-prefix ":" (symbol-name keyword)))

(defun org-ls--param-is-var? (param)
  (string= "var" (org-ls--keyword-name (first param))))
;; (org-ls--param-is-var? '(:var foo . "var"))
;; (org-ls--param-is-var? '(:name foo . "var"))

(require 'cl-lib)

(defun org-ls--get-var-params (params)
  (cl-remove-if-not 'org-ls--param-is-var? params))

;; (org-ls--get-val-props '((:foo bar . "baz") (:var app . "Google Chrome")))

(defun org-ls--get-var-param (vars keyword)
  (let ((name (org-ls--keyword-name keyword)))
    (seq-find
     (lambda (ele)
       (let* ((var-sym (second ele))
              (var-name (symbol-name var-sym)))
         (string= var-name name))) vars)))

(defun org-ls--var-name (param)
  (let* ((app-sym (second param)))
    (symbol-name app-sym)))

(defun org-ls--var-value (param)
  (cddr param))

(defun org-ls-load-file (file-name)
  (let ((expanded-name (expand-file-name file-name)))
    (org-babel-load-file expanded-name)
    (setq org-ls--support-file-name expanded-name)))

(defun org-ls-do (name &rest args)
  (with-temp-buffer
    (insert-file-contents org-ls--support-file-name)
    (org-babel-goto-named-src-block name)
    (let* ((info (org-babel-get-src-block-info))
           (params (third info))
           (vars (org-ls--get-var-params params)))
      (message "PRE: %s" info)
      (cl-loop for (key val) on args by #'cddr do
               (message (format "Key: %s" key))
               (message (format "Val: %s" val))
               (--when-let (org-ls--get-var-param vars key)
                 (let* ((var-name (org-ls--var-name it))
                        (var-sym (make-symbol var-name))
                        (new-cdr (cons var-sym val)))
                   (setcdr it new-cdr))))
      (message "POST: %s" info)
      (org-babel-execute-src-block nil info))))

(provide 'org-ls)
