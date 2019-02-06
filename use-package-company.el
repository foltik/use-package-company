;;; use-package-company.el --- :company keyword for use-package -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Jack Foltz
;;
;; Author: Jack Foltz <jack@foltz.io>
;; Maintainer: Jack Foltz <jack@foltz.io>
;; URL: http://github.com/foltik/use-package-company/
;; Created: 6 Feb 2019
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4"))
;; Keywords: convenience extensions use-package

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Adds the :company keyword, made available by requiring `use-package`,
;; which generates functions to add company backends and mode hooks.

;;; Code:

(require 'use-package-core)

(defcustom use-package-company-with-yas nil "Whether to add :with company-yasnippet to backends")

;;;###autoload
(defun use-package-normalize/:company (name keyword args)
  "Normalize :company definition to a list of pairs."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (consp arg) (use-package-non-nil-symbolp arg))
        (use-package-error
          (concat
            label
            "<symbol> or "
            "(<symbol or list of symbols> . <symbol or function>) or "
            "a list of these")))
      (use-package-normalize-pairs
        (lambda (k)
          (or (use-package-non-nil-symbolp k)
              (and (consp k)
                   (not (cdr (last k)))
                   (seq-every-p 'use-package-non-nil-symbolp k))))
        #'use-package-recognize-function
        name label arg))))

;;;###autoload
(defun use-package-handler/:company (name keyword args rest state)
  "Generate a function to add each backend and add hooks to the specified modes."
  (use-package-concat
    (use-package-process-keywords name rest state)
    (mapcan
      (lambda (def)
        (let ((modes (car def))
              (backend (cdr def))
              (fun (intern (concat "use-package-company-add-" (symbol-name (cdr def))))))
          (when backend
            (append
              `((defun ,fun ()
                (let ((backend ',
                        (if use-package-company-with-yas
                            (append (list backend) '(:with company-yasnippet))
                            backend)))
                  (unless (member backend company-backends)
                    (add-to-list 'company-backends backend)))))
              (mapcar
                (lambda (mode)
                  `(add-hook
                    ',(intern (concat (symbol-name mode) use-package-hook-name-suffix))
                    #',fun))
                (if (use-package-non-nil-symbolp modes) (list modes) modes))))))
        (use-package-normalize-commands args))))

(defalias 'use-package-autoloads/:company 'use-package-autoloads-mode)

(setq use-package-keywords
  (let ((idx (+ 1 (cl-position :hook use-package-keywords))))
    (append
      (seq-subseq use-package-keywords 0 idx)
      (list :company)
      (nthcdr idx use-package-keywords))))

(provide 'use-package-company)
;;; use-package-company.el ends here
