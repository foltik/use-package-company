# use-package-company

The `:company` keyword allows adding 
[company](https://github.com/company-mode/company-mode) backends triggered
by hooks. Only the basename of the hook is required. The following are equivalent:

```lisp
(use-package company-jedi
  :company python-mode)
  
(use-package company-jedi
  :company (python-mode . company-jedi))
  
(use-package company-jedi
  :commands company-jedi
  :init
  (defun use-package-company-add-company-jedi ()
    (unless (member 'company-jedi company-backends)
	  (add-to-list 'company-backends 'company-jedi)))
  (add-hook 'python-mode-hook #'use-package-company-add-company-jedi))
```

Or when `use-package-company-with-yas` is set to a non-nil value, 

```lisp
(use-package company-jedi
  :commands company-jedi
  :init
  (defun use-package-company-add-company-jedi ()
    (let ((backend '(company-jedi :with company-yasnippet)))
      (unless (member backend company-backends)
	    (add-to-list 'company-backends backend))))
  (add-hook 'python-mode-hook #'use-package-company-add-company-jedi))
```

With multiple hooks, the following are also equivalent:

```lisp
(use-package company-jedi
  :company (python-mode other-mode)
  
(use-package company-jedi
  :company ((python-mode other-mode) . company-jedi))

(use-package company-jedi
  :company ((python-mode . company-jedi)
            (other-mode  . company-jedi)))
  
(use-package company-jedi
  :commands company-jedi
  :init
  (defun use-package-company-add-company-jedi ()
    (unless (member 'company-jedi company-backends)
	  (add-to-list 'company-backends 'company-jedi)))
  (add-hook 'python-mode-hook #'use-package-company-add-company-jedi))
  (add-hook 'other-mode-hook  #'use-package-company-add-company-jedi))
```

The use of `:company` causes the functions being hooked to implicitly be read
as `:commands`, meaning interactive `autoload` definitions will be established for the
module, if not already defined as functions. In this case, `:defer t` is implied.
