;;; mdlite.el --- minimal markdown major mode with font locking -*- lexical-binding: t; -*-

;;; Commentary:
;; minimal markdown major mode providing font locking for basic markdown syntax

;;; Code:

(defgroup mdlite nil
  "minimal markdown mode"
  :group 'text)

(defface mdlite-header-face-1
  '((t :inherit outline-1))
  "face for level 1 headers"
  :group 'mdlite)

(defface mdlite-header-face-2
  '((t :inherit outline-2))
  "face for level 2 headers"
  :group 'mdlite)

(defface mdlite-header-face-3
  '((t :inherit outline-3))
  "face for level 3 headers"
  :group 'mdlite)

(defface mdlite-header-face-4
  '((t :inherit outline-4))
  "face for level 4 headers"
  :group 'mdlite)

(defface mdlite-header-face-5
  '((t :inherit outline-5))
  "face for level 5 headers"
  :group 'mdlite)

(defface mdlite-header-face-6
  '((t :inherit outline-6))
  "face for level 6 headers"
  :group 'mdlite)

(defface mdlite-bold-face
  '((t :weight bold))
  "face for bold text"
  :group 'mdlite)

(defface mdlite-italic-face
  '((t :slant italic))
  "face for italic text"
  :group 'mdlite)

(defface mdlite-code-face
  '((t :inherit shadow))
  "face for inline code"
  :group 'mdlite)

(defface mdlite-pre-face
  '((t :inherit secondary-selection))
  "face for code blocks"
  :group 'mdlite)

(defface mdlite-table-face
  '((t :inherit highlight))
  "face for tables"
  :group 'mdlite)

(defface mdlite-link-face
  '((t :inherit link))
  "face for links"
  :group 'mdlite)

(defconst mdlite-regex-header-atx
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "regex for atx-style headers")

(defconst mdlite-regex-bold
  "\\(?1:^\\|[^\\\\]\\)\\(?2:\\(?3:\\*\\*\\|__\\)\\(?4:[^ \n\t\\\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\\\ ]\\)\\(?5:\\3\\)\\)"
  "regex for bold text")

(defconst mdlite-regex-italic
  "\\(?:^\\|[^\\\\]\\)\\(?1:\\(?2:[*_]\\)\\(?3:[^ \n\t\\\\]\\|[^ \n\t*]\\(?:.\\|\n[^\n]\\)*?[^\\\\ ]\\)\\(?4:\\2\\)\\)"
  "regex for italic text")

(defconst mdlite-regex-code
  "\\(?:\\`\\|[^\\\\]\\)\\(?1:\\(?2:`+\\)\\(?3:\\(?:.\\|\n[^\n]\\)*?[^`]\\)\\(?4:\\2\\)\\)\\(?:[^`]\\|\\'\\)"
  "regex for inline code")

(defconst mdlite-regex-code-block-fenced
  "^[ \t]*\\(?1:```\\)\\(?2:[^`\n]*\\)?$\\(?3:\\(?:.\\|\n\\)*?\\)^[ \t]*\\(?4:```\\)[ \t]*$"
  "regex for fenced code blocks")

(defconst mdlite-regex-table
  "^[ \t]*|.*|[ \t]*$"
  "regex for table rows")

(defconst mdlite-regex-link-inline
  "\\(!\\)?\\[\\(\\(?:[^]\\\\]\\|\\\\.\\)+\\)\\](\\([^)]*\\))"
  "regex for inline links")

(defvar mdlite-font-lock-keywords
  `((,mdlite-regex-header-atx
     (1 'shadow)
     (2 (cond ((= (length (match-string 1)) 1) 'mdlite-header-face-1)
              ((= (length (match-string 1)) 2) 'mdlite-header-face-2)
              ((= (length (match-string 1)) 3) 'mdlite-header-face-3)
              ((= (length (match-string 1)) 4) 'mdlite-header-face-4)
              ((= (length (match-string 1)) 5) 'mdlite-header-face-5)
              (t 'mdlite-header-face-6)))
     (3 'shadow))
    (,mdlite-regex-bold
     (1 'shadow)
     (4 'mdlite-bold-face)
     (3 'shadow)
     (5 'shadow))
    (,mdlite-regex-italic
     (1 'shadow)
     (3 'mdlite-italic-face)
     (2 'shadow)
     (4 'shadow))
    (,mdlite-regex-code
     (1 'mdlite-code-face))
    (,mdlite-regex-code-block-fenced
     (1 'shadow)
     (2 'shadow)
     (3 'mdlite-pre-face)
     (4 'shadow))
    (,mdlite-regex-table
     (0 'mdlite-table-face))
    (,mdlite-regex-link-inline
     (0 'mdlite-link-face)))
  "font lock keywords for mdlite mode")

;;;###autoload
(define-derived-mode mdlite-mode text-mode "mdlite"
  "minimal markdown mode with font locking"
  (setq font-lock-defaults '(mdlite-font-lock-keywords))
  (setq font-lock-multiline t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.md\\'" . mdlite-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . mdlite-mode))

(provide 'mdlite)

;;; mdlite.el ends here
