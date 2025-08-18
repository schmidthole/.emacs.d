;;; claudia.el --- Claude Code emacs -*- lexical-binding: t -*-

;; Author: Your Name
;; Keywords: processes, terminals, ai, claude

;;; Commentary:

;; This is a comint-derived mode for interacting with Claude Code CLI.
;; It automatically handles conversation state and provides a shell-like
;; interface for chatting with Claude.

;;; Code:

(require 'comint)

;;; Customization

(defgroup claudia nil
  "Claude Code interactive mode."
  :group 'comint)

(defcustom claudia/claude-program "claude"
  "Claude Code CLI program to run."
  :type 'string
  :group 'claudia)

(defvar-local claudia/conversation-started nil
  "Whether a conversation has been started in this buffer.
Used to determine whether to use -c flag with claude.")

;;; Variables

(defvar claudia/mode-map
  (let ((map (make-sparse-keymap)))
    ;; Inherit from comint-mode-map
    (set-keymap-parent map comint-mode-map)
    ;; Add custom keybindings
    (define-key map (kbd "C-c C-l") 'claudia/clear-buffer)
    (define-key map (kbd "C-c C-d") 'claudia/send-eof)
    (define-key map (kbd "C-c C-c") 'comint-send-input)
    (define-key map (kbd "RET") 'newline)
    map)
  "Keymap for claudia mode.")

(defvar claudia/prompt-regexp "^USER > "
  "Regexp to match prompts in claude interactive mode.")

(defvar claudia/thinking-regexp "^\\[Thinking\.\.\.\\]"
  "Regexp to match thinking indicator.")

(defconst claudia/bold-regexp "\\*\\*\\([^*\n]+?\\)\\*\\*")

(defconst claudia/inline-code-regexp "\\(?:^\\|[^`]\\)\\(`\\([^`]*?\\)`\\)\\(?:[^`]\\|$\\)")

(defconst claudia/heading-regexp "^\\(#+[ \t]+.*\\)$")

(defun claudia/match-code-block (limit)
  "Match multiline code blocks up to LIMIT."
  (when (re-search-forward "^```\\(?:\\w+\\)?[ \t]*$" limit t)
    (let ((start (line-beginning-position)))
      (forward-line 1)
      (if (re-search-forward "^```[ \t]*$" nil t)
          (progn
            (put-text-property start (point) 'font-lock-multiline t)
            (set-match-data (list start (point)))
            t)
        (goto-char limit)
        nil))))

(defvar claudia/font-lock-keywords
  `((,claudia/prompt-regexp . 'comint-highlight-prompt)
    (,claudia/thinking-regexp . 'warning)
    (claudia/match-code-block . 'font-lock-string-face)
    (,claudia/bold-regexp 1 'font-lock-keyword-face)
    (,claudia/inline-code-regexp 2 'font-lock-string-face)  
    (,claudia/heading-regexp 1 'font-lock-function-name-face))
  "Font-lock keywords for claudia mode.")

;;;###autoload
(define-derived-mode claudia/mode comint-mode "Claude"
  "Major mode for interacting with Claude Code CLI.

  This mode is derived from `comint-mode' and provides an interactive
  interface for chatting with Claude."
  :group 'claudia
  :keymap claudia/mode-map

  (setq-local font-lock-defaults '((claudia/font-lock-keywords) nil nil nil nil (font-lock-multiline . t)))

  (font-lock-mode 1)

  (setq-local word-wrap t)
  (setq-local truncate-lines nil)
  (visual-line-mode 1)

  (setq comint-prompt-regexp claudia/prompt-regexp)

  (setq comint-prompt-read-only t)
  (setq comint-input-ignoredups t)

  (setq-local comint-completion-addsuffix t)
  (setq-local comint-completion-autolist t)

  (setq-local comint-input-ring-size 1000)
  (setq-local comint-input-ring-file-name "~/.claude_shell_history")

  (setq-local comint-process-echoes nil)
  (setq-local comint-eol-on-send nil)

  (setq comint-input-sender 'claudia/claude-input-sender)

  (setq claudia/conversation-started nil)

  (comint-read-input-ring t))

;;;###autoload
(defun claudia (&optional buffer)
  "Start a Claude Code interactive session in BUFFER.
  If BUFFER is nil, use a buffer named *claude*."
  (interactive)
  (let* ((buffer-name (or buffer "*claude*"))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (claudia/mode)
      ;; Create a dummy process to satisfy comint requirements
      (unless (comint-check-proc buffer)
        (let ((proc (start-process "claude-shell" buffer "sh" "-c" "while true; do read line; done")))
	  (set-process-filter proc 'claudia/process-filter)
	  (set-process-sentinel proc 'claudia/process-sentinel)
	  (goto-char (point-max))
	  (unless (bolp) (insert "\n"))
	  (insert "USER > ")
	  (set-marker (process-mark proc) (point)))))
    (switch-to-buffer buffer)))

(defun claudia/clear-buffer ()
  "Clear the claude buffer and reset conversation state."
  (interactive)
  (comint-clear-buffer)
  (setq claudia/conversation-started nil)
  (goto-char (point-max))
  (insert "USER > ")
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))

(defun claudia/send-eof ()
  "Send EOF to the claude process."
  (interactive)
  (comint-send-eof))

(defun claudia/remove-terminal-sequences (string)
  "Remove terminal control sequences from STRING."
  (replace-regexp-in-string "\e\\[\\??[0-9;]*[A-Za-z]" "" string))

;;; Claude-specific functions

(defun claudia/claude-input-sender (proc input)
  "Custom input sender for Claude Code CLI.
PROC is the process and INPUT is the user input string."
  (let* ((trimmed-input (string-trim input))
         (claude-args (if claudia/conversation-started
                          (list "--verbose" "-p" "-c" trimmed-input)
			(list "--verbose" "-p" trimmed-input)))
         (command (concat claudia/claude-program " " 
                          (mapconcat 'shell-quote-argument claude-args " "))))
    ;; Mark conversation as started after first prompt
    (setq claudia/conversation-started t)
    
    ;; Run claude command asynchronously and capture output
    (claudia/run-claude-async command)))

(defun claudia/process-output-to-json (output)
  "Convert claude OUTPUT to JSON objects, one per line."
  (mapcar 'claudia/parse-line-to-json 
          (split-string output "\n" t)))

(defun claudia/run-claude-async (command)
  "Run claude COMMAND asynchronously and insert output."
  (let ((target-buffer (current-buffer)))
    ;; Show "thinking" indicator
    (with-current-buffer target-buffer
      (goto-char (point-max))
      (insert "\n[Thinking...]\n"))
    
    ;; Run claude command
    (let ((proc (start-process "claude-cmd" nil "sh" "-c" command)))
      (set-process-filter proc 
			  (lambda (process output)
			    (let* ((sanitized-output (claudia/remove-terminal-sequences output))
				   (parsed-output ()))
			      (when (buffer-live-p target-buffer)
				(with-current-buffer target-buffer
				  (save-excursion
				    ;; Remove "thinking" indicator if it exists
				    (goto-char (point-max))
				    (when (search-backward "[Thinking...]\n" nil t)
				      (delete-region (match-beginning 0) (match-end 0))))
				  (goto-char (point-max))
				  (insert sanitized-output))))))
      
      (set-process-sentinel proc
			    (lambda (process event)
			      (when (buffer-live-p target-buffer)
				(with-current-buffer target-buffer
				  (goto-char (point-max))
				  (unless (bolp) (insert "\n\n"))
				  (insert "\nUSER > ")
				  (set-marker (process-mark (get-buffer-process target-buffer)) (point)))))))))

(defun claudia/process-filter (proc string)
  "Custom process filter that does nothing.
We handle all output through our async command system."
  ;; Do nothing - we don't want the dummy process output
  nil)

;;; Input filter functions

(defun claudia/input-filter (input)
  "Filter function for claude input.
Only save non-empty inputs to history."
  (not (string-match "\\`\\s *\\'" input)))

;;; Process sentinel

(defun claudia/process-sentinel (process event)
  "Sentinel function for the dummy shell process."
  (when (string-match "\\(finished\\|exited\\)" event)
    (message "Claude shell process has finished")))

;;; Hooks

(defvar claudia/mode-hook nil
  "Hook run when entering Claude shell mode.")

(provide 'claudia)

;;; my-shell.el ends here
