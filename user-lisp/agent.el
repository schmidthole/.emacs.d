;;; agent.el --- project agent terminals -*- lexical-binding: t; -*-

(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function ghostel-exec "ghostel" (buffer program &optional args identity))
(defvar ghostel-notification-function)
(defvar ghostel-exit-functions)
(defvar ghostel-kill-buffer-on-exit)

(defgroup agent nil
  "project agent terminals."
  :group 'terminals)

(defcustom agent-program 'codex
  "default program offered when creating a new agent."
  :type '(choice (const codex) (const claude) (const pi))
  :group 'agent)

(defvar-local agent-project-root nil
  "project root assigned when this agent was created.")
(defvar-local agent-project-name nil
  "project name assigned when this agent was created.")
(defvar-local agent-attention nil
  "non-nil when this agent has an unacknowledged notification.")
(defvar agent-switch-history nil
  "minibuffer history for switching agents.")

(defvar-keymap agent-mode-map
  "M-i a n" #'agent-new
  "M-i a a" #'agent-switch
  "M-i a r" #'agent-rename)

(defun agent--notify (&rest _args)
  "mark the current agent for attention and ring an audible bell once."
  (unless agent-attention
    (setq agent-attention t)
    (force-mode-line-update t)
    (let ((ring-bell-function nil)
          (visible-bell nil))
      (ding))))

(defun agent--exited (buffer _event)
  "request attention when the terminal in buffer exits."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (agent--notify))))

(defun agent--acknowledge ()
  "acknowledge notifications when interacting with this agent."
  (when agent-attention
    (setq agent-attention nil)
    (force-mode-line-update t)))

(define-minor-mode agent-mode
  "identify a ghostel buffer as a project agent."
  :lighter (:eval (if agent-attention " agent!" " agent"))
  (when (and agent-mode (not (derived-mode-p 'ghostel-mode)))
    (setq agent-mode nil)
    (user-error "agent mode requires a ghostel buffer"))
  (if agent-mode
      (progn
        (setq-local ghostel-notification-function #'agent--notify
                    ghostel-kill-buffer-on-exit nil)
        (add-hook 'ghostel-exit-functions #'agent--exited nil t)
        (add-hook 'post-command-hook #'agent--acknowledge nil t))
    (kill-local-variable 'ghostel-notification-function)
    (kill-local-variable 'ghostel-kill-buffer-on-exit)
    (remove-hook 'ghostel-exit-functions #'agent--exited t)
    (remove-hook 'post-command-hook #'agent--acknowledge t)
    (setq agent-attention nil)))

;;;###autoload
(defun agent-new (&optional program)
  "create a fresh ghostel agent named for the current project.
prompt for program interactively; otherwise default to `agent-program'."
  (interactive
   (list (intern (completing-read "agent program: " '("claude" "codex" "pi")
                                  nil t nil nil (symbol-name agent-program)))))
  (setq program (or program agent-program))
  (let* ((project (project-current t))
         (root (project-root project))
         (name (project-name project))
         (default-directory root)
         (shell (or (getenv "SHELL") shell-file-name))
         (command
          (pcase program
            ('codex '("codex" "-c" "tui.notifications=true"
                      "-c" "tui.notification_method=\"osc9\""
                      "-c" "tui.notification_condition=\"always\""))
            ('claude '("claude"))
            ('pi '("pi"))
            (_ (user-error "unknown agent program: %s" program)))))
    (require 'ghostel)
    (let ((buffer (generate-new-buffer (format "*agent: %s*" name))))
      (condition-case err
          (progn
            (pop-to-buffer-same-window buffer)
            (ghostel-exec buffer shell
                          (list "-l" "-i" "-c"
                                (concat "exec " (mapconcat #'shell-quote-argument
                                                           command " "))))
            (with-current-buffer buffer
              (agent-mode 1)
              (setq agent-project-root root
                    agent-project-name name))
            buffer)
        ((error quit)
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun agent-rename (name)
  "rename this agent to name without changing its project association."
  (interactive (list (read-string "agent name: " (buffer-name))))
  (unless agent-mode
    (user-error "this buffer is not an agent"))
  (when (string-empty-p (string-trim name))
    (user-error "agent name cannot be empty"))
  (rename-buffer name t))

(defun agent--buffers ()
  "return agent buffers in recent-use order, with attention first."
  (let ((buffers (seq-filter
                  (lambda (buffer) (buffer-local-value 'agent-mode buffer))
                  (buffer-list))))
    (append (seq-filter (lambda (buffer)
                          (buffer-local-value 'agent-attention buffer))
                        buffers)
            (seq-remove (lambda (buffer)
                          (buffer-local-value 'agent-attention buffer))
                        buffers))))

(defun agent--annotation (name)
  "annotate the agent named name with its project and attention state."
  (when-let* ((buffer (get-buffer name)))
    (with-current-buffer buffer
      (concat "  " (or agent-project-name "")
              (if agent-attention "  attention" "")))))

;;;###autoload
(defun agent-switch ()
  "switch to an agent using minibuffer completion."
  (interactive)
  (let ((buffers (agent--buffers)))
    (unless buffers
      (user-error "no agent buffers"))
    (let* ((names (mapcar #'buffer-name buffers))
           (completion-extra-properties
            '(:annotation-function agent--annotation
                                   :display-sort-function identity
                                   :cycle-sort-function identity))
           (name (completing-read "agent: " names nil t nil
                                  'agent-switch-history (car names))))
      (pop-to-buffer-same-window name)
      (agent--acknowledge))))

(provide 'agent)
;;; agent.el ends here
