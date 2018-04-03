;; Copyright (C) 2017 Joshua Wolfe

;; Author: Joshua Wolfe
;; Version: 1.0
;; Keywords: org, todo, project
;; URL: https://github.com/WolfeCub/todo-projectile

;;; Code:

(require 'subr-x)
(require 'cl)

;;* Customization
(defgroup todo-projectile nil
  "Org integration into projects"
  :group 'convenience)

;;* Variables
(defvar todo-projectile--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'todo-projectile--delete-bullet)
    map)
  "Keymap for todo-projectile-mode")

(defvar todo-projectile--file-name ".project.org"
  "The name of the file to store in each project")

(defvar todo-projectile-use-ag nil
  "If todo-projectile should use ag to speed up searches")

(defvar todo-projectile--search-regexp "TODO|FIXME|NOTE|XXX")

(defvar todo-projectile--ag-args '("--files-with-matches" "--nocolor")
  "Arguments passed to ag")

(defvar todo-projectile--grep-args
  '("--extended-regexp"
    "--dereference-recursive"
    "--files-with-matches"
    "--no-messages"
    "--color=never")
  "Arguments passed to grep")

;;* Functions
(defun todo-projectile--file-list (dir)
  "Generate a list of files that contain the appropriate keywords"
  (split-string
   (shell-command-to-string
    (cond (todo-projectile-use-ag
           (concat "ag " (string-join todo-projectile--ag-args " ") " \""
                   todo-projectile--search-regexp "\" " dir))
          (:else
           (concat "grep " (string-join todo-projectile--grep-args " ") " \""
                   todo-projectile--search-regexp "\" " dir))))
   "|\n" t "[ 	\n]"))

(defun todo-projectile--list-comments (file-path)
  "Parse through a file for a list of all the comments"
  (let (already-open
        buf
        start
        (comments '()))
    (setq already-open (find-buffer-visiting file-path)
          buf (find-file-noselect file-path))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (setq start (text-property-any
                          (point) (point-max)
                          'face 'font-lock-comment-face))
        (goto-char start)
        (let ((line (line-number-at-pos)))
          (goto-char (next-single-char-property-change (point) 'face))
          (let ((item (string-trim (buffer-substring-no-properties start (point)))))
            (when (string-match-p "TODO\\|FIXME\\|NOTE\\|XXX" item)
              (setq comments (cons (list item line)
                                   comments)))))))
    (unless already-open (kill-buffer buf))
    (reverse comments)))

(defun todo-projectile--collect-comments (dir)
  "Gather all comments from all the files"
  (loop for file in (todo-projectile--file-list dir)
        collect (cons file (todo-projectile--list-comments file))))

(defun todo-projectile--populate-file (dir)
  (progn
    (find-file (concat dir todo-projectile--file-name))
    (erase-buffer)
    (mapc (lambda (file-list)
            (let ((file-name (car file-list)))
              (insert (format "* %s\n" file-name))
              (mapc (lambda (comm)
                      (insert (format "** [[%s::%s][%s]] - %s\n" file-name (nth 1 comm) (nth 1 comm) (car comm))))
                    (cdr file-list))))
          (todo-projectile--collect-comments dir))
    (org-mode)
    (read-only-mode t)
    (todo-projectile-mode t)))

;;* Commands
(defun todo-projectile/open-project ()
  "Opens an existing todo-projectile"
  (interactive)
  (let ((project (completing-read "Project: " (projectile-relevant-known-projects))))
    (find-file (concat project todo-projectile--file-name))))

(defun todo-projectile/update-and-open-project ()
  "Opens an existing todo-projectile and re-searches for TODOs"
  (interactive)
  (let ((project (completing-read "Project: " (projectile-relevant-known-projects))))
    (todo-projectile--populate-file project)))

(defun todo-projectile--delete-bullet ()
  (interactive)
  (read-only-mode -1)
  (org-cut-subtree)
  (message "")
  (read-only-mode t))

;;;###autoload
(define-minor-mode todo-projectile-mode
  "Toggle todo-projectile-mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  :initial-value nil ; The initial value
  :lighter " Todo"
  :keymap todo-projectile--keymap
  :group 'todo-projectile)

(provide 'todo-projectile)

;;; todo-projectile.el ends here
