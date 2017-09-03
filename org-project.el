;; Copyright (C) 2017 Joshua Wolfe

;; Author: Joshua Wolfe
;; Version: 1.0
;; Keywords: org, todo, project
;; URL: https://github.com/WolfeCub/org-project

;;; Code:

(require 'subr-x)
(require 'cl)

;;* Customization
(defgroup org-project nil
  "Org integration into projects"
  :group 'convenience)

;;* Variables
(defvar org-project--hash nil
  "The hash map that contains the cached project file locations")

(defvar org-project--cache-location "~/.emacs.d/org-project.cache"
  "Where to store the cached locations of projects")

(defvar org-project--file-name ".project.org"
  "The name of the file to store in each project")

(defvar org-project-use-ag nil
  "If org-project should use ag to speed up searches")

;;* Functions
(defun org-project--serialize-hash (data filename)
  "Serialize hash-table data to filename."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun org-project--deserialize-hash (filename)
  "Deserialize hash-table data from filename."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (read (buffer-string))))))

(defun org-project--file-list (dir)
  "Generate a list of files that contain the appropriate keywords"
  (split-string
   (shell-command-to-string
    (cond ((and org-project-use-ag (executable-find "ag"))
           (concat "ag --nocolor -l 'TODO|FIXME|NOTE|XXX' " dir))
          ((executable-find "grep")
           (concat "grep -ERl --color=never 'TODO|FIXME|NOTE|XXX' " dir))
          (:else (message "Could not find ag or grep in your path."))))
   "|\n" t "[ 	\n]"))

(defun org-project--list-comments (file-path)
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

(defun org-project--collect-comments (dir)
  (loop for file in (org-project--file-list dir)
        collect (cons file (org-project--list-comments file))))

(defun org-project--save-cache ()
  "Saves current hash table to cache file"
  (org-project--serialize-hash org-project--hash org-project--cache-location))

(defun org-project--load-cache ()
  "Loads cache file into the currint hash table"
  (setq org-project--hash (org-project--deserialize-hash org-project--cache-location)))

(defun org-project--create-hash-if-not-exists ()
  "Initializes the hash table if it doesn't exist."
  (setq org-project--hash (make-hash-table :test 'equal)))

(defun org-project--on-exit ()
  "Ran when emacs closes"
  (org-project--save-cache))

(defun org-project--populate-file (name dir)
  (progn
    (find-file (concat dir org-project--file-name))
    (erase-buffer)
    (mapc (lambda (file-list)
            (let ((file-name (car file-list)))
              (insert (format "* %s\n" file-name))
              (mapc (lambda (comm)
                      (insert (format "** [[%s::%s][%s]] - %s\n" file-name (nth 1 comm) (nth 1 comm) (car comm))))
                    (cdr file-list))))
          (org-project--collect-comments dir))
    (org-mode)
    (read-only-mode t)
    (org-project-mode t)
    (puthash name dir org-project--hash)
    (org-project--save-cache)))

(defun org-project--execute-with-hash-check (fun &rest args)
  "First checks if the hash exists if not it trys to load from file and then checks again.
If the hash exists it executes the function otherwise it prints an error"
  ;; First try and load the hash table from file
  (when (not org-project--hash)
    (org-project--load-cache))
  (if org-project--hash
      (apply fun args)
    (message "No projects found. Run \"org-project/create-project\" to create one.")))

;;* Commands
(defun org-project/create-project (&optional directory)
  "Creates the org-file that will be associated with this project."
  (interactive)
  (let ((name (read-from-minibuffer "Project name: "))
        (dir (if directory directory (expand-file-name (read-file-name "Project root dir: ")))))
    (if (file-directory-p dir)
        (org-project--populate-file name dir)
      (message "Please specify a directory"))))

(defun org-project/open-project ()
  "Opens an existing org-project"
  (interactive)
  (org-project--execute-with-hash-check
   (lambda ()
     (find-file
      (concat
       (gethash (completing-read "Select from list: " (hash-table-keys org-project--hash))
                org-project--hash)
       org-project--file-name)))))

(defun org-project/update-and-open-project ()
  (interactive)
  (org-project--execute-with-hash-check
   (lambda ()
     (let ((name (completing-read "Select from list: " (hash-table-keys org-project--hash))))
       (org-project--populate-file name (gethash name org-project--hash))))))

(defun org-project/create-from-projectile ()
  "Select a projectile project to use"
  (interactive)
  (if (featurep 'projectile)
      (let ((project (completing-read "Project: " (projectile-relevant-known-projects))))
        (org-project/create-project project))
    (message "Projectile not detected. Are you sure you have it installed?")))

;;;###autoload
(define-minor-mode org-project-mode
  "Toggle org-project-mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state."
  nil ; The initial value
  :lighter " OProj"
  :group 'org-project
  (when org-project-mode
    ;; Load cached hash-table of projects into variable here
    ;; If it doesn't exist create a file at org-project--cache-location
    (if (file-exists-p (expand-file-name org-project--cache-location))
        (org-project--load-cache)
      (org-project--create-hash-if-not-exists))
    (add-hook 'kill-emacs-hook 'org-project--on-exit)))

(provide 'org-project)

;;; org-project.el ends here
