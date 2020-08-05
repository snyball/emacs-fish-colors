;;; ~/.config/doom/ml/fish-colors.el -*- lexical-binding: t; -*-

(require 'xdg)
(require 's)

(defvar fish-color-to-emacs-face
  '((fish_color_autosuggestion . font-lock-comment-face)
    (fish_color_cancel . error)
    (fish_color_command . font-lock-keyword-face)
    (fish_color_comment . font-lock-comment-face)
    (fish_color_cwd . default)
    (fish_color_cwd_root . default)
    (fish_color_end . default)
    (fish_color_error . error)
    (fish_color_escape . default)
    (fish_color_history_current . default)
    (fish_color_host . default)
    (fish_color_host_remote . default)
    (fish_color_match . default)
    (fish_color_normal . default)
    (fish_color_operator . font-lock-function-name-face)
    (fish_color_param . default)
    (fish_color_quote . font-lock-string-face)
    (fish_color_redirection . font-lock-function-name-face)
    (fish_color_search_match . default)
    (fish_color_selection . default)
    (fish_color_status . default)
    (fish_color_user . default)))


;;;###autoload
(defun fish-colors--face-to-fish-color (face)
  "Convert an Emacs face to set_color syntax."
  (let ((color (face-attribute face :foreground))
        (default-color (face-attribute 'default :foreground)))
    (s-join " " `(,(format "'%s'" (if (eq color :unspecified)
                                      default-color
                                    color))
                  ,(if (eq (face-attribute face :weight) 'bold)
                       "--bold"
                     "")
                  ,(let ((underline (face-attribute face :underline)))
                     (if (and underline (not (eq underline :unspecified)))
                         "--underline"
                       ""))))))


;;;###autoload
(defun fish-colors--make-commands (&optional indent)
  "Make the commands to set fish colors."
  (setq indent (if indent
                   (s-repeat indent " ")
                 ""))
  (let ((commands nil))
    (dolist (color fish-color-to-emacs-face)
      (pcase color
        (`(,fish-name . ,face-name)
         (push (format "%sset -gx %s %s" indent fish-name
                       (fish-colors--face-to-fish-color face-name))
               commands))))
    (s-join "\n" commands)))


;;;###autoload
(defun fish-colors--make-function ()
  "Make the set_emacs_colors function."
  (let ((lines `("function set_emacs_colors -d 'Set colorscheme according to Emacs faces'"
                 ,(fish-colors--make-commands 4)
                 "end"
                 "")))
    (s-join "\n" lines)))


;;;###autoload
(defun fish-colors-install-function ()
  "Install the set_emacs_colors function in $XDG_CONFIG_HOME/fish/functions/."
  (interactive)
  (let ((path (concat (file-name-as-directory (xdg-config-home))
                      "fish/functions/set_emacs_colors.fish")))
    (with-current-buffer (find-file-noselect path)
      (insert "#!/usr/bin/env fish")
      (newline)
      (newline)
      (insert (fish-colors--make-function))
      (save-buffer)
      (kill-buffer))))
