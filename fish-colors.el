;;; fish-colors.el --- Export Emacs color theme to fish. -*- lexical-binding: t -*-
;;; Commentary:
;;;
;;; Run M-x fish-colors-install-function and add the following to your
;;; fish config.el file:
;;;
;;;   if set -q INSIDE_EMACS
;;;       set_emacs_colors
;;;   end
;;;
;;; Author: Jonas Møller <jonas.moeller2@protonmail.com>
;;; URL: https://github.com/snyball/emacs-fish-colors
;;; Version: 0.1
;;; Keywords: color theme fish shell
;;; License:
;;;   Copyright (C) 2020 Jonas Møller
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright notice, this
;;;      list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright notice,
;;;      this list of conditions and the following disclaimer in the documentation
;;;      and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;;;   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
;;;   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;;   ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; Code:

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
  "Convert an Emacs FACE to set_color syntax."
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
  "Make the commands to set fish colors, optionally indented with INDENT spaces."
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

(provide 'fish-colors)

;;; fish-colors.el ends here
