 ;;; weechat-osx-notify --- OSX Notifications ;; -*- lexical-binding: t -*-

;; Copyright (C) 2016 Andreas Klein

;; Author: Andreas Klein <git@kungi.org>
;; Keywords: irc chat network weechat
;; URL: https://github.com/kungi/weechat-osx-notify.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Notifications using OSX system notifications
;; Required terminal-notifier which can be installed via homebrew with
;;
;;     brew install terminal-notifier

;;; Code:

(require 'weechat)

(defun djcb-shell-command-maybe (exe &optional paramstr)
  "run executable EXE with PARAMSTR, or warn if EXE's not available; eg. "
  " (djcb-shell-command-maybe \"ls\" \"-l -a\")"
  (if (executable-find exe)
    (shell-command (concat exe " " paramstr))
    (message (concat "'" exe "' not found found; please install"))))

(defun weechat-osx-notify-wrap-in-quotes (s)
  (concat "'" s "'"))

(defun weechat-osx-notify-prepare-params (title text)
  (apply #'concat
         (-interpose " "
                     (list "-message"
                           (weechat-osx-notify-wrap-in-quotes text)
                           "-title"
                           (weechat-osx-notify-wrap-in-quotes title)
                           "-group 'weeechat.notification'"
                           "-sender 'org.gnu.Emacs'"))))

(defun weechat-osx-notify-send-osx-notification (title text)
  (let ((params (weechat-osx-notify-prepare-params title text)))
    (message params)
    (djcb-shell-command-maybe "terminal-notifier"
                              params)))

(defun weechat-osx-notify-handler (type &optional sender text _date buffer-ptr)
  (setq text (if text (weechat-strip-formatting text)))
  (setq sender (if sender (weechat-strip-formatting sender)))
  (let ((jump-position (point-max-marker)))
    (let ((text (cl-case type
                  (:highlight
                   (weechat-osx-notify-send-osx-notification
                    (format "Highlight from %s"
                            sender)
                    (format "in %s: %s"
                            (weechat-buffer-name buffer-ptr)
                            text)))
                  (:query
                   (weechat-osx-notify-send-osx-notification
                    (format "Query from %s"
                            sender)
                    (format "%s"
                            text)))
                  (:disconnect
                   (weechat-osx-notify-send-osx-notification
                    "Disconnected from WeeChat"
                    ""))))))))

(add-hook 'weechat-notification-handler-functions
          'weechat-osx-notify-handler)

(provide 'weechat-osx-notify)

;;; weechat-osx-notify.el ends here
