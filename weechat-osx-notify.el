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

(defun send-osx-notification (text)
  (djcb-shell-command-maybe "terminal-notifier"
                            (concat "-message "
                                    "\""
                                    text
                                    "\" "
                                    "-title "
                                    "\""
                                    "From weechat-osx-notify"
                                    "\"")))

;; FIXME: somehow the text is not complete in osx notifications
(defun weechat-osx-notify-handler (type &optional sender text _date buffer-ptr)
  (setq text (if text (weechat-strip-formatting text)))
  (setq sender (if sender (weechat-strip-formatting sender)))
  (let ((jump-position (point-max-marker)))
    (let ((text (cl-case type
                  (:highlight
                   (format "%s in %s: %S"
                           sender
                           (weechat-buffer-name buffer-ptr)
                           text))
                  (:query
                   (format "Query from %s: %S"
                           sender
                           text))
                  (:disconnect
                   "Disconnected from WeeChat"))))
      (send-osx-notification text))))

(add-hook 'weechat-notification-handler-functions
          'weechat-osx-notify-handler)

(provide 'weechat-osx-notify)

;;; weechat-osx-notify.el ends here
