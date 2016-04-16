;;; weechat-alert.el --- Weechat notifier using alerts  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2016 Andreas Klein
;;
;; Author: Andreas Klein <git@kungi.org>
;; Keywords: irc chat network weechat
;; URL: https://github.com/kungi/weechat-alert
;; Version: 0.0.1
;; Package-Requires: ((weechat "0.3.1") (cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Weechat.el notifications using OSX system notifications
;; Requires terminal-notifier which can be installed via homebrew
;;
;;     brew install terminal-notifier
;;
;;; Code:

(require 'weechat)
(require 'cl-lib)

(defun weechat-alert-shell-command-maybe (exe &optional paramstr)
  "run executable EXE with PARAMSTR, or warn if EXE's not available; eg. "
  " (weechat-alert-shell-command-maybe \"ls\" \"-l -a\")"
  (if (executable-find exe)
      (shell-command (concat exe " " paramstr))
    (user-error "'%s' is not found, please install" exe)))

(defun weechat-alert-prepare-params (title text)
  (mapconcat #'identity
             (list "-message"
                   (shell-quote-argument text)
                   "-title"
                   (shell-quote-argument title)
                   "-group 'weeechat.notification'"
                   "-sender 'org.gnu.Emacs'")
             " "))

(defun weechat-alert-send-osx-notification (title text)
  (let ((params (weechat-alert-prepare-params title text)))
    (weechat-alert-shell-command-maybe "terminal-notifier"
                                            params)))

(defun weechat-alert-handler (type &optional sender text _date buffer-ptr)
  (setq text (if text (weechat-strip-formatting text)))
  (setq sender (if sender (weechat-strip-formatting sender)))
  (let ((jump-position (point-max-marker)))
    (let ((text (cl-case type
                  (:highlight
                   (weechat-alert-send-osx-notification
                    (format "Highlight from %s"
                            sender)
                    (format "in %s: %s"
                            (weechat-buffer-name buffer-ptr)
                            text)))
                  (:query
                   (weechat-alert-send-osx-notification
                    (format "Query from %s"
                            sender)
                    (format "%s"
                            text)))
                  (:disconnect
                   (weechat-alert-send-osx-notification
                    "Disconnected from WeeChat"
                    ""))))))))

(add-hook 'weechat-notification-handler-functions
          'weechat-alert-handler)

(provide 'weechat-alert)

;;; weechat-alert.el ends here
