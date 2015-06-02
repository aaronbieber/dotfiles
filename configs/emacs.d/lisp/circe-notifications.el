;;; circe-notifications.el

;; Copyright (C) 2014 Ruben Maher

;; Author: Ruben Maher <r@rkm.id.au>
;; URL: https://code.rkm.id.au/circe-notifications

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'circe)
(require 'xml)
(require 'dbus)
(require 'notifications)

(defgroup circe-notifications nil
  "Add desktop notifications to Circe."
  :prefix "circe-notifications-"
  :group 'circe)

(defvar circe-notifications-wait-list nil
  "An alist of nicks that have triggered notifications in the last
`circe-notifications-wait-for' seconds.")

(defvar circe-notifications-nicks-on-all-networks nil
  "A list of nicks in use according to `circe-network-options'.  It is generated
  by `circe-notifications-get-nicks-on-all-networks'.")

(defcustom circe-notifications-terminal-notifier-command
  (executable-find "terminal-notifier")
  "The path to terminal-notifier.  For OSX users.")

(defcustom circe-notifications-backend "dbus"
  "One of `dbus' or `terminal-notifier'."
  :type '(choice
          (const :tag "Use dbus" "dbus")
          (const :tag "Use terminal-notifier" "terminal-notifier"))
  :group 'circe-notifications)

(defcustom circe-notifications-wait-for 90
  "The number of seconds to wait before allowing some nick in
`circe-notifications-wait-list' to trigger a notification again."
  :type 'integer
  :group 'circe-notifications)

(defcustom circe-notifications-watch-nicks nil
  "A list of nicknames or words which can trigger a notification.  You don't
need to put your nick here, it is added automagically by
`circe-notifications-nicks-on-all-networks' when it checks the values in
`circe-network-options'."
  :type '(repeat string)
  :group 'circe-notifications)

(defcustom circe-notifications-check-window-focus nil
  "Enable use of external tools to check if Emacs is focused.

Requires:

  X to be running (well, it can be tricked by setting $DISPLAY)
  xprop
  xdotool

Emacs can be running in a terminal emulator as long as it is capable of setting
WM_NAME and `circe-notifications-term-name' is set."
  :type 'boolean
  :group 'circe-notifications)

(defcustom circe-notifications-term-name "xterm"
  "The name of the terminal you will run Emacs in (if any).  If you are unsure
check the value of WM_CLASS with xprop.  This is necessary to prevent false
positives."
  :type 'string
  :group 'circe-notifications)

(defcustom circe-notifications-desktop-entry
  (concat "emacs" (int-to-string emacs-major-version))
  "Icon to use for notifications."
  :type 'file
  :group 'circe-notifications)

(defcustom circe-notifications-sound-name "message-new-entry"
  "Sound to use for notifications."
  :type 'file
  :group 'circe-notifications)

(defcustom circe-notifications-timeout 1000
  "How long the notification should be displayed for (milliseconds)."
  :type 'integer
  :group 'circe-notifications)

(defun circe-notifications (nick user host command args)
  "Unless NICK is being ignored:

If ARGS, NICK or USER contains any of
`circe-notifications-nicks-on-all-networks' or
`circe-notifications-watch-nicks', if either channel (car ARGS) is in
`tracking-buffers' (i.e., not currently visible) or Emacs is not currently
focused by the window manager (detected if
`circe-notifications-check-window-focus' is true), and NICK has not triggered a
notification in the last `circe-notifications-wait-for' seconds, show a desktop
notification."
  (unless
      (cond ((circe-fool-p nick user host command args))
            ((circe-ignored-p nick user host command args)))
    (let ((body (cadr args))
          (channel (car args)))
      (ignore-errors ;; ignores server ping messages which have no NICK or USER
        (when (or (member channel tracking-buffers)
                  (not (circe-notifications-emacs-focused-p)))
          (if (and
               body
               (string-equal "PRIVMSG" command))
              (when
                  (cond ((dolist (n circe-notifications-nicks-on-all-networks)
                           (if (string-match n body)
                               (return t))))
                        ((member-ignore-case
                          nick
                          circe-notifications-watch-nicks))
                        ((member-ignore-case
                          user
                          circe-notifications-watch-nicks)))
                (when (circe-not-getting-spammed-by nick)
                  (circe-notifications-notify nick body)))
            ;; someone we are watching JOINed, PARTed, QUIT or was KICKed
            (when (and
                   channel
                   (cond ((member-ignore-case
                           nick
                           circe-notifications-watch-nicks))
                         ((member-ignore-case
                           user
                           circe-notifications-watch-nicks))))
              (when (circe-not-getting-spammed-by nick)
                (circe-notifications-notify
                 nick (concat command ": " channel))))))))))

(defun circe-notifications-notify (nick body)
  "Show a desktop notification with title NICK and body BODY."
    (if (string-equal circe-notifications-backend "dbus")
        (dbus-ignore-errors
          (notifications-notify
           :title (xml-escape-string nick)
           :body (xml-escape-string body)
           :timeout circe-notifications-timeout
           :desktop-entry circe-notifications-desktop-entry
           :sound-name circe-notifications-sound-name
           :transient))
      ;; otherwise use terminal-notifier
      (start-process "terminal-notifier"
                 "*terminal-notifier*"
                 circe-notifications-terminal-notifier-command
                 "-title" (xml-escape-string nick)
                 "-message" (xml-escape-string body)
                 "-activate" "org.gnu.Emacs")))

(defun circe-not-getting-spammed-by (nick)
  "Return an alist with NICKs that have triggered notifications in the last
`circe-notifications-wait-for' seconds, or nil if it has been less than
`circe-notifications-wait-for' seconds since the last notification from NICK."
  (if (assoc nick circe-notifications-wait-list)
      (circe-notifications-wait-a-bit nick)
    (add-to-list 'circe-notifications-wait-list
                 (cons nick (float-time)))))

(defun circe-notifications-wait-a-bit (nick)
  "Check if it has been more than `circe-notifications-wait-for' seconds since
the last message from NICK.  If so, remove them from
`circe-notifications-wait-list'."
  (let* ((last-time (assoc-default
                     nick
                     circe-notifications-wait-list
                     (lambda (x y)
                       (string-match y x))))
         (seconds-since (- (float-time) last-time)))
    (when (< circe-notifications-wait-for seconds-since)
      (progn (setq circe-notifications-wait-list
                   (delq
                    (assoc nick circe-notifications-wait-list)
                    circe-notifications-wait-list))
             ;; add NICK to the waitlist again
             (circe-not-getting-spammed-by nick)))))

(defun circe-notifications-get-nicks-on-all-networks ()
  "Get a list of all nicks in use according to `circe-network-options'."
  (let ((x 0))
    (while (< x (list-length circe-network-options))
      (add-to-list 'circe-notifications-nicks-on-all-networks
                   (nth 2 (nth x circe-network-options)))
      (setq x (+ 1 x)))))


(defun circe-notifications-has-x-tools-p ()
  "True if $DISPLAY is set and both xdotool and xprop are installed.'"
  (if (and (< 0 (string-width (shell-command-to-string "echo $DISPLAY")))
           (executable-find "xdotool")
           (executable-find "xprop"))
      t
    nil))

(defun circe-notifications-emacs-focused-p ()
  "True if Emacs is focused by the window manager."
  (when circe-notifications-check-window-focus
    (when (circe-notifications-has-x-tools-p)
      (let* ((focused-window (shell-command-to-string "xdotool getwindowfocus"))
             (window-class (shell-command-to-string
                            (concat "xprop WM_CLASS -id " focused-window)))
             (window-name (shell-command-to-string
                           (concat "xprop WM_NAME -id " focused-window))))
        (if (string-match "emacs" window-class)
            t
          (if (and circe-notifications-term-name
                   (string-match circe-notifications-term-name
                                 window-class)
                   (string-match "emacs" window-name))
              t
            nil))))))

(defun enable-circe-notifications ()
  "Turn on notifications."
  (interactive)
  (circe-notifications-get-nicks-on-all-networks)
  (add-hook 'circe-receive-message-functions 'circe-notifications))

(defun disable-circe-notifications ()
  "Turn on notifications."
  (interactive)
  (remove-hook 'circe-receive-message-functions 'circe-notifications))

(provide 'circe-notifications)
;;; circe-notifications.el ends here
