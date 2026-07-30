;;; agent-shell-cursor.el --- Cursor agent configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file includes Cursor-specific configurations.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'acp)

(declare-function agent-shell--indent-string "agent-shell")
(declare-function agent-shell-make-agent-config "agent-shell")
(autoload 'agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell--dwim "agent-shell")

(cl-defun agent-shell-cursor-make-authentication (&key api-key auth-token login none)
  "Create Cursor authentication configuration.

API-KEY is the Cursor API key string or function that returns it.
AUTH-TOKEN is the Cursor auth token string or function that returns it.
LOGIN when non-nil uses interactive \"cursor_login\" authentication.
NONE when non-nil indicates authentication is handled externally
\(for example via `agent login').

Only one of API-KEY, AUTH-TOKEN, LOGIN, or NONE should be provided."
  (when (> (seq-count #'identity (list api-key auth-token login none)) 1)
    (error "Cannot specify multiple authentication methods - choose one"))
  (unless (> (seq-count #'identity (list api-key auth-token login none)) 0)
    (error "Must specify one of :api-key, :auth-token, :login, or :none"))
  (cond
   (api-key `((:api-key . ,api-key)))
   (auth-token `((:auth-token . ,auth-token)))
   (login `((:login . t)))
   (none `((:none . t)))))

(defcustom agent-shell-cursor-authentication
  (agent-shell-cursor-make-authentication :none t)
  "Configuration for Cursor authentication.

By default authentication is handled externally: agent-shell sends no
ACP authenticate request and relies on an existing Cursor login (run
`agent login' once outside Emacs).  This matches how Cursor was used
before and needs no configuration.

Optionally, configure agent-shell to manage authentication instead.

For no authentication, handled externally (default):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :none t))

For login-based authentication (agent-shell drives \"cursor_login\"):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :login t))

For API key (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key \"your-key\"))

For API key (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :api-key (lambda () ...)))

For auth token (string):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token \"your-token\"))

For auth token (function):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :auth-token (lambda () ...)))

For no authentication (already authenticated via `agent login'):

  (setq agent-shell-cursor-authentication
        (agent-shell-cursor-make-authentication :none t))"
  :type 'alist
  :group 'agent-shell)

(defcustom agent-shell-cursor-acp-command
  '("agent" "acp")
  "Command and parameters for the Cursor agent client.

The first element is the command name, and the rest are command parameters."
  :type '(repeat string)
  :group 'agent-shell)

(defcustom agent-shell-cursor-environment
  nil
  "Environment variables for the Cursor agent client.

This should be a list of environment variables to be used when
starting the Cursor agent process."
  :type '(repeat string)
  :group 'agent-shell)

(defun agent-shell-cursor-make-agent-config ()
  "Create a Cursor agent configuration.

Returns an agent configuration alist using `agent-shell-make-agent-config'."
  (agent-shell-make-agent-config
   :identifier 'cursor
   :mode-line-name "Cursor"
   :buffer-name "Cursor"
   :shell-prompt "Cursor> "
   :shell-prompt-regexp "Cursor> "
   :icon-name "cursor.png"
   :welcome-function #'agent-shell-cursor--welcome-message
   ;; Only the interactive login flow uses an ACP authenticate request
   ;; (method id \"cursor_login\").  API key and auth token are supplied
   ;; out-of-band via CURSOR_API_KEY / CURSOR_AUTH_TOKEN, and :none is
   ;; authenticated externally (for example via `agent login').
   :needs-authentication (and (map-elt agent-shell-cursor-authentication :login) t)
   :authenticate-request-maker (lambda ()
                                 (acp-make-authenticate-request :method-id "cursor_login"))
   :client-maker (lambda (buffer)
                   (agent-shell-cursor-make-client :buffer buffer))
   :install-instructions "See https://cursor.com/docs/cli for installation."))

(defun agent-shell-cursor-start-agent ()
  "Start an interactive Cursor agent shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-cursor-make-agent-config)
                     :new-shell t))

(cl-defun agent-shell-cursor-make-client (&key buffer)
  "Create a Cursor agent ACP client with BUFFER as context.

Uses `agent-shell-cursor-authentication' for authentication configuration."
  (unless buffer
    (error "Missing required argument: :buffer"))
  (when (and (boundp 'agent-shell-cursor-command) agent-shell-cursor-command)
    (user-error "Please migrate to use agent-shell-cursor-acp-command and eval (setq agent-shell-cursor-command nil)"))
  (agent-shell--make-acp-client :command (car agent-shell-cursor-acp-command)
                                :command-params (cdr agent-shell-cursor-acp-command)
                                :environment-variables (append
                                                        (cond
                                                         ((map-elt agent-shell-cursor-authentication :api-key)
                                                          (let ((api-key (agent-shell-cursor--resolve-secret
                                                                          (map-elt agent-shell-cursor-authentication :api-key))))
                                                            (unless api-key
                                                              (user-error "Please set your `agent-shell-cursor-authentication'"))
                                                            (list (format "CURSOR_API_KEY=%s" api-key))))
                                                         ((map-elt agent-shell-cursor-authentication :auth-token)
                                                          (let ((auth-token (agent-shell-cursor--resolve-secret
                                                                             (map-elt agent-shell-cursor-authentication :auth-token))))
                                                            (unless auth-token
                                                              (user-error "Please set your `agent-shell-cursor-authentication'"))
                                                            (list (format "CURSOR_AUTH_TOKEN=%s" auth-token)))))
                                                        agent-shell-cursor-environment)
                                :context-buffer buffer))

(defun agent-shell-cursor--resolve-secret (value)
  "Resolve VALUE to a string.
VALUE may be a string or a function that returns a string."
  (cond ((stringp value) value)
        ((functionp value)
         (condition-case _err
             (funcall value)
           (error
            (error "Secret not found.  Check out `agent-shell-cursor-authentication'"))))
        (t nil)))

(defun agent-shell-cursor--welcome-message (config)
  "Return Cursor welcome message using `shell-maker' CONFIG."
  (let ((art (agent-shell--indent-string 4 (agent-shell-cursor--ascii-art)))
        (message (string-trim-left (shell-maker-welcome-message config) "\n")))
    (concat "\n\n"
            art
            "\n\n"
            message)))

(defun agent-shell-cursor--ascii-art ()
  "Cursor ASCII art."
  (let* ((is-dark (eq (frame-parameter nil 'background-mode) 'dark))
         (text (string-trim "
  ██████╗ ██╗   ██╗ ██████╗  ███████╗  ██████╗  ██████╗
 ██╔════╝ ██║   ██║ ██╔══██╗ ██╔════╝ ██╔═══██╗ ██╔══██╗
 ██║      ██║   ██║ ██████╔╝ ███████╗ ██║   ██║ ██████╔╝
 ██║      ██║   ██║ ██╔══██╗ ╚════██║ ██║   ██║ ██╔══██╗
 ╚██████╗ ╚██████╔╝ ██║  ██║ ███████║ ╚██████╔╝ ██║  ██║
  ╚═════╝  ╚═════╝  ╚═╝  ╚═╝ ╚══════╝  ╚═════╝  ╚═╝  ╚═╝
" "\n")))
    (propertize text 'font-lock-face (if is-dark
                                         '(:foreground "#00d4ff" :inherit fixed-pitch)
                                       '(:foreground "#0066cc" :inherit fixed-pitch)))))

(provide 'agent-shell-cursor)

;;; agent-shell-cursor.el ends here
