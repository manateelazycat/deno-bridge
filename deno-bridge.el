;;; deno-bridge.el --- Bridge between Emacs and Deno  -*- lexical-binding: t; -*-

;; Filename: deno-bridge.el
;; Description: Bridge between Emacs and Deno
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-10-07 12:22:01
;; Version: 0.1
;; Last-Updated: 2022-10-07 12:22:01
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/deno-bridge
;; Keywords:
;; Compatibility: GNU Emacs 28.2
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Bridge between Emacs and Deno
;;

;;; Installation:
;;
;; Put deno-bridge.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'deno-bridge)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET deno-bridge RET
;;

;;; Change log:
;;
;; 2022/10/07
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'websocket)
(require 'ansi-color)

;;; Code:

(defvar deno-bridge-app-list (list))

(defun deno-bridge-get-free-port ()
  (save-excursion
    (let* ((process-buffer " *temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           port)
      (setq port (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr port)))))

(cl-defmacro deno-bridge-start (app-name ts-path)
  (if (member app-name deno-bridge-app-list)
      (message "[DenoBridge] Application %s has start." app-name)
    (let* ((deno-port (deno-bridge-get-free-port))
           (emacs-port (deno-bridge-get-free-port))
           (server (intern (format "deno-bridge-server-%s" app-name)))
           (process (intern (format "deno-bridge-process-%s" app-name)))
           (process-buffer (format " *deno-bridge-app-%s*" app-name))
           (client (intern (format "deno-bridge-client-%s" app-name))))
      `(let ((process-environment (cons "NO_COLOR=true" process-environment)))
         (defvar ,process nil)
         (defvar ,server nil)
         (defvar ,client nil)

         (setq ,server
               (websocket-server
                ,emacs-port
                :host 'local
                :on-message (lambda (_websocket frame)
                              (let* ((info (json-parse-string (websocket-frame-text frame)))
                                     (info-type (gethash "type" info nil)))
                                (pcase info-type
                                  ("show-message" (message (gethash "content" info nil)))
                                  ("eval-code" (eval (read (gethash "content" info nil))))
                                  ("fetch-var" (websocket-send-text _websocket (json-encode (eval (read (gethash "content" info nil))))))
                                  )))
                :on-open (lambda (_websocket)
                           (setq ,client (websocket-open (format "ws://127.0.0.1:%s" ,deno-port))))
                :on-close (lambda (_websocket))))
         ;; Start Deno process.
         (setq ,process
               (start-process ,app-name ,process-buffer "deno" "run" "-A" "--unstable" ,ts-path ,app-name ,deno-port ,emacs-port))
         
         ;; Make sure ANSI color render correctly.
         (set-process-sentinel
          ,process
          (lambda (p _m)
            (when (eq 0 (process-exit-status p))
              (with-current-buffer (process-buffer p)
                (ansi-color-apply-on-region (point-min) (point-max))))))

         (add-to-list 'deno-bridge-app-list ,app-name t)))))

(defun deno-bridge-exit ()
  (interactive)
  (let* ((app-name (completing-read "[DenoBridge] Exit application: " deno-bridge-app-list)))
    (if (member app-name deno-bridge-app-list)
        (let* ((server (intern-soft (format "deno-bridge-server-%s" app-name)))
               (process (intern-soft (format "deno-bridge-process-%s" app-name)))
               (process-buffer (format " *deno-bridge-app-%s*" app-name))
               (client (intern-soft (format "deno-bridge-client-%s" app-name))))
          (when server
            (when (symbol-value server)
              (websocket-server-close (symbol-value server)))
            (makunbound server))

          (when client
            (when (symbol-value client)
              (websocket-close (symbol-value client)))
            (makunbound client))

          (when process
            (kill-buffer process-buffer)
            (makunbound process))

          (setq deno-bridge-app-list (delete app-name deno-bridge-app-list)))
      (message "[DenoBridge] Application %s has exited." app-name))))

(defun deno-bridge-call (app-name &rest func-args)
  "Call Deno TypeScript function from Emacs."
  (if (member app-name deno-bridge-app-list)
      (websocket-send-text (symbol-value (intern-soft (format "deno-bridge-client-%s" app-name)))
                           (json-encode (list "data" func-args)))
    (message "[DenoBridge] Application %s has exited." app-name)))

(provide 'deno-bridge)

;;; deno-bridge.el ends here
