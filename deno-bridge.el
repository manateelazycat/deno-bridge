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

;;; Code:

(setq deno-bridge-ts-path (concat (file-name-directory load-file-name) "deno_bridge.ts"))

(defvar deno-bridge-process nil)
(defvar deno-bridge-process-buffer nil)

(defvar deno-bridge-client nil)
(defvar deno-bridge-server nil)

(defun deno-bridge-start (app-name deno-port emacs-port)
  (interactive)
  (unless deno-bridge-server
    (setq deno-bridge-server
          (websocket-server
           emacs-port
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
                      (setq deno-bridge-client (websocket-open (format "ws://127.0.0.1:%s" deno-port))))
           :on-close (lambda (_websocket)))))
  (unless deno-bridge-process
    (setq deno-bridge-process-buffer app-name)
    (setq deno-bridge-process
          (start-process app-name app-name "deno" "run" "--allow-net" deno-bridge-ts-path app-name deno-port emacs-port))))

(defun deno-bridge-exit ()
  (interactive)
  (when deno-bridge-server
    (websocket-server-close deno-bridge-server)
    (setq deno-bridge-server nil))

  (when deno-bridge-client
    (websocket-close deno-bridge-client)
    (setq deno-bridge-client nil))

  (when deno-bridge-process
    (kill-buffer deno-bridge-process-buffer)
    (setq deno-bridge-process nil)))

(defun deno-bridge-call (func-name &rest func-args)
  "Call Deno TypeScript function from Emacs."
  (when deno-bridge-client
    (websocket-send-text deno-bridge-client (json-encode (list "function" func-name func-args)))))

(provide 'deno-bridge)

;;; deno-bridge.el ends here
