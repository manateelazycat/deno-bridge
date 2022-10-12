;;; deno-bridge-demo.el --- Demo for deno-bridge

;; Filename: deno-bridge-demo.el
;; Description: Demo for deno-bridge
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2022, Andy Stewart, all rights reserved.
;; Created: 2022-10-08 23:38:27
;; Version: 0.1
;; Last-Updated: 2022-10-08 23:38:27
;;           By: Andy Stewart
;; URL: https://www.github.org/manateelazycat/deno-bridge-demo
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
;; Demo for deno-bridge
;;

;;; Installation:
;;
;; Put deno-bridge-demo.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'deno-bridge-demo)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET deno-bridge-demo RET
;;

;;; Change log:
;;
;; 2022/10/08
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
(require 'deno-bridge)

;;; Code:

(setq deno-bridge-demo-ts-path (concat (file-name-directory load-file-name) "deno-bridge-demo.ts"))
(deno-bridge-start "demo" deno-bridge-demo-ts-path)

;; (deno-bridge-call "demo" "ping" "Hello from Emacs.")

(provide 'deno-bridge-demo)

;;; deno-bridge-demo.el ends here
