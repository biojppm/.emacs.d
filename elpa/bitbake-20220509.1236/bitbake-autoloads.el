;;; bitbake-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bitbake" "bitbake.el" (0 0 0 0))
;;; Generated autoloads from bitbake.el

(put 'bitbake-server-host 'risky-local-variable t)

(put 'server-port 'risky-local-variable t)

(autoload 'bitbake-start-server "bitbake" "\
Start a bitbake server instance.

Start a bitbake server using POKY-DIRECTORY to find the bitbake
binary and BUILD-DIRECTORY as the build directory.

\(fn POKY-DIRECTORY BUILD-DIRECTORY)" t nil)

(autoload 'bitbake-task "bitbake" "\
Run bitbake TASK on RECIPE.

If FORCE is non-nil, force running the task.

\(fn TASK RECIPE &optional FORCE)" t nil)

(autoload 'bitbake-recipe "bitbake" "\
Run bitbake RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-clean "bitbake" "\
Run bitbake clean on RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-compile "bitbake" "\
Run bitbake compile on RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-install "bitbake" "\
Run bitbake install on RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-fetch "bitbake" "\
Run bitbake install on RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-recompile "bitbake" "\
Run bitbake clean compile and install on RECIPE.

\(fn RECIPE)" t nil)

(autoload 'bitbake-deploy "bitbake" "\
Deploy artifacts of RECIPE to bitbake-deploy-ssh host.

\(fn RECIPE)" t nil)

(autoload 'bitbake-recompile-deploy "bitbake" "\
Recompile RECIPE and deploy its artifacts.

\(fn RECIPE)" t nil)

(autoload 'bitbake-image "bitbake" "\
Run bitbake IMAGE.

If FORCE is non-nil, force rebuild of image,

\(fn IMAGE &optional FORCE)" t nil)

(autoload 'bitbake-wic-create "bitbake" "\
Run wic WKS -e IMAGE.

\(fn WKS IMAGE)" t nil)

(autoload 'bitbake-hdd-image "bitbake" "\
Create an hdd image using wic based on WKS definition file and bitbake IMAGE.

\(fn WKS IMAGE)" t nil)

(autoload 'bitbake-flash-image "bitbake" "\
Create an hdd image using wic and flash it on bitbake-flash-device.

The hdd image is based on WKS definition file and bitbake IMAGE, see bitbake-hdd-image.

\(fn WKS IMAGE)" t nil)

(defvar bitbake-minor-mode-map nil "\
Keymap for bitbake-mode.")

(autoload 'bitbake-mode "bitbake" "\


\(fn)" t nil)

(register-definition-prefixes "bitbake" '("bitbake-" "wic-read-definition-file"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bitbake-autoloads.el ends here
