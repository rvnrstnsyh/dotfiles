;; ~/.emacs.d/init.el
;; Main configuration file that loads all modules

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")

;; Load core modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'packages)  ;; Load this first to ensure packages are available
(require 'core)
(require 'ui)
(require 'keybindings)

;; Load miscellaneous modules (functions)
;; Use `eval-after-load` for misc usage as a precaution in case the module is loaded late
(add-to-list 'load-path (expand-file-name "modules/misc" user-emacs-directory))
(require 'editing)

;; Load development modules (environments)
(add-to-list 'load-path (expand-file-name "modules/dev" user-emacs-directory))
(require 'common)     ;; Common development settings
(require 'typescript) ;; TypeScript/JavaScript specific settings

;; Load Custom File if it exists
(when (file-exists-p custom-file)
  (load-file custom-file))
