(message "SDS environment config")

(defun my-set-font-size()
  (interactive)
  (message "display-pixel-height is %s" (display-pixel-height))
  (if (<= (display-pixel-height) 1440)
      ;; Full HD
      (progn
        (message "setting font for FullHD")
        (set-face-attribute 'default nil :font "Inconsolata-12")
        )
    ;; 4K
    (progn
      (message "setting font for 4k")
      (set-face-attribute 'default nil :font "Inconsolata-14")
      )
    )
  )
(my-set-font-size)

(add-to-list 'compilation-error-regexp-alist 'unity_failed_tests)
(add-to-list 'compilation-error-regexp-alist-alist
             '(unity_failed_tests
               ;;TEST(ValidationUnitTestsJson, Sony_Json_getRootFailsOnEmptyTree)./ValidationTests/UnitTests/ValidationUnitTests.c:275::FAIL: Expected 3 Was 0
               "^[ \t]*TEST([A-Za-z0-9_]+, [A-Za-z0-9_]+)[ \t]*\\([A-Za-z0-9_/.]+\\):\\([0-9]+\\):"
                 1 2)
               )


;; forge settings
;;(load-file (concat emacs-dir "local.sds.gitlab.el"))
