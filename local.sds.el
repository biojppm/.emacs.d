(message "SDS environment config")

(message "display-pixel-height is %s" (display-pixel-height))

(if (or (= (display-pixel-height) 1080) (= (display-pixel-height) 1200))
    ;; Full HD
    (progn
      (message "setting font for FullHD")
      (set-face-attribute 'default nil :font "Inconsolata-10")
      )
  ;; 4K
  (progn
    (message "setting font for 4k")
    (set-face-attribute 'default nil :font "Inconsolata-14")
    )
  )

;; TEST(ValidationUnitTestsJson, Sony_Json_getRootFailsOnEmptyTree)./ValidationTests/UnitTests/ValidationUnitTests.c:275::FAIL: Expected 3 Was 0
(add-to-list 'compilation-error-regexp-alist 'unity_failed_tests)
(add-to-list 'compilation-error-regexp-alist-alist
             '(unity_failed_tests
               "^[ \t]*TEST([A-Za-z0-9_]+, [A-Za-z0-9_]+)[ \t]*\\([A-Za-z0-9_/.]+\\):\\([0-9]+\\):"
                 1 2)
               )

