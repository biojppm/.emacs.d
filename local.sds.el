(if (= (display-pixel-height) 1080)
    ;; Full HD
    (set-face-attribute 'default nil :font "Inconsolata-13")
  ;; 4K
  (set-face-attribute 'default nil :font "Inconsolata-18")
  )

;; TEST(ValidationUnitTestsJson, Sony_Json_getRootFailsOnEmptyTree)./ValidationTests/UnitTests/ValidationUnitTests.c:275::FAIL: Expected 3 Was 0
(add-to-list 'compilation-error-regexp-alist 'unity_failed_tests)
(add-to-list 'compilation-error-regexp-alist-alist
             '(unity_failed_tests
               "^[ \t]*TEST([A-Za-z0-9_]+, [A-Za-z0-9_]+)[ \t]*\\([A-Za-z0-9_/.]+\\):\\([0-9]+\\):"
                 1 2)
               )
