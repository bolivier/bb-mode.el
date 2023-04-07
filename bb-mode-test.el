(load-file "bb-mode.el")

(ert-deftest mvp ()
  (should (equal "hello" "hello")))

(provide 'bb-mode-test)
