(define-minor-mode rails-rspec-fixture-minor-mode
  "Minor mode for RubyOnRails rspec fixtures."
  :lighter " RSpecFixture"
  :keymap (rails-model-layout:keymap :rspec-fixture)
  (setq rails-primary-switch-func 'rails-model-layout:switch-to-rspec-model)
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-rspec-fixture-minor-mode)