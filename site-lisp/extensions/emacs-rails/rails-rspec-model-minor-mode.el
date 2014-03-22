(define-minor-mode rails-rspec-model-minor-mode
  "Minor mode for RubyOnRails models rspec."
  :lighter " ModelRSpec"
  :keymap (let ((map (rails-model-layout:keymap :rspec-model)))
            (define-key map rails-minor-mode-test-current-method-key 'rails-spec:run-this-spec)
            (define-key map [menu-bar rails-model-layout run] '("RSpec current method" . rails-spec:run-this-spec))
            map)
  (setq rails-primary-switch-func (lambda()
                                    (interactive)
                                    (if (rails-core:mailer-p (rails-core:current-model))
                                        (rails-model-layout:switch-to-mailer)
                                      (rails-model-layout:switch-to-model))))
  (setq rails-secondary-switch-func 'rails-model-layout:menu))

(provide 'rails-rspec-model-minor-mode)
