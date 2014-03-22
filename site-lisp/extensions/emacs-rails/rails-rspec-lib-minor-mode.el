(define-minor-mode rails-rspec-lib-minor-mode
  "Minor mode for RubyOnRails lib rspec."
  :lighter " LibRSpec"
  :keymap (let ((map (rails-lib-layout:keymap :rspec-lib)))
            (define-key map rails-minor-mode-test-current-method-key 'rails-spec:run-this-spec)
            (define-key map [menu-bar rails-lib-layout run] '("RSPec current method" . rails-spec:run-this-spec))
            map)
  (setq rails-primary-switch-func 'rails-lib-layout:switch-to-lib)
  (setq rails-secondary-switch-func 'rails-lib-layout:menu))

(provide 'rails-rspec-lib-minor-mode)
