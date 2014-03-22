Feature: Ruby Tools
  As an Emacs user and Ruby programmer
  I use ruby tools mode to be more productive

  Scenario: Start automatically when starting ruby-mode
    Given I turn on ruby-mode
    Then ruby-tools-mode should be active
