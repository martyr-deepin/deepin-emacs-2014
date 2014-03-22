Feature: String To Symbol
  As a ruby-tools-mode user
  I want to turn strings to symbols

  Scenario: Turn single quote string to symbol
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-:"
    Then I should see ":foo"
    And the cursor should be between "f" and "oo"

  Scenario: Turn double quote string to symbol
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-:"
    Then I should see ":foo"
    And the cursor should be between "f" and "oo"

  Scenario: Turn single quote string in method argument to symbol
    When I insert "foo('bar')"
    And I turn on ruby-mode
    And I go to character "b"
    And I press "C-:"
    Then I should see "foo(:bar)"
    And the cursor should be between "b" and "ar"

  Scenario: Turn double quote string in method argument to symbol
    When I insert "foo("bar")"
    And I turn on ruby-mode
    And I go to character "b"
    And I press "C-:"
    Then I should see "foo(:bar)"
    And the cursor should be between "b" and "ar"

  Scenario: Turn single quote string with underscores to symbol
    When I insert "'foo_bar'"
    And I turn on ruby-mode
    And I place the cursor between "o" and "_"
    And I press "C-:"
    Then I should see ":foo_bar"
    And the cursor should be between "foo" and "_bar"

  Scenario: Turn string to symbol when at beginning of string
    When I insert "'foo'"
    And I turn on ruby-mode
    # And I go to the front of character "f"
    And I go to character "'"
    # And I go in front of the c
    # And I place the cursor between "o" and "o"
    And I press "C-:"
    Then I should see ":foo"
    And the cursor should be before "foo"

  Scenario: Turn string to symbol when at end of string
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to the end of the word "foo"
    And I press "C-:"
    Then I should see ":foo"
    And the cursor should be after "foo"

  Scenario: Do not turn symbol to string when not on a string
    When I insert "foo('bar')"
    And I turn on ruby-mode
    And I place the cursor between "o" and "o"
    And I press "C-:"
    Then I should see "foo('bar')"
    And the cursor should be between "fo" and "o"

  Scenario: Do not turn symbol to string when invalid symbol characters in string
    When I insert "'foo bar'"
    And I turn on ruby-mode
    And I place the cursor between "o" and "o"
    And I press "C-:"
    Then I should see "'foo bar'"
    And the cursor should be between "fo" and "o b"

 Scenario: Turn empty string to symbol
    When I insert "''"
    And I turn on ruby-mode
    And I place the cursor between "'" and "'"
    And I press "C-:"
    Then I should see ":"
    And the cursor should be after ":"

 Scenario: Turn one character string to symbol
    When I insert "'s'"
    And I turn on ruby-mode
    And I go to character "s"
    And I press "C-:"
    Then I should see ":s"
    And the cursor should be after "s"
