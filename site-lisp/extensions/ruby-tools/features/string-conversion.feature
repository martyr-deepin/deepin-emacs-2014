Feature: String conversion
  As a ruby-tools-mode user
  I want to turn single quote strings to double quote string and reverse

  Scenario: Turn single quote string to double quote string
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-""
    Then I should see ""foo""
    And the cursor should be between "f" and "oo"

  Scenario: Turn single quote string to double quote string in method call
    When I insert "foo('bar')"
    And I turn on ruby-mode
    And I go to character "b"
    And I press "C-""
    Then I should see "foo("bar")"
    And the cursor should be between "b" and "ar"

  Scenario: Do not turn to single quote string when on single quote string
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-'"
    Then I should see "'foo'"
    And the cursor should be between "f" and "oo"

  Scenario: Turn double quote string to single quote string
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-'"
    Then I should see "'foo'"
    And the cursor should be between "f" and "oo"

  Scenario: Turn double quote string to single quote string in method call
    When I insert "foo("bar")"
    And I turn on ruby-mode
    And I go to character "b"
    And I press "C-'"
    Then I should see "foo('bar')"
    And the cursor should be between "b" and "ar"

  Scenario: Do not turn to double quote string when on double quote string
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-""
    Then I should see ""foo""
    And the cursor should be between "f" and "oo"

  Scenario: Turn empty single quote string to empty double quote string
    When I insert "''"
    And I turn on ruby-mode
    And I go to character "'"
    And I press "C-""
    Then I should see """"
    And the cursor should be between """ and """

  Scenario: Turn empty double quote string to empty single quote string
    When I insert """"
    And I turn on ruby-mode
    And I go to character """
    And I press "C-'"
    Then I should see "''"
    And the cursor should be between "'" and "'"

  Scenario: Turn single quote string with quotes to double quote string
    When I insert "'foo \' bar'"
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-""
    Then I should see ""foo ' bar""
    And the cursor should be between ""fo" and "o ' bar""

  Scenario: Turn single quote string with double quote inside to double quote string
    When I insert "'foo " bar'"
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-""
    Then I should see ""foo \" bar""
    And the cursor should be between ""fo" and "o \" bar""

  Scenario: Turn double quote string with quotes to single quote string
    When I insert ""foo \" bar""
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-'"
    Then I should see "'foo " bar'"
    And the cursor should be between "'fo" and "o " bar'"

  Scenario: Turn double quote string with single quote inside to single quote string
    When I insert ""foo ' bar""
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-'"
    Then I should see "'foo \' bar'"
    And the cursor should be between "'fo" and "o \' bar'"

  Scenario: Convert string with the number 1 in it
    When I insert ""foo 1 bar""
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-'"
    Then I should see "'foo 1 bar'"
    And the cursor should be between "'fo" and "o 1 bar'"

  Scenario: Convert only the string relevant to the cursor position
    When I insert ""foo" "bar""
    And I turn on ruby-mode
    And I go to character "o"
    And I press "C-'"
    Then I should see "'foo' "bar""
    And the cursor should be between "'fo" and "o' "bar""
