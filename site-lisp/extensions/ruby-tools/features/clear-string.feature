Feature: Clear string
  As a ruby-tools-mode user
  I want to clear strings of their content

  Scenario: Clear single quote string
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-;"
    Then I should see "''"
    And the cursor should be between "'" and "'"

  Scenario: Clear double quote string
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to character "f"
    And I press "C-;"
    Then I should see """"
    And the cursor should be between """ and """

  Scenario: Do not clear empty single quote string
    When I insert "''"
    And I turn on ruby-mode
    And I go to character "'"
    And I press "C-;"
    Then I should see "''"
    And the cursor should be between "'" and "'"

  Scenario: Do not clear empty single quote string
    When I insert """"
    And I turn on ruby-mode
    And I go to character """
    And I press "C-;"
    Then I should see """"
    And the cursor should be between """ and """

  Scenario: Do not clear string when not on a string
    When I insert "foo(:bar)"
    And I turn on ruby-mode
    And I place the cursor between "o" and "o"
    And I press "C-;"
    Then I should see "foo(:bar)"
    And the cursor should be between "fo" and "o"
