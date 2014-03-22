Feature: Interpolation
  As a ruby-tools-mode user
  I want to interpolate in double quote strings

  Scenario: Interpolate when in double quote string
    When I insert ""foo""
    And I turn on ruby-mode
    And I place the cursor between "f" and "o"
    And I press "#"
    Then I should see ""f#{}oo""
    And the cursor should be between "{" and "}"

  Scenario: Interpolate when in double quote string at the beginning
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to the front of the word "foo"
    And I press "#"
    Then I should see ""#{}foo""
    And the cursor should be between "{" and "}"

  Scenario: Interpolate when in double quote string at the end
    When I insert ""foo""
    And I turn on ruby-mode
    And I go to the end of the word "foo"
    And I press "#"
    Then I should see ""foo#{}""
    And the cursor should be between "{" and "}"

  Scenario: Interpolate when in shell command
    When I insert "`foo`"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "#"
    Then I should see "`f#{}oo`"
    And the cursor should be between "{" and "}"

  Scenario: Interpolate when in shell command at the beginning
    When I insert "`foo`"
    And I turn on ruby-mode
    And I go to the front of the word "foo"
    And I press "#"
    Then I should see "`#{}foo`"
    And the cursor should be between "{" and "}"

  Scenario: Interpolate when in shell command at the end
    When I insert "`foo`"
    And I turn on ruby-mode
    And I go to the end of the word "foo"
    And I press "#"
    Then I should see "`foo#{}`"
    And the cursor should be between "{" and "}"

  # TODO:
  #   Can not test this because of bug in Ecukes: See:
  #   https://github.com/rejeep/ecukes/issues/58
  #
  # Scenario: Interpolate when in percent syntax string
  #   When I insert "%(foo)"
  #   And I turn on ruby-mode
  #   And I go to point "4"
  #   And I press "#"
  #   Then I should see "%(f#{}oo)"
  #   And the cursor should be between "{" and "}"
  #
  # Scenario: Interpolate when in percent syntax string at the beginning
  #   When I insert "%(foo)"
  #   And I turn on ruby-mode
  #   And I go to point "3"
  #   And I press "#"
  #   Then I should see "%(#{}foo)"
  #   And the cursor should be between "{" and "}"
  #
  # Scenario: Interpolate when in percent syntax string at the end
  #   When I insert "%(foo)"
  #   And I turn on ruby-mode
  #   And I go to point "6"
  #   And I press "#"
  #   Then I should see "%(foo#{})"
  #   And the cursor should be between "{" and "}"

  Scenario: Do not interpolate when in single quote string
    When I insert "'foo'"
    And I turn on ruby-mode
    And I go to character "f"
    And I press "#"
    Then I should see "'f#oo'"

  Scenario: Do not interpolate outside strings
    When I insert:
    """
    it "should ignore ~user directories if the user doesn't exist" do
    
    end

    describe "on POSIX systems" do
    """
    And I turn on ruby-mode
    And I go to line "2"
    And I press "#"
    Then I should not see:
    """
    it "should ignore ~user directories if the user doesn't exist" do
    #{}
    end

    describe "on POSIX systems" do
    """
    But I should see:
    """
    it "should ignore ~user directories if the user doesn't exist" do
    #
    end

    describe "on POSIX systems" do
    """

  Scenario: Interpolate selected region
    When I insert ""foo bar baz""
    And I turn on ruby-mode
    And I select "bar"
    And I press "#"
    Then I should see ""foo #{bar} baz""
    And the cursor should be after "}"

  Scenario: Interpolate selected region (from left to right)
    When I insert ""foo bar baz""
    And I turn on ruby-mode
    And I go to point "6"
    And I press "C-SPC"
    And I press "C-u 3 C-f"
    And I press "#"
    Then I should see ""foo #{bar} baz""
    And the cursor should be after "}"

  Scenario: Interpolate selected region from (right to left)
    When I insert ""foo bar baz""
    And I turn on ruby-mode
    And I go to point "9"
    And I press "C-SPC"
    And I press "C-u 3 C-b"
    And I press "#"
    Then I should see ""foo #{bar} baz""
    And the cursor should be after "}"
