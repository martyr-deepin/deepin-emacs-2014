(require 'behave)

(context "Testing if string is camelized"
  (tag rails rails-lib camelized-p)
  (specify "should be nil if there are no capital characters"
    (expect (camelized-p "horse") equal nil))
  (specify "should be 0 for a word with first character being a capital"
    (expect (camelized-p "Horse") equal 0))
  (specify "should be nil if the first character is not a capital"
    (expect (camelized-p "dragonFly") equal nil))
  (specify "should be 0 if there are multiple capitals"
    (expect (camelized-p "DragonFly") equal 0))
  (specify "should be 0 if it has numbers"
    (expect (camelized-p "DragonFly69") equal 0))
  (specify "should be nil if all its characters are capital"
    (expect (camelized-p "DONKEY") equal nil)))

(context "Testing if string is underscored"
  (tag rails rails-lib underscored-p)
  (specify "should be nil if there is a capital character"
    (expect (underscored-p "horSe") equal nil))
  (specify "should be 0 if all characters are lowercase"
    (expect (underscored-p "horse") equal 0))
  (specify "should be 0 if some characters are numbers"
    (expect (underscored-p "horse12") equal 0))
  (specify "should be 0 if some characters are underscores"
    (expect (underscored-p "dragon_fly") equal 0))
  (specify "should be nil if the first character is not a letter"
    (expect (underscored-p "5_gold_rings") equal nil))
)

(context "Decamelizing"
  (tag rails rails-lib decamelize)
  (specify "should return a string of all lower case characters unchanged"
    (expect (decamelize "horse") equal "horse"))
  (specify "should replace an initial capital with a lower case character"
    (expect (decamelize "Horse") equal "horse"))
  (specify "should insert underscores before capital letters"
    (expect (decamelize "AntEaterTongue") equal "ant_eater_tongue"))
  (specify "should insert one underscore after last character of serie of capitals"
    (expect (decamelize "SMSMessage") equal "sms_message"))
  (specify "should insert an underscore between a digit and a capital"
    (expect (decamelize "Dalmatien101Movie") equal "dalmatien101_movie"))
)
