(require 'auto-complete)

(defvar ac-c++-sources
  '(ac-source-c++-keywords))

(ac-define-dictionary-source
 ac-source-c++-keywords
 '("and" "bool" "compl" "do" "export" "goto" "namespace" "or_eq" "return"
   "struct" "try" "using" "xor" "and_eq" "break" "const" "double" "extern"
   "if" "new" "private" "short" "switch" "typedef" "virtual" "xor_eq" "asm"
   "case" "const_cast" "dynamic_cast" "false" "inline" "not" "protected" 
   "signed" "template" "typeid" "void" "auto" "catch" "continue" "else" 
   "float" "int" "not_eq" "public" "sizeof" "this" "typename" "volatile"
   "bitand" "char" "default" "enum" "for" "long" "operator" "register"
   "static" "throw" "union" "wchar_t" "bitor" "class" "delete" "explicit"
   "friend" "mutable" "or" "reinterpret_cast" "static_cast" "true" 
   "unsigned" "while"))

(defun ac-c++-setup ()
  (setq ac-sources (append ac-c++-sources ac-sources)))

(defun ac-c++-init ()
  (add-hook 'c++-mode-hook 'ac-c++-setup))

(provide 'auto-complete-cpp)
