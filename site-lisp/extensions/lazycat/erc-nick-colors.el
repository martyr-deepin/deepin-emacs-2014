(setq nick-face-list '())

;; Define the list of colors to use when coloring IRC nicks.
(setq-default erc-colors-list '("LightGrey" "SlateBlue" "DeepPink"
                                "HotPink" "DodgerBlue1" "OliveDrab2"
                                "Chartreuse" "LightCyan" "DarkMagenta"
                                "DarkKhaki" "Grey" "Pink" "FireBrick3"
                                "Chocolate3" "sienna" "Orange2"))

(defun build-nick-face-list ()
  "build-nick-face-list builds a list of new faces using the
foreground colors specified in erc-colors-list.  The nick faces
created here will be used to format IRC nicks."
  (setq i -1)
  (setq nick-face-list
        (mapcar
         (lambda (COLOR)
           (setq i (1+ i))
           (list (custom-declare-face
                  (make-symbol (format "erc-nick-face-%d" i))
                  (list (list t (list :foreground COLOR)))
                  (format "Nick face %d" i))))
         erc-colors-list)))

(defun my-insert-modify-hook ()
  "This insert-modify hook looks for nicks in new messages and
computes md5(nick) and uses substring(md5_value, 0, 4) mod (length
nick-face-list) to index the face list and produce the same face for a
given nick each time it is seen.  We get a lot of collisions this way,
unfortunately, but it's better than some other methods I tried.
Additionally, if you change the order or size of the erc-colors-list,
you'll change the colors used for nicks."
  (if (null nick-face-list) (build-nick-face-list))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
        (let ((nick (match-string 1)))
          (put-text-property (match-beginning 1) (match-end 1)
                             'face (nth
                                    (mod (string-to-number
                                          (substring (md5 nick) 0 4) 16)
                                         (length nick-face-list))
                                    nick-face-list))))))

;; This adds the ERC message insert hook.
(add-hook 'erc-insert-modify-hook 'my-insert-modify-hook)

(provide 'erc-nick-colors)
