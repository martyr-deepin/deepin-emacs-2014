;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test-cases

(load-file (concat (file-name-directory load-file-name) "../emmet-mode.el"))

(emmet-defparameter *emmet-test-cases* nil)

(defun emmet-test-cases (&rest args)
  (let ((cmd (car args)))
    (flet
        ((run-cases
          (fn cases)
          (loop for c in cases
                for i to (1- (length cases)) do
                (let ((expected (cdr c))
                      (actual (funcall fn (car c))))
                  (when (not (equal expected actual))
                    (princ
                     (concat "*** [FAIL] | \"" name "\" " (number-to-string i) "\n\n"
                             (format "%s" (car c)) "\t=>\n\n"
                             "Expected\n" (format "%s" expected) "\n\nActual\n" (format "%s" actual) "\n\n"))
                    (return 'fail))))))
      (cond ((eql cmd 'assign)
             (let ((name (cadr args))
                   (fn   (caddr args))
                   (defs (cadddr args)))
               (let ((place (assoc name *emmet-test-cases*)))
                 (if place
                     (setf (cdr place) (cons fn defs))
                   (setq *emmet-test-cases*
                         (cons (cons name (cons fn defs)) *emmet-test-cases*))))))
            (t
             (loop for test in (reverse *emmet-test-cases*) do
                   (let ((name  (symbol-name (car test)))
                         (fn    (cadr test))
                         (cases (cddr test)))
                     (let ((res (run-cases fn cases)))
                       (if (not (eql res 'fail))
                           (princ (concat "    [PASS] | \"" name "\" "
                                          (number-to-string (length cases)) " tests.\n")))))))))))

(defmacro define-emmet-transform-test-case (name fn &rest tests)
  `(emmet-test-cases 'assign ',name
                         ,fn
                         ',(loop for x on tests by #'cddr collect
                                 (cons (car x)
                                       (emmet-join-string (cadr x)
                                                              "\n")))))

(defmacro define-emmet-transform-html-test-case (name &rest tests)
  `(define-emmet-transform-test-case ,name
     'emmet-html-transform
     ,@tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML-abbrev tests

(define-emmet-transform-html-test-case Tags
  "a"                      ("<a href=\"\"></a>")
  "a.x"                    ("<a class=\"x\" href=\"\"></a>")
  "a#q.x"                  ("<a id=\"q\" class=\"x\" href=\"\"></a>")
  "a#q.x.y.z"              ("<a id=\"q\" class=\"x y z\" href=\"\"></a>")
  "#q"                     ("<div id=\"q\"></div>")
  ".x"                     ("<div class=\"x\"></div>")
  "#q.x"                   ("<div id=\"q\" class=\"x\"></div>")
  "#q.x.y.z"               ("<div id=\"q\" class=\"x y z\"></div>"))

(define-emmet-transform-html-test-case Empty-tags
  "a/"                     ("<a href=\"\"/>")
  "a/.x"                   ("<a class=\"x\" href=\"\"/>")
  "a/#q.x"                 ("<a id=\"q\" class=\"x\" href=\"\"/>")
  "a/#q.x.y.z"             ("<a id=\"q\" class=\"x y z\" href=\"\"/>"))

(define-emmet-transform-html-test-case Self-closing-tags
  "input type=text"        ("<input type=\"text\" name=\"\" value=\"\"/>")
  "img"                    ("<img src=\"\" alt=\"\"/>")
  "img>metadata/*2"        ("<img src=\"\" alt=\"\">"
                            "    <metadata/>"
                            "    <metadata/>"
                            "</img>"))

(define-emmet-transform-html-test-case Siblings
  "a+b"                    ("<a href=\"\"></a>"
                            "<b></b>")
  "a+b+c"                  ("<a href=\"\"></a>"
                            "<b></b>"
                            "<c></c>")
  "a.x+b"                  ("<a class=\"x\" href=\"\"></a>"
                            "<b></b>")
  "a#q.x+b"                ("<a id=\"q\" class=\"x\" href=\"\"></a>"
                            "<b></b>")
  "a#q.x.y.z+b"            ("<a id=\"q\" class=\"x y z\" href=\"\"></a>"
                            "<b></b>")
  "a#q.x.y.z+b#p.l.m.n"    ("<a id=\"q\" class=\"x y z\" href=\"\"></a>"
                            "<b id=\"p\" class=\"l m n\"></b>"))

(define-emmet-transform-html-test-case Tag-expansion
  "table+"                 ("<table>"
                            "    <tr>"
                            "        <td></td>"
                            "    </tr>"
                            "</table>")
  "dl+"                    ("<dl>"
                            "    <dt></dt>"
                            "    <dd></dd>"
                            "</dl>")
  "ul+"                    ("<ul>"
                            "    <li></li>"
                            "</ul>")
  "ul++ol+"                ("<ul>"
                            "    <li></li>"
                            "</ul>"
                            "<ol>"
                            "    <li></li>"
                            "</ol>")
  "ul#q.x.y m=l+"          ("<ul id=\"q\" class=\"x y\" m=\"l\">"
                            "    <li></li>"
                            "</ul>"))

(define-emmet-transform-html-test-case Parent-child
  "a>b"                    ("<a href=\"\"><b></b></a>")
  "a>b>c"                  ("<a href=\"\"><b><c></c></b></a>")
  "a.x>b"                  ("<a class=\"x\" href=\"\"><b></b></a>")
  "a#q.x>b"                ("<a id=\"q\" class=\"x\" href=\"\"><b></b></a>")
  "a#q.x.y.z>b"            ("<a id=\"q\" class=\"x y z\" href=\"\"><b></b></a>")
  "a#q.x.y.z>b#p.l.m.n"    ("<a id=\"q\" class=\"x y z\" href=\"\"><b id=\"p\" class=\"l m n\"></b></a>")
  "#q>.x"                  ("<div id=\"q\"><div class=\"x\"></div></div>")
  "a>b+c"                  ("<a href=\"\">"
                            "    <b></b>"
                            "    <c></c>"
                            "</a>")
  "a>b+c>d"                ("<a href=\"\">"
                            "    <b></b>"
                            "    <c><d></d></c>"
                            "</a>"))

(define-emmet-transform-html-test-case Climb-up
  "a>b>c^d"                ("<a href=\"\">"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>")
  "a>b>c^^d"               ("<a href=\"\"><b><c></c></b></a>"
                            "<d></d>")
  "a*2>b*2>c^d"            ("<a href=\"\">"
                            "    <b><c></c></b>"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>"
                            "<a href=\"\">"
                            "    <b><c></c></b>"
                            "    <b><c></c></b>"
                            "    <d></d>"
                            "</a>")

  "div+a>p>span{foo}+em>b^^^p"
  ("<div></div>"
   "<a href=\"\">"
   "    <p>"
   "        <span>foo</span>"
   "        <em><b></b></em>"
   "    </p>"
   "</a>"
   "<p></p>")

  "div+div>p>span+em^blockquote{foo}"
  ("<div></div>"
   "<div>"
   "    <p>"
   "        <span></span>"
   "        <em></em>"
   "    </p>"
   "    <blockquote>"
   "        foo"
   "    </blockquote>"
   "</div>"))

(define-emmet-transform-html-test-case Multiplication
  "a*1"                    ("<a href=\"\"></a>")
  "a*2"                    ("<a href=\"\"></a>"
                            "<a href=\"\"></a>")
  "a/*2"                   ("<a href=\"\"/>"
                            "<a href=\"\"/>")
  "a*2+b*2"                ("<a href=\"\"></a>"
                            "<a href=\"\"></a>"
                            "<b></b>"
                            "<b></b>")
  "a*2>b*2"                ("<a href=\"\">"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>"
                            "<a href=\"\">"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>")
  "a>b*2"                  ("<a href=\"\">"
                            "    <b></b>"
                            "    <b></b>"
                            "</a>")
  "a#q.x>b#q.x*2"          ("<a id=\"q\" class=\"x\" href=\"\">"
                            "    <b id=\"q\" class=\"x\"></b>"
                            "    <b id=\"q\" class=\"x\"></b>"
                            "</a>")
  "a#q.x>b/#q.x*2"         ("<a id=\"q\" class=\"x\" href=\"\">"
                            "    <b id=\"q\" class=\"x\"/>"
                            "    <b id=\"q\" class=\"x\"/>"
                            "</a>"))

(define-emmet-transform-html-test-case Numbering
  "a.$x*3"                 ("<a class=\"1x\" href=\"\"></a>"
                            "<a class=\"2x\" href=\"\"></a>"
                            "<a class=\"3x\" href=\"\"></a>")
  "ul>li.item$*3"          ("<ul>"
                            "    <li class=\"item1\"></li>"
                            "    <li class=\"item2\"></li>"
                            "    <li class=\"item3\"></li>"
                            "</ul>")
  "ul>li.item$$$*3"        ("<ul>"
                            "    <li class=\"item001\"></li>"
                            "    <li class=\"item002\"></li>"
                            "    <li class=\"item003\"></li>"
                            "</ul>")
  "ul>li.item$@-*2"        ("<ul>"
                            "    <li class=\"item2\"></li>"
                            "    <li class=\"item1\"></li>"
                            "</ul>")
  "ul>li.item$@-1000*2"    ("<ul>"
                            "    <li class=\"item1001\"></li>"
                            "    <li class=\"item1000\"></li>"
                            "</ul>")
  "a.$*2>b.$$@-*3"         ("<a class=\"1\" href=\"\">"
                            "    <b class=\"03\"></b>"
                            "    <b class=\"02\"></b>"
                            "    <b class=\"01\"></b>"
                            "</a>"
                            "<a class=\"2\" href=\"\">"
                            "    <b class=\"03\"></b>"
                            "    <b class=\"02\"></b>"
                            "    <b class=\"01\"></b>"
                            "</a>")

  "(div>(a#id$$*2)+b.c$@-3+c#d$)*2"
  ("<div>"
   "    <a id=\"id01\" href=\"\"></a>"
   "    <a id=\"id02\" href=\"\"></a>"
   "    <b class=\"c4\"></b>"
   "    <c id=\"d1\"></c>"
   "</div>"
   "<div>"
   "    <a id=\"id01\" href=\"\"></a>"
   "    <a id=\"id02\" href=\"\"></a>"
   "    <b class=\"c3\"></b>"
   "    <c id=\"d2\"></c>"
   "</div>")

  "a:b$$$-c$$@-:d$@-3-e$$@100/#b.c$*3"
  ("<a:b001-c03:d5-e100 id=\"b\" class=\"c1\"/>"
   "<a:b002-c02:d4-e101 id=\"b\" class=\"c2\"/>"
   "<a:b003-c01:d3-e102 id=\"b\" class=\"c3\"/>")

  "ul>li.item${name: item$ price: $\\$}*3"
  ("<ul>"
   "    <li class=\"item1\">name: item1 price: 1$</li>"
   "    <li class=\"item2\">name: item2 price: 2$</li>"
   "    <li class=\"item3\">name: item3 price: 3$</li>"
   "</ul>"))

(define-emmet-transform-html-test-case Properties
  "a x"                    ("<a href=\"\" x=\"\"></a>")
  "a x="                   ("<a href=\"\" x=\"\"></a>")
  "a x=\"\""               ("<a href=\"\" x=\"\"></a>")
  "a x=y"                  ("<a href=\"\" x=\"y\"></a>")
  "a x=\"y\""              ("<a href=\"\" x=\"y\"></a>")
  "a x=\"()\""             ("<a href=\"\" x=\"()\"></a>")
  "a x m"                  ("<a href=\"\" x=\"\" m=\"\"></a>")
  "a x= m=\"\""            ("<a href=\"\" x=\"\" m=\"\"></a>")
  "a x=y m=l"              ("<a href=\"\" x=\"y\" m=\"l\"></a>")
  "a/ x=y m=l"             ("<a href=\"\" x=\"y\" m=\"l\"/>")
  "a#foo x=y m=l"          ("<a id=\"foo\" href=\"\" x=\"y\" m=\"l\"></a>")
  "a.foo x=y m=l"          ("<a class=\"foo\" href=\"\" x=\"y\" m=\"l\"></a>")
  "a#foo.bar.mu x=y m=l"   ("<a id=\"foo\" class=\"bar mu\" href=\"\" x=\"y\" m=\"l\"></a>")
  "a/#foo.bar.mu x=y m=l"  ("<a id=\"foo\" class=\"bar mu\" href=\"\" x=\"y\" m=\"l\"/>")
  "a x=y+b"                ("<a href=\"\" x=\"y\"></a>"
                            "<b></b>")
  "a x=y+b x=y"            ("<a href=\"\" x=\"y\"></a>"
                            "<b x=\"y\"></b>")
  "a x=y>b"                ("<a href=\"\" x=\"y\"><b></b></a>")
  "a x=y>b x=y"            ("<a href=\"\" x=\"y\"><b x=\"y\"></b></a>")
  "a x=y>b x=y+c x=y"      ("<a href=\"\" x=\"y\">"
                            "    <b x=\"y\"></b>"
                            "    <c x=\"y\"></c>"
                            "</a>"))

(define-emmet-transform-html-test-case Parentheses
  "(a)"                    ("<a href=\"\"></a>")
  "(a)+(b)"                ("<a href=\"\"></a>"
                            "<b></b>")
  "a>(b)"                  ("<a href=\"\"><b></b></a>")
  "(a>b)>c"                ("<a href=\"\"><b></b></a>")
  "(a>b)+c"                ("<a href=\"\"><b></b></a>"
                            "<c></c>")
  "z+(a>b)+c+k"            ("<z></z>"
                            "<a href=\"\"><b></b></a>"
                            "<c></c>"
                            "<k></k>")
  "(a)*2"                  ("<a href=\"\"></a>"
                            "<a href=\"\"></a>")
  "((a)*2)"                ("<a href=\"\"></a>"
                            "<a href=\"\"></a>")
  "((a))*2"                ("<a href=\"\"></a>"
                            "<a href=\"\"></a>")
  "(a>b)*2"                ("<a href=\"\"><b></b></a>"
                            "<a href=\"\"><b></b></a>")
  "(a+b)*2"                ("<a href=\"\"></a>"
                            "<b></b>"
                            "<a href=\"\"></a>"
                            "<b></b>"))

(define-emmet-transform-html-test-case Text
  "a{Click me}"            ("<a href=\"\">Click me</a>")
  "a>{Click me}*3"         ("<a href=\"\">"
                            "    Click me"
                            "    Click me"
                            "    Click me"
                            "</a>")
  "a{click}+b{here}"       ("<a href=\"\">click</a>"
                            "<b>here</b>")
  "a>{click}+b{here}"      ("<a href=\"\">"
                            "    click"
                            "    <b>here</b>"
                            "</a>")

  "p>{Click }+a{here}+{ to continue}"
  ("<p>"
   "    Click "
   "    <a href=\"\">here</a>"
   "     to continue"
   "</p>")

  "p{Click }+a{here}+{ to continue}"
  ("<p>Click </p>"
   "<a href=\"\">here</a>"
   " to continue")

  "xxx#id.cls p=1{txt}"
  ("<xxx id=\"id\" class=\"cls\" p=\"1\">txt</xxx>"))


(define-emmet-transform-html-test-case Filter-comment
  "a.b|c"                  ("<!-- .b -->"
                            "<a class=\"b\" href=\"\"></a>"
                            "<!-- /.b -->")
  "#a>.b|c"                ("<!-- #a -->"
                            "<div id=\"a\">"
                            "    <!-- .b -->"
                            "    <div class=\"b\"></div>"
                            "    <!-- /.b -->"
                            "</div>"
                            "<!-- /#a -->"))

(define-emmet-transform-html-test-case Filter-HAML
  "a|haml"                 ("%a")
  "a#q.x.y.z|haml"         ("%a#q.x.y.z")
  "a#q.x x=y m=l|haml"     ("%a#q.x{:x => \"y\", :m => \"l\"}")
  "div|haml"               ("%div")
  "div.footer|haml"        (".footer")
  ".footer|haml"           (".footer")

  "p>{This is haml}*2+a href=#+br|haml"
  ("%p"
   "    This is haml"
   "    This is haml"
   "    %a{:href => \"#\"}"
   "    %br"))

(define-emmet-transform-html-test-case Filter-Hiccup
  "a|hic"                  ("[:a]")
  "a#q.x.y.z|hic"          ("[:a#q.x.y.z]")
  "a#q.x x=y m=l|hic"      ("[:a#q.x {:x \"y\", :m \"l\"}]")
  ".footer|hic"            ("[:div.footer]")
  "p>a href=#+br|hic"      ("[:p"
                            "    [:a {:href \"#\"}]"
                            "    [:br]]")

  "#q>(a*2>b{x})+p>{m}+b|hic"
  ("[:div#q"
   "    [:a [:b \"x\"]]"
   "    [:a [:b \"x\"]]"
   "    [:p"
   "        \"m\""
   "        [:b]]]"))

(define-emmet-transform-html-test-case Filter-escape
  "script src=&quot;|e"    ("&lt;script src=\"&amp;quot;\"&gt;"
                            "&lt;/script&gt;"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSS-abbrev tests

(defmacro define-emmet-unit-test-case (name fn &rest tests)
  `(emmet-test-cases 'assign ',name
                         ,fn
                         ',(loop for x on tests by #'cddr collect
                                 (cons (car x) (cadr x)))))

(define-emmet-unit-test-case CSS-toknize
  #'emmet-css-toknize
  ""                     ("")
  "abc"                  ("abc")
  "abc+"                 ("abc+")
  "abc+cde"              ("abc" "cde")
  "abc++cde"             ("abc+" "cde")
  "abc+cde+"             ("abc" "cde+")
  "abc++cde+"            ("abc+" "cde+")
  "ab:c+0p0x#aa+p0+cde+" ("ab:c+0p0x#aa" "p0" "cde+")
  "ab+#0+p+#c+x++cde+"   ("ab+#0" "p+#c" "x+" "cde+")
  "abc def"              ("abc def")
  "-abc+-xyz"            ("-abc" "-xyz")
  "-abc+-10"             ("-abc+-10"))

(define-emmet-unit-test-case CSS-parse-arg-number
  #'emmet-css-arg-number
  ""                     (error "expected css number arguments")
  "0"                    (("0" "px") . "")
  "0-1-2"                (("0" "px") . "1-2")
  "-100"                 (("-100" "px") . "")
  "-10e-20"              (("-10" "em") . "-20")
  "35p#a"                (("35" "%") . "#a")
  " 0p"                  (("0" "%") . ""))

(define-emmet-unit-test-case CSS-parse-arg-color
  #'emmet-css-arg-color
  ""                     (error "expected css color argument")
  "abc"                  (error "expected css color argument")
  "#x"                   (error "expected css color argument")
  "#a"                   ("#aaa" . "")
  "#09"                  ("#090909" . "")
  "#3D5-2"               ("#3D5" . "-2")
  "#1a2B-3"              ("#1a2B1a" . "-3")
  "#1A2b3x"              ("#1A2b31" . "x")
  "#1a2B3Cx"             ("#1a2B3C" . "x")
  "#1A2B3C4D-2"          ("#1A2B3C" . "4D-2")
  " #abc"                ("#abc" . ""))

(define-emmet-unit-test-case CSS-parse-arg-something
  #'emmet-css-arg-something
  ""                         (error "expected css argument")
  "abc"                      ("abc" . "")
  "abc def"                  ("abc" . " def")
  "url(http://abc.com) auto" ("url(http://abc.com)" . " auto"))

(define-emmet-unit-test-case CSS-parse-args
  #'emmet-css-parse-args
  ""                     nil
  "1-2--3-4"             (("1" "px") ("2" "px") ("-3" "px") ("4" "px"))
  "-10-2p-30#abc"        (("-10" "px") ("2" "%") ("-30" "px") "#abc")
  "1p2x3-4e5x"           (("1" "%") ("2" "ex") ("3" "px") ("4" "em") ("5" "ex"))
  "#abc#de#f-3"          ("#abc" "#dedede" "#fff" ("-3" "px")))

(define-emmet-unit-test-case CSS-split-vendor-prefixes
  #'emmet-css-split-vendor-prefixes
  ""                     ("" nil)
  "-abc"                 ("abc" auto)
  "-wmso-abc"            ("abc" (119 109 115 111)))

(define-emmet-unit-test-case CSS-exprs
  #'emmet-css-expr
  ""                     (("" nil nil))
  "cl:l+ov:h+bg+"        (("cl:l" nil nil) ("ov:h" nil nil) ("bg+" nil nil))
  "m10-auto!"            (("m" nil t ("10" "px") "auto"))
  "bg++c!"               (("bg+" nil nil) ("c" nil t))
  "m+0-10-10--20!+p0-0"  (("m+" nil t ("0" "px") ("10" "px") ("10" "px") ("-20" "px"))
                          ("p" nil nil ("0" "px") ("0" "px")))
  "bg+#abc#bc#c-3!"      (("bg+" nil t "#abc" "#bcbcbc" "#ccc" ("-3" "px"))))

(defmacro define-emmet-transform-css-test-case (name &rest tests)
  `(define-emmet-transform-test-case ,name
     'emmet-css-transform
     ,@tests))

(define-emmet-transform-css-test-case CSS-transform
  ;; supplying values with units
  "m10"                    ("margin: 10px;")
  "m1.5"                   ("margin: 1.5em;")
  "m1.5ex"                 ("margin: 1.5ex;")
  "m1.5x"                  ("margin: 1.5ex;")
  "m10foo"                 ("margin: 10foo;")
  "m10ex20em"              ("margin: 10ex 20em;")
  "m10x20e"                ("margin: 10ex 20em;")
  "m10x-5"                 ("margin: 10ex -5px;")
  ;; Color values
  "c#3"                    ("color: #333;")
  "bd5#0rgb"               ("border: 5px rgb(0,0,0);")
  "bd5#20rgb"              ("border: 5px rgb(32,32,32);")
  "bd5#0s"                 ("border: 5px #000 solid;")
  "bd5#2rgbs"              ("border: 5px rgb(34,34,34) solid;")
  ;; Unitless property
  "lh2"                    ("line-height: 2;")
  "fw400"                  ("font-weight: 400;")
  ;;
  "m0+p0-1p2e3x"           ("margin: 0px;"
                            "padding: 0px 1% 2em 3ex;")
  "p!+m10e!+f"             ("padding:  !important;"
                            "margin: 10em !important;"
                            "font: ;")
  "fs"                     ("font-style: italic;")
  "xxxxxx 0 auto 0e auto!" ("xxxxxx: 0px auto 0em auto !important;")
  "p auto+m auto+bg+#F00 x.jpg 10 10 repeat-x"
                           ("padding: auto;"
                            "margin: auto;"
                            "background: #F00 url(x.jpg) 10px 10px repeat-x;")
  "-bdrs"                  ("-webkit-border-radius: ;"
                            "-moz-border-radius: ;"
                            "border-radius: ;")
  "-super-foo"             ("-webkit-super-foo: ;"
                            "-moz-super-foo: ;"
                            "-ms-super-foo: ;"
                            "-o-super-foo: ;"
                            "super-foo: ;")
  "-wm-trf"                ("-webkit-transform: ;"
                            "-moz-transform: ;"
                            "transform: ;")
  "@m print 1"             ("@media print {"
                            "	1px"
                            "}")
  "@i http://github.com/smihica/index.css"
                           ("@import url(http://github.com/smihica/index.css);")
  )

;; start
(emmet-test-cases)
