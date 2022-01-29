(import ./delims)
(import ./locations :as loc)

(def special-names
  ["case"
   "comment" # XXX: spork/fmt doesn't have this listed
   "compif" "compwhen" "cond" "coro"
   "def" "def-" "default" "defer" "defglobal"
   "defmacro" "defmacro-" "defn" "defn-" "do"
   "each" "eachk" "eachp" "eachy" "edefer"
   "ev/do-thread" "ev/spawn" "ev/with-deadline"
   "fn" "for" "forever" "forv"
   "generate"
   "if" "if-let" "if-not" "if-with"
   "label" "let" "loop"
   "match"
   "prompt"
   "repeat"
   "seq" "short-fn"
   "try"
   "unless"
   "var" "var-" "varfn" "varglobal"
   "when" "when-let" "when-with" "while"
   "with" "with-dyns" "with-syms" "with-vars"])

(def specials-set
  (zipcoll special-names
           (array/new-filled (length special-names) true)))

# XXX: some things here may be redundant, but make things
#      slightly faster?
(defn special?
  [sym-name]
  # XXX: predicate should return true or false?
  (if (or (get specials-set sym-name)
          (peg/match ~(choice "def"
                              "if-"
                              "when-"
                              "with-")
                     sym-name))
    true
    false))

(comment

  (special? "try")
  # =>
  true

  (special? "print")
  # =>
  false

  (special? "with-feeling")
  # =>
  true

  )

(defn calc-special-indent-col
  [ctxt loc]
  (def ntype (loc/node-type ctxt))
  (def inner (loc/content ctxt))
  (def [line col] (loc/start-pos ctxt))
  (assert (= :tuple ntype)
          (string/format "Not a tuple: %p" ctxt))
  (def elder-siblings @[])
  (each item inner
    (def [i-line i-col] (loc/start-pos item))
    (when (= loc [i-line i-col])
      (break))
    (def i-ntype (loc/node-type item))
    (when (not= :whitespace i-ntype)
      (array/push elder-siblings item)))
  (def num-elder-siblings
    (length elder-siblings))
  (cond
    (zero? num-elder-siblings)
    (+ col 1)
    #
    (one? num-elder-siblings)
    (+ col 2)
    # XXX: see prose description for alternate
    (+ col 2)))

# XXX: extremely similar to `calc-special-indent-col`
(defn calc-tuple-indent-col
  [ctxt loc]
  (def ntype (loc/node-type ctxt))
  (def inner (loc/content ctxt))
  (def [line col] (loc/start-pos ctxt))
  (assert (= :tuple ntype)
          (string/format "Not a tuple: %p" ctxt))
  (def elder-siblings @[])
  (each item inner
    (def [i-line i-col] (loc/start-pos item))
    (when (= loc [i-line i-col])
      (break))
    (def i-ntype (loc/node-type item))
    (when (not= :whitespace i-ntype)
      (array/push elder-siblings item)))
  (def num-elder-siblings
    (length elder-siblings))
  (cond
    (zero? num-elder-siblings)
    (+ col 1)
    #
    (one? num-elder-siblings)
    (+ col 2)
    # XXX: see prose description for alternate
    (let [second-sibling (get elder-siblings 1)
          [_ s-col] (loc/start-pos second-sibling)]
      s-col)))

(defn calc-indent
  [ctxt loc]
  (def ntype (loc/node-type ctxt))
  (def inner (loc/content ctxt))
  (def [line col] (loc/start-pos ctxt))
  # columns are 1-based, want indentation (values start at 0)
  (dec
    (if (not= :tuple ntype)
      (cond
        (loc/mutable-container? ctxt)
        (+ col 2)
        #
        (+ col 1))
      (cond
        (loc/empty-tuple? ctxt)
        (+ col 1)
        #
        (when-let [head-sym (loc/head-sym ctxt)]
          (special? head-sym))
        (calc-special-indent-col ctxt loc)
        #
        (calc-tuple-indent-col ctxt loc)))))

(comment

  (calc-indent (loc/ast (string "[:a\n"
                                ":b]"))
               [2 1])
  # =>
  1

  (calc-indent (loc/ast (string "@[\"1\"\n"
                                "\"2\"]"))
               [2 1])
  # =>
  2

  (calc-indent (loc/ast (string "{:a 1\n"
                                ":b 2}"))
               [2 1])
  # =>
  1

  (calc-indent (loc/ast (string "@{:x 9\n"
                                ":y 0}"))
               [2 1])
  # =>
  2

  (calc-indent (loc/ast (string "(\n)"))
               [2 1])
  # =>
  1

  (calc-indent (loc/ast (string "(def a\n"
                                "1)"))
               [2 1])
  # =>
  2

  (calc-indent (loc/ast (string "(let [x 1]\n"
                                "(+ x 1))"))
               [2 1])
  # =>
  2

  (calc-indent (loc/ast (string "(print\n"
                                "\"hello\")"))
               [2 1])
  # =>
  2

  (calc-indent (loc/ast (string "(print \"alpha\"\n"
                                "\"beta\")"))
               [2 1])
  # =>
  7

  (calc-indent (loc/ast (string "(put @{:a 1}\n"
                                "     :b 2\n"
                                "     # fun comment\n"
                                ":c 3)"))
               [4 1])
  # =>
  5

  )

(defn indentation-pos
  [line]
  (if-let [[pos]
           (peg/match ~(sequence (any :s)
                                 (if-not " " 1)
                                 (capture (position)))
                      line)]
    (dec pos)
    # when line only has whitespace or is empty, return 0
    0))

(comment

  (indentation-pos "    3")
  # =>
  4

  (indentation-pos ":a")
  # =>
  0

  (indentation-pos " ")
  # =>
  0

  (indentation-pos "")
  # =>
  0

  (indentation-pos " @``")
  # =>
  1

  )

# 1. if the first character of the opening delimiter of the string is the
#    first non-whitespace character on the line, indent appropriately.
#
# 2. if the first character of the opening delimiter of the string is after
#    some other non-whitespace character on the line, don't indent.
#
# for 1., the appropriate position to indent to differs based on what
# the delimiter is, matching is based on:
#
#  a. ordinary string - the position of the initial "
#
#  b. long string - the position of the initial `
#
#  c. long buffer - the position of the initial ` (it comes after a @)
#
#  for a. and b., this is the left-most character of the opening delimiter.
#  for c., it is the second character of the opening delimiter (if one
#  includes the @ as part of the delimiter sequence).
#
#  it's possible to not implement 2., but it seems like it would tend to
#  lead to rather large indentation values.
#
#  note that the parser does not report positions of leading @, but rather
#  ", `, (, {, and [, so there is no need to make adjustments for long
#  buffers.
(defn calc-mid-string-indent
  [fragment delims delim-start-pos delim-type]
  (let [[line col] delim-start-pos
        opener-line (get (string/split "\n" fragment)
                         (dec line))
        opener-indent (indentation-pos opener-line)]
    # columns are 1-based, want indentation (values start at 0)
    (dec
      (case delim-type
        :string
        (do
          # case 2.
          (unless (= opener-indent
                     (dec col))
            (break -1))
          # case 1.
          col)
        #
        :buffer
        (do
          # case 2.
          (unless (= opener-indent
                     (dec (- col 1))) # @ is one left of first `
            (break -1))
          # case 1.
          col)
        # unexpected type
        (break -3)))))

(comment

  (calc-mid-string-indent
    (string "`\n"
            " hello")
    "`" [1 1] :string)
  # =>
  0

  (calc-mid-string-indent
    (string "``\n"
            "  hello")
    "`" [1 1] :string)
  # =>
  0

  (calc-mid-string-indent
    (string "(def a\n"
            "  ``\n"
            "hi")
    "`" [2 3] :string)
  # =>
  2

  (calc-mid-string-indent
    (string " @``\n"
            "hi")
    "`" [1 3] :buffer)
  # =>
  2

  (calc-mid-string-indent
    (string "(def b\n"
            `  "Beginning\n`
            "next")
    `"` [2 3] :string)
  # =>
  2

  )

# XXX: there might be some scenario where it might make sense to have
#      the last line -- something to do with multi-line strings?
#      trying to remember...not sure
#
# XXX: non-space whitespace (e.g. \t, \0, \f, \v, etc.) might cause issues
(defn calc-last-line-indent-helper!
  [input-lines]
  # discard the last line -- may make a replacement below
  (array/pop input-lines)
  #
  (def preceding-region
    (string/join input-lines "\n"))
  # check whether the preceding region is "balanced" delimiter-wise
  (def [delims delim-start-pos delim-type]
    (delims/missing-delims preceding-region))
  # missing-delims had a problem (e.g. too many closing delimiters)
  (when (nil? delims)
    (break -2))
  # the lines before the last line have balanced delimiters and
  # thus the last line is at the top-level. indentation should be 0
  (when (empty? delims)
    (break 0))
  # getting here means that the lines before the last line do not have
  # balanced delimiters. thus the last line (already discarded) is
  # not at the top-level
  (def first-char (first delims))
  # handle mid-string case separately
  (when (or (= (chr "`") first-char)
            (= (chr `"`) first-char))
    (break (calc-mid-string-indent preceding-region
                                   delims delim-start-pos delim-type)))
  # replace the last line (already discarded) because:
  #
  # * if it's a comment (or the lines ends in a comment), cannot
  #   appropriately put closing delimiters on the same line (and cannot
  #   tell it's a comment as it could be the middle of a string...)
  #
  # * can fabricate the replacement line so that we know where the first
  #   non-whitespace entity is on it -- that location gets used later
  #   (see `sentinel-loc` being used below by `find-context` and
  #   `calc-indent`).  note that the content of the last line doesn't
  #   affect indentation (only lines before the target line matter
  #   for indentation).
  (array/push input-lines
              (string "11"     # any number, symbol, or keyword ok
                      delims))
  (def new-region
    (string/join input-lines "\n"))
  (def tree
    (loc/ast new-region))
  # line info is 1-based
  (def sentinel-loc
    [(length input-lines) 1]) # this is where the 11 + closing delims lives
  (def maybe-ctxt
    (loc/find-context tree sentinel-loc))
  (cond
    (nil? maybe-ctxt)
    -1
    #
    (= :top-level maybe-ctxt)
    0
    #
    (= :tuple (type maybe-ctxt))
    (calc-indent maybe-ctxt sentinel-loc)
    # XXX: unexpected result
    (do
      (eprintf "Unexpected context: %p" maybe-ctxt)
      nil)))

(defn calc-last-line-indent
  [fragment]
  (def input-lines
    (string/split "\n" fragment))
  (calc-last-line-indent-helper! input-lines))

(comment

  (calc-last-line-indent
    # non-spork/fmt formatting
    (string " (defn a\n"
            "   1"))
  # =>
  -1

  (calc-last-line-indent "(+ 2 8)")
  # =>
  0

  (calc-last-line-indent ":a")
  # =>
  0

  (calc-last-line-indent
    (string "(+ 2\n"
            "8)"))
  # =>
  3

  (calc-last-line-indent
    (string "(defn my-fn\n"
            "  [x]\n"
            "(+ x"))
  # =>
  2

  (calc-last-line-indent
    (string "{:a 1\n"
            ":b"))
  # =>
  1

  (calc-last-line-indent
    (string "`\n"
            " hello"))
  # =>
  0

  (calc-last-line-indent
    (string "``\n"
            "  hello"))
  # =>
  0

  (calc-last-line-indent
    (string "(def a\n"
            "  ``\n"
            "hi"))
  # =>
  2

  (calc-last-line-indent
    (string " @``\n"
            "hi"))
  # =>
  2

  (calc-last-line-indent
    (string "(def b\n"
            `  "Beginning\n`
            "next"))
  # =>
  2

  (calc-last-line-indent
    (string "{:a\n"
            `""`))
  # =>
  1

  (calc-last-line-indent
    (string "{:a\n"
            "[]"))
  # =>
  1

  (calc-last-line-indent
    (string "(def a\n"
            "(print 1))"))
  # =>
  2

  (calc-last-line-indent "(def a")
  # =>
  0

  (calc-last-line-indent "(def a\n1")
  # =>
  2

  # XXX: whitespace before newline needs to be cleaned by editor?
  (calc-last-line-indent "(def a \n1")
  # =>
  2

  (calc-last-line-indent (string "(try\n"
                                 "  1\n"
                                 "  #\n"
                                 "  ([err]\n"
                                 "2))"))
  # =>
  4

  (calc-last-line-indent (string "(defn my-fun\n"
                                 "[x]"))
  # =>
  2

  )

# XXX: tabs (and other non-space whitespace) are not handled -- problem?
(defn main
  [& args]
  (def indent
    (calc-last-line-indent (file/read stdin :all)))
  (eprintf "indent: %p" indent)
  (print indent))
