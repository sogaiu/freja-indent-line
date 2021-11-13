(def loc-grammar
  ~{:main (some :input)
    #
    :input (choice :non-form
                   :form)
    #
    :non-form (choice :whitespace
                      :comment)
    #
    :whitespace
    (cmt (capture (sequence (line) (column)
                            (choice (some (set " \0\f\t\v"))
                                    (choice "\r\n"
                                            "\r"
                                            "\n"))))
         ,|[:whitespace $2 $0 $1])
    #
    :comment (cmt (sequence (line) (column)
                            "#"
                            (capture (any (if-not (set "\r\n") 1))))
                  ,|[:comment $2 $0 $1])
    #
    :form (choice :reader-macro
                  :collection
                  :literal)
    #
    :reader-macro (choice :fn
                          :quasiquote
                          :quote
                          :splice
                          :unquote)
    #
    :fn (cmt (capture (sequence (line) (column)
                                "|"
                                (any :non-form)
                                :form))
             # $& is the remaining arguments
             ,|[:fn ;(slice $& 0 -2) $0 $1])
    #
    :quasiquote (cmt (capture (sequence (line) (column)
                                        "~"
                                        (any :non-form)
                                        :form))
                     ,|[:quasiquote ;(slice $& 0 -2) $0 $1])
    #
    :quote (cmt (capture (sequence (line) (column)
                                   "'"
                                   (any :non-form)
                                   :form))
                ,|[:quote ;(slice $& 0 -2) $0 $1])
    #
    :splice (cmt (capture (sequence (line) (column)
                                    ";"
                                    (any :non-form)
                                    :form))
                 ,|[:splice ;(slice $& 0 -2) $0 $1])
    #
    :unquote (cmt (capture (sequence (line) (column)
                                     ","
                                     (any :non-form)
                                     :form))
                  ,|[:unquote ;(slice $& 0 -2) $0 $1])
    #
    :literal (choice :number
                     :constant
                     :buffer
                     :string
                     :long-buffer
                     :long-string
                     :keyword
                     :symbol)
    #
    :collection (choice :array
                        :bracket-array
                        :tuple
                        :bracket-tuple
                        :table
                        :struct)
    #
    :number (cmt (capture (sequence (line) (column)
                                    (drop (cmt
                                            (capture (some :name-char))
                                            ,scan-number))))
                 ,|[:number $2 $0 $1])
    #
    :name-char (choice (range "09" "AZ" "az" "\x80\xFF")
                       (set "!$%&*+-./:<?=>@^_"))
    #
    :constant (cmt (capture (sequence (line) (column)
                                      (choice "false" "nil" "true")
                                      (not :name-char)))
                   ,|[:constant $2 $0 $1])
    #
    :buffer (cmt (sequence (line) (column)
                           "@\""
                           (capture
                             (any (choice :escape
                                          (if-not "\"" 1))))
                           "\"")
                 ,|[:buffer $2 $0 $1])
    #
    :escape (sequence "\\"
                      (choice (set "0efnrtvz\"\\")
                              (sequence "x" [2 :hex])
                              (sequence "u" [4 :hex])
                              (sequence "U" [6 :hex])
                              (error (constant "bad escape"))))
    #
    :hex (range "09" "af" "AF")
    #
    :string (cmt (sequence (line) (column)
                           "\""
                           (capture (any (choice :escape
                                                 (if-not "\"" 1))))
                           "\"")
                 ,|[:string $2 $0 $1])
    # XXX: includes delimiters
    :long-string (cmt (capture (sequence (line) (column)
                                         :long-bytes))
                      ,|[:long-string $2 $0 $1])
    #
    :long-bytes {:main (drop (sequence :open
                                       (any (if-not :close 1))
                                       :close))
                 :open (capture :delim :n)
                 :delim (some "`")
                 :close (cmt (sequence (not (look -1 "`"))
                                       (backref :n)
                                       (capture :delim))
                             ,=)}
    # XXX: includes delimiters
    :long-buffer (cmt (sequence (line) (column)
                                "@"
                                (capture :long-bytes))
                      ,|[:long-buffer $2 $0 $1])
    #
    :keyword (cmt (capture (sequence (line) (column)
                                     ":"
                                     (any :name-char)))
                  ,|[:keyword $2 $0 $1])
    #
    :symbol (cmt (capture (sequence (line) (column)
                                    (some :name-char)))
                 ,|[:symbol $2 $0 $1])
    #
    :array (cmt (capture (sequence (line) (column)
                                   "@("
                                   (any :input)
                                   (choice ")"
                                           (error (constant "missing )")))))
                ,|[:array ;(slice $& 0 -2) $0 $1])
    #
    :tuple (cmt (capture (sequence (line) (column)
                                   "("
                                   (any :input)
                                   (choice ")"
                                           (error (constant "missing )")))))
                ,|[:tuple ;(slice $& 0 -2) $0 $1])
    #
    :bracket-array
    (cmt (capture (sequence (line) (column)
                            "@["
                            (any :input)
                            (choice "]"
                                    (error (constant "missing ]")))))
         ,|[:bracket-array ;(slice $& 0 -2) $0 $1])
    #
    :bracket-tuple
    (cmt (capture (sequence (line) (column)
                            "["
                            (any :input)
                            (choice "]"
                                    (error (constant "missing ]")))))
         ,|[:bracket-tuple ;(slice $& 0 -2) $0 $1])
    #
    :table (cmt (capture (sequence (line) (column)
                                   "@{"
                                   (any :input)
                                   (choice "}"
                                           (error (constant "missing }")))))
                ,|[:table ;(slice $& 0 -2) $0 $1])
    #
    :struct (cmt (capture (sequence (line) (column)
                                    "{"
                                    (any :input)
                                    (choice "}"
                                            (error (constant "missing }")))))
                 ,|[:struct ;(slice $& 0 -2) $0 $1])
    })

(comment

  (peg/match loc-grammar "# i am a comment")
  # => '@[(:comment " i am a comment" 1 1)]

  (peg/match loc-grammar "true")
  # => '@[(:constant "true" 1 1)]

  (peg/match loc-grammar `"hello"`)
  # => '@[(:string "hello" 1 1)]

  (peg/match loc-grammar `@"breathe"`)
  # => '@[(:buffer "breathe" 1 1)]

  (deep=
    #
    (peg/match loc-grammar "1")
    #
    '@[(:number "1" 1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "(+ 1 1)")
    #
    '@[(:tuple (:symbol "+" 1 2) (:whitespace " " 1 3)
               (:number "1" 1 4) (:whitespace " " 1 5)
               (:number "1" 1 6)
               1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "|(+ $ 1)")
    #
    '@[(:fn
         (:tuple (:symbol "+" 1 3) (:whitespace " " 1 4)
                 (:symbol "$" 1 5) (:whitespace " " 1 6)
                 (:number "1" 1 7) 1 2)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "| [:hi $]")
    #
    '@[(:fn
         (:whitespace " " 1 2)
         (:bracket-tuple (:keyword ":hi" 1 4) (:whitespace " " 1 7)
                         (:symbol "$" 1 8)
                         1 3)
         1 1)])
  # => true

  (peg/match loc-grammar "'print")
  # => '@[(:quote (:symbol "print" 1 2) 1 1)]

  (deep=
    #
    (peg/match loc-grammar "' defer")
    #
    '@[(:quote
         (:whitespace " " 1 2)
         (:symbol "defer" 1 3)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "~fun")
    #
    '@[(:quasiquote
         (:symbol "fun" 1 2)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar ";[1 2]")
    #
    '@[(:splice
         (:bracket-tuple
           (:number "1" 1 3) (:whitespace " " 1 4)
           (:number "2" 1 5)
           1 2)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "~(,fun)")
    #
    '@[(:quasiquote
         (:tuple
           (:unquote
             (:symbol "fun" 1 4)
             1 3)
           1 2)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "{:a 1 :b 2}")
    #
    '@[(:struct
         (:keyword ":a" 1 2) (:whitespace " " 1 4)
         (:number "1" 1 5) (:whitespace " " 1 6)
         (:keyword ":b" 1 7) (:whitespace " " 1 9)
         (:number "2" 1 10)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "{:a 1\n:b 2}")
    #
    '@[(:struct
         (:keyword ":a" 1 2) (:whitespace " " 1 4)
         (:number "1" 1 5) (:whitespace "\n" 1 6)
         (:keyword ":b" 2 1) (:whitespace " " 2 3)
         (:number "2" 2 4)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar "[1 2 3]")
    #
    '@[(:bracket-tuple
         (:number "1" 1 2) (:whitespace " " 1 3)
         (:number "2" 1 4) (:whitespace " " 1 5)
         (:number "3" 1 6)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar
               (string "@(1\n"
                       "[:x :y]\n"
                       "(+ 0 9))"))
    #
    '@[(:array
         (:number "1" 1 3) (:whitespace "\n" 1 4)
         (:bracket-tuple (:keyword ":x" 2 2) (:whitespace " " 2 4)
                         (:keyword ":y" 2 5)
                         2 1)
         (:whitespace "\n" 2 8)
         (:tuple (:symbol "+" 3 2) (:whitespace " " 3 3)
                 (:number "0" 3 4) (:whitespace " " 3 5)
                 (:number "9" 3 6)
                 3 1)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar
               (string "@{:a 1\n"
                       ":b 2\n"
                       ":c 3}"))
    #
    '@[(:table
         (:keyword ":a" 1 3) (:whitespace " " 1 5)
         (:number "1" 1 6) (:whitespace "\n" 1 7)
         (:keyword ":b" 2 1) (:whitespace " " 2 3)
         (:number "2" 2 4) (:whitespace "\n" 2 5)
         (:keyword ":c" 3 1) (:whitespace " " 3 3)
         (:number "3" 3 4)
         1 1)])
  # => true

  (deep=
    #
    (peg/match loc-grammar
               (string "@[:fun\n"
                       "  :smile\n"
                       "  :breathe]"))
    #
    '@[(:bracket-array
         (:keyword ":fun" 1 3) (:whitespace "\n" 1 7)
         (:whitespace "  " 2 1)
         (:keyword ":smile" 2 3) (:whitespace "\n" 2 9)
         (:whitespace "  " 3 1)
         (:keyword ":breathe" 3 3)
         1 1)])
  # => true

  (peg/match loc-grammar "``a long string``")
  # => '@[(:long-string "``a long string``" 1 1)]

  (peg/match loc-grammar "@``a long buffer``")
  # => '@[(:long-buffer "``a long buffer``" 1 1)]

  )

(defn ast
  [code]
  (->> code
       (peg/match loc-grammar)
       first))

(comment

  (deep=
    #
    (ast "(/ 2 1)")
    #
    '(:tuple
       (:symbol "/" 1 2) (:whitespace " " 1 3)
       (:number "2" 1 4) (:whitespace " " 1 5)
       (:number "1" 1 6)
       1 1))
  # => true

  )

(defn node-type
  [node]
  (first node))

(comment

  (node-type (ast "(+ 1 1)"))
  # => :tuple

  (node-type (ast " "))
  # => :whitespace

  (node-type (ast "# hello"))
  # => :comment

  (node-type (ast "1"))
  # => :number

  )

(defn start-pos
  [node]
  (slice node -3))

(comment

  (start-pos '(:symbol "/" 1 2))
  # => [1 2]

  )

(defn content
  [node]
  (slice node 1 -3))

(comment

  (content '(:symbol "/" 1 2))
  # => '("/")

  (deep=
    #
    (content
      '(:tuple
         (:symbol "/" 1 2) (:whitespace " " 1 3)
         (:number "2" 1 4) (:whitespace " " 1 5)
         (:number "1" 1 6)
         1 1))
    #
    '[(:symbol "/" 1 2)
      (:whitespace " " 1 3)
      (:number "2" 1 4)
      (:whitespace " " 1 5)
      (:number "1" 1 6)])
  # => true

  )

(defn find-context
  [tree target]
  (var ctxt nil)
  (defn helper
    [node]
    (when (= :tuple (type node))
      (def inner (content node))
      (def [line col] (start-pos node))
      #
      (when (= target [line col])
        (set ctxt :top-level)
        (break nil))
      #
      (each item inner
        (when (= :tuple (type item))
          (def [i-line i-col] (start-pos item))
          (when (= target [i-line i-col])
            (set ctxt node)
            (break)))
        (when ctxt (break))
        (helper item))))
  (helper tree)
  ctxt)

(comment

  (find-context (ast "1")
                [1 1])
  # => :top-level

  (find-context (ast ":hello")
                [1 7])
  # => nil

  (deep=
    #
    (find-context (ast "(+ 2 1)")
                  [1 6])
    #
    '(:tuple
       (:symbol "+" 1 2) (:whitespace " " 1 3)
       (:number "2" 1 4) (:whitespace " " 1 5)
       (:number "1" 1 6)
       1 1))
  # => true

  (deep=
    #
    (find-context (ast (string "(defn my-fn\n"
                               "  [x]\n"
                               "  (+ x 1))"))
                  [3 6])
    #
    '(:tuple
       (:symbol "+" 3 4) (:whitespace " " 3 5)
       (:symbol "x" 3 6) (:whitespace " " 3 7)
       (:number "1" 3 8)
       3 3))
  # => true

  (deep=
    #
    (find-context (ast (string "(defn my-fn\n"
                               "  [x]\n"
                               "  (+ x (- 2 3)))"))
                  [3 11])
    #
    '(:tuple
       (:symbol "-" 3 9) (:whitespace " " 3 10)
       (:number "2" 3 11) (:whitespace " " 3 12)
       (:number "3" 3 13)
       3 8))
  # => true

  )

(defn head-sym
  [node]
  (def ntype (node-type node))
  (def inner (content node))
  (assert (= :tuple ntype)
          (string/format "Not a tuple: %p" node))
  (when-let [sym-node
             (->> inner
                  (filter (fn [child-node]
                            (= :symbol (node-type child-node))))
                  first)]
    (first (content sym-node))))

(comment

  (head-sym (ast "(+ 1 1)"))
  # => "+"

  (head-sym (ast (string "(\n"
                         " # hi there\n"
                         " + 1 1)")))
  # => "+"

  )

(defn mutable-container?
  [node]
  (if (get {:array true
            :bracket-array true
            :table true}
           (node-type node))
    true
    false))

(comment

  (mutable-container? (ast "@[]"))
  # => true

  (mutable-container? (ast "[]"))
  # => false

  )

(defn empty-tuple?
  [node]
  (def ntype (node-type node))
  (def inner (content node))
  (assert (= :tuple ntype)
          (string/format "Not a tuple: %p" node))
  (empty? (filter (fn [child-node]
                    (def c-ntype (node-type child-node))
                    (and (not= :comment c-ntype)
                         (not= :whitespace c-ntype)))
                  inner)))

(comment

  (empty-tuple?
    '(:tuple
       (:whitespace " " 3 5)
       (:whitespace " " 3 7)
       3 8))
  # => true

  (empty-tuple?
    '(:tuple
       (:comment " hi there" 3 5)
       (:whitespace " " 3 7)
       3 8))
  # => true

  (empty-tuple?
    '(:tuple
       (:symbol "+" 3 4) (:whitespace " " 3 5)
       (:symbol "x" 3 6) (:whitespace " " 3 7)
       (:number "1" 3 8)))
  # => false

  )
