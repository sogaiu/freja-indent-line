(import freja/new_gap_buffer :as gb)
(import freja/state)
(import freja/default-hotkeys :as dh)

(import ./indent)

# XXX: for investigation
(defn current-gb
  []
  (get-in state/editor-state [:left-state :editor :gb]))

(varfn point
  [gb]
  (gb :caret))

(varfn point-max
  [gb]
  (gb/gb-length gb))

(varfn char-after
  [gb i]
  (gb/gb-nth gb i))

(varfn goto-char
  [gb i]
  (gb/put-caret gb i))

(varfn beginning-of-buffer?
  [gb]
  (= (gb :caret) 0))

(varfn backward-line
  [gb]
  (let [curr-line (gb/index-start-of-line gb (gb :caret))
        # checking not at beginning of buffer not needed
        prev-line (gb/index-start-of-line gb (dec curr-line))]
    (put gb :caret prev-line)))

(comment

  # XXX: assumes more than one line in buffer
  (let [gb (current-gb)
        original (point gb)
        lines (gb :lines)]
    (var somewhere nil)
    (while true
      (set somewhere
           # XXX: seed somewhere else?
           (math/rng-int (math/rng (os/cryptorand 8))
                         (gb/gb-length gb)))
      (when (> somewhere (first lines))
        (break)))
    (goto-char gb somewhere)
    # XXX
    (tracev somewhere)
    (tracev (def someline-number
      (gb/current-line-number gb)))
    #
    (backward-line gb)
    (tracev (def prevline-number
      (gb/current-line-number gb)))
    #
    (defer (goto-char gb original)
      (= (dec someline-number)
         prevline-number)))
  # => true

 )

(varfn forward-line
  [gb]
  (let [curr-line (gb/index-end-of-line gb (gb :caret))
        # checking not at end of buffer not needed
        next-line (inc curr-line)]
    (put gb :caret next-line)))

(comment

  # XXX: assumes more than one line in buffer
  (let [gb (current-gb)
        original (point gb)
        lines (gb :lines)]
    (var somewhere nil)
    (while true
      (set somewhere
           # XXX: seed somewhere else?
           (math/rng-int (math/rng (os/cryptorand 8))
                         (gb/gb-length gb)))
      (when (> somewhere (first lines))
        (break)))
    (goto-char gb somewhere)
    # XXX
    (tracev somewhere)
    (tracev (def someline-number
      (gb/current-line-number gb)))
    #
    (forward-line gb)
    (tracev (def nextline-number
      (gb/current-line-number gb)))
    #
    (defer (goto-char gb original)
      (= (inc someline-number)
         nextline-number)))
  # => true

 )

# XXX: review
(varfn skip-whitespace-forward
  "Skips forward while there is whitespace on current line."
  [gb]
  (def {:caret caret} gb)

  (var target-i caret)
  (def start-i caret)

  (def f
    (fiber/new
      (fn []
        (gb/index-char-start gb start-i))))

  (when (= (chr "\n") (char-after gb caret))
    (break nil))

  (loop [[i c] :iterate (resume f)]
    (when (and (not= (chr " ") c)
               (not= (chr "\t") c))
      (set target-i i)
      (break)))

  (if (> target-i (gb/gb-length gb))
    nil
    (gb/move-n gb (- target-i start-i))))

(varfn begin-of-top-level
  [gb]
  # XXX: necessary?
  (-> gb gb/commit!)
  (defn begin-of-top-level-char?
    [char]
    (def botl-chars
      {(chr "(") true
       (chr "~") true
       (chr "'") true})
    (get botl-chars char))
  #
  (when (not (beginning-of-buffer? gb))
    (var pos (point gb))
    (gb/beginning-of-line gb)
    (if (begin-of-top-level-char? (char-after gb (point gb)))
      (set pos (point gb))
      (while true
        (backward-line gb)
        (cond
          (begin-of-top-level-char? (char-after gb (point gb)))
          (do
            (set pos (point gb))
            (break))
          #
          (beginning-of-buffer? gb)
          (break))))
    (goto-char gb pos)))

(varfn calculate-indent
  [gb]
  (def current (point gb))
  # restore the caret at the end
  (defer (goto-char gb current)
    # remember where the line starts
    (gb/beginning-of-line gb)
    (def bol (point gb))
    # early return
    (when (beginning-of-buffer? gb)
      (break 0))
    # remember the current indentation
    (skip-whitespace-forward gb)
    # XXX: is this still used?
    (def cur-indent (- (point gb) bol))
    # remember where the line ends - end of the region
    (gb/end-of-line gb)
    (def end (point gb))
    # find the containing top-level construct - start of region
    (backward-line gb)
    (begin-of-top-level gb)
    (def start (point gb))
    # ask indent for an indentation assessment
    (def region
      (string/slice (gb/content gb) start end))
    (def new-indent
        (indent/calc-last-line-indent region))
    (if (neg? new-indent)
      nil
      new-indent)))

(varfn current-column
  [gb]
  (gb/column! gb (gb :caret)))

(varfn indent-to!
  [gb amount]
  (gb/beginning-of-line gb)
  (def start (gb :caret))
  (skip-whitespace-forward gb)
  (def end (gb :caret))
  (gb/delete-region! gb start end)
  (gb/insert-string-at-caret! gb (string/repeat " " amount)))

(varfn indent-current-line!
  [gb]
  (when-let [result (calculate-indent gb)]
    # remember the cursor position relative to the end of the buffer
    (def pos (- (point-max gb) (point gb)))
    # find the first non-whitespace character on the line
    (gb/beginning-of-line gb)
    (def bol (point gb))
    (skip-whitespace-forward gb)
    # only adjust indentation if necessary
    (unless (= result (current-column gb))
      (gb/delete-region! gb bol (point gb))
      (indent-to! gb result))
    # restore cursor sensibly
    (when (< (point gb) (- (point-max gb) pos))
      (goto-char gb (- (point-max gb) pos))))
  # for composing with other functions
  gb)

(put dh/gb-binds
     :tab
     (comp dh/reset-blink indent-current-line!))

(varfn delete-whitespace-backward!
  ``
  From caret, delete any contiguous whitespace backward until non-whitespace
  or beginning of line is reached.
  ``
  [gb]
  # empty line, early return
  (when (and (gb/beginning-of-line? gb)
             (gb/end-of-line? gb))
    (break gb))
  # beginning of line, early return
  (when (gb/beginning-of-line? gb)
    (break gb))
  #
  (def end (gb :caret))
  (def start
    (gb/search-backward gb |(or (= $ (chr "\n"))
                                (and (not= $ (chr " "))
                                     (not= $ (chr "\t"))))
                        end))
  # no whitespace, early return
  (when (= start end)
    (break gb))
  (def curr-line-no
    (gb/current-line-number gb))
  # delete whitespace
  (gb/delete-region! gb start end))

(varfn newline-and-indent!
  [gb]
  (delete-whitespace-backward! gb)
  # after this, should be on next line
  (gb/insert-string-at-caret! gb "\n")
  (indent-current-line! gb)
  gb)

# XXX: any better way?
'(put dh/gb-binds
     :enter
     (comp dh/reset-blink newline-and-indent!))

(put-in dh/gb-binds
        [:shift :enter]
        (comp dh/reset-blink newline-and-indent!))
