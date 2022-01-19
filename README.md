# freja-indent-line

Line indentation support for freja.

## Prerequisites

The [freja editor](https://github.com/saikyun/freja).

## Setup

0. Clone this repository somewhere and cd to the resulting directory
1. Start freja with: `freja freja-indent-line/freja-indent-line.janet`
2. `Control+L` to load the file

## Example Usage

1. Put the cursor on a blank line and write a line of code (but don't
   press Enter).  The result should look like:
    ```
    (defn my-fn
    ```

2. Press `Shift+Enter` and type `[x]`.  The result
   should look like:
    ```
    (defn my-fn
      [x]
    ```

3. Again, press `Shift+Enter` and type `(+ x 1))`.  The result should look
   like:
    ```
    (defn my-fn
      [x]
      (+ x 1))
    ```

Note that it should not have been necessary to manually insert spaces at the beginning of each line, i.e. indentation should have happened automatically.

## Explanation

In this version, `Shift+Enter` invokes the function
`newline-and-indent!` which successively:

* Deletes any whitespace immediately before the cursor location
* Creates a new line and moves the cursor to it
* Indents the new line according to context, leaving the cursor at the "indented" location

It's also possible to indent the current line by typing `Tab`.
