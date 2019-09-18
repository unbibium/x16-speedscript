SpeedScript X16 Fork
--------------------

SpeedScript is a very small but full-featured word processor, originally written 
by Charles Brannon, and published in Compute Magazine for various 8-bit
computers. This project is a port of the Commodore 64 version of SpeedScript
to the Commander X16.

I'm using SpeedScript 3.1 for the Commodore 64 as my starting point. It is
one patch away from being SpeedScript 3.2, but this is the version which
had the source code published in print.  You can read the original book
[on archive.org][1].  I had to copy everything from hand and verify it
against a working binary I had downloaded, and I've included that version
in this repository, with some but not all of the comments from the book.

[1]: https://archive.org/details/Computes_Speedscript

Working features
----------------

The program will work in either 40 or 80 column mode, and probably any
other text mode supported in the future, as long as the convention of
saving the row to `V_M` and the column times two to `V_L` works, and
the row and column count is readable.

You can load and save; it is currently locked to device #1.  Printing
might work, it's unmodified.

The following functions work:

* Ctrl-I to toggle between overwrite mode (black header) and insert mode 
  (blue header).
* F7 to load and F8 to save.  Only device #1 is supported.
* Ctrl-D to delete a sentence, word, or paragraph to the left.
* Ctrl-E to erase one or more sentences, words, or paragraphs to the right.
* Ctrl-F to erase one character to the right.  (supposed to be Ctrl-Left Arrow)
* Ctrl-C to insert five spaces.
* Ctrl-X to swap letter under the cursor with letter to the right.
* Ctrl-B and Ctrl-L to change the color of the background and letters.

Missing features
----------------

* Doesn't recognize any Shift-Ctrl keys yet
* Clearing memory with CLR/HOME crashes the machine.
* Memory access beyond the first 32K
* Faster screen drawing in 80 column mode
* Disk directory (Ctrl-4) comes up blank.
* Ctrl-H just puts up "Not Found"
* Ctrl-R just puts up garbage.
* Ctrl-P just turns the screen black until you press Ctrl-B or Ctrl-L


