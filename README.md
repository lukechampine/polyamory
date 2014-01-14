polyamory
=========

Hello World is too simple. Project Euler is too mathematical. So instead, I have undertaken to write a Brainfuck interpreter for each language under my belt, and for each new language that I learn.

| language   | status      |
|------------|-------------|
| C          | Complete    |
| Perl       | In progress |
| JavaScript | Not started |
| Go         | Not started |
| Haskell    | Not started |

Brainfuck specification
-----------------------

A Brainfuck program has an implicit byte pointer, which is free to move around within an array. Each array location is initialized to zero. The pointer itself is initialized to point to the beginning of the array.

The Brainfuck programming language consists of eight commands, each of which is represented as a single character.

| command | description                                                               |
|---------|---------------------------------------------------------------------------|
| `>`     | Increment the pointer.                                                    |
| `<`     | Decrement the pointer.                                                    |
| `+`     | Increment the byte at the pointer.                                        |
| `-`     | Decrement the byte at the pointer.                                        |
| `.`     | Output the byte at the pointer.                                           |
| `,`     | Input a byte and store it in the byte at the pointer.                     |
| `[`     | Jump forward past the matching `]` if the byte at the pointer is zero.    |
| `]`     | Jump backward to the matching `[` unless the byte at the pointer is zero. |

Implementation details
----------------------
The original language specification left a lot unspecified, so the following rules will be adopted:
- Non-command characters are ignored.
- The array size is dynamic ("infinite").
- Array cells are represented as bytes.
- Cells wrap around (255 + 1 = 0, and 0 - 1 = 255).
- The end-of-line code is ASCII 10, `\n`.
- When `,` encounters the EOF character `ÿ`, it leaves the cell's value unchanged.

Additional rules
----------------
- Interpreters must be able to detect unmatched brackets and display an error message.
- If feasible, the interpreter should have some mechanism for detecting infinite loops (e.g. a timeout).

