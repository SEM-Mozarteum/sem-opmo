# Markdown Documents

This guide is the reference for writing Opusmodus Markdown documents. It is intended for cases where the literal Markdown syntax itself must be shown exactly, including fenced code blocks, toolbar directives, image syntax, and special Opusmodus fence names.

## Overview

Opusmodus Markdown documents are ordinary `.md` files opened in the Assistant window. They combine standard Markdown with a small set of Opusmodus-specific additions for code evaluation, notation display, graph display, image sizing, and function-page layout.

## Opening and Editing Markdown Files

A Markdown file can be opened in the Assistant from the Navigator.

1. Select the `.md` file in the Navigator.
2. Open the contextual menu.
3. Choose **Edit Markdown File**.

The **Edit Markdown File** command opens the Markdown source in the Workspace editor.

## Basic Markdown Syntax

The following standard Markdown forms are supported.

### Headings

```markdown
# Main Title
## Section
### Subsection 1
#### Subsection 2
```

### Paragraphs

```markdown
This is the first paragraph.

This is the second paragraph.
```

### Emphasis

```markdown
*italic*
**bold**
`code`
**`code`**
```

### Unordered List

```markdown
- first item
- second item
- third item
```

### Ordered List

```markdown
1. first item
2. second item
3. third item
```

### Links

```markdown
[Related Function](../Waves/Sine/gen-sine.md)
[Section Link](#examples)
[OpenAI](https://openai.com)
```

### Table

```markdown
| Argument | Description |
| --- | --- |
| `res` | Resolution. |
| `freq` | Frequency. |
```

### Horizontal Rule

```markdown
---
```

## Line Breaks with Backslash

In Markdown documents, a backslash `\` placed at the end of a line creates an explicit line break. This is useful when a line should continue visually on the next line without starting a new paragraph.

```markdown
First line\
Second line
```

## Blank-Line Spacing

Blank lines are recognised by the Opusmodus Markdown display. One or more empty lines between text blocks are preserved as visual spacing, so a document can be shaped more clearly without adding extra markup.

```markdown
Text...


Text...
```

This produces a larger visual separation between the two text blocks. Use this spacing deliberately for document design, for example before a new musical example, analytical note, table, or image.

## Fenced Code Blocks

Fenced code blocks use `triple backticks`. Begin the block with the `triple backticks` and a fence name such as `lisp` and end the block with the `triple backticks`.

Literal syntax:

````markdown
```lisp
(rndn 12 0.1 1.0)
```
````

## Supported Fence Types

Any code fence can be used as a plain code block, but Opusmodus provides special behaviour for the following fence names.

### Lisp

Standard Lisp fences use `lisp`.

Default toolbar: **Evaluate**, **Copy**.

````markdown
```lisp
(gen-sine 32 3 0.5)
```
````

### OMN

Fence `omn` is used for a single-voice notation display.

Default toolbar: **Evaluate**, **Notation**, **Copy**.

````markdown
```omn
(setf seq '(q c4 d4 e4 f4))
```
````

Fence `omn2` is used for multi-voice notation display.

Default toolbar: **Evaluate**, **Notation**, **Copy**.

````markdown
```omn2
((q c4 d4 e4 f4) (h g3 a3))
```
````

### Plot Fences

Fences `plot1` to `plot9` display a **Plot** button. The visible button name remains **Plot**; the fence type determines which graph function is used.

Default toolbar: **Evaluate**, **Plot**, **Copy**.

| Fence | Plot |
| --- | --- |
| `plot1` | Numbers plot |
| `plot2` | OMN plot |
| `plot3` | Length plot |
| `plot4` | Pitch plot |
| `plot5` | Velocity plot |
| `plot6` | Circle pitch plot |
| `plot7` | Circle rhythm plot |
| `plot8` | XY plot |
| `plot9` | Spectral plot |


````markdown
```plot1
(gen-sine 120 4 0.5)
```
````

````markdown
```plot2
'((-q -e. s f5 < stacc e e6 < stacc -s e6 fff stacc)
  (-q s c4 < leg e cs5 < -s -e. s c6f5 fff stacc))
```
````

## Toolbar Directives

A toolbar directive applies to the next fenced code block only. It must appear on its own line before that block.

Hide all buttons:

````markdown
[buttons: none]
```lisp
~/Opusmodus/Media/Analysis
```
````

The buttons behave as follows:

- **Evaluate** evaluates the whole block.
- **Notation** evaluates and displays notation.
- **Plot** evaluates and opens the graph corresponding to the fence type.
- **Copy** copies the block.

Evaluation is wrapped in `(progn ...)`, so a block may contain more than one expression.

## Function Page Layout

In Opusmodus function documentation, the first code block after the main title can be used as the function signature. If it is followed by the heading `Arguments and Values`, the block is treated as a signature block and no toolbar is shown.

Typical signature pattern:

````markdown
# gen-sine

```lisp
(gen-sine res freq amplitude
          &key phase endpoint fm fm-index
               am am-index phase-scaling ps-index
               phase-distortion pd-index)
```

## Arguments and Values
````

## Images

Markdown images in PNG format are supported.

Basic image syntax:

```markdown
![Caption](images/example.png)
```

An optional style block may follow the image.

```markdown
![Caption](images/example.png){center, width=720}
```

## Complete Example

````markdown
# scale-envelope

```lisp
(scale-envelope env scale
                &optional offset)
```

## Arguments and Values

### Required

| Argument | Description |
| --- | --- |
| `env` | Envelope. Breakpoint envelope to scale. |
| `scale` | Number. Multiplicative factor applied to each y value. |

### Optional

| Argument | Description |
| --- | --- |
| `offset` | Number. Value added after scaling. Default is `0.0`. |

## Description

**`scale-envelope`** multiplies the y values of an envelope by `scale` and then adds `offset`.

This is useful when an existing contour needs to be adapted to a new amplitude range without changing the x structure.

## Examples

```plot8
(scale-envelope '(0 0.2 1 0.8 3 0.4) 2.0)
```

```plot8
(scale-envelope '(0 0.2 1 0.8 3 0.4) 2.0 -0.1)
```

## Related Documents

- [Readme First - Envelopes](Readme First - Envelopes.md)
- [normalize-envelope](normalize-envelope.md)
- [x-norm](x-norm.md)
````

## Practical Notes

- Use `.md` as the normal file extension.
- Use `omn` for a single-voice notation result.
- Use `omn2` for multi-voice notation.
- Use `plot1` to `plot9` when the block should offer a **Plot** button.
- Use `[buttons: none]` when a syntax example should remain visually clean.
- Use a final backslash `\` when a line should break without starting a new paragraph.
- Use blank lines to create intentional vertical spacing between text blocks.
- Use relative links between related documents whenever possible.
- Keep examples directly executable.

