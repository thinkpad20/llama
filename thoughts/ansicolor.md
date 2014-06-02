type AnsiColor = Default; Black; Red; Green; Brown; Blue; Magenta; Cyan; Gray
type AnsiStyle (underline: Bool) (italic: Bool) (bold: Bool)

open (c: AnsiColor) =
  Default -> ''
  Black -> '\x1b[30m'
  Red -> '\x1b[31m'

open (s: AnsiStyle) = 
  ...

close (c: Color) = 
  Default -> ''
  ...

close (s: AnsiStyle) = 
  ...

color_str s (c: Color) = s.surround c.open_codes c.close_codes

type Colored with
  color: AnsiColor = Default
  style: AnsiStyle = AnsiStyle False False False

implement Callable String String for Colored with
  @call cs s = !result after
    result = ref (s.color_str cs.color)
    if cs._underline
      result .= surround underline_open underline_close
    if cs._bold
      result .= surround bold_open bold_close
    if cs._italic
      result .= surround italic_open italic_close

implement Callable AnsiColor Colored for Colored with
  @call cs c = cs with color=c

implement Callable AnsiStyle Colored for Colored with
  @call cs s = cs with style=s

underline (c: Colored) = c <| c.style with underline=True
bold (c: Colored) = c <| c.style with bold=True
italic (c: Colored) = c <| c.style with italic=True

println Colored.underline.Red 'hello, world!'
