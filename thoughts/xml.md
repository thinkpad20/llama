object UVector a = UVector [a]
  with elems: {s a} = {s}

trait Append a =
  append: a b -> b -> a b

trait Prepend a = 
  prepend: a b -> b -> a b

trait DoubleEnded a = {Append a; Prepend a}

impl Append UVector =
  uv.append x = case uv\elems.contains x of 
    True => uv
    False => UVector (uv\0.append x) with elems = uv\elems.insert x

object XmlNode = XmlNode (tag: Str)
  with attrs: {h Str => Str} = {h}
       ?text: Str
       ?tail: Str
       children: [XmlNode] = []

impl Show XmlNode = 
  show node = result after
    result = mut '<#[node\tag]'
    for attr, val in node\attrs do result += ' #[attr]=#{val}'
    result += '>#[node\text || ""]'
    for child in node\children do result += child.show
    result += '<#[node\tag]>#[node\tail || ""]'


foo = XmlNode 'foo'

my_tree = from_string '<foo bar="baz">Hello <qux blob="blab"/> wazzap</foo>'

assert my_tree == 
  XmlNode 'foo'
    with {
      attrs = {d 'bar' => 'baz'};
      text = Just 'Hello ';
      children = [
        XmlNode 'qux'
          with {
            attrs = {d 'blob'='blab'};
            children = [];
            text = Nothing;
            tail = Nothing
          }
      ]
      tail = Just ' wazzap';
    }

add_attribs! node (attribs, ?recurse=False) = 
  for name, val in attribs.items
    node\attrs.set! name val
  
