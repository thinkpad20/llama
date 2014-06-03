A `BinaryObject` is something which stores one or more encodings of a file, and only ever needs to be decoded once for any of its encodings. It keeps track of its mimetype.

```llama
type Encoding <: Str | derive Access
type BinaryObject 
  (bytes: Str, mimetype: Str; encoding: Encoding)
  with encodeds: {h Encoding => Str} = {encoding => bytes}

implement FromString Encoding with
  from_string s = case s.lower of
    'raw' -> Encoding('Raw')
    'base64' -> Encoding('Base64')
    'targz' -> Encoding('TarGz')
    _ -> throw Error 'Invalid encoding: #[s]'

decode (enc: Encoding) (input: Str): Str = ...
encode (enc: Encoding) (input: Str): Str = ...

get (bo: BinaryObject; encoding: Encoding='Raw') = 
  try bo.encodeds encoding
  catch KeyError do case !encoding of
    'Raw' -> 
      raw = bo.encoding.decode bo.bytes
      bo.encodeds['Raw'] := raw
    'Base64' -> case bo.encoding of
      'Raw' -> bo.encodeds['Base64'] := Encoding('Base64').encode bo.bytes
      'TarGz' -> 
        raw = Encoding('TarGz').decode bo.bytes
        bo.encodeds['Raw'] := raw
        bo.encodeds['Base64'] := Encoding('Base64').encode raw
    'TarGz' -> case bo.encoding of
      'Raw' -> bo.encodeds['TarGz'] := Encoding('TarGz').encode bo.bytes
      'TarGz' -> 
        raw = Encoding('TarGz').decode bo.bytes
        bo.encodeds['Raw'] := raw
        bo.encodeds['TarGz'] := Encoding('TarGz').encode raw
```

Using it:

```
bo1 = BinaryObject ("10100010011", 'text/plain')
assert bo1.encodeds.keys == ['Raw']
println bo1.get
assert bo1.encodeds.keys == ['Raw']
println get (bo1; encoding=Base64)
assert bo1.encodeds.keys == ['Raw', 'Base64']
```
