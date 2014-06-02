A `BinaryObject` is something which stores one or more encodings of a file, and only ever needs to be decoded once for any of its encodings. It keeps track of its mimetype.

```llama
type Encoding = Raw; Base64; TarGz
type BinaryObject 
  (bytes: Str, mimetype: Str; encoding: Encoding)
  with encodeds: Hash {Encoding=>Str} = {encoding => bytes}

get (bo: BinaryObject; encoding: Encoding=Raw) = 
  try bo.encodeds encoding
  catch KeyError do case encoding of
    Raw -> 
      raw = bo.encoding.decode bo.bytes
      bo.encodeds[Raw] := raw
    Encoding.Base64 -> case bo.encoding of
      Raw -> bo.encodeds[Base64] := Base64.encode bo.bytes
      TarGz -> 
        raw = TarGz.decode bo.bytes
        bo.encodeds[Raw] := raw
        bo.encodeds[Base64] := Base64.encode raw
    TarGz -> case bo.encoding of
      Raw -> bo.encodeds[TarGz] := TarGz.encode bo.bytes
      TarGz -> 
        raw = TarGz.decode bo.bytes
        bo.encodeds[Raw] := raw
        bo.encodeds[TarGz] := TarGz.encode raw
```

bo1 = BinaryObject ("10100010011", 'text/plain')
assert bo1.encodeds.keys == [Raw]
println bo1.get
assert bo1.encodeds.keys == [Raw]
println get (bo1; encoding=Base64)
assert bo1.encodeds.keys == [Raw, Base64]

