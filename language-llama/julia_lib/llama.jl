# L!Type is the type of Llama types.
abstract L!Type

# L! is the type of Llama values.
abstract L!
type L!int <: L! val::Int64 end
type L!str <: L! val::String end
type L!Object <: L!
  _type::L!Type
  constr::String
  values::Array{L!, 1}
  attributes::Dict{String, L!}
  extends::Union(L!, Nothing)
end

L!Object(_type::L!Type, constr::String) =
  L!Object(_type, constr, [], Dict(), Nothing())

# A constraint on a type. Asserts `name` exists of type `_type`.
type L!Constraint
  name::String
  _type::L!Type
end

# A trait is a set of one or more constraints.
type L!Trait
  constraints::Set{L!Constraint}
end

# A type variable, for polymorphic types.
type L!TVar <: L!Type name::String end

# A constant type. Defines what constructors and attributes are legal,
# and might have a parent.
type L!TConst <: L!Type
  name::String
  constructors::Array{String, 1}
  parent::Union(L!TConst, Nothing)
  attributes::Dict{String, L!Type}
end

type L!TApply <: L!Type
  left::L!Type
  right::L!Type
end

L!TConst(name::String) = L!TConst(name, [], Nothing(), Dict())

L!TApply(t1::L!Type, t2::L!Type, t3::L!Type) = L!TApply(L!TApply(t1, t2), t3)
function L!TApply(types)
  if length(types) < 2
    error("Not enough types to apply, need at least 2")
  end
  t = L!TApply(types[1], types[2])
  for t_ in types[3:end]
    t = L!TApply(t, t_)
  end
  t
end

L!absfunc!t = L!TConst("->")
L!func!t(from::L!Type, to::L!Type) = L!TApply(L!absfunc!t, from, to)
L!int!t = L!TConst("Int")
L!str!t = L!TConst("Str")

function L!tuple(types::Array{L!Type, 1})
  tup = L!TConst("Tuple($(length(types)))")
  if length(types) == 0
    return tup
  elseif length(types) == 1
    return types[1]
  end
  L!TApply(vcat([tup], types))
end

f = L!func!t(L!int!t, L!str!t)

println(f)
println()
println(L!tuple([L!int!t, L!str!t, f]))

type L!ValueSet
  values::Dict{L!Type, L!}
end

function single(v::L!)
  L!ValueSet({v._type => v})
end

function addval(vset::L!ValueSet, v::L!)
  insert!(vset.values, v._type, v)
end

i = L!int(1)
Maybe = L!TConst("Maybe")

println("Ok!")
