abstract Maybe!{T}
immutable None!{T} <: Maybe!{T} end
immutable Some!{T} <: Maybe!{T}
  val::T
end

copy{T}(m::None!{T}) = m
copy{T}(m::Some!{T}) = Some!{T}(m.val)
typealias Array1{T} Array{T, 1}
typealias SArray Array1{String}

type L!Type
  name::String
  constructors::SArray
  attributes::Dict{String, L!Type}
  parent::Maybe!{L!Type}
end

type L!Object
  _type::L!Type
  constr::String
  values::Array1{L!Object}
  attributes::Dict{String, L!Object}
  extends::Maybe!{L!Object}
end

Maybe = L!Type("Maybe", ["Some", "None"], Dict(), None!{L!Type})

abstract L!Trait

type TVar! <: Type!
  name::String
  traits::Array{L!Trait, 1}
end

type TConst! <: Type!
  name::String
  traits::Array{L!Trait, 1}
  parent::Maybe{TConst!}
end

type TTuple! <: Type!
  contains::Array{Type!, 1}
end

type TApply! <: Type!
  left::Type!
  right::Type!
end

TApply!(t1::Type!, t2::Type!, t3::Type!) = TApply!(TApply!(t1, t2), t3)

noparent(name, traits) = TConst!(name, traits, None!{TConst!})
justname(name) = TConst!(name, [], None!{TConst!})
AbsFunction! = noparent("->", [tr!("Callable")])
TFunction!(from, to) = TApply!(AbsFunction!, from, to)

println("Ok!")
