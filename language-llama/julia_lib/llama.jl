abstract Option{T}
immutable None{T} <: Option{T} end
immutable Some{T} <: Option{T}
  val::T
end

copy{T}(m::None{T}) = m
copy{T}(m::Some{T}) = Some{T}(m.val)

type LlamaType
  name::String
  constructors::Array{String, 1}
  attributes::Dict{String, LlamaType}
  parent::Option{LlamaType}
end

type LlamaObject
  _type::LlamaType
  extends::Option{LlamaObject}
  baz::Int
  qux::Float64
end
