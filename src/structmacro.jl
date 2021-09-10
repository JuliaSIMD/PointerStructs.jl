


macro pointer(ex)
  @assert ex.head === :struct
  args = ex.args
  ismut::Bool = args[1]
  name::Symbol = args[2]
  fieldargs::Vector{Any} = ((args[3])::Expr).args
  structfields = Expr(:block)
  structdef = Expr(:struct, false, name, structfields)
  # we mutate ex
  ret = Expr(:block, __source__, structdef)
  for field ∈ fieldargs
    if field isa LineNumberNode
      push!(structfields.args, field)
      continue
    end
    @assert Meta.isexpr(field, :(::), 2)
    sym::Symbol = field.args[1]
    typ = (field.args[2])::Union{Symbol,Expr}
    if typ isa Symbol
      type = getroperty(__module__, typ)
      if Base.isbitstype(type)
        
      end
    end
  end
  ret, fieldargs
end

