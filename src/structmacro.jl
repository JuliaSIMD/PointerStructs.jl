ispointerstruct(@nospecialize(T)) = false
function offset end

# function offset_expr(expr_offset::Vector{Any}, known_offset::Int)
#   px = :(pointer(x))
#   for ex ∈ expr_offset
#     px = :($px + $ex)
#   end
#   known_offset == 0 ? :($px + $known_offset) : px
# end

create_offset_expr(offset_expr::Expr, known_offset::Int) = known_offset == 0 ? offset_expr : :($offset_expr + $known_offset)

function push_getsets!(
  ret::Expr, structfields::Expr, 
  sym::Symbol, typ::Symbol, name::Symbol, offset_expr::Expr, known_offset::Int,
  field_mappings::Dict{Tuple{Symbol,Symbol},Symbol}, ismut::Bool, skipfirst::Bool
)
  construct_expr::Expr = if type <: Tuple
    Expr(:tuple)
  else
    Expr(:new, type)
  end
  fns = fieldnames(type); fts = fieldtypes(type)
  for i ∈ 1+skipfirst:length(fns)
    fn = (fns[i])::Symbol
    unpackprop = get(field_mappings, (sym,fn), nothing)
    if unpackprop ≢ nothing
      # unpack from earlier
      push!(construct_expr.args, :($unpack(x, $Val{$unpackprop}())))
    else
      ft = fts[i]
      new_field_name = QuoteNode(Symbol(sym, "_", fn))
      @assert isbitstype(ft)
      if ismut
        unpack_expr = :($unpack(x::$name, ::Val{$new_field_name}) = unsafe_load(reinterpret(Ptr{$ft}, $(create_offset_expr(offset_expr, known_offset)))))
        pack_expr = :($pack!(x::$name, ::Val{$new_field_name}, val) = unsafe_store!(reinterpret(Ptr{$ft}, $(create_offset_expr(offset_expr, known_offset))), convert($vt, val)))
        # offset_expr = :($offset_expr + $offset(x, $Val{$(QuoteNode(new_field_name))}()))
        push!(ret.args, unpack_expr, pack_expr)
        known_offset += sizeof(ft)
      else
        push!(structfields.args, Expr(:(::), new_field_name, ft))
        unpack_expr = :($unpack(x::$name, ::Val{$new_field_name}) = $getfield(x, $new_field_name))
      end
    end
  end
  construct_expr, offset_expr, known_offset
end

function push_field!(
  ret::Expr, structfields::Expr,
  sym::Symbol, typ::Symbol, name::Symbol, offset_expr::Expr, known_offset::Int, field_mappings::Dict{Tuple{Symbol,Symbol},QuoteNode}, ismut::Bool, mod::Module
)::Tuple{Expr,Int}
  type = getroperty(mod, typ)
  if ispointerstruct(type)::Bool
    construct_expr, offset_expr, known_offset = push_getsets!(
      ret, structfields, sym, typ, name, offset_expr, known_offset,
      field_mappings, ismut, true
    )
    
    unpack_expr = :(@inline $(UnPack.unpack)(x::$name, ::Val{$(QuoteNode(sym))}) = $construct_expr)
    offset_fun_expr = :(@inline $offset(x::$name, ::Val{(QuoteNode(sym))}) = $(create_offset_expr(offset_expr, known_offset)))
    push!(ret.args, unpack_expr, offset_fun_expr)

  elseif Base.isbitstype(type)
    fns = fieldnames(type);
    if length(fns) == 0
      # load/store
      if ismut
      else
        
      end
    else
      fts = fieldtypes(type)
    end
  else
  end
  offset_expr, known_offset
end

"""
  @pointer struct Foo
    a::((b,c)=>len)
    b::Bar # .b.len === a
    c::Buz # .c.len === a
  end


"""
macro pointer(ex)
  @assert ex.head === :struct
  args = ex.args
  ismut::Bool = args[1]
  name::Symbol = args[2]
  # name::Union{Symbol,Expr} = args[2]
  fieldargs::Vector{Any} = ((args[3])::Expr).args
  structfields = Expr(:block)
  structdef = Expr(:struct, false, name, structfields)
  # we mutate ex
  ret = Expr(:block, __source__, structdef)
  known_offset = 0
  offset_expr = :(pointer(x))
  field_mappings = Dict{Tuple{Symbol,Symbol},QuoteNode}() # (sym,
  for field ∈ fieldargs
    if field isa LineNumberNode
      push!(structfields.args, field)
      continue
    end
    @assert Meta.isexpr(field, :(::), 2)
    sym::Symbol = field.args[1]
    typ = (field.args[2])::Union{Symbol,Expr}
    if typ isa Symbol
      offset_expr, known_offset = push_field!(ret, structfields, sym, typ, name, offset_expr, known_offset, field_mappings, ismut, mod)
    elseif Meta.isexpr(typ, :call, 2)
      if typ.args[1] === :(=>)
        
      end
    end
  end
  ret, fieldargs
end
