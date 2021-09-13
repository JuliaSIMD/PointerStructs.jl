
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

function push_field!(
  sym::Symbol, typ::Symbol, offset_expr::Expr, known_offset::Int, field_mappings::Dict{Tuple{Symbol,Symbol},Symbol}, ismut::Bool, mod::Module
)::Tuple{Expr,Int}
  type = getroperty(mod, typ)
  if ispointerstruct(type)::Bool
    off_ex = known_offset == 0 ? offset_expr : :($offset_expr + $known_offset)
    construct_expr::Expr = if type <: Tuple
      Expr(:tuple)
    else
      Expr(:new, type)
    end
    # construct_expr = Expr(:call, typ, off_ex)
    Expr(:call, typ, off_ex)
    fns = fieldnames(type); fts = fieldtypes(type)
    for i ∈ 2:length(fns)
      fn = (fns[i])::Symbol
      unpackprop = get(field_mappings, (sym,fn), nothing)
      if unpackprop ≢ nothing
        # unpack from earlier
        push!(construct_expr.args, :($unpack(x, $Val{$(QuoteNode(unpackprop))}())))
      else
        ft = fts[i]
        new_field_name = Symbol(sym, "_", fn)
        @assert isbitstype(ft)
        if ismut
          
          offset_expr = :($offset_expr + $offset(x, $Val{$(QuoteNode(new_field_name))}()))            
        else
        end
        # if isbitstype(ft)
        # elseif ismut # new mut alloc
        # else # new field
        # end
        
      end
    end
    unpack_expr = :(@inline $(UnPack.unpack)(x::$name, ::Val{$(QuoteNode(sym))}) = $construct_expr)
    offset_fun_expr = :(@inline $offset(x::$name, ::Val{(QuoteNode(sym))}) = $(create_offset_expr(offset_expr, known_offset)))
    push!(ret.args, unpack_expr, offset_fun_expr)
    
  elseif Base.isbitstype(type)
    
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
  field_mappings = Dict{Tuple{Symbol,Symbol},Symbol}() # (sym,
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
      if ispointerstruct(type)::Bool
        off_ex = known_offset == 0 ? offset_expr : :($offset_expr + $known_offset)
        construct_expr::Expr = if type <: Tuple
          Expr(:tuple)
        else
          Expr(:new, type)
        end
        # construct_expr = Expr(:call, typ, off_ex)
        Expr(:call, typ, off_ex)
        fns = fieldnames(type); fts = fieldtypes(type)
        for i ∈ 2:length(fns)
          fn = (fns[i])::Symbol
          unpackprop = get(field_mappings, (sym,fn), nothing)
          if unpackprop ≢ nothing
            # unpack from earlier
            push!(construct_expr.args, :(UnPack.unpack(x, Val{$(QuoteNode(unpackprop))}())))
          else
            ft = fts[i]
            if isbitstype(ft)
              
            elseif ismut # new mut alloc
            else # new field
            end
          end
        end
        unpack_expr = :(@inline UnPack.unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $construct_expr)
        push!(ret.args, unpack_expr)
    elseif Meta.isexpr(typ, :call, 2)
      if typ.args[1] === :(=>)
        
      end
    end
  end
  ret, fieldargs
end

