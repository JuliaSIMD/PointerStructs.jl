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
const EMPTY_DICT_NEST = Dict{Symbol,Dict{Symbol,Symbol}}()
const EMPTY_DICT = Dict{Symbol,Symbol}()

function push_fields!(
  ret::Expr, construct_expr::Expr, structfields::Expr, pair_types::Dict{Symbol,DataType},
  sym::Symbol, @nospecialize(type::DataType), name::Symbol, offset_expr::Expr, known_offset::Int,
  field_map::Dict{Symbol,Symbol}, ismut::Bool, skipfirst::Bool
  )
  fns = fieldnames(type); fts = fieldtypes(type)
  for i ∈ 1+skipfirst:length(fns)
    fn = (fns[i])::Symbol
    unpackprop = get(field_map, fn, nothing)
    ft = fts[i]
    if unpackprop ≢ nothing
      # unpack from earlier
      push!(construct_expr.args, :($unpack(x, $(Val{unpackprop}()))))
      @assert get!(pair_types, unpackprop, ft) === ft
    else
      new_field_name = Symbol(sym, ".", fn)
      @assert isbitstype(ft)
      offset_expr, known_offset = push_nonnested_field!(ret, structfields, new_field_name, ft, name, offset_expr, known_offset, ismut)
      push!(construct_expr.args, :($unpack(x, $(Val{new_field_name}()))))
    end
  end
  offset_expr, known_offset
end
function align_offset(st::Int, known_offset::Int)
  alignment = (known_offset & min(st-1,7))
  ifelse(alignment == 0, known_offset, known_offset + min(st,8) - alignment)
end
function push_nonnested_field!(
  ret::Expr, structfields::Expr, sym::Symbol, @nospecialize(typ::DataType), name::Symbol, offset_expr::Expr, known_offset::Int, ismut::Bool
)
  if ismut
    st = sizeof(typ)::Int
    known_offset = align_offset(st, known_offset)
    unpack_expr = :($unpack(x::$name, ::Val{$sym}) = $unsafe_load($reinterpret(Ptr{$typ}, $(create_offset_expr(offset_expr, known_offset)))))
    pack_expr = :($pack!(x::$name, ::Val{$sym}, val) = $unsafe_store!($reinterpret(Ptr{$typ}, $(create_offset_expr(offset_expr, known_offset))), $convert($typ, val)))
    push!(ret.args, unpack_expr, pack_expr)
    known_offset += st
  else
    push!(structfields.args, Expr(:(::), sym, typ))
    unpack_expr = :($unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $getfield(x, $(QuoteNode(sym))))
    push!(ret.args, unpack_expr)
  end
  offset_expr, known_offset
end

function push_field!(
  ret::Expr, structfields::Expr, pair_types::Dict{Symbol,DataType},
  sym::Symbol, @nospecialize(type::DataType), name::Symbol, offset_expr::Expr, known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Symbol}}, ismut::Bool
)::Tuple{Expr,Int}
  
  if Base.isbitstype(type)
    if length(fieldnames(type)) == 0
      return push_nonnested_field!(ret, structfields, sym, type, name, offset_expr, known_offset, ismut)
    # elseif type <: Tuple
    #   skipfirst = false
    #   construct_expr = Expr(:tuple)
    #   field_map = EMPTY_DICT
    else
      skipfirst = ispointerstruct(type)::Bool
      construct_expr = if skipfirst
        # field_map = get(() -> Dict{Symbol,Symbol}(), field_mappings, sym)
        field_map = get(field_mappings, sym, EMPTY_DICT) # allows checking field_map === EMPTY_DICT
        Expr(:call, type)
      else
        field_map = get(field_mappings, sym, nothing)
        if field_map ≡ nothing # store as collection
          return push_nonnested_field!(ret, structfields, sym, type, name, offset_expr, known_offset, ismut)
        end
        Expr(:new, type)
      end
    end
    offset_expr, known_offset = push_fields!(
      ret, construct_expr, structfields, pair_types, sym, type, name, offset_expr, known_offset,
      field_map, ismut, skipfirst
    )
    
    if skipfirst
      insert!(construct_expr.args, 2, create_offset_expr(offset_expr, known_offset))
      offset_expr = :($offset_expr + $offset($unpack(x, $(Val{sym}()))))
    end

    unpack_expr = :(@inline $unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $construct_expr)
    # offset_fun_expr = :(@inline $offset(x::$name, ::Val{(QuoteNode(sym))}) = $(create_offset_expr(offset_expr, known_offset)))
    push!(ret.args, unpack_expr)#, offset_fun_expr)
  elseif type <: AbstractArray # PtrArray
    known_offset = align_offset(sizeof(eltype(type))::Int, known_offset)
    N::Int = ndims(type)
    ptype = PtrArray{eltype(type), N, NTuple{N,Int}, Tuple{One,Vararg{Int,N-1}}}
    push_field!(ret, structfields, pair_types, sym, ptype, name, offset_expr, known_offset, field_mappings, ismut)
  else # fall back, ordinary struct
    
  end
  offset_expr, known_offset
end
function push_field!(
  ret::Expr, structfields::Expr, pair_types::Dict{Symbol,DataType},
  sym::Symbol, typ::Symbol, name::Symbol, offset_expr::Expr, known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Symbol}}, ismut::Bool, mod::Module
)::Tuple{Expr,Int}
  type = getproperty(mod, typ)
  push_field!(ret, structfields, pair_types, sym, type, name, offset_expr, known_offset, field_mappings, ismut)
end
function curly_to_type(mod::Module, typ::Expr)
  base = typ.args[1]
  if base isa Symbol
    baset = getproperty(mod, base)
  else
    @assert Meta.isexpr(base, :curly)
    baset = curly_to_type(mod, base)
  end
  curlytypes = Vector{Any}(undef, length(typ.args)-1)
  for i ∈ eachindex(curlytypes)
    c = typ.args[1+i]
    if c isa Symbol
      curlytypes[i] = getproperty(mod, c)
    elseif Meta.isexpr(base, :curly)
      curlytypes[i] = curly_to_type(mod, c)
    else
      @assert isbitstype(c)
      curlytypes[i] = c
    end
  end
  baset{curlytypes...}
end
function push_field!(
  ret::Expr, structfields::Expr, pair_types::Dict{Symbol,DataType},
  sym::Symbol, typ::Expr, name::Symbol, offset_expr::Expr, known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Symbol}}, ismut::Bool, mod::Module
)::Tuple{Expr,Int}
  type = curly_to_type(mod, typ)
  push_field!(ret, structfields, pair_types, sym, type, name, offset_expr, known_offset, field_mappings, ismut)
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
  structfields = Expr(:block, Expr(:(::), Symbol("##pointer##"), Ptr{UInt8}))
  structdef = Expr(:struct, false, name, structfields)
  # we mutate ex
  ret = Expr(:block, __source__, structdef, :($ispointerstruct(::Type{$name})=true))
  known_offset = 0
  # offset_expr = :(pointer(x))
  offset_expr = :($getfield(x, Symbol("##pointer##")))
  field_mappings = Dict{Symbol,Dict{Symbol,Symbol}}() # (sym,
  for field ∈ fieldargs
    if Meta.isexpr(field, :(::), 2)
      typ = (field.args[2])::Union{Symbol,Expr}
      if Meta.isexpr(typ, :call, 3)
        if typ.args[1] === :(=>)
          lp = typ.args[2]
          rp::Symbol = typ.args[3]
          if lp isa Symbol
            get!(() -> Dict{Symbol,Symbol}(), field_mappings, lp)[rp] = field.args[1]
          else
            @assert Meta.isexpr(lp, :tuple)
            for _lps ∈ lp.args
              get!(() -> Dict{Symbol,Symbol}(), field_mappings, _lps::Symbol)[rp] = field.args[1]
            end
          end
        end
      end
    end
  end
  pair_types = Dict{Symbol,DataType}()
  for field ∈ fieldargs
    if field isa LineNumberNode
      push!(structfields.args, field)
      continue
    end
    @assert Meta.isexpr(field, :(::), 2)
    sym::Symbol = field.args[1]
    typ = (field.args[2])::Union{Symbol,Expr}
    if typ isa Symbol
      offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, typ, name, offset_expr, known_offset, field_mappings, ismut, __module__)
    elseif Meta.isexpr(typ, :curly)
      if typ.args[1] === :SizedArray
        sxₙ = typ.args[2]
        (sx::Int,st::Vector{Any}) = if sxₙ isa Int
          sxₙ, Any[StaticInt{sx}]
        else
          0, Any[Int]
        end
        xt = Any[One()]
        for n in 3:length(typ.args)-1
          if sx == 0
            push!(ex, Int)
          else
            push!(xt, StaticInt{sx})
          end
          sxₙ = typ.args[n]
          if sxₙ isa Int
            push!(st, StaticInt{sxₙ})
            sx *= sxₙ
          else
            push!(st, Int)
            sx = 0
          end
        end
        ptype = PtrArray{getproperty(__module__, last(typ.args)), length(typ.args)-2, Tuple{st...}, Tuple{xt...}}
        offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, ptype, name, offset_expr, known_offset, field_mappings, ismut, __module__)
      else
        offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, typ, name, offset_expr, known_offset, field_mappings, ismut, __module__)
      end
    elseif Meta.isexpr(typ, :&, 1)
      offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, (typ.args[1])::Union{Symbol,Expr}, name, offset_expr, known_offset, field_mappings, true, __module__)
    end
  end
  for (sym, type) ∈ pair_types
    offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, type, name, offset_expr, known_offset, field_mappings, ismut)    
  end
  esc(ret)
end

