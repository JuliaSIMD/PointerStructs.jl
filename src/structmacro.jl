ispointerstruct(@nospecialize(T)) = false
function offset end

# function offset_expr(expr_offset::Vector{Any}, known_offset::Int)
#   px = :(pointer(x))
#   for ex ∈ expr_offset
#     px = :($px + $ex)
#   end
#   known_offset == 0 ? :($px + $known_offset) : px
# end
function create_offset_expr(offset_expr::Vector{Expr}, known_offset::Int)
  offex = :($getfield(x, Symbol("##pointer##")))
  for ex ∈ offset_expr
    offex = :($offex + $ex)
  end
  known_offset == 0 ? offex : :($offex + $known_offset)
end

# create_offset_expr(offset_expr::Expr, known_offset::Int) = known_offset == 0 ? offset_expr : :($offset_expr + $known_offset)
# const EMPTY_DICT_NEST = Dict{Symbol,Dict{Symbol,Symbol}}()
const EMPTY_DICT = Dict{Symbol,Tuple{Symbol,Symbol}}()

function push_fields!(
  ret::Expr, construct_expr::Expr, structfields::Expr, offset_expr::Vector{Expr}, #pair_types::Dict{Symbol,DataType},
  sym::Symbol, @nospecialize(type::DataType), name::Symbol, known_offset::Int,
  field_map::Dict{Symbol,Tuple{Symbol,Symbol}}, ismut::Bool, skipfirst::Bool
)
  fns = fieldnames(type); fts = fieldtypes(type)
  for i ∈ 1+skipfirst:length(fns)
    fn = (fns[i])::Symbol
    unpacktup = get(field_map, fn, nothing)
    ft = fts[i]
    if unpacktup ≢ nothing
      unpackprop, unpackf = unpacktup
      # unpack from earlier
      unpack_expr = :($unpack(x, $(Val{unpackprop}())))
      if unpackf ≢ :identity
        unpack_expr = Expr(:call, unpackf, unpack_expr)
      end
      push!(construct_expr.args, unpack_expr)
      # (unpackf ≡ :identity) && (@assert get!(pair_types, unpackprop, ft) === ft)
    else
      @assert isbitstype(ft)
      if sizeof(ft) ≠ 0
        new_field_name = Symbol(sym, ".", fn)
        known_offset = push_nonnested_field!(ret, structfields, offset_expr, new_field_name, ft, name, known_offset, ismut)
        push!(construct_expr.args, :($unpack(x, $(Val{new_field_name}()))))
      else
        push!(construct_expr.args, ft.instance)
      end
    end
  end
  known_offset
end
function align_offset(st::Int, known_offset::Int)
  # half-hearted attempt at alignment
  alignment = (known_offset & min(st-1,7))
  ifelse(alignment == 0, known_offset, known_offset + min(st,8) - alignment)
end
function push_nonnested_field!(
  ret::Expr, structfields::Expr, offset_expr::Vector{Expr}, sym::Symbol, @nospecialize(typ::DataType), name::Symbol, known_offset::Int, ismut::Bool
)
  if ismut
    st = sizeof(typ)::Int
    known_offset = align_offset(st, known_offset)
    unpack_expr = :(@inline $UnPack.unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $unsafe_load($reinterpret($(Ptr{typ}), $(create_offset_expr(offset_expr, known_offset)))))
    pack_expr = :(@inline $UnPack.pack!(x::$name, ::Val{$(QuoteNode(sym))}, val) = $unsafe_store!($reinterpret($(Ptr{typ}), $(create_offset_expr(offset_expr, known_offset))), $convert($typ, val)))
    push!(ret.args, unpack_expr, pack_expr)
    known_offset += st
  else
    push!(structfields.args, Expr(:(::), sym, typ))
    unpack_expr = :(@inline $UnPack.unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $getfield(x, $(QuoteNode(sym))))
    push!(ret.args, unpack_expr)
  end
  known_offset
end

function push_field!(
  ret::Expr, structfields::Expr, offset_expr::Vector{Expr}, #pair_types::Dict{Symbol,DataType},
  sym::Symbol, @nospecialize(type::DataType), name::Symbol,
  known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, ismut::Bool
)::Int

  if Base.isbitstype(type)
    if length(fieldnames(type)) == 0
      # @show structfields
      return push_nonnested_field!(ret, structfields, offset_expr, sym, type, name, known_offset, ismut)
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
          return push_nonnested_field!(ret, structfields, offset_expr, sym, type, name, known_offset, ismut)
        end
        Expr(:new, type)
      end
    end
    known_offset = push_fields!(
      ret, construct_expr, structfields, offset_expr, #=pair_types,=# sym, type, name, known_offset,
      field_map, ismut, skipfirst
    )

    if skipfirst
      insert!(construct_expr.args, 2, create_offset_expr(offset_expr, known_offset))
      push!(offset_expr, :($offset($unpack(x, $(Val{sym}())))))
    end

    unpack_expr = :(@inline $UnPack.unpack(x::$name, ::Val{$(QuoteNode(sym))}) = $construct_expr)
    # offset_fun_expr = :(@inline $offset(x::$name, ::Val{(QuoteNode(sym))}) = $(create_offset_expr(offset_expr, known_offset)))
    push!(ret.args, unpack_expr)#, offset_fun_expr)
  elseif type <: AbstractArray # PtrArray
    known_offset = align_offset(sizeof(eltype(type))::Int, known_offset)
    N::Int = ndims(type)
    ptype = PtrArray{eltype(type), N, NTuple{N,Int}, Tuple{One,Vararg{Int,N-1}}}
    push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, ptype, name, known_offset, field_mappings, ismut)
    # push!(offset_expr, :($offset($unpack(x, $(Val{sym}())))))
  else # fall back, ordinary struct

  end
  known_offset
end
function push_field!(
  ret::Expr, structfields::Expr, offset_expr::Vector{Expr}, #pair_types::Dict{Symbol,DataType},
  sym::Symbol, typ::Symbol, name::Symbol, known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, ismut::Bool, mod::Module
)::Int
  type = getproperty(mod, typ)
  push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, type, name, known_offset, field_mappings, ismut)
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
  ret::Expr, structfields::Expr, offset_expr::Vector{Expr}, #pair_types::Dict{Symbol,DataType},
  sym::Symbol, typ::Expr, name::Symbol, known_offset::Int, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, ismut::Bool, mod::Module
)::Int
  type = curly_to_type(mod, typ)
  push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, type, name, known_offset, field_mappings, ismut)
end
function pushtofieldmapping!(ret::Expr, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, lp::Symbol, rp::Symbol, fieldname::Symbol, f::Symbol = :identity)
  get!(() -> Dict{Symbol,Symbol}(), field_mappings, lp)[rp] = (fieldname,f)
end
function addfieldmapfunction!(ret::Expr, _rpr::Expr, fieldname::Symbol)::Symbol
  rpr::Expr = _rpr
  if Meta.isexpr(rpr, :call)
    if length(rpr.args) == 2
      @assert rpr.args[2] === fieldname
      rprs = rpr.args[1]
      if rprs isa Symbol
        return rprs
      end
      rpr = rprs
    else
      throw("Only Single-arg functions are supported at the moment for lp => rp")
    end
  end
  @assert rpr.head === :(->)
  @assert rpr.args[1] isa Symbol
  anonname = gensym()
  push!(ret.args, Expr(:const, Expr(:(=), anonname, rpr)))
  anonname
end
function pushtofieldmapping!(ret::Expr, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, lp::Symbol, rp::Expr, fieldname::Symbol)
  if Meta.isexpr(rp, :tuple)
    for rpi ∈ rp.args
      if rpi isa Symbol
        pushtofieldmapping!(ret, field_mappings, lp, rpi::Symbol, fieldname)
      else
        pushtofieldmapping!(ret, field_mappings, lp, rpi::Expr, fieldname)
      end
    end
  elseif Meta.isexpr(rp, :(=), 2)
    rpl::Symbol = rp.args[1]
    rpr::Union{Expr,Symbol} = rp.args[2]
    if rpr isa Symbol
      pushtofieldmapping!(ret, field_mappings, lp, rpl, fieldname, rpr)
    else
      pushtofieldmapping!(ret, field_mappings, lp, rpl, fieldname, addfieldmapfunction!(ret, rpr, fieldname))
    end
  else
    throw("rp must be a tuple or a call, but the lp => rp was $lp => $rp")
  end
end
function pushtofieldmapping!(ret::Expr, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, lp::Expr, rp::Symbol, fieldname::Symbol)
  @assert Meta.isexpr(lp, :tuple)
  for _lps ∈ lp.args
    get!(() -> Dict{Symbol,Symbol}(), field_mappings, _lps::Symbol)[rp] = fieldname
  end
end
function pushtofieldmapping!(ret::Expr, field_mappings::Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}, lp::Expr, rp::Expr, fieldname::Symbol)
  @assert Meta.isexpr(lp, :tuple)
  if Meta.isexpr(rp, :tuple)
    for i ∈ eachindex(lp.args, rp.args)
      pushtofieldmapping!(ret, field_mappings, (lp.args[i])::Symbol, rp.args[i], fieldname)
    end
  else
    for lpi ∈ lp.args
      pushtofieldmapping!(ret, field_mappings, lpi::Symbol, rp, fieldname)
    end
  end
end
"""
  @pointer struct Foo
    a::((b,c)=>len)::Int
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
  structfields = Expr(:block, Expr(:(::), Symbol("##pointer##"), Ptr{Cvoid}))
  structdef = Expr(:struct, false, name, structfields)
  # we mutate ex
  # ret = Expr(:block, __source__, structdef, :($ispointerstruct(::Type{$name})=true))
  ret = Expr(:block, __source__, structdef, :($PointerStructs.ispointerstruct(::Type{$name})=true))
  known_offset = 0
  # offset_expr = :(pointer(x))
  # offset_expr = :($getfield(x, Symbol("##pointer##")))
  offset_expr = Expr[]
  field_mappings = Dict{Symbol,Dict{Symbol,Tuple{Symbol,Symbol}}}() # (sym,
  for field ∈ fieldargs
    if Meta.isexpr(field, :(::), 2)
      _sym::Union{Symbol,Expr} = field.args[1]
      if _sym isa Expr
        fieldname::Symbol = _sym.args[1]
        fieldmap::Expr = _sym.args[2]
        @assert fieldmap.args[1] === :(=>)
        lp::Union{Expr,Symbol} = fieldmap.args[2]
        rp::Union{Expr,Symbol} = fieldmap.args[3]
        pushtofieldmapping!(ret, field_mappings, lp, rp, fieldname)
      end
    end
  end
  # @show field_mappings
  # pair_types = Dict{Symbol,DataType}()
  for field ∈ fieldargs
    if field isa LineNumberNode
      push!(structfields.args, field)
      continue
    end
    @assert Meta.isexpr(field, :(::), 2)
    _sym::Union{Symbol,Expr} = field.args[1]
    sym::Symbol = _sym isa Symbol ? _sym : _sym.args[1]
    typ::Union{Symbol,Expr} = (field.args[2])::Union{Symbol,Expr}
    if typ isa Symbol
      # @show sym, typ, name structfields
      known_offset = push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, typ, name, known_offset, field_mappings, ismut, __module__)
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
        known_offset = push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, ptype, name, known_offset, field_mappings, ismut, __module__)
      else
        known_offset = push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, typ, name, known_offset, field_mappings, ismut, __module__)
      end
    elseif Meta.isexpr(typ, :&, 1)
      known_offset = push_field!(ret, structfields, offset_expr, #=pair_types,=# sym, (typ.args[1])::Union{Symbol,Expr}, name, known_offset, field_mappings, true, __module__)
    # elseif Meta.isexpr(typ, :call, 3)
    #   current_name = (structdef.args[2])::Union{Symbol,Expr}
    #   if current_name isa Symbol
    #     newtyp = gensym(:T)
    #     structdef.args[2] = Expr(:curly, current_name, newtyp)
    #   else
    #     newtyp = gensym(:T)
    #     push!(current_name.args, newtyp)
    #   end
    #   push!(structfields.args, Expr(:(::), sym, newtyp))
    end
  end
  if length(offset_expr) == 0
    push!(ret.args, :($PointerStructs.offset(x::$name) = $known_offset))
  else
    offex = offset_expr[1]
    for i in 2:length(offset_expr)
      offex = :($offex + $(offset_expr[i]))
    end
    offex = known_offset == 0 ? offex : :($offex + $known_offset)
    push!(ret.args, :($PointerStructs.offset(x::$name) = $offex))
  end
  # for (sym, type) ∈ pair_types
  #   offset_expr, known_offset = push_field!(ret, structfields, pair_types, sym, type, name, offset_expr, known_offset, field_mappings, ismut)
  # end
  esc(ret)
end
