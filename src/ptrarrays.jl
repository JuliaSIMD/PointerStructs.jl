const NInts{N} = Tuple{Vararg{Union{Int,StaticInt},N}}
struct PtrArray{T,N,S<:NInts{N},X<:NInts{N}} <: DenseArray{T,N}
  ptr::Ptr{T}
  size::S
  strides::X
end
ispointerstruct(::Type{<:PtrArray}) = true
# `(... + 7) & -8` is eliminated whenever `sizeof(T) == 8`
@inline offset(A::PtrArray{T}) where {T} = (sizeof(T) * length(A) + 7) & -8

@inline Base.size(A::PtrArray) = map(Int, getfield(A,:size))
@inline Base.length(A::PtrArray) = prod(size(A))
# @inline Base.axes(A::PtrArray) = map(CloseOpen, getfield(A,:size))
@inline Base.axes(A::PtrArray) = map(SafeCloseOpen, getfield(A,:size))
@inline Base.strides(A::PtrArray) = map(Int, getfield(A,:strides))
@inline Base.unsafe_convert(::Type{Ptr{T}}, A::PtrArray{T}) where {T} = getfield(A,:ptr)
@inline Base.pointer(A::PtrArray) = getfield(A, :ptr)

@inline sxcumprod(c, sz::Tuple{}) = ()
@inline sxcumprod(c, sz::NInts) = (c, sxcumprod(c * getfield(sz,1), Base.tail(sz))...)
@inline function ptrarray(ptr::Ptr, sz::NInts{N}) where {N}
  sx = sxcumprod(One(), sz)
  PtrArray(ptr, sz, sx), ptr + last(sz)*last(sx)
end
@inline ptrarray(ptr::Ptr, sz::Vararg{Integer,N}) where {N} = ptrarray(ptr, sz)

const PtrVector{T,L} = PtrArray{T,1,Tuple{L},Tuple{StaticInt{1}}}
const PtrMatrix{T,M,N} = PtrArray{T,2,Tuple{M,N},Tuple{StaticInt{1},M}}
const DPtrVector{T} = PtrVector{T,Int}
const DPtrMatrix{T} = PtrMatrix{T,Int,Int}

@inline filtercolon(p::Ptr, s::Tuple{}, x::Tuple{}, i::Tuple{}) = p, (), ()
@inline function filtercolon(p::Ptr{T}, s::Tuple{S}, x::Tuple{X}, i::Tuple{AbstractUnitRange}) where {T,S,X}
  r = getfield(i,1)
  x1 = getfield(x,1)
  p + sizeof(T)*(x1*first(r)), (length(r),), (x1,)
end
@inline filtercolon(p::Ptr, s::Tuple{S}, x::Tuple{X}, i::Tuple{Colon}) where {S,X} = p, (getfield(s,1),), (getfield(x,1),)
@inline filtercolon(p::Ptr{T}, s::Tuple{S}, x::Tuple{X}, i::Tuple{Integer}) where {S,T,X} = p + sizeof(T)*(getfield(x,1)*getfield(i,1)), (), ()

@inline function filtercolon(p::Ptr{T}, s::Tuple{S1,S2,Vararg}, x::Tuple{X1,X2,Vararg}, i::Tuple{I1,I2,Vararg}) where {T,S1,S2,X1,X2,I1<:AbstractUnitRange,I2}
  p2, s2, x2 = filtercolon(p, Base.tail(s), Base.tail(x), Base.tail(i))
  r = getfield(i,1); x1 = getfield(x,1)
  p2 + sizeof(T)*(x1*first(r)), (length(r), s2...), (x1, x2...)
end
@inline function filtercolon(p::Ptr, s::Tuple{S1,S2,Vararg}, x::Tuple{X1,X2,Vararg}, i::Tuple{Colon,I1,Vararg}) where {S1,S2,X1,X2,I1}
  p2, s2, x2 = filtercolon(p, Base.tail(s), Base.tail(x), Base.tail(i))
  p2, (getfield(s,1), s2...), (getfield(x,1), x2...)
end
@inline function filtercolon(p::Ptr{T}, s::Tuple{S1,S2,Vararg}, x::Tuple{X1,X2,Vararg}, i::Tuple{Integer,I1,Vararg}) where {T,S1,S2,X1,X2,I1}
  p2, s2, x2 = filtercolon(p, Base.tail(s), Base.tail(x), Base.tail(i))
  p2 + sizeof(T)*(getfield(x,1)*getfield(i,1)), s2, x2
end

@inline _getindex(p::Ptr, s::Tuple{}, x::Tuple{}) = unsafe_load(p)
@inline _getindex(p::Ptr, s::Tuple{S,Vararg}, x::Tuple{X,Vararg}) where {S,X} = PtrArray(p, s, x)
Base.@propagate_inbounds function Base.getindex(A::PtrArray{T,N}, i::Vararg{Union{Integer,Colon},N}) where {T,N}
  Base.@boundscheck checkbounds(A, i...)
  p, s, x = filtercolon(pointer(A), A.size, A.strides, i)
  _getindex(p, s, x)
end
Base.@propagate_inbounds function Base.getindex(A::PtrArray{T,N}, i::Integer) where {T,N}
  Base.@boundscheck checkbounds(A, i)
  _getindex(pointer(A) + i*sizeof(T), (), ())
end
Base.@propagate_inbounds function Base.getindex(A::PtrArray{T,1}, i::Integer) where {T}
  Base.@boundscheck checkbounds(A, i)
  _getindex(pointer(A) + i*sizeof(T), (), ())
end
Base.@propagate_inbounds function Base.getindex(A::PtrArray{T,1}, r::AbstractUnitRange) where {T}
  Base.@boundscheck checkbounds(A, r)
  p, s, x = filtercolon(pointer(A), A.size, A.strides, (r,))
  _getindex(p, s, x)
end

@inline _setindex!(p::Ptr{T}, v, ::Tuple{}, ::Tuple{}) where {T} = unsafe_store!(p, convert(T, v))
@inline _setindex!(p::Ptr{T}, v::AbstractArray{<:Any,0}, ::Tuple{}, ::Tuple{}) where {T} = unsafe_store!(p, convert(T, v[]))
@inline function _setindex!(p::Ptr, v::AbstractArray{<:Any,N}, s::NInts{N}, x::NInts{N}) where {N}
  d = PtrArray(p, s, x)
  for i in eachindex(d, v)
    @inbounds d[i] = v[i]
  end
end

Base.@propagate_inbounds function Base.setindex!(A::PtrArray{T,N}, v, i::Vararg{Union{Integer,Colon},N}) where {T,N}
  Base.@boundscheck checkbounds(A, i...)
  p, s, x = filtercolon(pointer(A), A.size, A.strides, i)
  _setindex!(p, v, s, x)
end
Base.@propagate_inbounds function Base.setindex!(A::PtrArray{T,N}, v, i::Integer) where {T,N}
  Base.@boundscheck checkbounds(A, i)
  _setindex!(pointer(A) + sizeof(T)*i, v, (), ())
end
Base.@propagate_inbounds function Base.setindex!(A::PtrArray{T,1}, v, i::Integer) where {T}
  Base.@boundscheck checkbounds(A, i)
  _setindex!(pointer(A) + sizeof(T)*i, v, (), ())
end

Base.@propagate_inbounds Base.getindex(A::PtrArray{<:Any,N}, I::CartesianIndices{N}) where {N} = A[Tuple(I)...]
Base.@propagate_inbounds Base.setindex!(A::PtrArray{<:Any,N}, v, I::CartesianIndices{N}) where {N} = A[Tuple(I)...] = v

function Base.getindex(a::PtrVector{T}, i::PtrVector{I}) where {T,I<:Integer}
  b = Vector{T}(undef, length(a))
  @inbounds for j ∈ eachindex(i)
    b[j+1] = a[i[j]]
  end
  return b
end

_indstyle(::StaticInt{1}) = Base.IndexLinear()
_indstyle(_) = Base.IndexCartesian()
Base.IndexStyle(A::PtrVector) = _indstyle(getfield(A.strides,1))

Base.LinearIndices(x::PtrVector) = CloseOpen(length(x))
function Base.Array(A::PtrArray{T}) where {T}
  B = Array{T}(undef, size(A))
  @inbounds for I ∈ eachindex(A)
    B[I + one(I)] = A[I]
  end
  return B
end
Base.Array{<:Any,N}(A::PtrArray{<:Any,N}) where {N} = Array(A)

