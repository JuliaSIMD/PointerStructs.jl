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
@inline Base.axes(A::PtrArray) = map(CloseOpen, getfield(A,:size))
@inline Base.strides(A::PtrArray) = map(Int, getfield(A,:strides))
@inline Base.unsafe_convert(::Type{Ptr{T}}, A::PtrArray{T}) where {T} = getfield(A,:ptr)
@inline Base.pointer(A::PtrArray) = getfield(A, :ptr)
@inline tdot(x::Tuple{X}, i::Tuple{I1,I2}) where {X,I1,I2} = getfield(x,1)*getfield(i,1)
@inline tdot(x::Tuple{X}, i::Tuple{I}) where {X,I} = getfield(x,1)*getfield(i,1)
@inline tdot(x::Tuple{X1,X2,Vararg}, i::Tuple{I1,I2,Vararg}) where {X1,X2,I1,I2} = getfield(x,1)*getfield(i,1) + tdot(Base.tail(x), Base.tail(i))
@inline tdot(x::Tuple{X1,X2,Vararg}, i::Tuple{I}) where {X1,X2,I} = getfield(x,1)*getfield(i,1)

Base.@propagate_inbounds function Base.getindex(x::PtrArray{T}, i::Vararg{Integer,K}) where {K,T}
  Base.@boundscheck checkbounds(x, i...)
  unsafe_load(pointer(x) + sizeof(T)*tdot(strides(x),i))
end
Base.@propagate_inbounds function Base.setindex!(x::PtrArray{T}, v, i::Vararg{Integer,K}) where {K,T}
  Base.@boundscheck checkbounds(x, i...)
  unsafe_store!(pointer(x) + sizeof(T)*tdot(strides(x),i), convert(T, v))
end

@inline sxcumprod(c, sz::Tuple{}) = ()
@inline sxcumprod(c, sz::NInts) = (c, sxcumprod(c * getfield(sz,1), Base.tail(sz))...)
@inline function ptrarray(ptr::Ptr, sz::NInts{N}) where {N}
  sx = sxcumprod(One(), sz)
  PtrArray(ptr, sz, sx), ptr + last(sz)*last(sx)
end
@inline ptrarray(ptr::Ptr, sz::Vararg{Integer,N}) where {N} = ptrarray(ptr, sz)

const PtrVector{T} = PtrArray{T,1,Tuple{Int},Tuple{StaticInt{1}}}
const PtrMatrix{T} = PtrArray{T,2,Tuple{Int,Int},Tuple{StaticInt{1},Int}}

@inline function Base.getindex(A::PtrArray{T,2}, ::Colon, i::Integer) where {T}
  p = pointer(A)
  s1,s2 = A.size
  x1,x2 = A.strides
  PtrArray{T}(p+i*s2*sizeof(T), (s1,), (x1,))
end
@inline function Base.getindex(A::PtrArray{T,2}, i::Integer, ::Colon) where {T}
  p = pointer(A)
  s1,s2 = A.size
  x1,x2 = A.strides
  PtrArray{T}(p+i*s1*sizeof(T), (s2,), (x2,))
end
@inline function Base.getindex(A::PtrArray{T,3}, ::Colon, ::Colon, i::Integer) where {T}
  p = pointer(A)
  s1,s2,s3 = A.size
  x1,x2,x3 = A.strides
  PtrArray{T}(p + sizeof(T)*s3*i, (s1,s2), (x1,x2))
end


