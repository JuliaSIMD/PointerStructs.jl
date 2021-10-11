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

mutable struct Memory{T,L}
  data::NTuple{L,T}
  Memory{T,L}(::UndefInitializer) where {L,T} = new{T,L}()
end
@inline Base.pointer(m::Memory{T}) where {T} = Base.unsafe_convert(Ptr{T}, pointer_from_objref(m))
struct DArray{T,N,S,X,L} <: DenseArray{T,N}
  p::PtrArray{T,N,S,X}
  m::Memory{T,L}
end
@inline ptrarray(A::DArray) = getfield(A, :p)
for f ∈ [:size,:strides,:pointer]
  @eval @inline Base.$f(A::DArray) = $f(ptrarray(A))
end
Base.@propagate_inbounds function Base.getindex(A::DArray, i::Vararg{Integer,K}) where {K}
  @unpack p, m = A
  GC.@preserve m begin
    p[i...]
  end
end
Base.@propagate_inbounds function Base.getindex(A::DArray, i::Vararg{Union{Integer,Colon},K}) where {K}
  @unpack p, m = A
  GC.@preserve m begin
    DArray(p[i...], m)
  end
end
Base.@propagate_inbounds function Base.setindex!(A::DArray, v, i::Vararg{Any,K}) where {K}
  @unpack p, m = A
  GC.@preserve m begin
    p[i...] = v
  end
end

const DVector{T,L} = DArray{T,1,Tuple{Int},Tuple{StaticInt{1}},L}
const DMatrix{T,L} = DArray{T,2,Tuple{Int,Int},Tuple{StaticInt{1},Int},L}

@inline DArray(m::Memory, sz::Tuple) = DArray(ptrarray(pointer(m), sz), m)
@inline DArray{T,L}(::UndefInitializer, sz) where {T,L} = DArray(Memory{T,L}(undef), sz)

@inline function DArray{T,L}(f::F, sz) where {F,T,L}
  A = DArray(Memory{T,L}(undef), sz)
  p = pointer(A)
  for l ∈ 1:L
    unsafe_store!(p, f(T), l)
  end
  A
end
@inline DVector{T,L}(f::F, s) where {F,T,L} = DArray{T,L}(f, (s,))
@inline DMatrix{T,L}(f::F, m, n) where {F,T,L} = DArray{T,L}(f, (m,n))

# @inline function sum2to1!(x::AbstractVector, A::AbstractMatrix)
#     for i ∈ eachindex(x)
#         x[i] = A[0,i] + A[1,i]
#     end
#     x
# end
# @inline sum2to1(A::DMatrix{T}) where {T} = sum2to1!(DVector{T,MAXNEST}(undef, size(A,2)), A)


struct PushVector{T}
  ptr::Ptr{T}
end
@inline Base.length(A::PushVector) = unsafe_load(Ptr{Int}(getfield(A, :ptr)))
@inline Base.pointer(A::PushVector) = getfield(A, :ptr) + 8
@inline Base.size(A::PushVector) = (length(A),)
@inline Base.axes(A::PushVector) = (CloseOpen(length(A)), )
@inline Base.strides(A::PushVector) = (StaticInt(1),)
@inline Base.unsafe_convert(::Type{Ptr{T}}, A::PushVector{T}) where {T} = pointer(A)
Base.@propagate_inbounds function Base.getindex(A::PushVector{T}, i::Integer) where {T}
  @boundscheck checkbounds(A, i)
  unsafe_load(pointer(A) + sizeof(T)*i)
end
Base.@propagate_inbounds function Base.setindex!(A::PushVector{T}, v, i::Integer) where {T}
  @boundscheck checkbounds(A, i)
  unsafe_store!(pointer(A) + sizeof(T)*i, convert(T, v))
end
@inline function Base.push!(A::PushVector{T}, v) where {T}
  p = Ptr{Int}(getfield(A, :ptr))
  sz = unsafe_load(p)
  unsafe_store!(p, sz + 1)
  A[sz] = v
end

