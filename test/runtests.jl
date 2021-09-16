using PointerStructs
using Test

@testset "PointerStructs.jl" begin
  struct Bar
    a::Float64
    b::Int
  end
  struct Foo
    a::Int
    b::(c => a)
    c::Bar
    d::Vector{Int32}
  end
  
end
