using PointerStructs, Aqua
using Test

@pointer struct Bar
  a::Vector{Float64}
  b::Vector{Float64}
  c::Matrix{Float64}
  d::((a,b,c,c)=>(size=tuple,size=tuple,size=x->(x,x),strides=x->(static(1),x)))::Int
end
@pointer struct Foo
  a::Int
  b::&Float64
  d::(c => d)::Int
  c::Bar
  z::&Float64
end
  

@testset "PointerStructs.jl" begin
  x = rand(10*(2+10)+2);
  GC.@preserve x begin
    foo = Foo(pointer(x), 5, 10)
    @test foo.a == 5
    @unpack a, b, d, c, z = foo;
    @test a == 5
    @test b == x[1]
    b = 3.14
    @pack! foo = b
    b = 0.0
    @unpack b = foo;
    @test b == 3.14
    
    @test d == 10
    @unpack a, b, c = c;
    @test collect(a) == view(x, 2:11)
    @test collect(b) == view(x, 12:21)
    @test collect(c) == reshape(view(x, 22:121),(10,10))

    @test z == x[end]
    z = 123.4
    @pack! foo = z;
    @test x[end] == 123.4
  end
  Aqua.test_all(PointerStructs)
end
