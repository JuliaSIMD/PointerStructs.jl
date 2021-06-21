using PointerStructs
using Documenter

DocMeta.setdocmeta!(PointerStructs, :DocTestSetup, :(using PointerStructs); recursive=true)

makedocs(;
    modules=[PointerStructs],
    authors="Julia Computing and contributors",
    repo="https://github.com/JuliaSIMD/PointerStructs.jl/blob/{commit}{path}#{line}",
    sitename="PointerStructs.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://JuliaSIMD.github.io/PointerStructs.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/JuliaSIMD/PointerStructs.jl",
)
