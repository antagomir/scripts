using FdeSolver, SummarizedExperiments, Microbiome
using DataFrames, DataStructures, Random
include("funcs.jl")

# define parameters
tSpan = [0, 50]  # time span
h = 0.1          # time step
N = 20           # number of species
β = ones(N)      # order of derivatives
X0 = 2 * rand(N) # initial abundances

# define system of differential equations
Random.seed!(1234)
par = [2, 2 * rand(N), rand(N), 4 * rand(N, N), N]

function F(t, x, par)

    l = par[1] # Hill coefficient
    b = par[2] # growth rates
    k = par[3] # death rates
    K = par[4] # inhibition matrix
    N = par[5] # number of species

    F = zeros(N)
    for i in 1:N

        # inhibition functions
        f = prod(K[i, 1:end .!= i].^l ./ (K[i, 1:end .!= i] .^ l .+ x[1:end .!= i].^l))

        # ode
        F[i] = x[i] .* (b[i] .* f .- k[i] .* x[i])

    end

    return F

end

# evaluate numerical solution
t, Xapp = FDEsolver(F, tSpan, X0, β, par, h = h)

# convert transposed time series into Dictionary and store it into assays
assays = OrderedDict{String, AbstractArray}("sim" => Xapp')

# produce feature data including feature name (because it's required by the
# SummarizedExperiment function) and information on genus and species
rowdata = DataFrame(
    name = ["strain$i" for i in 1:20],
    genus = ["g$i" for i in 1:20],
    species = ["s$i" for i in 1:20]
)

# produce sample data including sample name (because it's required by the
# SummarizedExperiment function) and sampling site
coldata = DataFrame(
    name = ["t$i" for i in 1:501],
    condition = rand(["lake", "ocean", "river"], 501)
)

# create SummarizedExperiment object
se = SummarizedExperiment(assays, rowdata, coldata)

# transfer information on features and samples from se to Taxon and
# MicrobiomeSampleobjects, respectively, which can provide a link between
# SummarizedExperiments and Microbiome packages and could be automatised as a
# new method for the CommunityProfile structure
taxa = [Taxon(i, :species) for i in se.rowdata[:, 3]]
samples = MicrobiomeSample.(se.coldata[:, 1])
comm = CommunityProfile(assay(se, "sim"), taxa, samples)

# estimate shannon diversity index
shannon_diversity = shannon(comm)
shannon_output = shannon(se, "sim") # Using funcs.jl

#using SummarizedExperiments
#x = exampleobject(20, 10)
#SummarizedExperiments.coldata(x)