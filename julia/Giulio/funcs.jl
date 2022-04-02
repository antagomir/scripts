# method of Microbiome.shannon compatible with SummarizedExperiments
# the inputs are the se object and the name of the assay
function Microbiome.shannon(se::SummarizedExperiment, assay_name::String)

    # create empty vector with length = n° samples
    shannon_vector = zeros(size(assay(se, assay_name))[2])

    # evaluate shannon diversity index for each sample / column
    for sample in 1:size(assay(se, assay_name))[2]

        v = assay(se, assay_name)[:, sample]

        total = sum(v)
        relab = map(x -> x / total, v)

        shannon_vector[sample] = -sum([log(x^x) for x in relab])

    end

    # output vector with results for each sample
    return shannon_vector

end


