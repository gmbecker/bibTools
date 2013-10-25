library(bibtex)
library(XML)
res = bib_to_docbook("~/gabe/checkedout/GabeResearch/Thesis/thesisbib_bibtex.bib")

ths = xmlParse("Thesis.Rdb", xinclude = FALSE)
rt = xmlRoot(ths)

if("bibliography" %in% names(rt)) {
    rt[["bibliography"]] = res
} else {
    addChildren(rt, res)
}
saveXML(ths, file = "thesistest.Rdb")

library(RCurl)
id = httpGET("http://citeseerx.ist.psu.edu/oai2?verb=ListMetadataFormats")
stuff = httpGET("http://citeseerx.ist.psu.edu/oai2?verb=ListRecords&metadataPrefix=oai_dc")
