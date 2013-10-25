

bib_to_docbook = function(bibfile, bibdata = read.bib(bibfile))
{
    bibdb = newXMLNode("bibliography")
    sapply(bibdata, bibentry_to_db, parent = bibdb)
    bibdb
}


bibentry_to_db <- function(entry, parent)
{
    node <- switch(attr(unclass(entry)[[1]], "bibtype"),
                   Article = bibarticle_to_db(entry, parent),
                   InProceedings = bibarticle_to_db(entry, parent),
                   InCollection = bibarticle_to_db(entry, parent),
                   Misc = bibmisc_to_db(entry, parent))
    parent
}


bibarticle_to_db = function(data, parent)
{
    entry = newXMLNode("biblioentry", attrs= c(id = attr(unclass(data)[[1]], "key")), parent = parent)
 

    article <- newXMLNode("biblioset", attrs = c(relation="article"), parent = entry)
    agroup <- newXMLNode("authorgroup", kids = sapply(data$author, person_to_db), parent = article)
    newXMLNode("title", clean_brackets(data$title), parent = article)
    if(!is.null(data$pages))
        newXMLNode("pagenums", data$pages, parent = article)
    if(!is.null(data$doi))
        newXMLNode("biblioid", data$doi, attrs = c(class="doi"), parent = article)
 
    journal = newXMLNode("biblioset", attrs = c(relation = "journal"), parent = entry)
    newXMLNode("publisher", newXMLNode("publishername", clean_brackets(data$publisher)), parent = journal)
    if(!is.null(data$isbn))
        newXMLNode("isbn", data$isbn, parent = entry)
    newXMLNode("issuenum", data$number, parent = journal)
    newXMLNode("volumenum", data$volume, parent = journal)
    newXMLNode("pubdate", data$year, parent = journal)
    if(!is.null(data$journal))
        jtitle = data$journal
    else if (!is.null(data$booktitle))
        jtitle = data$booktitle
    
    newXMLNode("title", clean_brackets(data$journal), parent = journal)
    if(!is.null(data$url))
        newXMLNode("ulink", attrs = c("url" = data$url), parent = entry)
    entry
 }

person_to_db = function(person)
{
    author = newXMLNode("author", newXMLNode("firstname", clean_brackets(person$given)), newXMLNode("surname", clean_brackets(person$family)))
    author

}

clean_brackets = function(char) gsub("\\{([^\\}]*)\\}", "\\1", char)


bibmisc_to_db = function(data, parent)
{
    entry = newXMLNode("biblioentry", attrs = c(id = attr(unclass(data)[[1]], "key")), parent = parent)
    agroup <- newXMLNode("authorgroup", kids = sapply(data$author, person_to_db), parent = entry)
    newXMLNode("title", clean_brackets(data$title), parent = entry)
    if(!is.null(data$url))
        newXMLNode("ulink", attr=c(url=data$url), parent= entry)
    if(!is.null(data$year))
        newXMLNode("pubdate", data$year, parent = entry)
    entry
}

