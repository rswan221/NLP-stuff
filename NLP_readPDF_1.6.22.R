library(tm)
library(NLP)
library(lda)
library(pdftools)

#Load the PDFs into R. **Set the working directory to CAPS folder on my thumbdrive
files <- list.files(pattern = "pdf$")

#some code was adapted from: https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/

files #check that the correct files show up

#'pdftools' function to extract text is pdf_text using lapply function
opinions <- lapply(files, pdf_text) #brought up PDF error: Invalid Font Weight

#opinions - this showed that the text was extracted. I believe it is safe to ignore the errors from the previous lines of code
length(opinions)
lapply(opinions, length) #gives the length for each PDF document

###Files loaded, now see what can be done - Corpus function creates a corpus
# URI - Uniform Resource Identifier
# readerControl tells Corpus which reader to use to read in the text from the PDF files
# readerControl argument requires a listof control parameters, one of which is reader
# therefore, we enter list(reader = readPDF) 
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF))

# Now that we have a corpus, we can create a term-document matrix (TDM)
# TDM stores counts of terms for each document, and 'tm' package provides a function TermDocumentMatrix

# This cleans up the corpus, with the list of control parameters to remove
opinions.tdm <- TermDocumentMatrix(corp,
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf))))

#Inspect the TDM to see what it looks like
inspect(opinions.tdm[1:10,])

# Can manually remove more of the punctuation with removePunctuation function & tm_map
# use the 'ucp' argument in removePunctuation (ucp = TRUE). ucp - Unicode Punctuation
corp <- tm_map(corp, removePunctuation, ucp = TRUE)

# re-create the TDM with the removePunctuation = TRUE removed
# This does a better job of removing punctuation (e.g., '--', '""', etc.)
opinions.tdm <- TermDocumentMatrix(corp,
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf))))

inspect(opinions.tdm[1:10,])

# notice there are some "half" words, e.g. 'abil', 'absenc', 'acceler', etc.
# These words are stemmed versions of longer words (e.g., 'achiev' stemmed from 'achieve', 'achieved')

# tm package can run some good summary statistics
findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf) #these are words that appear >= 100 times

ft <- findFreqTerms(opinions.tdm, lowfreq = 100, highfreq = Inf)
as.matrix(opinions.tdm[ft,])

# to seethe total counts for those words, we can save the matrix and apply the 'sum' function across rows
ft.tdm <- as.matrix(opinions.tdm[ft,])
sort(apply(ft.tdm, 1, sum), decreasing = TRUE)

###New code line to test out GitHub connection