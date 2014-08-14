\name{fission}
\alias{fission}
\docType{data}
\title{Fission yeast time course}
\description{Read counts per gene for fission yeast time course RNA-Seq experiment}
\details{
  From the GEO series:

  Global transcription profiles of fission yeast
  wild type (WT) and atf21del strains over an osmotic stress time course
  following treatment with 1M sorbitol at 0, 15, 30, 60, 120 and 180
  mins. Strand-specific single end sequencing of total RNA was performed
  in biological triplicates on the Applied Biosystems SOLiD 5500xl
  Genetic Analyzer System.

  Sequencing reads were aligned to the fission yeast genome (PomBase
  database release 11) using SHRiMP2 aligner with default
  parameters. Total number of reads that can be aligned to the genome at
  exactly one locus per sample range from 7.5 to 20.1 millions. These
  uniquely mapped reads were used to identify stretches of unambiguous
  transcription. Reads that aligned to more than one locus (generally
  paralogous regions in the genome) were discarded. Adjacent unambiguous
  transcription regions with minimum peak height of two and located
  within 50 bases of each other were merged to yield an extensive
  transcription map of S. pombe. These regions were then positioned
  relative to known annotation and labelled according to the gene(s)
  they overlapped with using the Bioconductor package annmap.
}
\usage{
data("fission")
}
\format{SummarizedExperiment}
\source{Read count matrix prepared and provided by authors of the study}
\references{Leong HS, Dawson K, Wirth C, Li Y, Connolly Y, Smith DL,
  Wilkinson CR, Miller CJ. 'A global non-coding RNA system modulates
  fission yeast protein levels in response to stress'. Nat Commun
  2014. PMID: 24853205. GEO: GSE56761.}
\keyword{datasets}
