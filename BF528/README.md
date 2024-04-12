Project 1 RNA-Seq methods

 The 8 Paired-end RNA-seq samples acquired from GSE64403 were assesed for quality
through fastQC and combined into a comprehensive report with multiQC. Star(v.2.7.10b)
in combination with a STAR genome index created from the gencode GRCm39 primary assembly,
was used to align the reads. Default parameters were used. Post-alignment QC was performed 
using samtools flagstats. Annotation and quantification of RNA expression was evaluated 
through VERSE(v.0.1.5) with default parameters. Statistical analysis,DESeq2(v.1.36.0), 
and fgsea(v.1.22.0) were performed in R-Studio.Sample information of the time-points 
was generated from the count-matrix column names. GSEA was performed on C2 canonical 
pathways using log2FoldChange based ranking of the differential expression results. 
Figure 1D was replicated by using deseq normalized counts and taking the row-wise mean
of the replicates.

