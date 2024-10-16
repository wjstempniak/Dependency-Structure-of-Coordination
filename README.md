## Dependency Structure of Coordination - an Analysis of Universal Dependencies Corpora

The project is a cross-linguistic analysis of UD corpora. Its main goal is to replicate and extent a study done by Przepiórkowski and Woźniak (2023). 
It consists of:
- finding and extracting coordinations in order to examine tendencies to put the shorter conjunct at the begining of coordination,
- a comparison of coordinations in different languages focused on their head-directionality, and
- an analysis of the interaction of DLM effect on cooridination composition.

### Contents
- data_processing
   - extracting_coordinations.py -  extracting coordinations from UD corpora and save their data (except for conjunct heads data)
   - conjunct_heads.py - extracting coordinations from UD corpora and save conjunct heads data
   - counting_syllables.py - algorithm used to count syllables
   - languages.py - dictionary containing data about used languages, e.g. which heuristics were used in which language
   - scraping.py - code used to get an UD corpora list
   - treebanks.csv - obtained corpora list and data
- evaluation
   - evaluation.R - obtaining a sample of English and Polish coordinations for evaluation
   - evaluation_tr.R - obtaining a sample of Turkish coordinations for evaluation
   - English/Polish/Turkish - folders containing respectively: a sample, a sample evaluated by a competent language user and a sample with resolved conflicts
- stats
   - ptb_coordinations_acl2023_modified.r - original code used analisis in the work of Przepiórkowski and Woźniak (2023)
   - analysis.R - computing monofactorial logistic regression for the change in proportion of the first conjunct of coordination appearing beaing shorter than the last by the absolute difference change between the conjuncts length (measured with words, syllables or characters) for different governor position and language (the tendencies and contrasts)
   - conjunct-length.R - checking which conjunct tends to be shorter in different languages
   - gov-pos.R - proportions of coordinations with different governor position by language
   - heads.R -  where does the conjunct head tend to be in different languages (double-checking head-directionality of languages)
- thesis
   - results - results of computations found in stats directory
   - the rest of the directory constitutes contents of thesis describing the project in Polish.
  
### References

Przepiórkowski, A., & Woźniak, M. (2023). [Conjunct Lengths in English, Dependency Length Minimization, and Dependency Structure of Coordination](https://aclanthology.org/2023.acl-long.864). In *Proceedings of the 61st Annual Meeting of the Association for Computational Linguistics (Volume 1: Long Papers)* (pp. 15494-15512).
