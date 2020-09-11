# PP2020

This repository contains supplementary material for my presentation _“Automatische Optimierungsverfahren in der akustischen Analyse soziophonetischer Feldaufnahmen”_ at the (virtual) P&amp;P 2020, 16. Tagung zur Phonetik und Phonologie
im deutschsprachigen Raum, Trier. <www.pundp2020.uni-trier.de>

## Contact

**Daniel Duran**

daniel.duran@germanistik.uni-freiburg.de

http://paul.igl.uni-freiburg.de/duran/


## Dependencies

* a working installation of Praat <http://www.fon.hum.uva.nl/praat/>
* the R package **foreach** <https://CRAN.R-project.org/package=foreach>
* the R package **doMC** (on Linux) <https://CRAN.R-project.org/package=doMC>
* the R package **doSNOW** (on Windows) <https://CRAN.R-project.org/package=doSNOW>
* the R package **lme4** <https://cran.r-project.org/package=lme4>
* the R package **textgRid** <https://cran.r-project.org/package=textgRid>
* R packages from **tidyverse** <https://cran.r-project.org/package=tidyverse>
* audio (wav) and TextGrid files (not provided)


## Experiment 1

Semi-supervised optimization of Praat analysis parameters for voicing measurements by recording room.

### Running the optimization

In order to run the optimization, you only need to run the R-script **optVoicing.R**.

#### Configuration

The main configuration is done within the R-script, by setting the respective parameters in the list-object **KONFIG** at the beginning of the script.
Specifically, you need to set the paths to the Praat executable, the path to the Praat script **voice-Advanced.praat**, and the path to the input files table CSV-file: **EXAMPLEvoicingFiles.csv**.

If the script crashes or if it is aborted for some reason, you may chose to initialize the search with previously found _optimal_ parameters.

### Issues

* The number of available voice labels per room is not checked. This implementation requires a minimum of 2 tokens per label (in function load_gold_voice_annotations). However, a (much) larger number should be provided for reasonable optimization results.
* The train/test split is random. Set seed to achive identical results in different runs.
* Praat runs always on all data. The train/test split is taken into accout only for the evaluation.


## Experiment 2

Unsupervised optimization of Praat analysis parameters for formant measurements by speaker.

### Running the optimization

In order to run the optimization, you only need to run the R-script **optFormants.R**.

#### Configuration

The main configuration is done within the R-script, by setting the respective parameters in the list-object **KONFIG** at the beginning of the script.
Specifically, you need to set the paths to the Praat executable, the path to the Praat script **extractFormantsChunked.praat**, and the path to the input files table CSV-file: **EXAMPLEformantFiles.csv**.


## Acknowledgements

This work was funded by the German Research Foundation (DFG), within the research project _Soziophonetische Untersuchungen zum deutschen Multi-Ethnolekt_; grant to [Prof. Dr. Dr. h.c. Peter Auer](http://portal.uni-freiburg.de/sdd/personen/auer/).

## References

* Auer, Peter, Daniel Duran, and Christina Davril: _“Voicing and Vowel Quality in the German (Multi-)Ethnolect.”_ Presentation presented at the International Conference on Language Variation in Europe (ICLaVE 10), Leeuwarden (NL), June 2019. <https://www.fryske-akademy.nl/en/iclave100>
* Douglas Bates, Martin Mächler, Ben Bolker and Steve Walker: _“Fitting Linear Mixed-Effects Models Using lme4.”_ Journal of Statistical Software, 67:1, 2015: 1-48. DOI: 10.18637/jss.v067.i01
* Boersma, Paul, and David Weenink: _“Praat: Doing Phonetics by Computer.”_ <http://www.praat.org>
* Kisler, Thomas, Uwe Reichel, and Florian Schiel: _“Multilingual Processing of Speech via Web Services.”_ Computer Speech &amp; Language 45 (September 2017): 326–47. DOI: 10.1016/j.csl.2017.01.005
* Microsoft and Steve Weston: _“foreach: Provides Foreach Looping Construct.”_ R package version 1.4.7. 2019. <https://CRAN.R-project.org/package=foreach>
* Nelder, J. A., and R. Mead. _“A Simplex Method for Function Minimization.”_ The Computer Journal, 7:4, 1965: 308–13. DOI: 10.1093/comjnl/7.4.308
* Hong Ooi, Microsoft Corporation and Stephen Weston: _“doSNOW: Foreach Parallel Adaptor for the 'snow' Package.”_ R package. 2019. <https://CRAN.R-project.org/package=doSNOW>
* Patrick Reidy: _“textgRid: Praat TextGrid Objects in R.”_ 2016. R package. <https://CRAN.R-project.org/package=textgRid>
* R Core Team: _“R: A Language and Environment for Statistical Computing.”_ R Foundation for Statistical Computing. Vienna, Austria. 2019. <https://www.R-project.org>
* Revolution Analytics and Steve Weston: _“doMC: Foreach Parallel Adaptor for 'parallel'.”_ R package version 1.3.6. 2019. <https://CRAN.R-project.org/package=doMC>
* Schiel, Florian: _“Automatic Phonetic Transcription of Non-Prompted Speech.”_ In 14th International Congress of Phonetic Sciences (ICPhS-14), 607–10, 1999. <https://www.internationalphoneticassociation.org/icphs-proceedings/ICPhS1999/p14_0607.html>
* Hadley Wickham et al.: _“Welcome to the tidyverse.”_ Journal of Open Source Software, 4:43, 2019: 1686. DOI: 10.21105/joss.01686
