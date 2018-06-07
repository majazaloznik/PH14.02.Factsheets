# VARIABLE DEFINITIONS  #######################################################
###############################################################################
# folders #####################################################################
DIR = .#
CODE = $(DIR)/code

DATA = $(DIR)/data

FIG = $(DIR)/figures

DT/P = $(DATA)/processed
DT/R = $(DATA)/raw
DT/I = $(DATA)/interim

JRN := $(DIR)/docs/journals
RPRT := $(DIR)/docs/reports

RESULTS := $(DIR)/results/human-readable

# FILES #######################################################################
# plot .eps files
FIG/.eps :=  $(wildcard $(FIG)/*.eps)

# all interim data filee
DT/I/.rds :=  $(wildcard $(DT/I)/*.rds)

# all processed files
DT/P/.rds := $(wildcard $(DT/P)/*.rds)

# poster filename
POSTER := $(DIR)/docs/presentations/PH14.02.Factsheet



# COMMANDS ####################################################################
# recipe to make .dot file  of this makefile
define make2dot
	@echo creating the .dot file from the dependencies in this makefile ----------
	python $(DIR)/code/makefile2dot.py < $< > $@
	sed -i 's/rankdir="BT"/rankdir="TB"/' $(DT/P)/make.dot	
	@echo done -------------------------------------------------------------------
endef 

# recipe to make .png file  from the dot file
define dot2png
@echo Creating the .png from the .dot ----------------------------------------
  Rscript -e "source('$<')"
@echo done -------------------------------------------------------------------
  endef

# recipe to knit pdf from first prerequisite
define rmd2pdf
@echo creating the $(@F) file by knitting it in R. ---------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown)));\
render('$<', output_dir = '$(@D)', output_format = 'pdf_document',\
quiet = TRUE )"
-rm $(wildcard $(@D)/tex2pdf*) -fr
endef 

# recipe to knit html from first prerequisite
define rmd2html
@echo creating the $(@F) file by knitting it in R.---------------------------
  Rscript -e "suppressWarnings(suppressMessages(require(rmarkdown))); \
render('$<', output_dir = '$(@D)', output_format = 'html_document',\
quiet = TRUE )"
endef 

# recipe run latex with bibtex
define tex2dvi
latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $< 
  bibtex $(basename $@)
latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $<
  latex -interaction=nonstopmode --output-directory=$(@D) --aux-directory=$(@D) $<
  endef 

# recipe run dvips for a0poster i.e. move the header file
define dvi2ps
cp docs/presentations/a0header.ps a0header.ps
dvips  -Pdownload35 -o $@ $<
  rm a0header.ps
endef

# recipe for creating pdf from ps file
define ps2pdf
ps2pdf $< $@
endef

# recipe for sourcing the prerequisite R file

define sourceR
	Rscript -e "source('$<')"
endef



# DEPENDENCIES   ##############################################################
###############################################################################

.PHONY: all

all: journal readme methods analysis dot

pdf: $(POSTER).pdf


# make chart from .dot #########################################################

dot: $(FIG)/make.png 

# make chart from .dot
$(FIG)/make.png: $(CODE)/dot2png.R $(DT/P)/make.dot
	@$(dot2png)

# make file .dot from the .makefile
$(DT/P)/make.dot: $(DIR)/Makefile
	@$(make2dot)



# journals from Rmds ###########################################################
journal: $(JRN)/journal.html $(JRN)/journal.pdf 

# journal (with graph) render to  pdf
$(JRN)/journal.pdf:  $(JRN)/journal.Rmd 
	$(rmd2pdf)

# journal (with graph) render to  html
$(JRN)/journal.html:  $(JRN)/journal.Rmd 
	$(rmd2html)


# README from Rmds #############################################################
readme: README.html

README.html: README.md
	$(rmd2html)

# methods from Rmds ############################################################
methods: $(RPRT)/methods.pdf

$(RPRT)/methods.pdf:  $(RPRT)/methods.Rmd  
	$(rmd2pdf)


# POSTER #######################################################################
$(POSTER).pdf: $(POSTER).ps
	$(ps2pdf)

$(POSTER).ps: $(POSTER).dvi
	$(dvi2ps)

$(POSTER).dvi: $(POSTER).tex docs/presentations/lit.bib $(FIG/.eps)
	$(tex2dvi)


# DATA ANALYSIS ################################################################
# plotting #####################################################################
# produces .eps plots 
#$(FIG/.eps): $(CODE)/03-data-plotting.R 
#$(sourceR)

#results: $(RESULTS)/final.data.csv	
## dependency secondary outut of 03-data-plotting
#$(RESULTS)/final.data.csv: $(CODE)/03-data-plotting.R 

## required funcitons and data needed for the plotting script
#$(CODE)/03-data-plotting.R: $(CODE)/FunPlotBar.R $(CODE)/FunTablePrep.R $(DT/P)/catalog.final.csv $(DT/P/.rds)
#touch $@
  
  
# cleaning data ################################################################
# # dependency - multiple target 
#$(DT/P/.rds): $(CODE)/02-clean.R 
#$(sourceR)

# dependency secondary outut of 02-clean.R
#$(DT/P)/catalog.final.csv: $(CODE)/02-clean.R

# all the required funcitons and data for the cleaning script
#$(CODE)/02-clean.R: $(CODE)/FunDataExtractor.R $(DT/R)/UNcodes.csv $(DT/I/.rds) $(DT/I)/catalog.rds
#touch $@
  
  
  
# importing data ################################################################
# # dependency - multiple target 
#$(DT/I/.rds): $(CODE)/01-import.R
#$(sourceR)

# dependency -- secondary output of 01-import
#$(DT/I)/catalog.rds: $(CODE)/01-import.R


analysis: $(CODE)/02-clean-data.R

# required data for input to 02-clean-data
$(CODE)/02-clean-data.R: $(DT/P)/mena.pop.rds $(DT/P)/mena.lt.rds $(CODE)/FunSpline.R
	touch $@
	
$(DT/P)/mena.pop.rds: $(CODE)/01-import.R
	Rscript -e "source('$<')"

# dependency only
$(DT/P)/mena.lt.rds: $(CODE)/01-import.R

# required data for input to 01-import
$(CODE)/01-import.R: $(DT/R)/WPP2017_PBSAS.csv $(DT/R)/WPP2017_LifeTable.csv $(DT/R)/cntry.list.csv
	touch $@
  

# download all WPP 2017 population data
$(DT/R)/WPP2017_PBSAS.csv: 
	curl -o $@ "https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationBySingleAgeSex.csv"

# download all WPP 2017 life table data
$(DT/R)/WPP2017_LT.csv:
	curl  -o $@	"https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_LifeTable.csv" 

   
       
    






