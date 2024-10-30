SHELL=bash

all: ../output/monthly_food.csv

../output/monthly_food.csv: monthly_food.R ../input/WLD_RTFP_mkt_2024-10-21.csv | ../output
	Rscript $<

../input:
	mkdir $@
../output:
	mkdir $@

