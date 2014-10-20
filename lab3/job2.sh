#!/bin/bash
export OMP_NUM_THREADS=$NSLOTS
R CMD BATCH --no-save lab3_spca.R lab3_spca_row2.Rout
