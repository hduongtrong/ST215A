#!/bin/bash
export OMP_NUM_THREADS=$NSLOTS
R CMD BATCH --no-save  lab3_spca_kmean.R kmean5.Rout
