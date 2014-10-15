#!/bin/bash
export OMP_NUM_THREADS=$NSLOTS
R CMD BATCH --no-save lab3.R lab3.5.Rout
