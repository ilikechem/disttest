#!/bin/bash
for INDEX in {1..50}; do
 	qsub -v INDEX=$INDEX Rootjob.pbs
done