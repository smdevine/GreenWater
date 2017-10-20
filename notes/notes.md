Fri Oct 20 11:38:24 PDT 2017

Clark Fitzgerald and Scott talking after HPC workshop.

To run this code on SLURM:

1. Convert script into a script that takes command line arguments and takes
   between 5 minutes and a few hours to run. For example:
```

Rscript test.R Scott

or

./test.R Scott

```
2. Convert model parameter data into form that can be fed into the script
2. Change script to not depend on local info, ie. directories.
3. Have it run locally, then run one single job on cluster, monitor
   resource usage.
4. Run everything on cluster
