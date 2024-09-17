#!/bin/bash
#SBATCH -J hmmer_annotate            # job name
#SBATCH -o hmmer_annotate.o%j        # output and error file name (%j expands to jobID)
#SBATCH -e hmmer_annotate.e%j        # name of stderr error file.
#SBATCH -N 1                                             # number of nodes requested
#SBATCH -n 15                                            # total number of mpi tasks requested
#SBATCH -p normal                                        # queue (partition) -- normal, development, etc.
#SBATCH -t 6-00:00:00                                      # run time (hh:mm:ss) - 48 hours
#SBATCH --mail-user=mnc3287@mavs.uta.edu
#SBATCH --mail-type=begin                                # email me when the job starts
#SBATCH --mail-type=end                                  # email me when the job finishes

PATH=$PATH:/home/mnc3287/hmmer-3.4/src

for FILE in *.fa; do
        echo ${FILE}
        SAMP=$(basename -s .fa $FILE)
        echo $SAMP

/home/mnc3287/hmmer-3.4/src/hmmsearch --tblout ${SAMP}_tblout.txt --incE .00001 --cpu 10 /home/mnc3287/hmmer-3.4/Pfam-A.hmm.gz ${SAMP}.fa
/home/mnc3287/hmmer-3.4/src/hmmsearch --domtblout ${SAMP}_domtblout.txt --incE .00001 --cpu 10 /home/mnc3287/hmmer-3.4/Pfam-A.hmm.gz ${SAMP}.fa
/home/mnc3287/hmmer-3.4/src/hmmsearch --pfamtblout ${SAMP}_pfamtblout.txt --incE .00001 --cpu 10 /home/mnc3287/hmmer-3.4/Pfam-A.hmm.gz ${SAMP}.fa
done 

#/home/mnc3287/hmmer-3.4/src/hmmsearch --domtblout tyrosinase_protein_update_4_8_2024_domtblout.txt --incE .00001 --cpu 10 /home/mnc3287/hmmer-3.4/Pfam-A.hmm.gz tyrosinase_protein_update_4_8_2024.fa
#/home/mnc3287/hmmer-3.4/src/hmmsearch --pfamtblout tyrosinase_protein_update_4_8_2024_pfamtblout.txt --incE .00001 --cpu 10 /home/mnc3287/hmmer-3.4/Pfam-A.hmm.gz tyrosinase_protein_update_4_8_2024.fa

