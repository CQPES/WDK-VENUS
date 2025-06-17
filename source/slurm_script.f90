subroutine slurmscript(sii,finp,fout,frun)
implicit none

character*100 finp,frun,fout,sii

open(130,file=frun)

write(130,'(a)')'#!/bin/bash'
write(130,'(a)')'#SBATCH -J '//'O3-5AP-'//trim(sii)
write(130,'(a)')'#SBATCH --time=720:00:00'
write(130,'(a)')'#SBATCH -N 1 '
write(130,'(a)')'#SBATCH -p gworkq '
write(130,'(a)')'#SBATCH --ntasks-per-node=2'
write(130,'(a)')' '
write(130,'(a)')'ulimit -d unlimited'
write(130,'(a)')'ulimit -s unlimited'
write(130,'(a)')'ulimit -t unlimited'
write(130,'(a)')'ulimit -v unlimited'
write(130,'(a)')' '
write(130,'(a)')' '
write(130,'(a)')'cd $SLURM_SUBMIT_DIR'
write(130,'(a)')'username=`whoami`'
write(130,'(a)')' '
write(130,'(a)')' '
write(130,'(a)')'VENUS_TMPDIR=/tmp/${username}.$SLURM_JOBID'
write(130,'(a)')'if [ ! -a $VENUS_TMPDIR ]; then'
write(130,'(a)')'   mkdir -p $VENUS_TMPDIR'
write(130,'(a)')'fi'
write(130,'(a)')'export VENUS_TMPDIR'
write(130,'(a)')' '
write(130,'(a)')'echo "Starting Venus run at `hostname` on ${SLURM_JOB_NODELIST}:" `date` > ${SLURM_JOB_ID}.log'
write(130,'(a)')' '
write(130,'(a)')'srun -n 1 -c $SLURM_NTASKS ./venus-E.e < '//trim(finp)//' > '//trim(fout)
write(130,'(a)')' '
write(130,'(a)')'echo "Finished Venus run on ${SLURM_JOB_NODELIST}:" `date` >> ${SLURM_JOB_ID}.log'
write(130,'(a)')' '
write(130,'(a)')'rm -rf $VENUS_TMPDIR '

close(130)

return
end subroutine slurmscript
