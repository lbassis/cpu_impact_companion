#!/bin/bash
#SBATCH --job-name=xeon_max_error
#SBATCH --partition=tupi
#SBATCH --nodelist=tupi1
#SBATCH --time=23:59:00  
#SBATCH --output=experiments/xeon_performance/xeon_max_error.ou
#SBATCH --error=experiments/xeon_performance/xeon_max_error.err
export STARPU_DISPLAY_BINDINGS=1
export STARPU_SCHED=dmdas
rm  ~/.starpu/sampling/codelets/45/*.tupi1
. .scripts/setup_chameleon_xeon.sh
echo 0 > /sys/devices/system/cpu/cpufreq/boost
export STARPU_HISTORY_MAX_ERROR=50
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi1 experiments/xeon_performance/max_error/50/.
rm  ~/.starpu/sampling/codelets/45/*.tupi1
export STARPU_HISTORY_MAX_ERROR=40
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi1 experiments/xeon_performance/max_error/40/.
rm  ~/.starpu/sampling/codelets/45/*.tupi1
export STARPU_HISTORY_MAX_ERROR=30
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi1 experiments/xeon_performance/max_error/30/.
rm  ~/.starpu/sampling/codelets/45/*.tupi1
export STARPU_HISTORY_MAX_ERROR=20
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi1 experiments/xeon_performance/max_error/20/.
rm  ~/.starpu/sampling/codelets/45/*.tupi1
export STARPU_HISTORY_MAX_ERROR=10
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi1 experiments/xeon_performance/max_error/10/.
rm  ~/.starpu/sampling/codelets/45/*.tupi1
