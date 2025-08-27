#!/bin/bash
#SBATCH --job-name=i9_max_error
#SBATCH --partition=tupi
#SBATCH --nodelist=tupi6
#SBATCH --time=23:59:00  
#SBATCH --output=experiments/i9_performance/i9_max_error.ou
#SBATCH --error=experiments/i9_performance/i9_max_error.err
export STARPU_DISPLAY_BINDINGS=1
export STARPU_SCHED=dmdas
export STARPU_PERF_MODEL_HOMOGENEOUS_CPU=0
rm  ~/.starpu/sampling/codelets/45/*.tupi6
. .scripts/setup_chameleon.sh
echo 0 > /sys/devices/system/cpu/cpufreq/boost
export STARPU_HISTORY_MAX_ERROR=50
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi6 experiments/i9_performance/max_error/50/.
rm  ~/.starpu/sampling/codelets/45/*.tupi6
export STARPU_HISTORY_MAX_ERROR=40
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi6 experiments/i9_performance/max_error/40/.
rm  ~/.starpu/sampling/codelets/45/*.tupi6
export STARPU_HISTORY_MAX_ERROR=30
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi6 experiments/i9_performance/max_error/30/.
rm  ~/.starpu/sampling/codelets/45/*.tupi6
export STARPU_HISTORY_MAX_ERROR=20
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi6 experiments/i9_performance/max_error/20/.
rm  ~/.starpu/sampling/codelets/45/*.tupi6
export STARPU_HISTORY_MAX_ERROR=10
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -o getrf_nopiv -t 24 -g 1 -n 32768 -b 1024
cp -r .starpu/sampling/codelets/45/*.tupi6 experiments/i9_performance/max_error/10/.
rm  ~/.starpu/sampling/codelets/45/*.tupi6
