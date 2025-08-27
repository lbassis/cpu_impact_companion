#!/bin/bash
#SBATCH --job-name=i9_parquet_analysis_fixed_freq
#SBATCH --partition=tupi
#SBATCH --nodelist=tupi5
#SBATCH --time=08:00:00  
#SBATCH --output=experiments/i9_performance/i9_parquet_analysis_fixed_freq_tupi5.ou
#SBATCH --error=experiments/i9_performance/i9_parquet_analysis_fixed_freq_tupi5.err
export STARPU_DISPLAY_BINDINGS=1
export STARPU_SCHED=dmdas
export STARPU_FXT_TRACE=1
export STARPU_GENERATE_TRACE=1
export STARPU_FXT_PREFIX=~/trace
export PATH=~/spack-trace/opt/spack/linux-debian12-skylake/gcc-11.3.0/pajeng-1.3.6-ya3f3vqnnurys2k6octtwkf6ks2dtm5k/bin:$PATH
export PATH=~/spack-trace/opt/spack/linux-debian12-skylake/gcc-11.3.0/starpu-master-biiplxobhxnnhr76rzh55rs7nws6rvhi/bin:$PATH
export PATH=~/spack-trace/opt/spack/linux-debian12-skylake/gcc-11.3.0/chameleon-1.2.0-bk2eehdrxllwbi6un6h4fjdozekwlyjd/bin:$PATH
rm -rf ~/.starpu
export STARVZ_TOOLS=$(Rscript -e 'cat(system.file("tools/", package = "starvz"), sep="\n")')
echo 0 > /sys/devices/system/cpu/cpufreq/boost
rm ~/trace/*
echo 3201000 > /sys/devices/system/cpu/cpu0/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu0/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu1/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu1/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu1/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu1/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu2/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu2/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu2/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu2/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu3/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu3/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu3/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu3/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu4/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu4/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu4/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu4/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu5/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu5/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu5/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu5/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu6/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu6/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu6/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu6/cpufreq/scaling_setspeed
echo 3201000 > /sys/devices/system/cpu/cpu7/cpufreq/scaling_min_freq
echo 3201000 > /sys/devices/system/cpu/cpu7/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu7/cpufreq/scaling_governor
echo 3201000 > /sys/devices/system/cpu/cpu7/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu8/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu8/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu8/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu8/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu9/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu9/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu9/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu9/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu10/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu10/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu10/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu10/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu11/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu11/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu11/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu11/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu12/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu12/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu12/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu12/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu13/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu13/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu13/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu13/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu14/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu14/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu14/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu14/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu15/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu15/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu15/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu15/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu16/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu16/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu16/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu16/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu17/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu17/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu17/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu17/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu18/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu18/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu18/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu18/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu19/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu19/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu19/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu19/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu20/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu20/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu20/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu20/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu21/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu21/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu21/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu21/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu22/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu22/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu22/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu22/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu23/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu23/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu23/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu23/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu24/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu24/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu24/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu24/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu25/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu25/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu25/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu25/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu26/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu26/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu26/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu26/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu27/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu27/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu27/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu27/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu28/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu28/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu28/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu28/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu29/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu29/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu29/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu29/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu30/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu30/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu30/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu30/cpufreq/scaling_setspeed
echo 2500000 > /sys/devices/system/cpu/cpu31/cpufreq/scaling_min_freq
echo 2500000 > /sys/devices/system/cpu/cpu31/cpufreq/scaling_max_freq
echo "userspace" > /sys/devices/system/cpu/cpu31/cpufreq/scaling_governor
echo 2500000 > /sys/devices/system/cpu/cpu31/cpufreq/scaling_setspeed
export STARPU_PERF_MODEL_HOMOGENEOUS_CPU=1
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_1_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_1_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_2_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_2_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_3_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_3_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_4_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_4_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_5_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_5_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_1_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_1_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_2_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_2_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_3_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_3_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_4_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_4_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_5_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_5_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_1_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_1_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_2_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_2_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_3_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_3_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_4_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_4_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_5_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_5_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_1_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_1_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_2_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_2_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_3_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_3_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_4_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_4_homo
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_5_homo
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_5_homo
rm ~/trace/*
rm -rf ~/.starpu
export STARPU_PERF_MODEL_HOMOGENEOUS_CPU=0
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_1_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_1_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_2_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_2_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_3_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_3_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_4_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_4_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_5_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_getrf_nopiv_5_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_1_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_1_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_2_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_2_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_3_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_3_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_4_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_4_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_stesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_stesting_tupi5_potrf_5_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_stesting_tupi5_potrf_5_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_1_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_1_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_2_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_2_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_3_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_3_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_4_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_4_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o getrf_nopiv -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_5_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_getrf_nopiv_5_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_1_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_1_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_2_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_2_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_3_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_3_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_4_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_4_hetero
rm ~/trace/*
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31" chameleon_dtesting -T -o potrf -t 23 -g 1 -n 65536 -b 1024
$STARVZ_TOOLS/starvz ${STARPU_FXT_PREFIX}
mkdir experiments/parquets/chameleon_dtesting_tupi5_potrf_5_hetero
mv ${STARPU_FXT_PREFIX}/* experiments/parquets/chameleon_dtesting_tupi5_potrf_5_hetero
rm ~/trace/*
