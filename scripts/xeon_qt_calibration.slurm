#!/bin/bash
#SBATCH --job-name=xeon_qt_calibration
#SBATCH --partition=tupi
#SBATCH --nodelist=tupi1
#SBATCH --time=23:59:00  
#SBATCH --output=experiments/xeon_performance/xeon_qt_calibration.ou
#SBATCH --error=experiments/xeon_performance/xeon_qt_calibration.err
export STARPU_DISPLAY_BINDINGS=1
export STARPU_SCHED=dmda
export STARPU_TRACE_BUFFER_SIZE=256
export PATH=~/spack/opt/spack/linux-debian12-broadwell/gcc-11.3.0/chameleon-1.2.0-ieer6l5tfxxxrnnfbjdgqa2q5xfrxy32/bin:$PATH
rm  ~/.starpu/sampling/codelets/45/*.tupi1
echo 0 > /sys/devices/system/cpu/cpufreq/boost
rm experiments/xeon_performance/executions_calibration.csv
touch experiments/xeon_performance/executions_calibration.csv
echo "order,op,hash,size,flops,mean,dev,sum,sum2,n" >> experiments/xeon_performance/executions_calibration.csv
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
for op in sgemm sgetrf_nopiv splgsy strsm
do
echo "# execucao 1, operacao ${op}" >> experiments/xeon_performance/executions_calibration.csv
cat .starpu/sampling/codelets/45/${op}.tupi1 | grep -A 1 -e "device type\|flops" | grep -v -e "device type\|flops\|--" | paste -sd "\t\n" | sed 's/\t/;/g' | sed 's/ //g' >> experiments/xeon_performance/executions_calibration.csv
done
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
for op in sgemm sgetrf_nopiv splgsy strsm
do
echo "# execucao 2, operacao ${op}" >> experiments/xeon_performance/executions_calibration.csv
cat .starpu/sampling/codelets/45/${op}.tupi1 | grep -A 1 -e "device type\|flops" | grep -v -e "device type\|flops\|--" | paste -sd "\t\n" | sed 's/\t/;/g' | sed 's/ //g' >> experiments/xeon_performance/executions_calibration.csv
done
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
for op in sgemm sgetrf_nopiv splgsy strsm
do
echo "# execucao 3, operacao ${op}" >> experiments/xeon_performance/executions_calibration.csv
cat .starpu/sampling/codelets/45/${op}.tupi1 | grep -A 1 -e "device type\|flops" | grep -v -e "device type\|flops\|--" | paste -sd "\t\n" | sed 's/\t/;/g' | sed 's/ //g' >> experiments/xeon_performance/executions_calibration.csv
done
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
for op in sgemm sgetrf_nopiv splgsy strsm
do
echo "# execucao 4, operacao ${op}" >> experiments/xeon_performance/executions_calibration.csv
cat .starpu/sampling/codelets/45/${op}.tupi1 | grep -A 1 -e "device type\|flops" | grep -v -e "device type\|flops\|--" | paste -sd "\t\n" | sed 's/\t/;/g' | sed 's/ //g' >> experiments/xeon_performance/executions_calibration.csv
done
STARPU_WORKERS_CPUID="0 2 4 6 8 10 12 14" chameleon_stesting -o getrf_nopiv -t 7 -g 1 -n 32768 -b 1024
for op in sgemm sgetrf_nopiv splgsy strsm
do
echo "# execucao 5, operacao ${op}" >> experiments/xeon_performance/executions_calibration.csv
cat .starpu/sampling/codelets/45/${op}.tupi1 | grep -A 1 -e "device type\|flops" | grep -v -e "device type\|flops\|--" | paste -sd "\t\n" | sed 's/\t/;/g' | sed 's/ //g' >> experiments/xeon_performance/executions_calibration.csv
done
echo "RESULTADOS"
cat experiments/xeon_performance/executions_calibration.csv
