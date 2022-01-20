using CpuId, Distributed, MimiGIVE

addprocs(cpucores() - nprocs())