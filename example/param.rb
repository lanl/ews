
nx = 300
nz = 150
dx = 10
dz = 10
nt = 3000
dt = 1.0e-3

# number of sources 
ns = 2

# ascii file of sources in the format of
# sx1 sz1 f0
# sx2 sz2 f0
# ...
file_src = src.txt

# number of receivers
nr = 300

# ascii file of receivers in the format of
# rx1 rz1
# rx2 rz2
# ...
file_rec = rec.txt

# directory to save results
dir_working = result

# raw binary files stored in z-x order
# for mask file, 1 indicates the target region where 0 indicates background
file_vp = vp.bin
file_vs = vs.bin
file_rho = rho.bin
file_mask = mask.bin

# the following three parameters are optional
# select which parameter to compute sensivity
# by default, the program will only compute vp, vs
sensi_vp = y
sensi_vs = y
sensi_rho = n

# the following parameter is optional
# select which type of modeling to execute
# select one of 
# wavefield -- output only regular wavefields
# sensitivity -- output only sensitivity wavefields
# both -- output both
# by default, it is sensitivity
modeling_type = both
