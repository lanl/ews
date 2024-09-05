# Description
**EWSMod-2D: A Fortran Code for 2D Elastic-Wave Sensitivity Modeling**

This Fortran code is for modeling 2D elastic-wave sensitivity propagation and computing the spatial distribution of elastic-wave sensitivity energies in a heterogeneous model, which can be used for optimal design of time-lapse seismic monitoring surveys. We name this code EWSMod-2D -- Elastic-Wave Sensitivity Modeling in two-dimensional (2D) elastic models.

This work is funded by the Monitoring, Verification, and Accounting project, a part of National Risk Assessment Partnership (NRAP) funded by the U.S. Department of Energy through Los Alamos National Laboratory (LANL). LANL is operated by Triad National Security, LLC, for the National Nuclear Security Administration (NNSA) of the U.S. Department of Energy (DOE) under Contract No. 89233218CNA000001. The research used high-performance computing resources provided by LANL's Institutional Computing program.

The work is under LANL open source approval reference O4779.

# Reference

The work implements part of the methodology developed in:

- __H. Denli, L. Huang, 2010, Elastic-wave sensitivity propagation, Geophysics, doi: [10.1190/1.3428403](https://doi.org/10.1190/1.3428403)__

- __X. Shang, L. Huang, 2012, Optimal designs of time-lapse seismic surveys for monitoring CO<sub>2</sub> leakage through fault zones, International Journal of Greenhouse Gas Control, doi: [10.1016/j.ijggc.2012.07.006](https://doi.org/10.1016/j.ijggc.2012.07.006)__

- __K. Gao, L. Huang, 2020, Numerical modeling of anisotropic elastic-wave sensitivity propagation for optimal design of time-lapse seismic surveys, Communications in Computational Physics, doi: [10.4208/cicp.OA-2018-0192](https://doi.org/10.4208/cicp.OA-2018-0192)__

- __K. Gao, H. Denli, X. Shang, L. Huang, 2022, Elastic-wave sensitivity propagation for optimal time-lapse seismic survey design, Geophysical Monitoring for Geologic Carbon Storage, doi: [10.1002/9781119156871.ch7](https://doi.org/10.1002/9781119156871.ch7)__

Please cite the works if you use the code in your work.

# Requirement
The code is written in Fortran. Currently, it only support Intel's Fortran compiler, which is freely available through [Intel oneAPI Base Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/base-toolkit.html#gs.bed72v) and [Intel HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit.html#gs.bed5op). We have compiled and tested the code with ifx 2024.2.

# Use
To install,

```
cd src
make
```

The compiled executables will be at `bin`.

To remake,

```
cd src
make clean
make
```

We include one example in [example](example). To run the test,

```
cd example
bash run.sh
```

# License
&copy; 2024. Triad National Security, LLC. All rights reserved.

This program is Open-Source under the BSD-3 License.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

- Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Author
Kai Gao <kaigao@lanl.gov>
