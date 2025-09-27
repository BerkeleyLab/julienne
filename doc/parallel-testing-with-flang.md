Parallel Testing with Flang
===========================

LLVM Flang 22 supports the native Fortran parallel programming model used by
Julienne: features that enable launching multiple images, which are instances
of a program.  As of this writing, "Flang 22" refers to the main branch of 
[llvm-project](https://github.com/llvm/llvm-project), which when built from
source, responds to `flang-new --version` with text that includes `22.0.0git`.

To launch multi-image runs, Flang generates calls to the Parallel Runtime
Interface for Fortran [(PRIF)](https://go.lbl.gov/prif).  Using these features
requires linking programs to a PRIF implementation such as 
[Caffeine](https://go.lbl.gov/caffeine).  This document offers a rough sketch
of a workflow for building LLVM Flang 22 on one platform.

Rough Workflow
--------------
Steps like those below have worked on a macOS 15.5 system with Apple Silicon and
with the Homebrew package manager installed.  If similar steps fail for you,
please contact fortran@lbl.gov.

### Build a LLVM Flang
```bash
brew install gcc@14 # (gcc@15 failed to build LLVM on the tested computer)
git clone -b gcc14.3.0-julienne3.2.0-caffeine0.6.0 \
   https://github.com/BerkeleyLab/flang-testing-project.git
git clone https://github.com/rouson/handy-dandy
chmod u+x handy-dandy/src/fresh-llvm-build.sh
cd flang-testing-project
../handy-dandy/src/fresh-llvm-build.sh --prefix=<llvm-install-path>
cd ..
```
where angular brackets denote variables to replace with your chosen value.

### Build Caffeine and GASNet
An `.install.sh` invocation of the form below _should_ install Caffeine and
GASNet in `<caffeine-install-path>/lib`.
```bash
git clone -b 0.6.0 https://github.com/BerkeleyLab/caffeine.git
cd caffeine
FC=<llvm-install-path>/bin/flang-new \
 CC=<llvm-install-path>/bin/clang \
 CXX=<llvm-install-path>/bin/clang++ \
 ./install.sh --prefix=<caffeine-install-path>
cd ..
```
As of this writing, however, the above `install.sh` does not install the
Caffeine library file due to an apparent `fpm` bug.  Therefore, the next step is
to find the resulting library.  For example, use
```
find build -name libcaffeine.a
```
Then move `libcaffeine.a` to the `<caffeine-install-path>/lib` directory.

### Build and Test Julienne
```
git clone -b 3.2.0 https://github.com/BerkeleyLab/julienne.git
cd julienne
fpm test \
  --compiler flang-new \
  --flag "-O3 -DHAVE_MULTI_IMAGE_SUPPORT -fcoarray" \
  --link-flag "-lcaffeine -lgasnet-smp-seq -L<caffeine-install-path>/lib"
```
If successful, the resulting output will not indicate any test failures,
but will describe any tests that are skipped by default.
