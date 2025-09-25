Parallel Testing with Caffeine
==============================

LLVM Flang 22 supports the native Fortran parallel programming model used by
Julienne: features that enable launching multiple images, which are instances
of a program.  To launch multi-image runs, Flang generates calls to the
Parallel Runtime Interface for Fortran [(PRIF)](https://go.lbl.gov/prif).
Using these features requires linking programs to a PRIF implementation such
as [Caffeine](https://go.lbl.gov/caffeine).

Rough Workflow
--------------
The steps below are intended more for reading rather than for running. Steps
like these have worked on a macOS 15.5 system with Apple Silicon and with the
Homebrew package manager installed.  If similar steps fail for you, please
contact fortran@lbl.gov.

### Build a LLVM Flang
```bash
brew install gcc@14
git clone -b gcc14.3.0-julienne3.2.0-caffeine0.6.0  https://github.com/BerkeleyLab/flang-testing-project.git
git clone https://github.com/rouson/handy-dandy
chmod u+x handy-dandy/src/fresh-llvm-build.sh
cd flang-testing-project
../handy-dandy/src/fresh-llvm-build.sh --prefix=<llvm-install-path>
cd ..
```

### Build Caffeine and GASNet
An `.install.sh` invocation of the form below installs Caffeine and GASNet in
`<caffeine-install-path>/lib`.
```bash
git clone -b 0.6.0 https://github.com/BerkeleyLab/caffeine.git
cd caffeine
FC=<llvm-install-path>/bin/flang-new \
 CC=<llvm-install-path>/bin/clang \
 CXX=<llvm-install-path>/bin/clang++ \
 ./install.sh --prefix=<caffeine-install-path>
cd ..
```

### Build Julienne
```
git clone -b 3.2.0 https://github.com/BerkeleyLab/julienne.git
cd julienne
fpm test \
  --compiler flang-new \
  --flag "-O3 -DHAVE_MULTI_IMAGE_SUPPORT -fcoarray" \
  --link-flag "-lcaffeine -lgasnet-smp-seq -L<caffeine-install-path>/lib"
```
