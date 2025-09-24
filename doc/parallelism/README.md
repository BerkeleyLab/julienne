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
like these worked recently on a macOS 15.5 system with Apple Silicon, with the
Homebrew package manager installed, and with `$HOME/.local/` in the `PATH`.
```bash
brew install gcc@14
git clone -b gcc14.3.0-julienne3.2.0-caffeine0.6.0  https://github.com/BerkeleyLab/flang-testing-project.git
git clone https://github.com/rouson/handy-dandy
chmod u+x handy-dandy/src/fresh-llvm-build.sh
cd flang-testing-project
./handy-dandy/src/fresh-llvm-build.sh --prefix=$HOME/.local
cd ..
git clone https://github.com/BerkeleyLab/caffeine.git
cd caffeine
FC=flang-new CC=clang CXX=clang++ ./install.sh --prefix=$HOME/.local
cd ..
git clone https://github.com/berkeleylab/julienne
cd julienne
fpm test --compiler flang-new --flag -O3 --link-flag <link-to-caffeine>
```
If something similar to the above workflow does not work for you, please
contact fortran@lbl.gov for assistance.
