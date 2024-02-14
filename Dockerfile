# Building:
#   docker build --platform linux/amd64 -t alvie .
# (Platform is linux/amd64 because mCRL2 is not available for arm64 yet)
# Running:
#   docker run --rm -it alvie

FROM ubuntu:22.04
RUN apt update; apt install -y software-properties-common
RUN add-apt-repository ppa:mcrl2/release-ppa
RUN apt update; DEBIAN_FRONTEND=noninteractive apt install -y build-essential cmake iverilog tk binutils-msp430 gcc-msp430 msp430-libc msp430mcu expect-dev git autoconf python3 flex bison pkg-config libffi-dev python3-dev nano joe python3-pip mcrl2

###### Verilator from https://verilator.org/guide/latest/install.html
# Prerequisites:
#sudo apt-get install git perl python3 make autoconf g++ flex bison ccache
#sudo apt-get install libgoogle-perftools-dev numactl perl-doc
#sudo apt-get install libfl2  # Ubuntu only (ignore if gives error)
#sudo apt-get install libfl-dev  # Ubuntu only (ignore if gives error)
#sudo apt-get install zlibc zlib1g zlib1g-dev  # Ubuntu only (ignore if gives error)

# RUN apt install -y git
RUN git clone https://github.com/verilator/verilator   # Only first time

# Every time you need to build:
# unsetenv VERILATOR_ROOT  # For csh; ignore error if on bash
RUN unset VERILATOR_ROOT  # For bash
WORKDIR "/verilator"
RUN git pull         # Make sure git repository is up-to-date
RUN git tag          # See what versions exist
#git checkout master      # Use development branch (e.g. recent bug fixes)
#git checkout stable      # Use most recent stable release
RUN git checkout v5.002  # Switch to specified release version

RUN autoconf         # Create ./configure script
RUN ./configure      # Configure and create Makefile
RUN make -j `nproc`  # Build Verilator itself (if error, try just 'make')
RUN make install

WORKDIR "/"

#### ocaml
RUN add-apt-repository ppa:avsm/ppa; apt update; apt install -y opam
# RUN apt install -y ocaml-dune

RUN adduser alvie

COPY . /home/alvie/
RUN chown -R alvie:alvie /home/alvie

USER alvie
RUN pip3 install Verilog_VCD

RUN opam init --disable-sandboxing; eval $(opam env); opam switch create 4.13.1
RUN eval $(opam env); opam install dune
RUN eval $(opam env)

WORKDIR "/home/alvie/alvie/code"

# RUN opam install . --deps-only
RUN opam install -y py core alcotest angstrom core_unix logs fmt ocamlgraph shexp ppx_deriving qcheck
RUN opam env >> /home/alvie/.bashrc

RUN eval $(opam env); dune build

RUN cd /home/alvie; git clone https://github.com/martonbognar/sancus-core-gap

#WORKDIR "/home/alvie"

#USER root
