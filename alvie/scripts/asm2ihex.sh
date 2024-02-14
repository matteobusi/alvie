#!/bin/bash
#------------------------------------------------------------------------------
# Copyright (C) 2001 Authors
#
# This source file may be used and distributed without restriction provided
# that this copyright statement is not removed from the file and that any
# derivative work contains the original copyright notice and the associated
# disclaimer.
#
# This source file is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; either version 2.1 of the License, or
# (at your option) any later version.1
#
# This source is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this source; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#------------------------------------------------------------------------------
#
# File Name: asm2ihex.sh
#
# Author(s):
#             - Olivier Girard,    olgirard@gmail.com
#
#------------------------------------------------------------------------------
# $Rev: 207 $
# $LastChangedBy: olivier.girard $
# $LastChangedDate: 2015-10-20 22:58:27 +0200 (Tue, 20 Oct 2015) $
#------------------------------------------------------------------------------

###############################################################################
#                            Parameter Check                                  #
###############################################################################

# MSPGCC version prefix
if [ -z "$MSPGCC_PFX" ]; then
    if command -v msp430-gcc >/dev/null; then
	MSPGCC_PFX=msp430
    else
	MSPGCC_PFX=msp430-elf
    fi
fi

# If on MacOS, used gsed
# if [[ "$OSTYPE" == "darwin"* ]]; then
#     alias sed="gsed"
# fi


###############################################################################
#               Check if definition & assembler files exist                   #
###############################################################################

if [ ! -e $2 ]; then
    echo "Assembler file doesn't exist: $2"
    exit 1
fi
if [ ! -e $3 ]; then
    echo "Linker definition file template doesn't exist: $3"
    exit 1
fi
if [ ! -e $4 ]; then
    echo "Assembler definition file template doesn't exist: $4"
    exit 1
fi


###############################################################################
#               Generate the linker definition file                           #
###############################################################################

PER_SIZE=$7
DMEM_SIZE=$6
PMEM_SIZE=$5
PMEM_BASE=$((0x10000-$PMEM_SIZE))
STACK_INIT=$((PER_SIZE+0x0080))

cp  $3  "$8/pmem.x"
cp  $4  "$8/pmem_defs.asm"
sed -ie "s/PMEM_BASE/$PMEM_BASE/g"         "$8/pmem.x"
sed -ie "s/PMEM_SIZE/$PMEM_SIZE/g"         "$8/pmem.x"
sed -ie "s/DMEM_SIZE/$DMEM_SIZE/g"         "$8/pmem.x"
sed -ie "s/PER_SIZE/$PER_SIZE/g"           "$8/pmem.x"
sed -ie "s/STACK_INIT/$STACK_INIT/g"       "$8/pmem.x"

sed -ie "s/PMEM_SIZE/$PMEM_SIZE/g"         "$8/pmem_defs.asm"
sed -ie "s/PER_SIZE_HEX/$PER_SIZE/g"       "$8/pmem_defs.asm"
if [ $MSPGCC_PFX == "msp430-elf" ]; then
    sed -ie "s/PER_SIZE/.data/g"           "$8/pmem_defs.asm"
    sed -ie "s/PMEM_BASE_VAL/.text/g"      "$8/pmem_defs.asm"
    sed -ie "s/PMEM_EDE_SIZE/0/g"          "$8/pmem_defs.asm"
else
    sed -ie "s/PER_SIZE/$PER_SIZE/g"       "$8/pmem_defs.asm"
    sed -ie "s/PMEM_BASE_VAL/$PMEM_BASE/g" "$8/pmem_defs.asm"
    sed -ie "s/PMEM_EDE_SIZE/$PMEM_SIZE/g" "$8/pmem_defs.asm"
fi


###############################################################################
#                  Compile, link & generate IHEX file                         #
###############################################################################
echo ""
echo "\$ $MSPGCC_PFX-as      -alsm $2 -o $1.o > $1.l43"
$MSPGCC_PFX-as   -I ../../src/gap-attacks -I $8 --defsym "__SECRET=${__SECRET}"  -alsm $2   -o $1.o     > $1.l43
echo "\$ $MSPGCC_PFX-objdump -xdsStr $1.o >> $1.l43"
$MSPGCC_PFX-objdump -xdsStr       $1.o              >> $1.l43

#pwd
echo "\$ $MSPGCC_PFX-ld      -T $8/pmem.x $1.o -o $1.elf"
$MSPGCC_PFX-ld      -T $8/pmem.x   $1.o   -o $1.elf
echo "\$ $MSPGCC_PFX-objcopy -O ihex $1.elf $1.ihex"
$MSPGCC_PFX-objcopy -O ihex       $1.elf    $1.ihex
echo ""
