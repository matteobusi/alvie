//=============================================================================
// Copyright (C) 2001 Authors
//
// This source file may be used and distributed without restriction provided
// that this copyright statement is not removed from the file and that any
// derivative work contains the original copyright notice and the associated
// disclaimer.
//
// This source file is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published
// by the Free Software Foundation; either version 2.1 of the License, or
// (at your option) any later version.
//
// This source is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
// License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this source; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//
//-----------------------------------------------------------------------------
//
// File Name: submit.f
//
// Author(s):
//             - Olivier Girard,    olgirard@gmail.com
//
//-----------------------------------------------------------------------------
// $Rev: 202 $
// $LastChangedBy: olivier.girard $
// $LastChangedDate: 2015-07-01 23:13:32 +0200 (Wed, 01 Jul 2015) $
//=============================================================================
// $LastChangedBy: matteo busi $
// $LastChangedDate: 02/02/2022 $
// $Changelog: Simplified and adapted for model mining
//------------------------------------------------------------------------------

//=============================================================================
// Testbench related
//=============================================================================
+incdir+../bench/verilog/
 -y ../bench/verilog/
../bench/verilog/tb_openMSP430.v

+incdir+{{tmp_dir}}/sancus-core-gap/core/bench/verilog
 -y {{tmp_dir}}/sancus-core-gap/core/bench/verilog


//=============================================================================
// CPU
//=============================================================================

+incdir+{{tmp_dir}}/sancus-core-gap/core/rtl/verilog
-y {{tmp_dir}}/sancus-core-gap/core/rtl/verilog

+incdir+{{tmp_dir}}/sancus-core-gap/core/rtl/verilog/crypto
-y {{tmp_dir}}/sancus-core-gap/core/rtl/verilog/crypto

+incdir+{{tmp_dir}}/sancus-core-gap/core/rtl/verilog/periph
-y {{tmp_dir}}/sancus-core-gap/core/rtl/verilog/periph
