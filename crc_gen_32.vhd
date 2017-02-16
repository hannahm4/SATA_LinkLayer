----------------------------------------------------------------------------
--
--! @file       crc_gen_32.vhd
--! @brief      CRC generator for the Link Layer of the SATA controller with a 32bit wide data bus.
--! @details    Takes input data and computes the CRC 
--! @author     Hannah Mohr
--! @date       February 2017
--! @copyright  Copyright (C) 2017 Ross K. Snider and Hannah D. Mohr
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS for A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-- ******************************** 
-- 			Source Code
-- ******************************** 
-- CRC Engine RTL Design 
-- Copyright (C) www.ElectronicDesignworks.com 
-- Source code generated by ElectronicDesignworks IP Generator (CRC).
-- Documentation can be downloaded from www.ElectronicDesignworks.com 
-- ******************************** 
--            License     
-- ******************************** 
-- This source file may be used and distributed freely provided that this
-- copyright notice, list of conditions and the following disclaimer is
-- not removed from the file.                    
-- Any derivative work should contain this copyright notice and associated disclaimer.                    
-- This source code file is provided "AS IS" AND WITHOUT ANY WARRANTY, 
-- without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
-- PARTICULAR PURPOSE.
-- ********************************
--           Specification 
-- ********************************
-- File Name       : CRC32_DATA32.vhd    
-- Description     : CRC Engine ENTITY 
-- Clock           : Positive Edge 
-- Reset           : Active Low
-- First Serial    : MSB 
-- Data Bus Width  : 32 bits 
-- Polynomial      : (0 1 2 4 5 7 8 10 11 12 16 22 23 26 32)                   
-- Date            : 18-Jan-2017  
-- Version         : 1.0        
--
--  Hannah Mohr
--  Electrical and Computer Engineering
--  Montana State University
--  hannah.mohr@msu.montana.edu
--
----------------------------------------------------------------------------
                    
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

----------------------------------------------------------------------------
--
--! @brief      link layer crc generator 
--! @details    Takes input data and computes the CRC 
--!
--! @param      clk	    			system clock
--! @param      rst_n           	active low reset
--! @param      soc     			input flag indicating that the CRC computation should begin
--! @param      data			    input data on which the CRC is calculated
--! @param      data_valid	     	input flag indicating that the values on the data input line can be used for the CRC
--! @param      eoc			     	input flag indicating the end of the CRC computation
--! @param      crc     			output CRC which is updated as the computation progresses
--
----------------------------------------------------------------------------

entity crc_gen_32 is
   port(           
           clk        : in  std_logic; 
           rst_n      : in  std_logic; 
           soc        : in  std_logic; 
           data       : in  std_logic_vector(31 downto 0); 
           data_valid : in  std_logic; 
           eoc        : in  std_logic; 
           crc        : out std_logic_vector(31 downto 0)
       );
end crc_gen_32; 

architecture crc_gen_32_arch of crc_gen_32 is
-- constants
constant crc_const      : std_logic_vector(31 downto 0) := x"52325032";		-- seed for the crc calculation

-- interface signals
 signal s_clk			: std_logic;										-- input: system clock
 signal s_rst_n			: std_logic;										-- input: active low reset
 signal s_soc			: std_logic;										-- input: start crc calculation
 signal s_data			: std_logic_vector(31 downto 0);					-- input: data for crc calculation
 signal s_data_valid	: std_logic;										-- input: flag indicating the crc calculation can proceed
 signal s_eoc			: std_logic;										-- input: end crc calculation
 signal s_crc			: std_logic_vector(31 downto 0);					-- output: valid crc at each stage of the calculation

-- internal signals
 signal crc_r          : std_logic_vector(31 downto 0);					-- crc holder which is updated when the data_valid flag is asserted
 signal crc_c          : std_logic_vector(31 downto 0);					-- signal which holds the crc as it is calculated
 signal crc_i          : std_logic_vector(31 downto 0);					-- signal which holds the previous crc to be used in the calculation

begin 
-- signal assignments
	-- interface signals
s_clk				<= clk;
s_rst_n				<= rst_n;
s_soc 				<= 	soc when data_valid = '0' else 
						'0' when data_valid = '1' and rising_edge(clk);
s_data_valid		<= data_valid;
s_eoc				<= eoc;
crc					<= s_crc;

	-- internal signals
crc_i    <= crc_const when s_soc = '1' else			-- the crc to be used in the calculation is the previous crc except the first time, when it is the seed
            crc_r;
	
	
-- crc calculation
crc_c(0) <= data(0)  XOR data(6)  XOR data(9)  XOR data(10) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(28) XOR crc_i(28) XOR crc_i(10) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(25) XOR crc_i(25) XOR data(12) XOR data(16) XOR data(30) XOR crc_i(6) XOR crc_i(30) XOR crc_i(16) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(1) <= data(0)  XOR data(1)  XOR data(7)  XOR data(11) XOR crc_i(1) XOR crc_i(11) XOR data(27) XOR crc_i(27) XOR data(13) XOR data(17) XOR crc_i(7) XOR crc_i(17) XOR crc_i(13) XOR data(6) XOR data(9) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(28) XOR crc_i(28) XOR crc_i(9) XOR data(12) XOR data(16) XOR crc_i(6) XOR crc_i(16) XOR crc_i(12); 
crc_c(2) <= data(0)  XOR data(1)  XOR data(2)  XOR data(8) XOR crc_i(2) XOR data(14) XOR data(18) XOR crc_i(8) XOR crc_i(18) XOR crc_i(14) XOR data(7) XOR crc_i(1) XOR data(13) XOR data(17) XOR crc_i(7) XOR crc_i(17) XOR crc_i(13) XOR data(6) XOR data(9) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(16) XOR data(30) XOR crc_i(6) XOR crc_i(30) XOR crc_i(16) XOR data(31) XOR crc_i(31); 
crc_c(3) <= data(1)  XOR data(2)  XOR data(3)  XOR data(9) XOR crc_i(3) XOR data(15) XOR data(19) XOR crc_i(9) XOR crc_i(19) XOR crc_i(15) XOR data(8) XOR crc_i(2) XOR data(14) XOR data(18) XOR crc_i(8) XOR crc_i(18) XOR crc_i(14) XOR data(7) XOR data(10) XOR data(25) XOR crc_i(1) XOR crc_i(25) XOR data(27) XOR crc_i(27) XOR crc_i(10) XOR data(17) XOR data(31) XOR crc_i(7) XOR crc_i(31) XOR crc_i(17); 
crc_c(4) <= data(0)  XOR data(2)  XOR data(3)  XOR data(4) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR crc_i(3) XOR data(15) XOR data(19) XOR crc_i(19) XOR crc_i(15) XOR data(8) XOR data(11) XOR crc_i(2) XOR crc_i(11) XOR data(18) XOR crc_i(8) XOR crc_i(18) XOR data(6) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(25) XOR crc_i(25) XOR data(12) XOR data(30) XOR crc_i(6) XOR crc_i(30) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(5) <= data(0)  XOR data(1)  XOR data(3)  XOR data(4) XOR data(5) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR crc_i(3) XOR data(19) XOR crc_i(19) XOR data(7) XOR crc_i(1) XOR data(13) XOR crc_i(7) XOR crc_i(13) XOR data(6) XOR data(10) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(28) XOR crc_i(28) XOR crc_i(10) XOR crc_i(6); 
crc_c(6) <= data(1)  XOR data(2)  XOR data(4)  XOR data(5) XOR data(6) XOR crc_i(6) XOR data(22) XOR crc_i(22) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR data(8) XOR crc_i(2) XOR data(14) XOR crc_i(8) XOR crc_i(14) XOR data(7) XOR data(11) XOR data(25) XOR crc_i(1) XOR crc_i(25) XOR data(30) XOR crc_i(30) XOR data(29) XOR crc_i(29) XOR crc_i(11) XOR crc_i(7); 
crc_c(7) <= data(0)  XOR data(2)  XOR data(3)  XOR data(5) XOR data(7) XOR crc_i(7) XOR data(23) XOR crc_i(23) XOR data(22) XOR crc_i(22) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR crc_i(3) XOR data(15) XOR crc_i(15) XOR data(8) XOR crc_i(2) XOR crc_i(8) XOR data(10) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(28) XOR crc_i(28) XOR crc_i(10) XOR data(25) XOR crc_i(25) XOR data(16) XOR crc_i(16); 
crc_c(8) <= data(0)  XOR data(1)  XOR data(3)  XOR data(4) XOR data(8) XOR crc_i(8) XOR data(23) XOR crc_i(23) XOR data(22) XOR crc_i(22) XOR crc_i(4) XOR crc_i(3) XOR data(11) XOR crc_i(1) XOR crc_i(11) XOR data(17) XOR crc_i(17) XOR data(10) XOR crc_i(0) XOR data(28) XOR crc_i(28) XOR crc_i(10) XOR data(12) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(9) <= data(1)  XOR data(2)  XOR data(4)  XOR data(5) XOR data(9) XOR crc_i(9) XOR data(24) XOR crc_i(24) XOR data(23) XOR crc_i(23) XOR crc_i(5) XOR crc_i(4) XOR data(12) XOR crc_i(2) XOR crc_i(12) XOR data(18) XOR crc_i(18) XOR data(11) XOR crc_i(1) XOR data(29) XOR crc_i(29) XOR crc_i(11) XOR data(13) XOR crc_i(13); 
crc_c(10) <= data(0) XOR data(2)  XOR data(3)  XOR data(5) XOR crc_i(5) XOR data(13) XOR crc_i(3) XOR crc_i(13) XOR data(19) XOR crc_i(19) XOR crc_i(2) XOR data(14) XOR crc_i(14) XOR data(9) XOR crc_i(0) XOR data(29) XOR crc_i(29) XOR data(28) XOR crc_i(28) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(16) XOR crc_i(16) XOR data(31) XOR crc_i(31); 
crc_c(11) <= data(0) XOR data(1)  XOR data(3)  XOR data(4) XOR data(14) XOR crc_i(4) XOR crc_i(14) XOR data(20) XOR crc_i(20) XOR crc_i(3) XOR data(15) XOR crc_i(15) XOR crc_i(1) XOR data(27) XOR crc_i(27) XOR data(17) XOR crc_i(17) XOR data(9) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(28) XOR crc_i(28) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(25) XOR crc_i(25) XOR data(12) XOR data(16) XOR crc_i(16) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(12) <= data(0) XOR data(1)  XOR data(2)  XOR data(4) XOR data(5) XOR data(15) XOR crc_i(5) XOR crc_i(15) XOR data(21) XOR crc_i(21) XOR crc_i(4) XOR crc_i(2) XOR data(18) XOR crc_i(18) XOR crc_i(1) XOR data(27) XOR crc_i(27) XOR data(13) XOR data(17) XOR crc_i(17) XOR crc_i(13) XOR data(6) XOR data(9) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR crc_i(9) XOR data(12) XOR data(30) XOR crc_i(6) XOR crc_i(30) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(13) <= data(1) XOR data(2)  XOR data(3)  XOR data(5) XOR data(6) XOR data(16) XOR crc_i(6) XOR crc_i(16) XOR data(22) XOR crc_i(22) XOR crc_i(5) XOR crc_i(3) XOR data(19) XOR crc_i(19) XOR crc_i(2) XOR data(28) XOR crc_i(28) XOR data(14) XOR data(18) XOR crc_i(18) XOR crc_i(14) XOR data(7) XOR data(10) XOR data(25) XOR crc_i(1) XOR crc_i(25) XOR crc_i(10) XOR data(13) XOR data(31) XOR crc_i(7) XOR crc_i(31) XOR crc_i(13); 
crc_c(14) <= data(2) XOR data(3)  XOR data(4)  XOR data(6) XOR data(7) XOR data(17) XOR crc_i(7) XOR crc_i(17) XOR data(23) XOR crc_i(23) XOR crc_i(6) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR crc_i(3) XOR data(29) XOR crc_i(29) XOR data(15) XOR data(19) XOR crc_i(19) XOR crc_i(15) XOR data(8) XOR data(11) XOR data(26) XOR crc_i(2) XOR crc_i(26) XOR crc_i(11) XOR data(14) XOR crc_i(8) XOR crc_i(14); 
crc_c(15) <= data(3) XOR data(4)  XOR data(5)  XOR data(7) XOR data(8) XOR data(18) XOR crc_i(8) XOR crc_i(18) XOR data(24) XOR crc_i(24) XOR crc_i(7) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR crc_i(4) XOR data(30) XOR crc_i(30) XOR data(16) XOR data(20) XOR crc_i(20) XOR crc_i(16) XOR data(9) XOR data(12) XOR data(27) XOR crc_i(3) XOR crc_i(27) XOR crc_i(12) XOR data(15) XOR crc_i(9) XOR crc_i(15); 
crc_c(16) <= data(0) XOR data(4)  XOR data(5)  XOR data(8) XOR data(19) XOR crc_i(19) XOR crc_i(8) XOR data(22) XOR crc_i(22) XOR crc_i(5) XOR data(17) XOR data(21) XOR crc_i(21) XOR crc_i(17) XOR data(13) XOR crc_i(4) XOR crc_i(13) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(26) XOR crc_i(26) XOR data(12) XOR data(30) XOR crc_i(30) XOR crc_i(12); 
crc_c(17) <= data(1) XOR data(5)  XOR data(6)  XOR data(9) XOR data(20) XOR crc_i(20) XOR crc_i(9) XOR data(23) XOR crc_i(23) XOR crc_i(6) XOR data(18) XOR data(22) XOR crc_i(22) XOR crc_i(18) XOR data(14) XOR crc_i(5) XOR crc_i(14) XOR data(25) XOR crc_i(1) XOR crc_i(25) XOR data(30) XOR crc_i(30) XOR data(27) XOR crc_i(27) XOR data(13) XOR data(31) XOR crc_i(31) XOR crc_i(13); 
crc_c(18) <= data(2) XOR data(6)  XOR data(7)  XOR data(10) XOR data(21) XOR crc_i(21) XOR crc_i(10) XOR data(24) XOR crc_i(24) XOR crc_i(7) XOR data(19) XOR data(23) XOR crc_i(23) XOR crc_i(19) XOR data(15) XOR crc_i(6) XOR crc_i(15) XOR data(26) XOR crc_i(2) XOR crc_i(26) XOR data(31) XOR crc_i(31) XOR data(28) XOR crc_i(28) XOR data(14) XOR crc_i(14); 
crc_c(19) <= data(3) XOR data(7)  XOR data(8)  XOR data(11) XOR data(22) XOR crc_i(22) XOR crc_i(11) XOR data(25) XOR crc_i(25) XOR crc_i(8) XOR data(20) XOR data(24) XOR crc_i(24) XOR crc_i(20) XOR data(16) XOR crc_i(7) XOR crc_i(16) XOR data(27) XOR crc_i(3) XOR crc_i(27) XOR data(29) XOR crc_i(29) XOR data(15) XOR crc_i(15); 
crc_c(20) <= data(4) XOR data(8)  XOR data(9)  XOR data(12) XOR data(23) XOR crc_i(23) XOR crc_i(12) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(21) XOR data(25) XOR crc_i(25) XOR crc_i(21) XOR data(17) XOR crc_i(8) XOR crc_i(17) XOR data(28) XOR crc_i(4) XOR crc_i(28) XOR data(30) XOR crc_i(30) XOR data(16) XOR crc_i(16); 
crc_c(21) <= data(5) XOR data(9)  XOR data(10) XOR data(13) XOR data(24) XOR crc_i(24) XOR crc_i(13) XOR data(27) XOR crc_i(27) XOR crc_i(10) XOR data(22) XOR data(26) XOR crc_i(26) XOR crc_i(22) XOR data(18) XOR crc_i(9) XOR crc_i(18) XOR data(29) XOR crc_i(5) XOR crc_i(29) XOR data(31) XOR crc_i(31) XOR data(17) XOR crc_i(17); 
crc_c(22) <= data(0) XOR data(11) XOR data(14) XOR crc_i(14) XOR crc_i(11) XOR data(23) XOR data(27) XOR crc_i(27) XOR crc_i(23) XOR data(19) XOR crc_i(19) XOR data(18) XOR crc_i(18) XOR data(9) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(29) XOR crc_i(29) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(12) XOR data(16) XOR crc_i(16) XOR data(31) XOR crc_i(31) XOR crc_i(12); 
crc_c(23) <= data(0) XOR data(1)  XOR data(15) XOR crc_i(15) XOR data(20) XOR crc_i(20) XOR data(19) XOR crc_i(19) XOR crc_i(1) XOR data(27) XOR crc_i(27) XOR data(13) XOR data(17) XOR crc_i(17) XOR crc_i(13) XOR data(6) XOR data(9) XOR crc_i(0) XOR data(29) XOR crc_i(29) XOR data(26) XOR crc_i(26) XOR crc_i(9) XOR data(16) XOR crc_i(6) XOR crc_i(16) XOR data(31) XOR crc_i(31); 
crc_c(24) <= data(1) XOR data(2)  XOR data(16) XOR crc_i(16) XOR data(21) XOR crc_i(21) XOR data(20) XOR crc_i(20) XOR crc_i(2) XOR data(28) XOR crc_i(28) XOR data(14) XOR data(18) XOR crc_i(18) XOR crc_i(14) XOR data(7) XOR data(10) XOR crc_i(1) XOR data(30) XOR crc_i(30) XOR data(27) XOR crc_i(27) XOR crc_i(10) XOR data(17) XOR crc_i(7) XOR crc_i(17); 
crc_c(25) <= data(2) XOR data(3)  XOR data(17) XOR crc_i(17) XOR data(22) XOR crc_i(22) XOR data(21) XOR crc_i(21) XOR crc_i(3) XOR data(29) XOR crc_i(29) XOR data(15) XOR data(19) XOR crc_i(19) XOR crc_i(15) XOR data(8) XOR data(11) XOR crc_i(2) XOR data(31) XOR crc_i(31) XOR data(28) XOR crc_i(28) XOR crc_i(11) XOR data(18) XOR crc_i(8) XOR crc_i(18); 
crc_c(26) <= data(0) XOR data(3)  XOR data(4)  XOR data(18) XOR crc_i(18) XOR data(23) XOR crc_i(23) XOR data(22) XOR crc_i(22) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR crc_i(3) XOR data(19) XOR crc_i(19) XOR data(6) XOR data(10) XOR data(24) XOR crc_i(0) XOR crc_i(24) XOR data(28) XOR crc_i(28) XOR crc_i(10) XOR data(26) XOR crc_i(26) XOR data(25) XOR crc_i(25) XOR crc_i(6) XOR data(31) XOR crc_i(31); 
crc_c(27) <= data(1) XOR data(4)  XOR data(5)  XOR data(19) XOR crc_i(19) XOR data(24) XOR crc_i(24) XOR data(23) XOR crc_i(23) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR crc_i(4) XOR data(20) XOR crc_i(20) XOR data(7) XOR data(11) XOR data(25) XOR crc_i(1) XOR crc_i(25) XOR data(29) XOR crc_i(29) XOR crc_i(11) XOR data(27) XOR crc_i(27) XOR data(26) XOR crc_i(26) XOR crc_i(7); 
crc_c(28) <= data(2) XOR data(5)  XOR data(6)  XOR data(20) XOR crc_i(20) XOR data(25) XOR crc_i(25) XOR data(24) XOR crc_i(24) XOR crc_i(6) XOR data(22) XOR crc_i(22) XOR crc_i(5) XOR data(21) XOR crc_i(21) XOR data(8) XOR data(12) XOR data(26) XOR crc_i(2) XOR crc_i(26) XOR data(30) XOR crc_i(30) XOR crc_i(12) XOR data(28) XOR crc_i(28) XOR data(27) XOR crc_i(27) XOR crc_i(8); 
crc_c(29) <= data(3) XOR data(6)  XOR data(7)  XOR data(21) XOR crc_i(21) XOR data(26) XOR crc_i(26) XOR data(25) XOR crc_i(25) XOR crc_i(7) XOR data(23) XOR crc_i(23) XOR crc_i(6) XOR data(22) XOR crc_i(22) XOR data(9) XOR data(13) XOR data(27) XOR crc_i(3) XOR crc_i(27) XOR data(31) XOR crc_i(31) XOR crc_i(13) XOR data(29) XOR crc_i(29) XOR data(28) XOR crc_i(28) XOR crc_i(9); 
crc_c(30) <= data(4) XOR data(7)  XOR data(8)  XOR data(22) XOR crc_i(22) XOR data(27) XOR crc_i(27) XOR data(26) XOR crc_i(26) XOR crc_i(8) XOR data(24) XOR crc_i(24) XOR crc_i(7) XOR data(23) XOR crc_i(23) XOR data(10) XOR data(14) XOR data(28) XOR crc_i(4) XOR crc_i(28) XOR crc_i(14) XOR data(30) XOR crc_i(30) XOR data(29) XOR crc_i(29) XOR crc_i(10); 
crc_c(31) <= data(5) XOR data(8)  XOR data(9)  XOR data(23) XOR crc_i(23) XOR data(28) XOR crc_i(28) XOR data(27) XOR crc_i(27) XOR crc_i(9) XOR data(25) XOR crc_i(25) XOR crc_i(8) XOR data(24) XOR crc_i(24) XOR data(11) XOR data(15) XOR data(29) XOR crc_i(5) XOR crc_i(29) XOR crc_i(15) XOR data(31) XOR crc_i(31) XOR data(30) XOR crc_i(30) XOR crc_i(11); 

-- process to update the crc 
crc_gen_process : process(s_clk, s_rst_n, s_data_valid) 
begin                                 
 if(s_rst_n = '0') then  				-- reset
    crc_r <= crc_const;					-- starting value is returned to the seed for a consistent initial condition
 elsif(rising_edge(clk)) then	 		-- update each positive clock edge
    if(s_data_valid = '1') then 		-- only update crc_r if the input data are valid
         crc_r <= crc_c; 				-- save the calculated crc if the data is valid
    end if; 
 end if;    
end process crc_gen_process;      

s_crc <= crc_r;							-- output crc assignment

end crc_gen_32_arch;
                      
