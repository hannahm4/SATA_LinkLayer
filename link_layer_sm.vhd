----------------------------------------------------------------------------
--
--! @file       link_layer_sm.vhd
--! @brief      The state machine for the link layer
--! @details    Takes input from the transport and physical layers to determine
--!				the receiving and sending of frames
--! @author     Hannah Mohr
--! @date       December 2016
--! @copyright  Copyright (C) 2016 Ross K. Snider and Hannah Mohr
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--  Hannah Mohr
--  Electrical and Computer Engineering
--  Montana State University
--  hannah.mohr@msu.montana.edu
--
----------------------------------------------------------------------------

Library IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.NUMERIC_STD.all;

----------------------------------------------------------------------------
--
--! @brief      link layer state machine
--! @details    Takes input from the transport and physical layers to determine
--!				the receiving and sending of frames
--! @param      clk_75    		clock at 75MHz
--! @param      reset           active low reset
--! @param      status_out      status vector to transport layer
--! @param      tx_data_in      transmit data from transport layer
--! @param      rx_data_out     receive data to transport layer
--! @param      tx_data_out     transmit data to physical layer
--! @param      rx_data_in      receive data from physical layer
--! @param      status_in       status vector from the physical layer
--! @param      perform_init    send initiation command to physical layer
--
----------------------------------------------------------------------------

entity link_layer_sm is
	
   port(	
	-- Input
	clk_75			:	in std_logic;
	rst_n			:	in std_logic;

	--Interface with Transport Layer
	status_out		:	out std_logic_vector(31 downto 0);
	tx_data_in		:	in std_logic_vector(63 downto 0);
	rx_data_out		:	out std_logic_vector(63 downto 0);

	--Interface with Physical Layer
	tx_data_out		:	out std_logic_vector(63 downto 0);
	rx_data_in		:	in std_logic_vector(63 downto 0);
	status_in		:	in std_logic_vector(31 downto 0);	
	perform_init	:	out std_logic);

end entity link_layer_sm;

architecture link_layer_sm_arch of link_layer_sm is
  
  type State_Type is (L_Idle, L_SyncEscape, L_NoCommErr, L_NoComm, L_SendAlign, L_RESET, 
						L_SendChkRdy, L_SendSOF, L_SendData, L_RcvrHold, L_SendHold, L_SendCRC,
						L_SendEOF, L_Wait, L_RcvChkRdy, L_RcvWaitFifo, L_RcvData, L_Hold, L_RcvHold,
						L_RcvEOF, L_GoodCRC, L_GoodEnd, L_BadEnd, L_PMDeny);
  signal current_state, next_state : State_Type;
  
  -- primitives
  constant ALIGNp 	: std_logic_vector(31 downto 0) := x"7B4A4ABC";
  constant CONTp 	: std_logic_vector(31 downto 0) := x"9999AA7C";
  constant DMATp 	: std_logic_vector(31 downto 0) := x"3636B57C";
  constant EOFp 	: std_logic_vector(31 downto 0) := x"D5D5B57C";
  constant HOLDp 	: std_logic_vector(31 downto 0) := x"D5D5AA7C";
  constant HOLDAp	: std_logic_vector(31 downto 0) := x"9595AA7C";
  constant PMACKp 	: std_logic_vector(31 downto 0) := x"9595957C";
  constant PMNAKp	: std_logic_vector(31 downto 0) := x"F5F5957C";
  constant PMREQ_Pp : std_logic_vector(31 downto 0) := x"1717B57C";
  constant PMREQ_Sp	: std_logic_vector(31 downto 0) := x"7575957C";
  constant R_ERRp	: std_logic_vector(31 downto 0) := x"5656B57C";
  constant R_IPp 	: std_logic_vector(31 downto 0) := x"5555B57C";
  constant R_OKp 	: std_logic_vector(31 downto 0) := x"3535B57C";
  constant R_RDYp 	: std_logic_vector(31 downto 0) := x"4A4A957C";
  constant SOFp 	: std_logic_vector(31 downto 0) := x"3737B57C";
  constant SYNCp 	: std_logic_vector(31 downto 0) := x"B5B5957C";
  constant WTRMp 	: std_logic_vector(31 downto 0) := x"5858B57C";
  constant X_RDYp 	: std_logic_vector(31 downto 0) := x"5757B57C";
  
  -- status signals
  constant PHYRDY 	: std_logic_vector(31 downto 0) := x"0A0A0A0A";
  constant PHYRDYn 	: std_logic_vector(31 downto 0) := x"0A0A0A0A";
  constant LRESET	: std_logic_vector(31 downto 0) := x"0A0A0A0A";
  constant Dec_Err	: std_logic_vector(31 downto 0) := x"0A0A0A0A";
  constant CRC 		: std_logic_vector(31 downto 0) := x"0A0A0A0A";
  
begin
  
  STATE_MEMORY: process (clk_75,rst_n)
    begin
      if (rst_n = '0') then 
        current_state <= L_Idle;
      elsif (clk_75'event and clk_75='1') then
        current_state <= next_state;
      end if;
    end process;
    
    NEXT_STATE_LOGIC: process (current_state, rx_data_in)
      begin 
        case (current_state) is
			-- Idle SM states (top level)
			when L_Idle  		=> if (rx_data_in(31 downto 0)=PHYRDYn) then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_SendChkRdy;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_SendChkRdy;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_SendChkRdy;
									else 
										next_state <= L_Idle;
									end if;
			when L_SyncEscape  	=> if (rx_data_in(31 downto 0)=PHYRDYn) then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=X_RDYp or rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_SendChkRdy;
									end if;
			when L_NoCommErr  	=>	next_state <= L_NoComm;
			when L_NoComm  		=> if (rx_data_in(31 downto 0)=PHYRDYn) then
										next_state <= L_NoComm;
									elsif (rx_data_in(31 downto 0)=PHYRDY) then
										next_state <= L_SendAlign;
									end if;
			when L_SendAlign  	=> if (rx_data_in(31 downto 0)=PHYRDYn) then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=PHYRDY) then
										next_state <= L_Idle;
									end if;
			when L_RESET	  	=> if (rx_data_in(31 downto 0)=PHYRDYn) then
										next_state <= L_RESET;
									else 
										next_state <= L_NoComm;
									end if;
									
			-- Power Management SM states
			-- Transmit SM states
			-- Receive SM states
			when others =>  next_state <= L_Idle;
        end case;
      end process;
          
    OUTPUT_LOGIC: process (current_state, rx_data_in)
      begin
        case (current_state) is
        when L_Idle => if (rx_data_in(31 downto 0)=SYNCp) then
                      rx_data_out(31 downto 0)<=SOFp;
                     else 
                      rx_data_out(31 downto 0)<=SOFp;
                    end if;
       when others =>  rx_data_out(31 downto 0)<=x"00000000";
        end case;
      end process;
          
end architecture; 
