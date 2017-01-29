----------------------------------------------------------------------------
--
--! @file       link_layer_sm.vhd
--! @brief      The state machine for the link layer
--! @details    Takes input from the transport and physical layers to determine
--!				the receiving and sending of frames
--! @author     Hannah Mohr
--! @date       January 2016
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
	trans_status_in :	in std_logic_vector(7 downto 0);		-- [FIFO_RDY/n, transmit request, data complete, escape, bad FIS, error, good FIS]
	trans_status_out:	out std_logic_vector(2 downto 0);		-- [crc good/bad, comm error, fail transmit]
	tx_data_in		:	in std_logic_vector(63 downto 0);
	rx_data_out		:	out std_logic_vector(63 downto 0);

	--Interface with Physical Layer
	tx_data_out		:	out std_logic_vector(63 downto 0);
	rx_data_in		:	in std_logic_vector(63 downto 0);
	phy_status_in	:	in std_logic_vector(1 downto 0);		-- [PHYRDY/n, Dec_Err]
	phy_status_out	:	out std_logic_vector(0 downto 0);		-- [clear status signals]
	perform_init	:	out std_logic);

end entity link_layer_sm;

architecture link_layer_sm_arch of link_layer_sm is
  
  type State_Type is (L_Idle, L_SyncEscape, L_NoCommErr, L_NoComm, L_SendAlign, L_RESET, 
						L_SendChkRdy, L_SendSOF, L_SendData, L_RcvrHold, L_SendHold, L_SendCRC,
						L_SendEOF, L_Wait, L_RcvChkRdy, L_RcvWaitFifo, L_RcvData, L_Hold, L_RcvHold,
						L_RcvEOF, L_GoodCRC, L_GoodEnd, L_BadEnd, L_PMDeny);
  signal current_state, next_state : State_Type;
  
  signal crc_good: std_logic;
  signal crc: std_logic_vector(31 downto 0);
  
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
  constant Dec_Err				: std_logic_vector(31 downto 0) := x"0A0A0A0A";
 
  
begin
  
  STATE_MEMORY: process (clk_75,rst_n)
    begin
      if (rst_n = '0') then 
        current_state <= L_RESET;
      elsif (clk_75'event and clk_75='1') then
        current_state <= next_state;
      end if;
    end process;
    
    NEXT_STATE_LOGIC: process (current_state, rx_data_in)
      begin 
        case (current_state) is
			-- Idle SM states (top level)
			when L_Idle  		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (trans_status_in(5)='1') then			-- needs to be updated
										next_state <= L_RcvWaitFifo;
									elsif (rx_data_in(31 downto 0)=X_RDYp) then
										next_state <= L_RcvWaitFifo;
									elsif (rx_data_in(31 downto 0)=PMREQ_Pp or rx_data_in(31 downto 0)=PMREQ_Sp) then
										next_state <= L_PMDeny;
									else 
										next_state <= L_Idle;
									end if;
			when L_SyncEscape  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=X_RDYp or rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_SendChkRdy;
									end if;
			when L_NoCommErr  	=>	next_state <= L_NoComm;
			when L_NoComm  		=> if (phy_status_in(1)='0') then
										next_state <= L_NoComm;
									else
										next_state <= L_SendAlign;
									end if;
			when L_SendAlign  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									else
										next_state <= L_Idle;
									end if;
			when L_RESET	  	=> if (rst_n = '0') then			-- active low
										next_state <= L_RESET;
									else 
										next_state <= L_NoComm;
									end if;
									
			-- Power Management SM states
			when L_PMDeny	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=PMREQ_Pp or rx_data_in(31 downto 0)=PMREQ_Sp) then
										next_state <= L_PMDeny;
									else 
										next_state <= L_Idle;
									end if;
			-- Transmit SM states
			when L_SendChkRdy	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=X_RDYp) then
										next_state <= L_RcvWaitFifo;
									elsif (rx_data_in(31 downto 0)=R_RDYp) then
										next_state <= L_SendSOF;
									else 
										next_state <= L_SendChkRdy;
									end if;
			when L_SendSOF	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_SendData;
									end if;
			when L_SendData	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=DMATp or trans_status_in(4) = '0') then
										next_state <= L_SendCRC;
									elsif ((trans_status_in(4) = '1' and rx_data_in(31 downto 0)=HOLDp)) then
										next_state <= L_RcvrHold;
									elsif (trans_status_in(4) = '1' and trans_status_in(6)='1') then
										next_state <= L_SendHold;
									else 
										next_state <= L_SendData;
									end if;
			when L_RcvrHold	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=DMATp and trans_status_in(4) = '1') then
										next_state <= L_SendCRC;
									elsif (trans_status_in(4) = '1' and rx_data_in(31 downto 0)=HOLDp) then
										next_state <= L_RcvrHold;
									else 
										next_state <= L_SendData;
									end if;
			when L_SendHold	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=DMATp or trans_status_in(4) = '0') then
										next_state <= L_SendCRC;
									elsif ((trans_status_in(4) = '1' and rx_data_in(31 downto 0)=HOLDp)) then
										next_state <= L_RcvrHold;
									elsif ((trans_status_in(4) = '1' and trans_status_in(6)='1')) then
										next_state <= L_SendHold;
									else 
										next_state <= L_SendData;
									end if;
			when L_SendCRC	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_SendEOF;
									end if;
			when L_SendEOF	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_Wait;
									end if;
			when L_Wait		  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=R_OKp) then
										next_state <= L_Idle;
									elsif (rx_data_in(31 downto 0)=R_ERRp) then
										next_state <= L_Idle;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_Wait;
									end if;
			-- Receive SM states
			when L_RcvChkRdy		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=X_RDYp) then
										next_state <= L_RcvChkRdy;
									elsif (rx_data_in(31 downto 0)=SOFp) then
										next_state <= L_RcvData;
									else 
										next_state <= L_Idle;
									end if;
			when L_RcvWaitFifo		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=X_RDYp) then
										if (trans_status_in(6) = '1') then					-- FIFO has room (ready)
											next_state <= L_RcvChkRdy;
										else 
											next_state <= L_RcvWaitFifo;
										end if;
									elsif (rx_data_in(31 downto 0)=SOFp) then
										next_state <= L_RcvData;
									else 
										next_state <= L_Idle;
									end if;
			when L_RcvData		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=Holdp) then
										next_state <= L_RcvHold;
									elsif ((rx_data_in(31 downto 0)=EOFp)) then
										next_state <= L_RcvEOF;
									elsif ((rx_data_in(31 downto 0)=WTRMp)) then
										next_state <= L_BadEnd;
									elsif (rx_data_in(31 downto 0)=HoldAp) then
										next_state <= L_RcvData;
									elsif (trans_status_in(6) = '0') then			-- FIFO full
										next_state <= L_Hold;
									else 
										next_state <= L_RcvData;
									end if;
			when L_Hold		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=Holdp and trans_status_in(6) = '1') then			-- FIFO ready
										next_state <= L_RcvHold;
									elsif ((rx_data_in(31 downto 0)=EOFp)) then
										next_state <= L_RcvEOF;
									elsif (trans_status_in(6) = '0') then											-- FIFO full
										next_state <= L_Hold;
									else 
										next_state <= L_RcvData;
									end if;
			when L_RcvHold		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then
										next_state <= L_SyncEscape;
									elsif (rx_data_in(31 downto 0)=Holdp) then
										next_state <= L_RcvHold;
									elsif ((rx_data_in(31 downto 0)=EOFp)) then
										next_state <= L_RcvEOF;
									else 
										next_state <= L_RcvData;
									end if;
			when L_RcvEOF		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (crc_good='1') then				-- crc status from Link Layer component
										next_state <= L_GoodCRC;
									elsif (crc_good='0') then
										next_state <= L_BadEnd;
									else 
										next_state <= L_RcvEOF;
									end if;
			when L_GoodCRC		=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									elsif (trans_status_in(0)='1') then
										next_state <= L_GoodEnd;
									elsif (trans_status_in(2)='1') then
										next_state <= L_BadEnd;
									elsif (trans_status_in(1)='1') then
										next_state <= L_BadEnd;
									elsif (crc_good='0') then
										next_state <= L_BadEnd;
									else 
										next_state <= L_GoodCRC;
									end if;
			when L_GoodEnd	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_GoodEnd;
									end if;
			when L_BadEnd	  	=> if (phy_status_in(1)='0') then
										next_state <= L_NoCommErr;
									elsif (rx_data_in(31 downto 0)=SYNCp) then
										next_state <= L_Idle;
									else 
										next_state <= L_BadEnd;
									end if;
			when others =>  next_state <= L_Idle;
        end case;
      end process;
          
    OUTPUT_LOGIC: process (current_state, rx_data_in)
      begin
        case (current_state) is
					-- Idle SM states (top level)
			when L_Idle  		=> rx_data_out(31 downto 0) <= SYNCp;
			when L_SyncEscape  	=> trans_status_out(0)  <= '1';
			when L_NoCommErr  	=> rx_data_out(31 downto 0) <= ALIGNp;
									phy_status_out(0) <= '1';
									trans_status_out(1)  <= '1';			-- comm_err
									trans_status_out(0)  <= '1';			-- fail transmit
			when L_NoComm  		=> rx_data_out(31 downto 0) <= ALIGNp;
			when L_SendAlign  	=> rx_data_out(31 downto 0) <= SYNCp;
			when L_RESET	  	=> trans_status_out(2 downto 0) <= "000";
									phy_status_out(0 downto 0) <= "0";
									
			-- Power Management SM states
			when L_PMDeny	  	=> tx_data_out(31 downto 0) <= PMNAKp;
			-- Transmit SM states
			when L_SendChkRdy	=> tx_data_out(31 downto 0) <= X_RDYp;
			when L_SendSOF	  	=> tx_data_out(31 downto 0) <= SOFp;
			when L_SendData	  	=> tx_data_out(31 downto 0) <= tx_data_in(31 downto 0);
			when L_RcvrHold	  	=> tx_data_out(31 downto 0) <= HOLDAp;
			when L_SendHold	  	=> tx_data_out(31 downto 0) <= HOLDp;
			when L_SendCRC	  	=> tx_data_out(31 downto 0) <= crc;
			when L_SendEOF	  	=> tx_data_out(31 downto 0) <= EOFp;
			when L_Wait		  	=> tx_data_out(31 downto 0) <= WTRMp;
			-- Receive SM states
			when L_RcvChkRdy	=> tx_data_out(31 downto 0) <= R_RDYp;
			when L_RcvWaitFifo	=> tx_data_out(31 downto 0) <= SYNCp;
			when L_RcvData		=> if(trans_status_in(3) = '1') then
										tx_data_out(31 downto 0) <= DMATp;
									else 
										tx_data_out(31 downto 0) <= R_IPp;
										rx_data_out <= rx_data_in;
									end if;
			when L_Hold			=> tx_data_out(31 downto 0) <= HOLDp;
			when L_RcvHold		=> if(trans_status_in(3) = '1') then
										tx_data_out(31 downto 0) <= DMATp;
									else 
										tx_data_out(31 downto 0) <= HoldAp;
									end if;
			when L_RcvEOF		=> tx_data_out(31 downto 0) <= R_IPp;
			when L_GoodCRC		=> tx_data_out(31 downto 0) <= R_IPp;
									
			when L_GoodEnd	  	=> tx_data_out(31 downto 0) <= R_OKp;
			when L_BadEnd		=> tx_data_out(31 downto 0) <= R_ERRp;
       when others =>  rx_data_out(31 downto 0) <= x"00000000";
        end case;
      end process;
          
end architecture; 
