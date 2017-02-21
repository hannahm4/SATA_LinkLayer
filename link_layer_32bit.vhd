----------------------------------------------------------------------------
--
--! @file       link_layer_32bit.vhd
--! @brief      Link Layer of the SATA controller with a 32bit wide data bus.
--! @details    Takes input from the transport and physical layers to facilitate
--				the receiving and sending of frames
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
--  Hannah Mohr
--  Electrical and Computer Engineering
--  Montana State University
--  hannah.mohr@msu.montana.edu
--
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all; 
use ieee.std_logic_arith.all ;
use ieee.std_logic_unsigned.all ;

use work.sata_defines.all;

----------------------------------------------------------------------------
--
--! @brief      link layer state machine
--! @details    Takes input from the transport and physical layers to determine
--!				the receiving and sending of frames
--! @param      clk	    			system clock
--! @param      reset           	active low reset
--! @param      trans_status_in     status vector from transport layer	[FIFO_RDY/n, transmit request, data complete, escape, bad FIS, error, good FIS]
--! @param      trans_status_out    status vector to transport layer	[Link Idle, transmit bad status, transmit good status, crc good/bad, comm error, fail transmit]
--! @param      tx_data_in	     	transmit data from transport layer
--! @param      rx_data_out     	receive data to transport layer
--! @param      tx_data_out     	transmit data to physical layer
--! @param      rx_data_in      	receive data from physical layer
--! @param      trans_status_in     status vector from physical layer	[primitive, PHYRDY/n, Dec_Err]
--! @param      trans_status_out    status vector to physical layer		[primitive, clear status signals]
--! @param      status_in       	status vector from the physical layer
--! @param      perform_init    	send initiation command to physical layer
--
----------------------------------------------------------------------------

entity link_layer_32bit is
	port(-- Input
			clk				:	in std_logic;
			rst_n			:	in std_logic;

			--Interface with Transport Layer
			trans_status_in :	in std_logic_vector(7 downto 0);		-- [FIFO_RDY/n, transmit request, data complete, escape, bad FIS, error, good FIS]
			trans_status_out:	out std_logic_vector(5 downto 0);		-- [Link Idle, transmit bad status, transmit good status, crc good/bad, comm error, fail transmit]
			tx_data_in		:	in std_logic_vector(31 downto 0);
			rx_data_out		:	out std_logic_vector(31 downto 0);

			--Interface with Physical Layer
			tx_data_out		:	out std_logic_vector(31 downto 0);
			rx_data_in		:	in std_logic_vector(31 downto 0);
			phy_status_in	:	in std_logic_vector(2 downto 0);		-- [primitive, PHYRDY/n, Dec_Err]
			phy_status_out	:	out std_logic_vector(1 downto 0);		-- [primitive, clear status signals]
			perform_init	:	out std_logic);
end entity;

architecture link_layer_32bit_arch of link_layer_32bit is

-- constants (naming convention: c --> constant, l --> link layer)
	-- trans_status_in
constant c_l_pause_transmit		: integer := 7;						-- Asserted when Transport Layer is not ready to transmit
constant c_l_fifo_ready	 		: integer := 6;						-- Asserted when Transport Layer FIFO has room for more data
constant c_l_transmit_request	: integer := 5;						-- Asserted when Transport Layer wants to begin a transmission
constant c_l_data_done	 		: integer := 4;						-- Asserted the clock cycle after the last of the Transport Layer data has been transmitted
constant c_l_escape		 		: integer := 3;						-- Asserted when the Transport Layer wants to terminate a transmission
constant c_l_bad_fis	 		: integer := 2;						-- Asserted at the end of a "read" when a bad FIS is received by the Transport Layer
constant c_l_error		 		: integer := 1;						-- Asserted at the end of a "read" when there is a different error in the FIS received by the Transport Layer
constant c_l_good_fis	 		: integer := 0;						-- Asserted at the end of a "read" when a good FIS is received by the Transport Layer
	-- trans_status_out
constant c_l_link_idle	 		: integer := 5;						-- Asserted when the Link Layer is in the Idle state and is ready for a transmit request
constant c_l_transmit_bad 		: integer := 4;						-- Asserted at the end of transmission to indicate in error
constant c_l_transmit_good		: integer := 3;						-- Asserted at the end of transmission to successful transmission
constant c_l_crc_good	 		: integer := 2;						-- Asserted when the CRC has been verified
constant c_l_comm_err	 		: integer := 1;						-- Asserted when there is an error in the communication channel (PHYRDYn)
constant c_l_fail_transmit 		: integer := 0;						-- Asserted when the communication channel fails during transmission
	-- phy_status_in
constant c_l_primitive_in 		: integer := 2;						-- Asserted when a valid primitive is being sent by the Physical Layer on the rx_data_in line
constant c_l_phyrdy		 		: integer := 1;						-- Asserted when the Physical Layer has successfully established a communication channel
constant c_l_dec_err	 		: integer := 0;						-- Asserted when there is an 8B10B encoding error
	-- phy_status_out
constant c_l_primitive_out 		: integer := 1;						-- Asserted when a valid primitive is being sent to the Physical Layer on the tx_data_out line
constant c_l_clear_status	 	: integer := 0;						-- Asserted to indicate to the Physical Layer to clear its status vector

-- state machine states
 type State_Type is (L_Idle, L_SyncEscape, L_NoCommErr, L_NoComm, L_SendAlign, L_RESET, 
						L_SendChkRdy, L_SendSOF, L_SendData, L_RcvrHold, L_SendHold, L_SendCRC,
						L_SendEOF, L_Wait, L_RcvChkRdy, L_RcvWaitFifo, L_RcvData, L_Hold, L_RcvHold,
						L_RcvEOF, L_GoodCRC, L_GoodEnd, L_BadEnd, L_PMDeny);
  signal current_state, next_state : State_Type;
  
-- components
	-- linear feedback shift register (lfsr) scrambler/descrambler component
component scrambler is 
  port (clk 		: in std_logic;
		rst_n		: in std_logic;
		scram_en	: in std_logic;
		scram_rst	: in std_logic;
		data_in 	: in std_logic_vector (31 downto 0);
		data_out 	: out std_logic_vector (31 downto 0));
end component;

	-- crc generator component
component crc_gen_32 is
   port(clk	       : in  std_logic; 
        rst_n      : in  std_logic; 
        soc        : in  std_logic; 
        data       : in  std_logic_vector(31 downto 0); 
        data_valid : in  std_logic; 
        eoc        : in  std_logic; 
        crc        : out std_logic_vector(31 downto 0));
end component;

-- signals 
	-- general signals
signal s_clk 				: std_logic;							-- clock for FPGA logic
signal s_rst_n 				: std_logic;							-- active low reset

	-- state machine signals
signal s_trans_status_in 	: std_logic_vector(7 downto 0);			-- input status vector from the Transport Layer. Checked in next_state_logic and output_logic
signal s_trans_status_out	: std_logic_vector(5 downto 0);			-- output status vector to the Transport Layer. Updated during output_logic
signal s_tx_data_in			: std_logic_vector(31 downto 0);		-- transmit data in (from Transport Layer)
signal s_rx_data_out		: std_logic_vector(31 downto 0);		-- receive data out (from Physical Layer)
signal s_tx_data_out		: std_logic_vector(31 downto 0);		-- transmit data out (Physical Layer)
signal s_rx_data_in			: std_logic_vector(31 downto 0);		-- receive data in (from Physical Layer)
signal s_phy_status_in 		: std_logic_vector(2 downto 0);			-- input status vector from the Physical Layer. Checked in next_state_logic and output_logic
signal s_phy_status_out		: std_logic_vector(1 downto 0);			-- output status vector to the Physical Layer. Updated during output_logic
signal s_crc				: std_logic_vector(31 downto 0);		-- 32bit CRC output from the crc generator component, to be appended to a write or used to check a read

	-- scrambler (lfsr) signals
signal s_lfsr_data_in		: std_logic_vector(31 downto 0);		-- data from Transport to be scrambled, or data from Physical to be descrambled
signal s_lfsr_data_out		: std_logic_vector(31 downto 0);		-- scrambled data from Transport to be sent to Physical, or descrambled data from Physical to be sent to Transport
signal s_lfsr_en			: std_logic;							-- enable to begin scrambling/descrambling
signal s_lfsr_rst			: std_logic;							-- internal reset signal to reset scrambling without a system reset

	-- crc generator signals
signal s_crc_data_in		: std_logic_vector(31 downto 0);		-- data from which the CRC is calculated
signal s_sof				: std_logic;							-- begin CRC generator
signal s_eof				: std_logic;							-- end CRC generator
signal s_crc_data_valid		: std_logic;							-- flag indicating that the input data is valid. Used to pause CRC computation
signal s_rx_data_in_temp	: std_logic_vector(31 downto 0);		-- vector that holds the previous primitive
signal s_cont_flag			: std_logic;							-- flag to indicate that CONTp has been received

begin 
-- assign signals to inputs/outputs
s_clk 				<= clk;
s_rst_n 			<= rst_n;
s_trans_status_in 	<= trans_status_in;
trans_status_out 	<= s_trans_status_out;
s_tx_data_in 		<= tx_data_in;
rx_data_out 		<= s_rx_data_out;
tx_data_out 		<= s_tx_data_out;
s_phy_status_in 	<= phy_status_in;
phy_status_out 		<= s_phy_status_out;

-- Receive CONTp Functionality (assigns s_rx_data_in)
CONTP_MEMORY: process (s_clk,s_rst_n,s_phy_status_in,rx_data_in)						-- memory to latch the previous valid primitive from the Physical Layer
    begin
      if (s_rst_n = '0') then 
        s_rx_data_in_temp	<= x"00000000";												-- initialize the temp value when reset is active
      elsif (rising_edge(s_clk)) then
		if(rx_data_in /= CONTp and s_phy_status_in(c_l_primitive_in) = '1') then
			s_rx_data_in_temp <= s_rx_data_in;											-- update the temp value if there is a valid primitive from the Physical Layer that is not CONTp
		end if;
      end if;
	  end process;
	  
CONTP_SUPPORT: process (s_phy_status_in,rx_data_in)										-- assign s_rx_data_in based on whether CONTp is active
    begin
      if (s_rst_n = '0') then 
		s_rx_data_in 	<= rx_data_in;													-- default signal assignment for s_rx_data_in when CONTp has not yet been used
		s_cont_flag		<= '0';															-- indicate that CONTp is not active
      else
		if(s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in = CONTp) then		-- Physical Layer sends a valid CONTp
			s_rx_data_in <= s_rx_data_in_temp;											-- s_rx_data_in gets assigned to be the previous valid primitive
			s_cont_flag <= '1';															-- indicate that CONTp is active
		elsif(s_phy_status_in(c_l_primitive_in) = '1') then								-- Physical Layer sends a new valid primitive
			s_rx_data_in <= rx_data_in;													-- s_rx_data_in gets assigned the input from the Physical Layer
			s_cont_flag <= '0';															-- CONTp is inactive
		elsif(s_cont_flag = '0') then
			s_rx_data_in <= rx_data_in;													-- when CONTp is inactive, s_rx_data_in gets assigned the data line from the Physical Layer
		end if;
      end if;
  end process;
 

-- call components
lfsr_component : scrambler
         port map  (clk					=> s_clk,
		 			rst_n				=> s_rst_n,
					scram_en			=> s_lfsr_en,
					scram_rst			=> s_lfsr_rst,
					data_in				=> s_lfsr_data_in,
					data_out 			=> s_lfsr_data_out);
					
crc_component : crc_gen_32
		 port map (clk      			=> s_clk,
				   rst_n      			=> s_rst_n,
				   soc        			=> s_sof,
				   data       			=> s_crc_data_in,
				   data_valid 			=> s_crc_data_valid,
				   eoc        			=> s_eof,
				   crc        			=> s_crc);
				   
 ----------------------------------------------------------------------------------
			   
				   
-- State Machine
STATE_MEMORY: process (s_clk,s_rst_n)
    begin
      if (s_rst_n = '0') then 					-- reset
        current_state 	<= L_RESET;
      elsif (rising_edge(s_clk)) then
        current_state 	<= next_state;			-- update current state with the next state on the rising clock edge
	  end if;
    end process;
    
    NEXT_STATE_LOGIC: process (current_state, s_rx_data_in, s_trans_status_in, s_tx_data_in, s_phy_status_in, s_crc)
      begin 
        case (current_state) is
			-- Idle SM states (top level)
			-- In L_Idle, the Link Layer waits for a request to transmit from the Transport Layer or a request for a receive packet from the Physical Layer
			when L_Idle  		=> if (s_phy_status_in(c_l_phyrdy)='0') then					-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_trans_status_in(c_l_transmit_request)='1') then	-- Transport Layer requests frame transmission
										next_state <= L_SendChkRdy;
									elsif (s_rx_data_in(31 downto 0)=X_RDYp) then				-- Physical Layer indicates data is ready to be sent to the Transport Layer
										next_state <= L_RcvWaitFifo;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=PMREQ_Pp) then		-- Power management requested
										next_state <= L_PMDeny;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=PMREQ_Sp) then		-- Power management requested
										next_state <= L_PMDeny;
									else 
										next_state <= L_Idle;								-- wait for input from the Transport Layer or Physical Layer
									end if;
			-- In L_SyncEscape, the Link Layer ends the transmission in response to a flag from the Transport Layer
			when L_SyncEscape  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0) = X_RDYp) then		-- Physical Layer indicates data is ready to be sent to the Transport Layer
										next_state <= L_Idle;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0) = SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									else 
										next_state <= L_SyncEscape;							-- wait for response from the Physical Layer
									end if;
			-- L_NoCommErr is a transition state used to inform the Transport Layer of the communication failure
			when L_NoCommErr  	=>	next_state <= L_NoComm; 								-- always move on to the recovery state
			-- L_NoComm is used to wait for the Physical Layer to restart the communication channel
			when L_NoComm  		=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoComm;
									else
										next_state <= L_SendAlign;							-- If the Physical Layer indicates the communication channel is operational, exit the NoComm state
									end if;
			-- L_SendAlign is used to send the ALIGNp primitive to the Physical Layer
			when L_SendAlign  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									else
										next_state <= L_Idle;								-- Once ALIGNp has been sent, return to the Idle state to await further input
									end if;
			-- L_RESET is entered when the system reset is active 
			when L_RESET	  	=> --next_state <= L_NoComm;	
									if (rst_n = '0') then					-- active low
										next_state <= L_RESET;
									else 
										next_state <= L_NoComm;				-- when reset is not active, begin to reset the communication channel through the Physical Layer
									end if;
									
			-- Power Management SM states
			-- L_PMDeny informs the Physical Layer that power management is not supported by the controller
			when L_PMDeny	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=PMREQ_Pp) then
										next_state <= L_PMDeny;								-- if power management requests persist, continue to inform the Physical Layer they are not supported
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=PMREQ_Sp) then
										next_state <= L_PMDeny;								-- if power management requests persist, continue to inform the Physical Layer they are not supported
									else 
										next_state <= L_Idle;								-- when power management requests end, return to the idle state to await further input
									end if;
									
			-- Transmit SM states
			-- In L_SendChkRdy, the Link Layer verifies that the Physical Layer is ready to receive the data before sending the frame from the Transport Layer
			when L_SendChkRdy	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then			-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=X_RDYp) then			-- if the Physical Layer is ready to send data, switch to the receive state machine
										next_state <= L_RcvWaitFifo;
									elsif (s_rx_data_in(31 downto 0)=R_RDYp) then			-- if the Physical Layer is ready to receive data, send the Start of Frame (SOF) primitive
										next_state <= L_SendSOF;
									else 
										next_state <= L_SendChkRdy;							-- wait for the Physical Layer to respond
									end if;
			-- L_SendSOF sends the primitive which informs later processing that the frame is starting
			when L_SendSOF	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									else 
										next_state <= L_SendData;							-- immediately after the SOFp has been sent, begin sending the FIS payload
									end if;
			-- L_SendData connects the input from the Transport Layer to the output leading to the Physical Layer
			when L_SendData	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (s_trans_status_in(c_l_escape)='1') then			-- Transport Layer requests the end of the FIS transmission
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=DMATp) then	-- Physical Layer requests end of transmission
										next_state <= L_SendCRC;
									elsif (s_trans_status_in(c_l_data_done) = '0') then											-- No more data to transmit
										next_state <= L_SendCRC;
									elsif ((s_trans_status_in(c_l_data_done) = '1' and s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HOLDp)) then	-- more data to transmit
										next_state <= L_RcvrHold;
									elsif (s_trans_status_in(c_l_data_done) = '1' and s_trans_status_in(c_l_pause_transmit)='1') then			-- more data to transmit and transport not ready to transmit (pause)
										next_state <= L_SendHold;
									else 
										next_state <= L_SendData;				-- continue to send data from Transport to Physical until further instructions are received
									end if;
			-- L_RcvrHold is the response to a hold request from the Physical Layer
			when L_RcvrHold	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (s_trans_status_in(c_l_escape)='1') then			-- transport requests escape transmit
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=DMATp and s_trans_status_in(c_l_data_done) = '1') then		-- more data to transmit
										next_state <= L_SendCRC;
									elsif (s_trans_status_in(c_l_data_done) = '1' and s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HOLDp) then			-- more data to transmit and Physical Layer requests a hold
										next_state <= L_RcvrHold;
									else 
										next_state <= L_SendData;		-- If the Physical Layer no longer requests a hold, return to sending data
									end if;
			-- L_SendHold informs the Physical Layer that the Transport Layer has requested a hold
			when L_SendHold	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_trans_status_in(c_l_escape)='1') then			-- transport requests escape transmit
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=DMATp) then		-- Physical Layer requests a transmission end
										next_state <= L_SendCRC;
									elsif (s_trans_status_in(c_l_data_done) = '0') then												-- no more data to transmit
										next_state <= L_SendCRC;
									elsif ((s_trans_status_in(c_l_data_done) = '1' and s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HOLDp)) then	-- more data to transmit and Physical Layer responds with a hold primitive
										next_state <= L_RcvrHold;
									elsif ((s_trans_status_in(c_l_data_done) = '1' and s_trans_status_in(c_l_pause_transmit)='1')) then			-- more data to transmit and FIFO full/ not ready
										next_state <= L_SendHold;
									else 
										next_state <= L_SendData;		-- If the Transport Layer no longer requests a hold, return to sending data
									end if;
			-- L_SendCRC is entered when there is no more FIS data to transmit, enabling the CRC to be transmitted
			when L_SendCRC	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									else 
										next_state <= L_SendEOF;					-- once the CRC has been sent, proceed to send the end of frame primitive
									end if;
			-- L_SendEOF is the state which sends the end of frame primitive to the Physical Layer 
			when L_SendEOF	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then		-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									else 
										next_state <= L_Wait;						-- once the EOF primitive has been sent, proceed to the next state to wait for a status response from the Physical Layer
									end if;
			-- L_Wait is the state in which the Link Layer waits for a response from the Physical Layer on the quality of the completed transmission
			when L_Wait		  	=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0) = R_OKp) then			-- Physical Layer indicates that the transmission was successful
										next_state <= L_Idle;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0) = R_ERRp) then			-- Physical Layer indicates there was an error with the transmission
										next_state <= L_Idle;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0) = SYNCp) then			-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									else 																								-- wait for a response from the Physical Layer
										next_state <= L_Wait;
									end if;
			-- Receive SM states
			-- In L_RcvWaitFifo, the Link Layer waits for the Transport Layer to indicate that the FIFO buffers are ready to receive data
			when L_RcvWaitFifo		=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=X_RDYp) then		-- if the Physical Layer continues signal data is ready, check the Transport FIFO status
										if (s_trans_status_in(c_l_fifo_ready) = '1') then			-- FIFO has room (ready), so proceed to send that the Transport Layer is ready to receive data
											next_state <= L_RcvChkRdy;
										else 
											next_state <= L_RcvWaitFifo;							-- if the FIFO is full, continue to wait
										end if;
									else 
										next_state <= L_Idle;										-- if the Physical Layer no longer signals that data is ready, return to the Idle state to await further commands
									end if;
			-- L_RcvChkRdy sends a response to the Physical Layer that the Transport Layer is ready to receive
			when L_RcvChkRdy		=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=X_RDYp) then			-- wait for a response from the Physical Layer 
										next_state <= L_RcvChkRdy;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SOFp) then				-- receipt of the start of frame primitive indicates the data transmission is about to begin
										next_state <= L_RcvData;
									else 
										next_state <= L_Idle;							-- if the Physical Layer does not respond with a valid primitive, return to the Idle state to await further commands
									end if;
			-- In L_RcvData, the data from the Physical Layer is sent to the Transport Layer
			when L_RcvData		=> if (s_phy_status_in(c_l_phyrdy)='0') then				-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and rx_data_in(31 downto 0)=SYNCp) then				-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (s_trans_status_in(c_l_escape)='1') then														-- transport requests escape transmit
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=Holdp) then 			-- The Physical Layer requests a data hold
										next_state <= L_RcvHold;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=EOFp) then				-- Reception of the End of Frame primitive indicates the data FIS is complete
										next_state <= L_RcvEOF;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=WTRMp) then			-- WTRMp is received before the End of Frame primitive, there has been an error
										next_state <= L_BadEnd;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HoldAp) then			-- If the Physical Layer acknowledges a hold, continue to receive data
										next_state <= L_RcvData;
									elsif (s_trans_status_in(c_l_fifo_ready) = '0') then												-- if the Transport Layer indicates the FIFO is full, inform the Physical Layer of a hold command
										next_state <= L_Hold;
									else 
										next_state <= L_RcvData;																		-- continue to receive data until a valid command is received
									end if;
			-- L_Hold informs the Physical Layer that the Transport Layer does not have FIFO space available
			when L_Hold		=> if (s_phy_status_in(c_l_phyrdy)='0') then																-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then			-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (trans_status_in(3)='1') then																	-- Transport Layer requests escape transmit
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=Holdp) then			-- Physical Layer requests a hold
										next_state <= L_RcvHold;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=EOFp) then				-- if the next primitive is End of Frame, the FIS data is complete
										next_state <= L_RcvEOF;
									elsif (s_trans_status_in(c_l_fifo_ready) = '0') then												-- the Transport FIFO is full
										next_state <= L_Hold;
									else 
										next_state <= L_RcvData;																		-- if the Transport FIFO is not full, return to receive data
									end if;
			-- L_RcvHold is the state in which the Link Layer waits while the Physical Layer transmits a hold
			when L_RcvHold		=> if (s_phy_status_in(c_l_phyrdy)='0') then															-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;	
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then			-- Physical Layer requests synchronization, failing the transmission
										next_state <= L_Idle;
									elsif (s_trans_status_in(c_l_escape)='1') then														-- Transport requests escape transmit
										next_state <= L_SyncEscape;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=Holdp) then			-- Physical Layer continues to request a hold
										next_state <= L_RcvHold;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=EOFp) then				-- if the next primitive is End of Frame, the FIS data is complete
										next_state <= L_RcvEOF;
									else 
										next_state <= L_RcvData;																		-- if the Physical Layer stops transmitting HOLD, return to data reception 
									end if;
			-- L_RcvEOF is the state at the end of a frame reception
			when L_RcvEOF		=> if (s_phy_status_in(c_l_phyrdy)='0') then															-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_crc = x"00000000") then																	-- if the crc result from the Link Layer component is zero, the crc was correct
										next_state <= L_GoodCRC;																		-- go to GoodCRC state to inform the Transport Layer of the good CRC result
									elsif (s_crc /= x"00000000") then																	-- if the crc result is not zero, there is a crc error
										next_state <= L_BadEnd;																			-- go to BadEnd state to inform the Transport Layer of the error
									else 
										next_state <= L_RcvEOF;																			-- wait for crc calculation
									end if;
			-- L_GoodCRC informs the Transport Layer that the correct CRC was attached to the FIS data
			when L_GoodCRC		=> if (s_phy_status_in(c_l_phyrdy)='0') then															-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then			-- Physical Layer requests synchronization without failing the reception
										next_state <= L_Idle;
									elsif (s_trans_status_in(c_l_good_fis)='1') then													-- if the Transport Layer indicates the received FIS was good, the reception is considered successful
										next_state <= L_GoodEnd;
									elsif (s_trans_status_in(c_l_bad_fis)='1') then														-- if the Transport Layer indicates the received FIS was bad, the reception failed
										next_state <= L_BadEnd;
									elsif (s_trans_status_in(c_l_error)='1') then														-- if the Transport Layer indicates a different error, the reception failed
										next_state <= L_BadEnd;
									else 
										next_state <= L_GoodCRC;																		-- wait for response from the Transport Layer
									end if;
			-- L_GoodEnd is used to indicate the reception succeeded
			when L_GoodEnd	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then															-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then			-- Physical Layer requests synchronization without failing the reception
										next_state <= L_Idle;
									else 
										next_state <= L_GoodEnd;																		-- Wait for Physical Layer to sync
									end if;
			-- L_BadEnd is used to indicate the reception failed
			when L_BadEnd	  	=> if (s_phy_status_in(c_l_phyrdy)='0') then															-- PHYRDYn: Physical Layer indicates the communication channel failed
										next_state <= L_NoCommErr;
									elsif (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=SYNCp) then			-- Physical Layer requests synchronization without failing the reception
										next_state <= L_Idle;
									else 
										next_state <= L_BadEnd;																			-- Wait for Physical Layer to sync
									end if;
			-- return to Idle if an unknown condition occurs
			when others =>  next_state <= L_Idle;
        end case;
      end process;
          
    OUTPUT_LOGIC: process (current_state, s_rx_data_in, s_trans_status_in, s_tx_data_in, s_phy_status_in, s_lfsr_data_out, s_crc, s_sof)
      begin
        case (current_state) is
					-- Idle SM states (top level)
			
			when L_Idle  		=> s_tx_data_out(31 downto 0) 						<= SYNCp;			-- transmit SYNCp to the Physical Layer, indicating that the Link Layer is waiting
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid 								<= '0';				-- inform the crc component that the input data is not valid (not part of a FIS payload)
									s_trans_status_out(c_l_link_idle)				<= '1';				-- inform the Transport Layer that the Link Layer is Idle
									s_trans_status_out(c_l_transmit_bad) 			<= '0';				-- reset the transmit bad status to zero
									s_trans_status_out(c_l_transmit_good) 			<= '0';				-- reset the transmit good status to zero
									s_trans_status_out(c_l_crc_good) 				<= '0';				-- reset the CRC valid flag to zero (the crc computation is not complete)
			
			when L_SyncEscape  	=> s_trans_status_out(c_l_fail_transmit)  			<= '1';				-- indicate the failed transmission to the Transport Layer
									s_crc_data_valid 								<= '0';				-- reset the CRC valid flag to zero (the crc computation is not complete)
									
			when L_NoCommErr  	=> s_tx_data_out(31 downto 0) 						<= ALIGNp;			-- Transmit ALIGNp to the Physical Layer, indicating the Link Layer wants to reset the comm channel
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_phy_status_out(0) 							<= '1';
									s_trans_status_out(c_l_comm_err)  				<= '1';				-- inform the Transport Layer that there has been a comm error
									s_trans_status_out(c_l_fail_transmit) 			<= '1';				-- inform the Transport Layer that the transmission failed
									s_crc_data_valid  								<= '0';				-- inform the CRC generator that the input data is no longer valid
									s_trans_status_out(c_l_link_idle)				<= '0';				-- inform the Transport Layer that the Link Layer is not Idle
									
			when L_NoComm  		=> s_tx_data_out(31 downto 0) 						<= ALIGNp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
			when L_SendAlign  	=> s_tx_data_out(31 downto 0) 						<= ALIGNp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
			when L_RESET	  	=> s_trans_status_out(5 downto 0)					<= "000000";
									s_phy_status_out(1 downto 0) 					<= "00";
									s_rx_data_out 									<= x"00000000";
									s_tx_data_out 									<= x"00000000";
									s_crc_data_in									<= x"00000000";
									s_eof 											<= '0';
									s_sof											<= '0';
									s_crc_data_valid 								<= '0';
									s_lfsr_data_in									<= x"00000000";
									s_lfsr_en										<= '0';
									s_lfsr_rst										<= '1';
									s_trans_status_out(c_l_crc_good) 				<= '0';				-- reset the CRC valid flag to zero
			-- Power Management SM states
			when L_PMDeny	  	=> s_tx_data_out(31 downto 0) 						<= PMNAKp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_trans_status_out(c_l_link_idle)				<= '0';				-- inform the Transport Layer that the Link Layer is not Idle
			
			-- Transmit SM states
			when L_SendChkRdy	=> s_tx_data_out(31 downto 0) 						<= X_RDYp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_trans_status_out(c_l_link_idle)				<= '0';				-- inform the Transport Layer that the Link Layer is not Idle
			when L_SendSOF	  	=> s_tx_data_out(31 downto 0) 						<= SOFp;
									s_phy_status_out(c_l_primitive_out)				<= '1';
									s_sof	 										<= '1';
									s_crc_data_valid  								<= '0';
									s_lfsr_en										<= '0';
									s_crc_data_in									<= x"00000000";
									s_lfsr_rst										<= '0';
			when L_SendData	  	=> 
									s_phy_status_out(c_l_primitive_out)				<= '0';				-- inform the Physical Layer that a valid primitive is NOT being transmitted
									s_lfsr_rst										<= '1';
									s_crc_data_in									<= s_tx_data_in;
									s_lfsr_data_in									<= s_tx_data_in;
									s_tx_data_out									<= s_lfsr_data_out;
									s_crc_data_valid  								<= '1';
									s_lfsr_en										<= '1';
									s_sof											<= '0';
									if (s_trans_status_in(c_l_data_done) = '0') then
										s_eof 										<= '1';
										s_lfsr_data_in 								<= s_crc;
										s_crc_data_valid 							<= '0';
									end if;
									if (s_trans_status_in(c_l_data_done) = '1' and s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HOLDp) then		-- more data to transmit and Physical sends HOLDAp
										s_crc_data_valid 							<= '0';
										s_lfsr_en									<= '0';
									end if;
			when L_RcvrHold	  	=> s_tx_data_out(31 downto 0) 						<= HOLDAp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid  								<= '0';
									s_lfsr_en										<= '0';
									s_crc_data_in									<= x"00000000";
			when L_SendHold	  	=> s_tx_data_out(31 downto 0) 						<= HOLDp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid 								<= '0';
									s_lfsr_en										<= '0';
			when L_SendCRC	  	=> 	s_tx_data_out									<= s_lfsr_data_out;
									s_eof 											<= '0';
									s_crc_data_valid  								<= '0';
									s_lfsr_en										<= '0';
			when L_SendEOF	  	=> s_tx_data_out(31 downto 0) 						<= EOFp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
			when L_Wait		  	=> s_tx_data_out(31 downto 0) 						<= WTRMp;			-- also need status to Transport
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									if(s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in = R_OKp) then
										s_trans_status_out(c_l_transmit_good) 		<= '1';
									elsif(s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in = R_ERRp) then
										s_trans_status_out(c_l_transmit_bad) 		<= '1';
									end if;
			-- Receive SM states
			when L_RcvWaitFifo	=> s_tx_data_out(31 downto 0) 						<= SYNCp;			-- transmit SYNCp to the Physical Layer, indicating that the Link Layer is waiting
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid  								<= '0';				-- inform the crc component that the input data is not ready
									s_sof 											<= '1';				-- set the initial condition of the crc component
									s_lfsr_rst										<= '0';				-- reset the descrambler to set the initial condition
									s_trans_status_out(c_l_link_idle)				<= '0';				-- inform the Transport Layer that the Link Layer is not Idle
			when L_RcvChkRdy	=> s_tx_data_out(31 downto 0) 						<= R_RDYp;			-- transmit R_RDYp to the Physical Layer, indicating readiness for reception
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
			when L_RcvData		=> if(s_trans_status_in(c_l_escape) = '1') then							-- if the Transport Layer requests an end to the FIS
										s_tx_data_out(31 downto 0) 					<= DMATp;			-- transmit DMATp to the Physical Layer, requesting the FIS transmission stop
										s_phy_status_out(c_l_primitive_out)			<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
										s_crc_data_valid  							<= '0';				-- inform the CRC component that the input data is not valid
										s_crc_data_in								<= x"00000000";		-- reset the input to the CRC component
									else 
										s_tx_data_out(31 downto 0) 					<= R_IPp;			-- Transmit R_IPp to the Physical Layer, indicating that reception is in progress
										s_phy_status_out(c_l_primitive_out)			<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
										s_lfsr_data_in								<= s_rx_data_in;	-- connect the data from the Physical Layer to the input of the descrambler component
										s_crc_data_in								<= s_lfsr_data_out;	-- connect the descrambled output of the descrambler to the input of the crc component
										s_lfsr_en									<= '1';				-- start the descrambler component
										s_lfsr_rst									<= '1';				-- verify the component reset is high (since it is active low)
										if (s_lfsr_data_out'event) then									-- when the output of the descrambler changes, the output is ready for the CRC
											s_crc_data_valid  						<= '1';				-- inform the crc component that the input is valid
											s_sof 									<= '0';				-- deassert the initial condition
										end if;
										if (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=EOFp) then		-- if EOFp is received, the FIS data is over
											s_eof 									<= '1';											-- signal the crc component to stop
											--s_lfsr_data_in 							<= s_crc;									-- scramble the final crc
										end if;
										if (s_sof = '0') then																		-- if the descrambler output is valid
											s_rx_data_out							<= s_lfsr_data_out;								-- send the descrambler output to the Transport Layer
										end if;
										if (s_phy_status_in(c_l_primitive_in) = '1' and s_rx_data_in(31 downto 0)=HOLDp) then		-- more data to transmit and Physical sends HOLDAp
										s_lfsr_en									<= '0';											-- pause the descrambler (set enable to zero)
										end if;
										if (s_trans_status_in(c_l_fifo_ready) = '0') then											-- if the FIFO is not ready
											s_lfsr_en								<= '0';											-- pause the descrambler (set enable to zero)
										end if;
									end if;
			when L_Hold			=> s_tx_data_out(31 downto 0) 						<= HOLDp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid  								<= '0';
									s_lfsr_en										<= '0';
									s_crc_data_in									<= x"00000000";
									s_sof											<= '0';
									if (s_trans_status_in(c_l_fifo_ready) = '1') then
										s_lfsr_en									<= '1';
									end if;
			when L_RcvHold		=> s_crc_data_valid  								<= '0';
									s_crc_data_in									<= x"00000000";
									s_sof											<= '0';
									s_lfsr_en										<= '0';
									if(s_trans_status_in(c_l_escape) = '1') then
										s_tx_data_out(31 downto 0) 					<= DMATp;
										s_phy_status_out(c_l_primitive_out)			<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									else 
										s_tx_data_out(31 downto 0)			 		<= HoldAp;
										s_phy_status_out(c_l_primitive_out)			<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									end if;
									
			when L_RcvEOF		=> s_tx_data_out(31 downto 0) 						<= R_IPp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_eof 											<= '1';
									s_crc_data_valid 								<= '0';	
									s_lfsr_en										<= '0';
									s_sof											<= '0';
			when L_GoodCRC		=> s_tx_data_out(31 downto 0) 						<= R_IPp;
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid  								<= '0';
									s_crc_data_in									<= x"00000000";
									s_trans_status_out(c_l_crc_good) 				<= '1';				-- inform the Transport Layer that the CRC was valid
			when L_GoodEnd	  	=> s_tx_data_out(31 downto 0) 						<= R_OKp;			-- transmit R_OKp to the Physical Layer, indicating the reception was successful
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
			when L_BadEnd		=> s_tx_data_out(31 downto 0) 						<= R_ERRp;			-- transmit R_ERRp to the Physical Layer, indicating the reception was unsuccessful
									s_phy_status_out(c_l_primitive_out)				<= '1';				-- inform the Physical Layer that a valid primitive is being transmitted
									s_crc_data_valid  								<= '0';				-- 
									s_crc_data_in									<= x"00000000";
			when others 		=> s_rx_data_out(31 downto 0) 						<= x"00000000";
        end case;
      end process;
	  

end architecture; 
