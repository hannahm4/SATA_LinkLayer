library IEEE;
use IEEE.std_logic_1164.all;
use work.sata_defines.all;

entity link_layer_32bit_TB is
end entity;

architecture link_layer_32bit_TB_arch of link_layer_32bit_TB is
  
  constant t_clk_per : time := 50 ns;
  
  component link_layer_32bit
   port(-- Input
			clk					:	in std_logic;
			rst_n				:	in std_logic;

			--Interface with Transport Layer
			trans_status_in 	:	in std_logic_vector(7 downto 0);		-- [FIFO_RDY/n, transmit request, data complete, escape, bad FIS, error, good FIS]
			trans_status_out	:	out std_logic_vector(5 downto 0);		-- [crc good/bad, comm error, fail transmit]
			tx_data_in			:	in std_logic_vector(31 downto 0);
			rx_data_out			:	out std_logic_vector(31 downto 0);

			--Interface with Physical Layer
			tx_data_out			:	out std_logic_vector(31 downto 0);
			rx_data_in			:	in std_logic_vector(31 downto 0);
			phy_status_in		:	in std_logic_vector(2 downto 0);		-- [PHYRDY/n, Dec_Err]
			phy_status_out		:	out std_logic_vector(1 downto 0);		-- [clear status signals]
			
			perform_init		:	out std_logic);
  end component;
 
 
 -- Test bench signals
  signal clk_TB   				: std_logic;
  signal rst_n_TB 				: std_logic;
  
  signal trans_status_in_TB  	: std_logic_vector(7 downto 0); 
  signal trans_status_out_TB  	: std_logic_vector(5 downto 0);
  signal tx_data_in_TB			: std_logic_vector(31 downto 0);
  signal rx_data_out_TB			: std_logic_vector(31 downto 0);

  signal tx_data_out_TB			: std_logic_vector(31 downto 0);
  signal rx_data_in_TB			: std_logic_vector(31 downto 0);
  signal phy_status_in_TB		: std_logic_vector(2 downto 0);		-- [PHYRDY/n, Dec_Err]
  signal phy_status_out_TB		: std_logic_vector(1 downto 0);		-- [clear status signals]
  
  signal perform_init_TB 		: std_logic;
  
begin
    
  DUT1 : link_layer_32bit port map (
			-- Input
			clk					=> clk_TB,
			rst_n				=> rst_n_TB,

			--Interface with Transport Layer
			trans_status_in 	=> trans_status_in_TB,
			trans_status_out	=> trans_status_out_TB,
			tx_data_in			=> tx_data_in_TB,
			rx_data_out			=> rx_data_out_TB,

			--Interface with Physical Layer
			tx_data_out			=> tx_data_out_TB,
			rx_data_in			=> rx_data_in_TB,
			phy_status_in		=> phy_status_in_TB,
			phy_status_out		=> phy_status_out_TB,
			perform_init		=> perform_init_TB);
	
-----------------------------------------------
      CLOCK_STIM : process
       begin
          clk_TB <= '0'; wait for 0.5*t_clk_per; 
          clk_TB <= '1'; wait for 0.5*t_clk_per; 
       end process;
-----------------------------------------------      
      RESET_STIM : process
       begin
          rst_n_TB <= '0'; wait for 1.5*t_clk_per; 
          rst_n_TB <= '1'; wait; 
       end process;
-----------------------------------------------      
	  
	  DIN_STIM : process
       begin
			-- reset
			trans_status_in_TB 			<= "00000000";
			phy_status_in_TB	 		<= "000";
			rx_data_in_TB 				<= x"00000000";
			tx_data_in_TB 				<= x"00000000";
			
			wait for 3.5*t_clk_per; 					-- reset occurring 
			-- initialize
			phy_status_in_TB(1) 		<= '1';			-- PHYRDY
			wait for 3.0*t_clk_per; 					-- SendAlign, Idle
			-- start write
			trans_status_in_TB(5)	 	<= '1';			-- Transport Request
			wait for 1.0*t_clk_per;
			trans_status_in_TB(5) 		<= '0';
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB				<= R_RDYp;
			trans_status_in_TB(4) 		<= '1';			-- more data
			wait for 2.0*t_clk_per;						-- state transition
			tx_data_in_TB				<= x"00000001";
			wait for 1.0*t_clk_per;
			tx_data_in_TB				<= x"00000002";
			wait for 1.0*t_clk_per;
			---- Physical Layer Hold
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB			 	<= HOLDp;
			wait for 2.0*t_clk_per;						-- 1 clock for state transition, 1 clocks for continued HOLD
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB				<= x"00000000";	-- some other DWord from PHY
			--data transfer
			wait for 1.0*t_clk_per;
			tx_data_in_TB				<= x"00000003";
			-- Transport Layer Hold
			trans_status_in_TB(7) 		<= '1';
			wait for 2.0*t_clk_per;
			trans_status_in_TB(7) 		<= '0';
			-- last bit of data
			wait for 1.0*t_clk_per;
			tx_data_in_TB				<= x"00000004";
			wait for 1.0*t_clk_per;
			trans_status_in_TB(4) 		<= '0';			-- data done
			wait for 2.0*t_clk_per;
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB				<= R_OKp;
			
			wait for 5.0*t_clk_per;			-- time between write and read
			-- start read
			phy_status_in_TB(2) 			<= '1';		-- valid primitive
			rx_data_in_TB(31 downto 0) 		<= X_RDYp;	-- physical layer ready to send data
			trans_status_in_TB(6) 			<= '1';		-- FIFO has space
			wait for 2.0*t_clk_per;
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB(31 downto 0) 		<= SOFp;
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB(31 downto 0) 		<= x"C2D2768C";
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB(31 downto 0)		<= x"1F26B36A";
			wait for 1.0*t_clk_per;
			
			---- Physical Layer Hold
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB			 		<= HOLDp;
			wait for 3.0*t_clk_per;						-- 1 clock for state transition, 2 clocks for continued HOLDp
				-- CONTp testing
			rx_data_in_TB					<= CONTp;
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB					<= x"A0A0A0A0";		-- scrambled, irrelevant data
			wait for 5.0*t_clk_per;
			rx_data_in_TB			 		<= HOLDp;
			phy_status_in_TB(2) 			<= '1';
				-- return to normal testing
			wait for 1.0*t_clk_per;	
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB					<= x"00000000";	-- some other DWord from PHY
			--data transfer
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB					<= x"A508436F";
			-- Transport Layer Hold
			trans_status_in_TB(6) 			<= '0';
			wait for 2.0*t_clk_per;
			trans_status_in_TB(6) 			<= '1';
			
			-- last bit of data
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB(31 downto 0) 		<= x"3452D350";
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '0';
			rx_data_in_TB(31 downto 0) 		<= x"6B149732";			-- crc
			wait for 1.0*t_clk_per;
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB(31 downto 0) 		<= EOFp;
			wait for 1.0*t_clk_per;
			trans_status_in_TB(0) 			<= '1';				-- FIS good from Transport
			wait for 3.0*t_clk_per;
			phy_status_in_TB(2) 			<= '1';
			rx_data_in_TB(31 downto 0) 		<= SYNCp;
			
            wait;       
       end process;
  
end architecture;

