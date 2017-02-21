library ieee;
use ieee.std_logic_1164.ALL;
use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;

package sata_defines is
    -- primitives
	constant ALIGNp 	: std_logic_vector(31 downto 0) := x"7B4A4ABC";
	constant CONTp 		: std_logic_vector(31 downto 0) := x"9999AA7C";
	constant DMATp 		: std_logic_vector(31 downto 0) := x"3636B57C";
	constant EOFp 		: std_logic_vector(31 downto 0) := x"D5D5B57C";
	constant HOLDp 		: std_logic_vector(31 downto 0) := x"D5D5AA7C";
	constant HOLDAp		: std_logic_vector(31 downto 0) := x"9595AA7C";
	constant PMACKp 	: std_logic_vector(31 downto 0) := x"9595957C";
	constant PMNAKp		: std_logic_vector(31 downto 0) := x"F5F5957C";
	constant PMREQ_Pp 	: std_logic_vector(31 downto 0) := x"1717B57C";
	constant PMREQ_Sp	: std_logic_vector(31 downto 0) := x"7575957C";
	constant R_ERRp		: std_logic_vector(31 downto 0) := x"5656B57C";
	constant R_IPp 		: std_logic_vector(31 downto 0) := x"5555B57C";
	constant R_OKp 		: std_logic_vector(31 downto 0) := x"3535B57C";
	constant R_RDYp 	: std_logic_vector(31 downto 0) := x"4A4A957C";
	constant SOFp 		: std_logic_vector(31 downto 0) := x"3737B57C";
	constant SYNCp 		: std_logic_vector(31 downto 0) := x"B5B5957C";
	constant WTRMp 		: std_logic_vector(31 downto 0) := x"5858B57C";
	constant X_RDYp 	: std_logic_vector(31 downto 0) := x"5757B57C";


    constant SYNC_PATTERN   : std_logic_vector(7  downto 0)  := x"BC";

    -- constants
    constant CHARS_PER_WORD           : integer     := 40;

    constant TRANSMIT_PULSE_CHARS     : integer     := 160;
--    constant TRANSMIT_PULSE_COUNT     : integer     := TRANSMIT_PULSE_CHARS / CHARS_PER_WORD;
    constant TRANSMIT_PULSE_COUNT     : integer     := TRANSMIT_PULSE_CHARS / CHARS_PER_WORD - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.
    constant MIN_DETECT_PULSE_COUNT   : integer    := TRANSMIT_PULSE_COUNT - 2;
    constant MAX_DETECT_PULSE_COUNT   : integer    := TRANSMIT_PULSE_COUNT + 2;


    constant COMRESET_PAUSE_CHARS     : integer     := 480;
--    constant COMRESET_PAUSE_COUNT     : integer     := COMRESET_PAUSE_CHARS / CHARS_PER_WORD;
    constant COMRESET_PAUSE_COUNT     : integer     := COMRESET_PAUSE_CHARS / CHARS_PER_WORD - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.
    constant MIN_COMRESET_DETECT_PAUSE_COUNT    : integer    := COMRESET_PAUSE_COUNT - 2;
    constant MAX_COMRESET_DETECT_PAUSE_COUNT    : integer    := COMRESET_PAUSE_COUNT + 2;


    constant COMINIT_PAUSE_CHARS     : integer     := 480;
--    constant COMINIT_PAUSE_COUNT      : integer     := COMINIT_PAUSE_CHARS / CHARS_PER_WORD;
    constant COMINIT_PAUSE_COUNT      : integer     := COMINIT_PAUSE_CHARS / CHARS_PER_WORD - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.
    constant MIN_COMINIT_DETECT_PAUSE_COUNT : integer    := COMINIT_PAUSE_COUNT - 2;
    constant MAX_COMINIT_DETECT_PAUSE_COUNT : integer    := COMINIT_PAUSE_COUNT + 2;


    constant COMWAKE_PAUSE_CHARS      : integer     := 160;
--    constant COMWAKE_PAUSE_COUNT      : integer     := COMWAKE_PAUSE_CHARS / CHARS_PER_WORD;
    constant COMWAKE_PAUSE_COUNT      : integer     := COMWAKE_PAUSE_CHARS / CHARS_PER_WORD - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.
    constant MIN_COMWAKE_DETECT_PAUSE_COUNT : integer    := COMWAKE_PAUSE_COUNT - 2;
    constant MAX_COMWAKE_DETECT_PAUSE_COUNT : integer    := COMWAKE_PAUSE_COUNT + 2;

--    constant NUM_PAUSES_TO_SEND       : integer     := 6;
--    constant NUM_PULSES_TO_SEND       : integer     := 6;
    constant NUM_PAUSES_TO_SEND       : integer     := 6 - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.
    constant NUM_PULSES_TO_SEND       : integer     := 6 - 1; -- Add minus 1 because counting from 0... probably not the best way to do this.

    constant MIN_IDLE_DETECT_COUNT    : integer     := 20; -- minimum of 525 ns idle time

    constant RETRY_INTERVAL           : integer     := 375000; -- minimum of 10ms retry time
    constant ALIGN_INTERVAL           : integer     := 32768; -- minimum of 873.8us alignp time


     -- types
    type OOB_SIGNAL     is (COMWAKE, COMRESET, COMINIT, NONE, INVALID);
    type OOB_STATE_TYPE is (IDLE, SEND_PAUSE, SEND_PULSE);

    type PHYINIT_STATE_TYPE is (HP1_HR_Reset,
                                HP2_HR_AwaitCOMINIT,
                                HP2B_HR_AwaitNoCOMINIT,
                                HP3_HR_Calibrate,
                                HP4_HR_COMWAKE,
                                HP5_HR_AwaitCOMWAKE,
                                HP5B_HR_AwaitNoCOMWAKE,
                                HP6_HR_AwaitAlign,
                                HP7_HR_SendAlign,
                                HP8_HR_Ready,
                                HP9_HR_Partial,
                                HP10_HR_Slumber,
                                HP11_HR_AdjustSpeed);

     -- programs
end sata_defines;
