
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity quadrature_controller is
port(
    clk         : in std_logic;
    rst         : in std_logic;

	 ps2_mouse   : in std_logic_vector(24 downto 0);

    x_a         : out std_logic;
    x_b         : out std_logic;
    y_a         : out std_logic;
    y_b         : out std_logic;
    left        : out std_logic;
    middle      : out std_logic;
    right       : out std_logic
);
end quadrature_controller;

architecture Behavioral of quadrature_controller is

signal flg: std_logic;
signal ms_valid: std_logic;


begin

	ms_valid <= '1' when flg /= ps2_mouse(24) else '0';
	process(clk) begin
		if rising_edge(clk) then
			flg <= ps2_mouse(24);
		end if;
	end process;

    -- Instantiate seperate instances of the quadrature encoder state machine
    -- for X and Y directions
    quad_x_fsm: entity work.quadrature_fsm port map (clk, rst, ms_valid, ps2_mouse(15 downto 8),  ps2_mouse(4), x_a, x_b);
    quad_y_fsm: entity work.quadrature_fsm port map (clk, rst, ms_valid, ps2_mouse(23 downto 16), ps2_mouse(5), y_a, y_b);
              
    manage_fsm: process(clk,rst)
    begin
        if (rst = '1') then
            left   <= '0';
            middle <= '0';
            right  <= '0';
        elsif (rising_edge(clk)) then
            if ms_valid = '1' then
					left   <= ps2_mouse(0);
					middle <= ps2_mouse(2);
					right  <= ps2_mouse(1);
				end if;
        end if;
    end process manage_fsm;

end Behavioral;
