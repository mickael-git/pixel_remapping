------------------------------------------------------------------------
--  tb_pixel_remapping.vhd
--  testbench
--
--  Copyright (C) 2013 M.FORET
--
--  This program is free software: you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation, either version
--  2 of the License, or (at your option) any later version.
------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pkg_pixel_remapping.all;


entity testbench is
end;

architecture tb of testbench is

component sensor_model
  generic (
    NB_LINES     : positive := 4;
    NB_LANES     : positive := 16;
    DATA_WIDTH   : positive := 8
  );
  port (
    clk        : in  std_logic;

    start      : in  std_logic;

    fval_out   : out std_logic;
    lval_out   : out std_logic;
    dval_out   : out std_logic;
    dout       : out std_logic_vector(NB_LANES*DATA_WIDTH-1 downto 0)
  );
end component;

constant NB_LANES     : positive := 16;
constant DATA_WIDTH   : positive := 12;

signal clk   : std_logic := '0';

signal fval_in      : std_logic := '0';
signal lval_in      : std_logic := '0';
signal dval_in      : std_logic := '0';
signal din          : std_logic_vector(NB_LANES*DATA_WIDTH-1 downto 0) := (others=>'0');

signal fval_out     : std_logic := '0';
signal lval_out     : std_logic := '0';
signal dval_out     : std_logic := '0';
signal dout         : std_logic_vector(NB_LANES*DATA_WIDTH-1 downto 0) := (others=>'0');

signal start        : std_logic := '0';

  procedure waiting(signal clk : std_logic; nb : integer) is
  begin
    for i in 1 to nb loop
      wait until rising_edge(clk);
    end loop;
  end;

begin

clk <= not(clk) after 5 ns;

  process
  begin

    wait for 500 ns;
    start <= '1';
    wait for 100 ns;
    start <= '0';

    wait for 20 us;

    start <= '1';
    wait for 100 ns;
    start <= '0';

    wait;
  end process;


sensor0 : sensor_model
  generic map (
    NB_LINES     => 4,
    NB_LANES     => NB_LANES,
    DATA_WIDTH   => DATA_WIDTH
  )
  port map (
    clk        => clk,

    start      => start,

    fval_out   => fval_in,
    lval_out   => lval_in,
    dval_out   => dval_in,
    dout       => din
  );

-- /////////////////////////////////////////////////////////////////////

uut0 : pixel_remapping
  generic map (
    NB_LANES     => NB_LANES,
    DATA_WIDTH   => DATA_WIDTH
  )
  port map (
    clk        => clk       ,

    fval_in    => fval_in   ,
    lval_in    => lval_in   ,
    dval_in    => dval_in   ,
    din        => din       ,

    fval_out   => fval_out,
    lval_out   => lval_out,
    dval_out   => dval_out,
    dout       => dout
  );

end tb;
