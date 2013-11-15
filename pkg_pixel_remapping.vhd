------------------------------------------------------------------------
--  pkg_pixel_remapping.vhd
--  package with all components
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
use ieee.numeric_std.ALL;


package pkg_pixel_remapping is

  component ram_sdp_reg
  generic (
    DATA_WIDTH  : positive :=18;
    ADDR_WIDTH  : positive :=12
  );
  port (
    clka   : in  std_logic;
    ena    : in  std_logic;
    wea    : in  std_logic_vector(0 downto 0);
    addra  : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    dina   : in  std_logic_vector(DATA_WIDTH-1 downto 0);
    clkb   : in  std_logic;
    enb    : in  std_logic;
    addrb  : in  std_logic_vector(ADDR_WIDTH-1 downto 0);
    doutb  : out std_logic_vector(DATA_WIDTH-1 downto 0):=(others =>'0')
  );
  end component;

  component pixel_remapping
  generic (
    NB_LANES     : positive := 16;
    DATA_WIDTH   : positive := 8  -- data width output (1 to 16)
  );
  port (
    clk        : in  std_logic;

    fval_in    : in  std_logic;
    lval_in    : in  std_logic;
    dval_in    : in  std_logic;
    din        : in  std_logic_vector(NB_LANES*DATA_WIDTH-1 downto 0);

    fval_out   : out std_logic;
    lval_out   : out std_logic;
    dval_out   : out std_logic;
    dout       : out std_logic_vector(NB_LANES*DATA_WIDTH-1 downto 0)
  );
  end component;

end pkg_pixel_remapping;
