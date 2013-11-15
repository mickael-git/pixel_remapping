------------------------------------------------------------------------
--  sensor_model.vhd
--  sensor model
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


entity sensor_model is
  generic (
    NB_LINES     : positive := 4;  -- must be even
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
end entity;

architecture model of sensor_model is

constant NB_PIX     : positive := 128;
constant LOOP_LINE  : positive := 32 / NB_LANES;

signal fval     : std_logic;
signal lval     : std_logic;
signal dval     : std_logic;

begin

  process
    variable count    : integer;
  begin

    lval <= '0';
    fval <= '0';
    dval <= '0';
    dout <= (others=>'0');

    wait until rising_edge(start);

    wait until rising_edge(clk);

    for l in 1 to NB_LINES loop

      fval <= '1';
      lval <= '1';

      count := 0;

      for i in 1 to LOOP_LINE loop

        for p in 1 to NB_PIX loop

          dval <= '1';
          for nb in 0 to NB_LANES-1 loop
            --dout((nb+1)*DATA_WIDTH-1 downto nb*DATA_WIDTH) <= std_logic_vector(to_unsigned(count + NB_PIX*LOOP_LINE, DATA_WIDTH));
            dout((nb+1)*DATA_WIDTH-1 downto nb*DATA_WIDTH) <= std_logic_vector(to_unsigned((l-1)*32+nb+1, DATA_WIDTH));
          end loop;
          wait until rising_edge(clk);
          count := count + 1;

        end loop;

        if (i = LOOP_LINE) then
          lval <= '0';
        end if;

        if  l /= NB_LINES or i /= LOOP_LINE then
          -- OH
          dval <= '0';
          dout <= (others=>'0');
          wait until rising_edge(clk);
        end if;

      end loop;

    end loop;

  end process;

fval_out <= fval;
lval_out <= lval;
dval_out <= dval;

end model;
