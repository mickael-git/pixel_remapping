------------------------------------------------------------------------
--  pixel_remapping.vhd
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

entity pixel_remapping is
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
end entity;

architecture rtl of pixel_remapping is

function log2(val: natural) return natural is
  variable res : natural;
begin
  for i in 30 downto 0 loop
    if (val > (2**i)) then
      res := i;
      exit;
    end if;
  end loop;
  return (res + 1);
end function log2;

constant NB_LANES_WIDTH     : positive := log2(NB_LANES);
constant SIZE_LINE          : positive := 4096 / NB_LANES;
constant ADDR_WIDTH         : positive := log2(SIZE_LINE);
constant ADDR_WIDTH_SYNC    : positive := 10;

type sel_mem_t is array (natural range <>) of unsigned(NB_LANES_WIDTH-1 downto 0);
signal sel_wr   : sel_mem_t(NB_LANES-1 downto 0);

type vect_data is array (natural range <>) of std_logic_vector(DATA_WIDTH-1 downto 0);
signal wr_data  : vect_data(NB_LANES-1 downto 0);
signal rd_data  : vect_data(NB_LANES-1 downto 0);

type vect_addr is array (natural range <>) of unsigned(ADDR_WIDTH-1 downto 0);
signal addr    : vect_addr(NB_LANES-1 downto 0);
signal addr_rd : vect_addr(NB_LANES-1 downto 0);
type vect_addr1 is array (natural range <>) of unsigned(ADDR_WIDTH downto 0);
signal mem_addr_rd : vect_addr1(NB_LANES-1 downto 0);

signal fval1       : std_logic := '0';
signal lval1       : std_logic := '0';
signal dval1       : std_logic := '0';

signal addr_wr     : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
signal msb_addr_wr : std_logic := '0';
signal mem_addr_wr : unsigned(ADDR_WIDTH downto 0) := (others=>'0');
signal msb_addr_rd : std_logic := '0';
signal sel_mem     : unsigned(NB_LANES_WIDTH-1 downto 0);
signal sel_rd      : unsigned(NB_LANES_WIDTH-1 downto 0);
signal wea         : std_logic_vector(0 downto 0);

signal addr_wr_sync : unsigned(ADDR_WIDTH_SYNC-1 downto 0);
signal addr_rd_sync : unsigned(ADDR_WIDTH_SYNC-1 downto 0);
signal wea_sync     : std_logic_vector(0 downto 0);
signal din_sync     : std_logic_vector(1 downto 0);
signal dout_sync    : std_logic_vector(1 downto 0);
signal read_sync    : std_logic := '0';
signal read_sync1   : std_logic := '0';
signal read_sync2   : std_logic := '0';
signal fval_rd      : std_logic := '0';
signal lval_rd      : std_logic := '0';
signal dval_rd      : std_logic := '0';
signal fval_delay   : std_logic_vector(3 downto 0);
signal lval_delay   : std_logic_vector(3 downto 0);
signal dval_delay   : std_logic_vector(3 downto 0);

begin

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- writing into RAM

  process(clk)
  begin
    if rising_edge(clk) then
      dval1 <= dval_in;
      lval1 <= lval_in;
      fval1 <= fval_in;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if lval1 = '0' then
        addr_wr <= (others=>'0');
      elsif (dval1 = '1') then
        addr_wr <= addr_wr + 1;
      end if;
    end if;
  end process;

  process(clk)
  begin
    if rising_edge(clk) then
      if fval1 = '0' then
        msb_addr_wr <= '0';
      elsif (fval1 = '1' and lval1 = '0') then
        msb_addr_wr <= not(msb_addr_wr);
      end if;
    end if;
  end process;

mem_addr_wr <= msb_addr_wr & addr_wr;

  process(clk)
    variable sel  : unsigned(NB_LANES_WIDTH-1 downto 0) := (others=>'0');
  begin
    if rising_edge(clk) then
      if lval_in = '0' then
       sel := (others=>'0');
      elsif (dval_in = '1') then
        sel := sel - 1;
      end if;
      for i in 0 to NB_LANES-1 loop
        sel_wr(i) <= sel + i;
      end loop;
    end if;
  end process;

gen_data_wr:
for i in 0 to NB_LANES-1 generate
  process(clk)
    variable sel : natural;
  begin
    if rising_edge(clk) then
      sel := to_integer(sel_wr(i));
      wr_data(i) <= din((sel+1)*DATA_WIDTH-1 downto sel*DATA_WIDTH);
    end if;
  end process;
end generate;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- reading from RAM

  process(clk)
    variable sel  : unsigned(ADDR_WIDTH-1 downto 0) := (others=>'0');
  begin
    if rising_edge(clk) then
      if lval_rd = '0' then
        sel := (others=>'0');
      elsif (dval_rd = '1') then
        sel := sel + 16;
      end if;
      for i in 0 to NB_LANES-1 loop
        addr(i) <= sel + i;
      end loop;
    end if;
  end process;

  process(clk)
    variable cnt_lane  : unsigned(NB_LANES_WIDTH-1 downto 0) := (others=>'0');
  begin
    if rising_edge(clk) then
      if lval_rd = '0' then
        cnt_lane := (others=>'0');
        sel_mem <= (others=>'0');
        sel_rd  <= (others=>'0');
      elsif (dval_rd = '1') then
        cnt_lane := cnt_lane + 1;
        if (cnt_lane = 0) then
          sel_mem <= sel_mem - 1;
          sel_rd  <= sel_rd + 1;
        end if;
      end if;
    end if;
  end process;

gen_addr_rd:
for i in 0 to NB_LANES-1 generate
  process(clk)
    variable sel : unsigned(NB_LANES_WIDTH-1 downto 0);
  begin
    if rising_edge(clk) then
      sel := sel_mem + i;
      addr_rd(i) <= addr(to_integer(sel));
    end if;
  end process;
end generate;

  process(clk)
  begin
    if rising_edge(clk) then
      if lval_rd = '0' then
        msb_addr_rd <= not(msb_addr_wr);
      end if;
    end if;
  end process;

gen_mem_addr_rd:
for i in 0 to NB_LANES-1 generate
  mem_addr_rd(i) <= msb_addr_rd & addr_rd(i);
end generate;

gen_out:
for i in 0 to NB_LANES-1 generate
  process(clk)
    variable sel : unsigned(NB_LANES_WIDTH-1 downto 0);
  begin
    if rising_edge(clk) then
      sel := sel_rd + i;
      dout((i+1)*DATA_WIDTH-1 downto i*DATA_WIDTH) <= rd_data(to_integer(sel));
    end if;
  end process;
end generate;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- manage delay with input sync, wait 1 line before starting to read

-- writing pointer
  process(clk)
  begin
    if rising_edge(clk) then
      if fval_in = '0' and read_sync='0' then
        addr_wr_sync <= (others=>'0');
      elsif fval_in = '1' then
        addr_wr_sync <= addr_wr_sync + 1;
      end if;
    end if;
  end process;

-- reading pointer
  process(clk)
  begin
    if rising_edge(clk) then
      if read_sync = '0' then
        addr_rd_sync <= (others=>'0');
      else
        addr_rd_sync <= addr_rd_sync + 1;
      end if;
    end if;
  end process;

wea_sync <= (others=>fval_in);

din_sync <= lval_in & dval_in;

-- start read after the 1rst line of frame and stop when pointers are equal
  process(clk)
  begin
    if rising_edge(clk) then
      if lval_in = '0' and fval_in = '1' then
        read_sync <= '1';
      elsif (addr_wr_sync=addr_rd_sync) then
        read_sync <= '0';
      end if;
      --
      read_sync1 <= read_sync;
      read_sync2 <= read_sync1;
    end if;
  end process;

-- mask ouput of RAM when they are not valid
fval_rd <= read_sync2 and read_sync1;
lval_rd <= dout_sync(1) and read_sync2 and read_sync1;
dval_rd <= dout_sync(0) and read_sync2 and read_sync1;

ram_sync0 : ram_sdp_reg
  generic map (
    DATA_WIDTH => 2,
    ADDR_WIDTH => ADDR_WIDTH_SYNC
  )
  port map (
    clka   => clk,
    ena    => '1',
    wea    => wea_sync,
    addra  => std_logic_vector(addr_wr_sync),
    dina   => din_sync,
    clkb   => clk,
    enb    => '1',
    addrb  => std_logic_vector(addr_rd_sync),
    doutb  => dout_sync
  );

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- output (delay due to RAM latency and address calculation)

  process(clk)
  begin
    if rising_edge(clk) then
      fval_delay <= fval_delay(fval_delay'high-1 downto 0) & fval_rd;
      dval_delay <= dval_delay(dval_delay'high-1 downto 0) & dval_rd;
      lval_delay <= lval_delay(lval_delay'high-1 downto 0) & lval_rd;
    end if;
  end process;

fval_out <= fval_delay(fval_delay'high);
lval_out <= lval_delay(lval_delay'high);
dval_out <= dval_delay(dval_delay'high);

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wea <= (others=>dval1);

gen_all_mem:
for i in 0 to NB_LANES-1 generate
  ram_data0 : ram_sdp_reg
    generic map (
      DATA_WIDTH => DATA_WIDTH,
      ADDR_WIDTH => ADDR_WIDTH+1
    )
    port map (
      clka   => clk,
      ena    => '1',
      wea    => wea,
      addra  => std_logic_vector(mem_addr_wr),
      dina   => wr_data(i),
      clkb   => clk,
      enb    => '1',
      addrb  => std_logic_vector(mem_addr_rd(i)),
      doutb  => rd_data(i)
    );
end generate;

end rtl;
