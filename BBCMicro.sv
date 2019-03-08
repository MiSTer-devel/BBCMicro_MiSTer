//============================================================================
//  BBCMicro port to MiSTer
//  Copyright (C) 2018 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [44:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	output  [7:0] VIDEO_ARX,
	output  [7:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output  [1:0] VGA_SL,

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
	input         TAPE_IN,

	// SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..5 - USR1..USR4
	// Set USER_OUT to 1 to read from USER_IN.
	input   [5:0] USER_IN,
	output  [5:0] USER_OUT,

	input         OSD_STATUS
);

assign USER_OUT = '1;
assign {UART_RTS, UART_TXD, UART_DTR} = 0;
assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = 0;
 
assign LED_USER  = ioctl_download | (vsd_sel & sd_act);
assign LED_DISK  = {1'b1,~vsd_sel & sd_act};
assign LED_POWER = 0;

assign VIDEO_ARX = status[1] ? 8'd16 : 8'd4;
assign VIDEO_ARY = status[1] ? 8'd9  : 8'd3; 

wire [1:0] scale = status[3:2];

`include "build_id.v" 
parameter CONF_STR = {
	"BBCMicro;;",
	"-;",
	"S,VHD;",
	"OC,Autostart,Yes,No;",
	"-;",
	"O1,Aspect ratio,4:3,16:9;",
	"O23,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%;",
	"OA,Mouse as Joystick,Yes,No;",
	"OB,Swap Joysticks,No,Yes;",
	"-;",
	"O4,Model,B(MOS6502),Master(R65SC12);",
	"O56,Co-Processor,None,MOS65C02;",
	"O79,Default video mode,0,1,2,3,4,5,6,7;",
	"-;",
	"R0,Reset;",
	"JA,Fire;",
	"V,v",`BUILD_DATE
};

/////////////////  CLOCKS  ////////////////////////

wire clk_sys;
wire clk_24 = clk_sys & ce_24;
wire clk_32 = clk_sys & ce_32;

pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_sys)
);

reg ce_32, ce_24;
always @(negedge clk_sys) begin
	reg [1:0] div24, div32;
	
	div24 <= div24 + 1'd1;
	ce_24 <= !div24;
	
	div32 <= div32 + 1'd1;
	if(div32 == 2) div32 <= 0;
	ce_32 <= !div32;
end


/////////////////  HPS  ///////////////////////////

wire [31:0] status;
wire  [1:0] buttons;

wire [15:0] joy1, joy2;
wire  [7:0] joy1_x,joy1_y,joy2_x,joy2_y;

wire [10:0] ps2_key;
wire [24:0] ps2_mouse;

wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        forced_scandoubler;

wire [31:0] sd_lba;
wire        sd_rd;
wire        sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire        img_mounted;
wire        img_readonly;
wire [63:0] img_size;
wire        sd_ack_conf;

wire [64:0] RTC;

hps_io #(.STRLEN($size(CONF_STR)>>3)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.conf_str(CONF_STR),

	.buttons(buttons),
	.status(status),
	.forced_scandoubler(forced_scandoubler),

	.RTC(RTC),

	.ps2_key(ps2_key),
	.ps2_mouse(ps2_mouse),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),

	.sd_lba(sd_lba),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_ack_conf(sd_ack_conf),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sd_buff_din),
	.sd_buff_wr(sd_buff_wr),
	.img_mounted(img_mounted),
	.img_readonly(img_readonly),
	.img_size(img_size),

	.joystick_0(joy1),
	.joystick_1(joy2),
	.joystick_analog_0({joy1_y,joy1_x}),
	.joystick_analog_1({joy2_y,joy2_x})
);

/////////////////  RESET  /////////////////////////

wire reset = RESET | status[0] | buttons[1] | (~status[12] & img_mounted);

////////////////  MEMORY  /////////////////////////

reg m128 = 0;
always @(posedge clk_sys) if(reset_req) m128 <= status[4];

wire        mem_we_n;
wire [18:0] mem_addr;
wire  [7:0] mem_din;

reg  [17:0] rom_addr;
wire  [7:0] rom_dout;
reg   [7:0] rom_data;
spram #(8, 18, 229376, "roms/rom.mif") rom
(
	.clock(clk_sys),
	.address(reset ? ioctl_addr[17:0] : rom_addr),
	.data(ioctl_dout),
	.wren(!ioctl_index && ioctl_wr && reset),
	.q(rom_dout)
);

// Beeb ROM Images

// 00 00xx empty     
// 00 01xx empty     
// 00 10xx empty     
// 00 11xx empty     
// 01 00xx bbcb/os12.rom         
// 01 01xx empty     
// 01 10xx empty     
// 01 11xx empty     
// 10 00xx bbcb/swmmfs.rom       
// 10 01xx empty     
// 10 10xx empty     
// 10 11xx empty     
// 11 00xx empty     
// 11 01xx empty     
// 11 10xx bbcb/ram_master_v6.rom
// 11 11xx bbcb/basic2.rom       

// Master ROM Images

// 00 00xx empty     
// 00 01xx empty     
// 00 10xx m128/adfs1-57.rom     
// 00 11xx m128/mammfs.rom       
// 01 00xx m128/mos.rom          
// 01 01xx empty     
// 01 10xx empty     
// 01 11xx empty     
// 10 00xx empty     
// 10 01xx m128/dfs.rom          
// 10 10xx m128/viewsht.rom      
// 10 11xx m128/edit.rom         
// 11 00xx m128/basic4.rom       
// 11 01xx m128/adfs.rom         
// 11 10xx m128/view.rom         
// 11 11xx m128/terminal.rom      

always_comb begin
	rom_addr[13:0] = mem_addr[13:0];
	case({m128, mem_addr[17:14]})
		'b0_01_00: rom_addr[17:14] =  0; //bbcb/os12.rom         
		'b0_10_00: rom_addr[17:14] =  1; //bbcb/swmmfs.rom       
		'b0_11_10: rom_addr[17:14] =  2; //bbcb/ram_master_v6.rom
		'b0_11_11: rom_addr[17:14] =  3; //bbcb/basic2.rom       
		'b1_00_10: rom_addr[17:14] =  4; //m128/adfs1-57.rom     
		'b1_00_11: rom_addr[17:14] =  5; //m128/mammfs.rom       
		'b1_01_00: rom_addr[17:14] =  6; //m128/mos.rom          
		'b1_10_01: rom_addr[17:14] =  7; //m128/dfs.rom          
		'b1_10_10: rom_addr[17:14] =  8; //m128/viewsht.rom      
		'b1_10_11: rom_addr[17:14] =  9; //m128/edit.rom         
		'b1_11_00: rom_addr[17:14] = 10; //m128/basic4.rom       
		'b1_11_01: rom_addr[17:14] = 11; //m128/adfs.rom         
		'b1_11_10: rom_addr[17:14] = 12; //m128/view.rom         
		'b1_11_11: rom_addr[17:14] = 13; //m128/terminal.rom      
		  default: rom_addr[17:14] =  0;
	endcase
end

always_comb begin
	case({m128, mem_addr[17:14]})
		'b0_01_00,
		'b0_10_00,
		'b0_11_10,
		'b0_11_11,
		'b1_00_10,
		'b1_00_11,
		'b1_01_00,
		'b1_10_01,
		'b1_10_10,
		'b1_10_11,
		'b1_11_00,
		'b1_11_01,
		'b1_11_10,
		'b1_11_11: rom_data = rom_dout;
		  default: rom_data = 0;
	endcase
end

wire  [7:0] ram_dout;
spram #(8, 18, 212992) ram
(
	.clock(clk_sys),
	.address(mem_addr[17:0]),
	.data(mem_din),
	.wren(mem_addr[18] & old_we & ~mem_we_n),
	.q(ram_dout)
);

reg old_we;
always @(posedge clk_sys) old_we <= mem_we_n;

// 00 00xx  Co Processor
// 00 01xx  Co Processor
// 00 10xx  Co Processor
// 00 11xx  Co Processor
// 01 00xx  RAM Slot 4
// 01 01xx  RAM Slot 5
// 01 10xx  RAM Slot 6
// 01 11xx  RAM Slot 7
// 10 00xx  Main memory
// 10 01xx  Main memory
// 10 1000  Filing System RAM (4K, at C000-CFFF) (unused in Beeb Mode)
// 10 1001  Filing System RAM (4K, at D000-DFFF) (unused in Beeb Mode)
// 10 1010  Private RAM (4K, at 8000-8FFF)       (unused in Beeb Mode)
// 10 1011  Shadow memory (4K, at 3000-3FFF)     (unused in Beeb Mode)
// 10 11xx  Shadow memory (16K, at 4000-7FFF)    (unused in Beeb Mode)
// 11 00xx  RAM Slot 8 (B600-BFFF)
// 11 01xx  unused
// 11 10xx  unused
// 11 11xx  unused


///////////////////////////////////////////////////

wire reset_req;

wire [7:0] joya_x = 8'hFF - {~ax[7],ax[6:0]};
wire [7:0] joya_y = 8'hFF - {~ay[7],ay[6:0]};
wire [7:0] joyb_x = 8'hFF - {~joy2_x[7],joy2_x[6:0]};
wire [7:0] joyb_y = 8'hFF - {~joy2_y[7],joy2_y[6:0]};

bbc_micro_core BBCMicro
(
	.clock_32(clk_32),
	.clock_24(clk_24),

	.hard_reset_n(~reset),
	.reset_req(reset_req),

	.ps2_key(ps2_key),
	.ps2_mouse(status[10] ? ps2_mouse : 25'd0),

	.video_sel(clk_sel),
	.video_cepix(ce_pix),
	.video_red(r),
	.video_green(g),
	.video_blue(b),
	.video_vblank(vblank),
	.video_hblank(hblank),
	.video_vsync(vs),
	.video_hsync(hs),

	.audio_sn(audio_sn),

	.ext_nOE(),
	.ext_nWE(mem_we_n),
	.ext_A(mem_addr),
	.ext_Dout(mem_addr[18] ? ram_dout : rom_data),
	.ext_Din(mem_din),

	.SDMISO(sdmiso),
	.SDCLK(sdclk),
	.SDMOSI(sdmosi),
	.SDSS(sdss),

	.caps_led(),
	.shift_led(),
	
	.RTC(RTC),

	.keyb_dip({4'd0, ~status[12], ~status[9:7]}),

	.joystick1_x(    status[11] ? {joyb_x,joyb_x[7:4]} : {joya_x,joya_x[7:4]}),
	.joystick1_y(    status[11] ? {joyb_y,joyb_y[7:4]} : {joya_y,joya_y[7:4]}),
	.joystick1_fire( status[11] ? ~joy2[4] : ~af),

	.joystick2_x(   ~status[11] ? {joya_x,joya_x[7:4]} : {joyb_x,joyb_x[7:4]}),
	.joystick2_y(   ~status[11] ? {joya_y,joya_y[7:4]} : {joyb_y,joyb_y[7:4]}),
	.joystick2_fire(~status[11] ? ~joy2[4] : ~af),

	.m128_mode(m128),
	.copro_mode(|status[6:5])
);

wire [7:0] audio_sn;

assign AUDIO_L = {audio_sn, 8'd0};
assign AUDIO_R = {audio_sn, 8'd0};
assign AUDIO_MIX = 0;
assign AUDIO_S = 1;

wire hs, vs, hblank, vblank, ce_pix, clk_sel;
wire r,g,b;

assign CLK_VIDEO = clk_sys;
video_mixer #(640, 1) mixer
(
	.clk_sys(CLK_VIDEO),
	
	.ce_pix(ce_pix & (clk_sel ? ce_32 : ce_24)),
	.ce_pix_out(CE_PIXEL),

	.hq2x(scale == 1),
	.scanlines(0),
	.scandoubler(scale || forced_scandoubler),

	.R({4{r}}),
	.G({4{g}}),
	.B({4{b}}),

	.mono(0),

	.HSync(hs),
	.VSync(vs),
	.HBlank(hblank),
	.VBlank(vblank),

	.VGA_R(VGA_R),
	.VGA_G(VGA_G),
	.VGA_B(VGA_B),
	.VGA_VS(VGA_VS),
	.VGA_HS(VGA_HS),
	.VGA_DE(VGA_DE)
);

assign VGA_F1 = 0;
assign VGA_SL = scale ? scale - 1'd1 : 2'd0;

//////////////////   SD   ///////////////////

wire sdclk;
wire sdmosi;
wire sdmiso = vsd_sel ? vsdmiso : SD_MISO;
wire sdss;

reg vsd_sel = 0;
always @(posedge clk_sys) if(img_mounted) vsd_sel <= |img_size;

wire vsdmiso;
sd_card sd_card
(
	.*,

	.clk_spi(clk_sys),
	.sdhc(1),
	.sck(sdclk),
	.ss(sdss | ~vsd_sel),
	.mosi(sdmosi),
	.miso(vsdmiso)
);

assign SD_CS   = sdss   |  vsd_sel;
assign SD_SCK  = sdclk  & ~vsd_sel;
assign SD_MOSI = sdmosi & ~vsd_sel;

reg sd_act;

always @(posedge clk_sys) begin
	reg old_mosi, old_miso;
	integer timeout = 0;

	old_mosi <= sdmosi;
	old_miso <= sdmiso;

	sd_act <= 0;
	if(timeout < 2000000) begin
		timeout <= timeout + 1;
		sd_act <= 1;
	end

	if((old_mosi ^ sdmosi) || (old_miso ^ sdmiso)) timeout <= 0;
end


//////////////////   ANALOG AXIS   ///////////////////
reg        emu = 0;
wire [7:0] ax = emu ? mx[7:0] : joy1_x;
wire [7:0] ay = emu ? my[7:0] : joy1_y;
wire [7:0] af = emu ? |ps2_mouse[1:0] : joy1[4];

reg  signed [8:0] mx = 0;
wire signed [8:0] mdx = {ps2_mouse[4],ps2_mouse[4],ps2_mouse[15:9]};
wire signed [8:0] mdx2 = (mdx > 10) ? 9'd10 : (mdx < -10) ? -8'd10 : mdx;
wire signed [8:0] nmx = mx + mdx2;

reg  signed [8:0] my = 0;
wire signed [8:0] mdy = {ps2_mouse[5],ps2_mouse[5],ps2_mouse[23:17]};
wire signed [8:0] mdy2 = (mdy > 10) ? 9'd10 : (mdy < -10) ? -9'd10 : mdy;
wire signed [8:0] nmy = my - mdy2;

always @(posedge clk_sys) begin
	reg old_stb = 0;
	
	old_stb <= ps2_mouse[24];
	if(old_stb != ps2_mouse[24]) begin
		emu <= 1;
		mx <= (nmx < -128) ? -9'd128 : (nmx > 127) ? 9'd127 : nmx;
		my <= (nmy < -128) ? -9'd128 : (nmy > 127) ? 9'd127 : nmy;
	end

	if(joy1 || reset_req || status[10]) begin
		emu <= 0;
		mx <= 0;
		my <= 0;
	end
end

endmodule

//////////////////////////////////////////////

module spram #(parameter DATAWIDTH=8, ADDRWIDTH=8, NUMWORDS=1<<ADDRWIDTH, MEM_INIT_FILE="")
(
	input	                 clock,
	input	 [ADDRWIDTH-1:0] address,
	input	 [DATAWIDTH-1:0] data,
	input	                 wren,
	output [DATAWIDTH-1:0] q
);

altsyncram altsyncram_component
(
	.address_a (address),
	.clock0 (clock),
	.data_a (data),
	.wren_a (wren),
	.q_a (q),
	.aclr0 (1'b0),
	.aclr1 (1'b0),
	.address_b (1'b1),
	.addressstall_a (1'b0),
	.addressstall_b (1'b0),
	.byteena_a (1'b1),
	.byteena_b (1'b1),
	.clock1 (1'b1),
	.clocken0 (1'b1),
	.clocken1 (1'b1),
	.clocken2 (1'b1),
	.clocken3 (1'b1),
	.data_b (1'b1),
	.eccstatus (),
	.q_b (),
	.rden_a (1'b1),
	.rden_b (1'b1),
	.wren_b (1'b0)
);

defparam
	altsyncram_component.clock_enable_input_a = "BYPASS",
	altsyncram_component.clock_enable_output_a = "BYPASS",
	altsyncram_component.init_file = MEM_INIT_FILE,
	altsyncram_component.intended_device_family = "Cyclone V",
	altsyncram_component.lpm_type = "altsyncram",
	altsyncram_component.numwords_a = NUMWORDS,
	altsyncram_component.operation_mode = "SINGLE_PORT",
	altsyncram_component.outdata_aclr_a = "NONE",
	altsyncram_component.outdata_reg_a = "UNREGISTERED",
	altsyncram_component.power_up_uninitialized = "FALSE",
	altsyncram_component.read_during_write_mode_port_a = "NEW_DATA_NO_NBE_READ",
	altsyncram_component.widthad_a = ADDRWIDTH,
	altsyncram_component.width_a = DATAWIDTH,
	altsyncram_component.width_byteena_a = 1;


endmodule
