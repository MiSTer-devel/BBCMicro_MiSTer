/*
 * Copyright (c) 2014, Aleksander Osman
 * Copyright (c) 2018, Sorgelig
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

module rtc_chip
(
	input            clk,
	input            rst_n,

	output reg       irq,

	input      [5:0] address,

	input            rd,
	output reg [7:0] rddata,

	input            wr,
	input      [7:0] wrdata,
	
	input     [64:0] RTC
);

parameter [26:0] cycles_in_second = 32000000;

//------------------------------------------------------------------------------ io rd

always @(*) begin
	case(address)
		 0: rddata = rtc_second;
		 1: rddata = alarm_second;
		 2: rddata = rtc_minute;
		 3: rddata = alarm_second;
		 4: rddata = rtc_hour;
		 5: rddata = alarm_hour;
		 6: rddata = rtc_dayofweek;
		 7: rddata = rtc_dayofmonth;
		 8: rddata = rtc_month;
		 9: rddata = rtc_year;
		10: rddata = { sec_state == SEC_UPDATE_IN_PROGRESS || sec_state == SEC_SECOND_START, divider, periodic_rate };
		11: rddata = { crb_freeze, crb_int_periodic_ena, crb_int_alarm_ena, crb_int_update_ena,
                            1'b0, crb_binarymode, crb_24hour, crb_daylightsaving };
		12: rddata = { irq, periodic_interrupt, alarm_interrupt, update_interrupt, 4'd0 };
		default: rddata = 0;
	endcase
end


//------------------------------------------------------------------------------ irq

wire interrupt_start = irq == 1'b0 && (
    (crb_int_periodic_ena && periodic_interrupt) ||
    (crb_int_alarm_ena    && alarm_interrupt) ||
    (crb_int_update_ena   && update_interrupt) );

always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   irq <= 0;
    else if(rd && address == 12) irq <= 0;
    else if(interrupt_start)     irq <= 1;
end

//------------------------------------------------------------------------------ once per second state machine

localparam [2:0] SEC_UPDATE_START       = 3'd0;
localparam [2:0] SEC_UPDATE_IN_PROGRESS = 3'd1;
localparam [2:0] SEC_SECOND_START       = 3'd2;
localparam [2:0] SEC_SECOND_IN_PROGRESS = 3'd3;
localparam [2:0] SEC_STOPPED            = 3'd4;

localparam [13:0] cycles_in_122us       = cycles_in_second[26:13];

reg [2:0] sec_state;

always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                                                                      sec_state <= SEC_UPDATE_START;

    else if(crb_freeze || divider[2:1] == 2'b11)                                                    sec_state <= SEC_STOPPED;
    else if(sec_state == SEC_STOPPED)                                                               sec_state <= SEC_UPDATE_START;

    else if(sec_state == SEC_UPDATE_START)                                                          sec_state <= SEC_UPDATE_IN_PROGRESS;
    else if(sec_state == SEC_UPDATE_IN_PROGRESS && sec_timeout == 0)                                sec_state <= SEC_SECOND_START;
    else if(sec_state == SEC_SECOND_START)                                                          sec_state <= SEC_SECOND_IN_PROGRESS;
    else if(sec_state == SEC_SECOND_IN_PROGRESS && sec_timeout == { 13'd0, cycles_in_122us, 1'b0 }) sec_state <= SEC_UPDATE_START;
end

reg [27:0] sec_timeout;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                      sec_timeout <= {cycles_in_122us,1'b0}-1'd1;
    else if(crb_freeze || divider[2:1] == 2'b11)    sec_timeout <= {cycles_in_122us,1'b0}-1'd1;
    else if(sec_timeout == 0)                       sec_timeout <= cycles_in_second;
    else                                            sec_timeout <= sec_timeout - 1'd1;
end

reg update_interrupt;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                              update_interrupt <= 0;
    else if(rd && address == 12)            update_interrupt <= 0;
    else if(sec_state == SEC_SECOND_START)  update_interrupt <= 1;
end

//------------------------------------------------------------------------------

wire max_second = 
    (crb_binarymode && rtc_second >= 8'd59) ||
    (~(crb_binarymode) && (rtc_second[7:4] >= 4'd6 || (rtc_second[7:4] == 4'd5 && rtc_second[3:0] >= 4'd9)));

wire [7:0] next_second =
    (max_second)?                                       8'd0 :
    (~(crb_binarymode) && rtc_second[3:0] >= 4'd9)?     { rtc_second[7:4] + 4'd1, 4'd0 } :
                                                        rtc_second + 8'd1;

wire max_minute = 
    (crb_binarymode && rtc_minute >= 8'd59) ||
    (~(crb_binarymode) && (rtc_minute[7:4] >= 4'd6 || (rtc_minute[7:4] == 4'd5 && rtc_minute[3:0] >= 4'd9)));

wire [7:0] next_minute =
    (max_minute)?                                       8'd0 :
    (~(crb_binarymode) && rtc_minute[3:0] >= 4'd9)?     { rtc_minute[7:4] + 4'd1, 4'd0 } :
                                                        rtc_minute + 8'd1;

wire dst_april   = crb_daylightsaving && rtc_dayofweek == 8'd1 && rtc_month == 8'd4 &&
    ((crb_binarymode && rtc_dayofmonth >= 8'd24) || (~(crb_binarymode) && rtc_dayofmonth[7:4] >= 4'd2 && rtc_dayofmonth[3:0] >= 4'd4)) &&
    rtc_hour == 8'd1;

wire dst_october = crb_daylightsaving && rtc_dayofweek == 8'd1 &&
    ((crb_binarymode && rtc_month == 8'd10) || (~(crb_binarymode) && rtc_month[7:4] == 4'd1 && rtc_month[3:0] == 4'd0)) &&
    ((crb_binarymode && rtc_dayofmonth >= 8'd25) || (~(crb_binarymode) && rtc_dayofmonth[7:4] >= 4'd2 && rtc_dayofmonth[3:0] >= 4'd5)) &&
    rtc_hour == 8'd1;
    
wire max_hour =
    (~(crb_24hour) && crb_binarymode    && rtc_hour[7] && rtc_hour[6:0] >= 7'd12) ||
    (crb_24hour    && crb_binarymode    && rtc_hour >= 8'd23) ||
    (~(crb_24hour) && ~(crb_binarymode) && rtc_hour[7] && (rtc_hour[6:4] >= 3'd2 || (rtc_hour[6:4] == 3'd1 && rtc_hour[3:0] >= 4'd2))) ||
    (crb_24hour    && ~(crb_binarymode) && (rtc_hour[7:4] >= 4'd3 || (rtc_hour[7:4] == 4'd2 && rtc_hour[3:0] >= 4'd3)));

wire [7:0] next_hour =
    (dst_april)?                                                                                8'd3 :
    (dst_october)?                                                                              8'd1 :
    (~(crb_24hour) && max_hour)?                                                                8'd1 :
    (crb_24hour    && max_hour)?                                                                8'd0 :
    (~(crb_24hour) && crb_binarymode    && rtc_hour[6:0] >= 7'd12)?                             8'h81 :
    (~(crb_24hour) && ~(crb_binarymode) && rtc_hour[6:4] == 3'd1 && rtc_hour[3:0] >= 4'd2)?     8'h81 :
    (~(crb_24hour) && ~(crb_binarymode) && rtc_hour[6:4] == 3'd0 && rtc_hour[3:0] >= 4'd9)?     { rtc_hour[7], 3'b1, 4'd0 }  :
    (crb_24hour    && ~(crb_binarymode) && rtc_hour[3:0] >= 4'd9)?                              { rtc_hour[7:4] + 4'd1, 4'd0 } :
                                                                                                rtc_hour + 8'd1;

wire max_dayofweek = rtc_dayofweek >= 8'd7;

wire [7:0] next_dayofweek =
    (max_dayofweek)?    8'd1 :
                        rtc_dayofweek + 8'd1;

//simplified leap year condition
wire leap_year =
    (crb_binarymode    && rtc_year[1:0] == 2'b00) ||
    (~(crb_binarymode) && ((rtc_year[1:0] == 2'b00 && rtc_year[4] == 1'b0) || (rtc_year[1:0] == 2'b10 && rtc_year[4] == 1'b1)));

wire max_dayofmonth = 
    (crb_binarymode && (
        (rtc_month <= 8'd1  && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd2  && ((~(leap_year) && rtc_dayofmonth >= 8'd28) || (leap_year && rtc_dayofmonth >= 8'd29))) ||
        (rtc_month == 8'd3  && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd4  && rtc_dayofmonth >= 8'd30) ||
        (rtc_month == 8'd5  && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd6  && rtc_dayofmonth >= 8'd30) ||
        (rtc_month == 8'd7  && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd8  && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd9  && rtc_dayofmonth >= 8'd30) ||
        (rtc_month == 8'd10 && rtc_dayofmonth >= 8'd31) ||
        (rtc_month == 8'd11 && rtc_dayofmonth >= 8'd30) ||
        (rtc_month >= 8'd12 && rtc_dayofmonth >= 8'd31))
    ) ||
    (~(crb_binarymode) && (
        (rtc_month <= 8'h01 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h02 && ((~(leap_year) && (rtc_dayofmonth[7:4] >= 4'd3 || (rtc_dayofmonth[7:4] == 4'd2 && rtc_dayofmonth[3:0] >= 4'd8))) ||
                               (leap_year    && (rtc_dayofmonth[7:4] >= 4'd3 || (rtc_dayofmonth[7:4] == 4'd2 && rtc_dayofmonth[3:0] >= 4'd9))))) ||
        (rtc_month == 8'h03 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h04 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3))) ||
        (rtc_month == 8'h05 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h06 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3))) ||
        (rtc_month == 8'h07 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h08 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h09 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3))) ||
        (rtc_month == 8'h10 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))) ||
        (rtc_month == 8'h11 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3))) ||
        (rtc_month >= 8'h12 && (rtc_dayofmonth[7:4] >= 4'd4 || (rtc_dayofmonth[7:4] == 4'd3 && rtc_dayofmonth[3:0] >= 4'd1))))
    );

wire [7:0] next_dayofmonth =
    (max_dayofmonth)?                                       8'd1 :
    (~(crb_binarymode) && rtc_dayofmonth[3:0] >= 4'd9)?     { rtc_dayofmonth[7:4] + 4'd1, 4'd0 } :
                                                            rtc_dayofmonth + 8'd1;

wire max_month =
    (crb_binarymode && rtc_month >= 8'd12) || (~(crb_binarymode) && (rtc_month[7:4] >= 4'd2 || (rtc_month[7:4] == 4'd1 && rtc_month[3:0] >= 4'd2)));

wire [7:0] next_month =
    (max_month)?                                    8'd1 :
    (~(crb_binarymode) && rtc_month[3:0] >= 4'd9)?  { rtc_month[7:4] + 4'd1, 4'd0 } :
                                                    rtc_month + 8'd1;
    
wire max_year =
    (crb_binarymode && rtc_year >= 8'd99) || (~(crb_binarymode) && (rtc_year[7:4] >= 4'd10 || (rtc_year[7:4] == 4'd9 && rtc_year[3:0] >= 4'd9)));

wire [7:0] next_year =
    (max_year)?                                     8'd0 :
    (~(crb_binarymode) && rtc_year[3:0] >= 4'd9)?   { rtc_year[7:4] + 4'd1, 4'd0 } :
                                                    rtc_year + 8'd1;

//------------------------------------------------------------------------------

wire rtc_second_update = sec_state == SEC_SECOND_START;
wire rtc_minute_update = rtc_second_update && max_second;
wire rtc_hour_update   = rtc_minute_update && max_minute;
wire rtc_day_update    = rtc_hour_update   && max_hour;
wire rtc_month_update  = rtc_day_update    && max_dayofmonth;
wire rtc_year_update   = rtc_month_update  && max_month;

//------------------------------------------------------------------------------

reg rtc_set;
always @(posedge clk) begin
	reg old_stb = 0;
	
	old_stb <= RTC[64];
	rtc_set <= (old_stb != RTC[64]);
end

reg [7:0] rtc_second;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_second <= 0;
    else if(wr && address == 0)  rtc_second <= wrdata;
    else if(rtc_second_update)   rtc_second <= next_second;
	 else if(rtc_set)             rtc_second <= RTC[7:0];
end

reg [7:0] rtc_minute;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_minute <= 0;
    else if(wr && address == 2)  rtc_minute <= wrdata;
    else if(rtc_minute_update)   rtc_minute <= next_minute;
	 else if(rtc_set)             rtc_minute <= RTC[15:8];
end

reg [7:0] rtc_hour;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_hour <= 0;
    else if(wr && address == 4)  rtc_hour <= wrdata;
    else if(rtc_hour_update)     rtc_hour <= next_hour;
	 else if(rtc_set)             rtc_hour <= RTC[23:16];
end

reg [7:0] rtc_dayofweek;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_dayofweek <= 0;
    else if(wr && address == 6)  rtc_dayofweek <= wrdata;
    else if(rtc_day_update)      rtc_dayofweek <= next_dayofweek;
	 else if(rtc_set)             rtc_dayofweek <= RTC[55:48]+1'd1;
end

reg [7:0] rtc_dayofmonth;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_dayofmonth <= 0;
    else if(wr && address == 7)  rtc_dayofmonth <= wrdata;
    else if(rtc_day_update)      rtc_dayofmonth <= next_dayofmonth;
	 else if(rtc_set)             rtc_dayofmonth <= RTC[31:24];
end

reg [7:0] rtc_month;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_month <= 0;
    else if(wr && address == 8)  rtc_month <= wrdata;
    else if(rtc_month_update)    rtc_month <= next_month;
	 else if(rtc_set)             rtc_month <= RTC[39:32];
end

reg [7:0] rtc_year;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   rtc_year <= 0;
    else if(wr && address == 9)  rtc_year <= wrdata;
    else if(rtc_year_update)     rtc_year <= next_year;
	 else if(rtc_set)             rtc_year <= RTC[47:40];
end

//------------------------------------------------------------------------------

reg [7:0] alarm_second;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   alarm_second <= 0;
    else if(wr && address == 1)  alarm_second <= wrdata;
end

reg [7:0] alarm_minute;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   alarm_minute <= 0;
    else if(wr && address == 3)  alarm_minute <= wrdata;
end

reg [7:0] alarm_hour;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   alarm_hour <= 0;
    else if(wr && address == 5)  alarm_hour <= wrdata;
end

wire alarm_interrupt_activate =
    (alarm_second[7:6] == 2'b11 || (rtc_second_update && next_second == alarm_second)) &&
    (alarm_minute[7:6] == 2'b11 || (rtc_minute_update && next_minute == alarm_minute) || (~(rtc_minute_update) && rtc_minute == alarm_minute)) &&
    (alarm_hour[7:6] == 2'b11   || (rtc_hour_update && next_hour == alarm_hour)       || (~(rtc_hour_update)   && rtc_hour == alarm_hour));

reg alarm_interrupt;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                                         alarm_interrupt <= 0;
    else if(rd && address == 12)                                       alarm_interrupt <= 0;
    else if(sec_state == SEC_SECOND_START && alarm_interrupt_activate) alarm_interrupt <= 1;
end

//------------------------------------------------------------------------------

/*
crb_freeze 1: no update, no alarm
*/

reg crb_freeze;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_freeze <= 0;
    else if(wr && address == 11) crb_freeze <= wrdata[7];
end

reg crb_int_periodic_ena;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_int_periodic_ena <= 0;
    else if(wr && address == 11) crb_int_periodic_ena <= wrdata[6];
end

reg crb_int_alarm_ena;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_int_alarm_ena <= 0;
    else if(wr && address == 11) crb_int_alarm_ena <= wrdata[5];
end

reg crb_int_update_ena;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_int_update_ena <= 0;
    else if(wr && address == 11) crb_int_update_ena <= ~(wrdata[7]) & wrdata[4];
end

reg crb_binarymode;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_binarymode <= 0;
    else if(wr && address == 11) crb_binarymode <= wrdata[2];
end

reg crb_24hour;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_24hour <= 1;
    else if(wr && address == 11) crb_24hour <= wrdata[1];
end

reg crb_daylightsaving = 0;
/*
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   crb_daylightsaving <= 0;
    else if(wr && address == 11) crb_daylightsaving <= wrdata[0]; 
end
*/

//------------------------------------------------------------------------------

/*
divider 00x : no periodic
divider 11x : no update, no alarm
*/

reg [2:0] divider;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   divider <= 2;
    else if(wr && address == 10) divider <= wrdata[6:4];
end

reg [3:0] periodic_rate;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                   periodic_rate <= 6;
    else if(wr && address == 10) periodic_rate <= wrdata[3:0];
end

wire periodic_enabled = divider[2:1] != 2'b00 && periodic_rate != 4'd0;
wire periodic_start   = periodic_enabled && (
                            (periodic_minor == 0 && periodic_major == 13'd0) ||
                            (periodic_minor == 0 && periodic_major == 13'd1));
wire periodic_count   = periodic_enabled && periodic_major >= 13'd1;

wire [12:0] periodic_major_initial = {
    periodic_rate == 4'd15, periodic_rate == 4'd14, periodic_rate == 4'd13,  periodic_rate == 4'd12,
    periodic_rate == 4'd11, periodic_rate == 4'd10, periodic_rate == 4'd9 || periodic_rate == 4'd2,  periodic_rate == 4'd8 || periodic_rate == 4'd1,  
    periodic_rate == 4'd7,  periodic_rate == 4'd6,  periodic_rate == 4'd5,   periodic_rate == 4'd4,
    periodic_rate == 4'd3 };

reg [13:0] periodic_minor;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                      periodic_minor <= 0;
    else if(~(periodic_enabled))                    periodic_minor <= 0;
    else if(periodic_start)                         periodic_minor <= cycles_in_122us;
    else if(periodic_count && periodic_minor == 0)  periodic_minor <= cycles_in_122us;
    else if(periodic_count)                         periodic_minor <= periodic_minor - 1'd1;
end

reg [12:0] periodic_major;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                      periodic_major <= 0;
    else if(~(periodic_enabled))                    periodic_major <= 0;
    else if(periodic_start)                         periodic_major <= periodic_major_initial;
    else if(periodic_count && periodic_minor == 0)  periodic_major <= periodic_major - 1'd1;
end

reg periodic_interrupt;
always @(posedge clk or negedge rst_n) begin
    if(!rst_n)                                                              periodic_interrupt <= 0;
    else if(rd && address == 12)                                            periodic_interrupt <= 0;
    else if(periodic_enabled && periodic_minor == 0 && periodic_major == 1) periodic_interrupt <= 1;
end

endmodule
