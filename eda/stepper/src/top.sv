/**
* @file top.sv
* @brief Top module for PicoRV32 matrix LED example
*/
// Copyright 2023 Kenta IDA
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)
`default_nettype none

module top (
  input  wire  clock,
  output logic com_ser,
  output logic com_rclk,
  output logic com_srclk,
  output logic com_oe,
  output logic seg_ser,
  output logic seg_rclk,
  output logic seg_srclk,
  output logic seg_oe,
  output logic [5:0] led_out,
  input  wire  [7:0] switch_in,
  input  wire  uart_rx,
  output logic uart_tx,

  // Stepper motor
  output logic [3:0] stepper_m1p,
  output logic [3:0] stepper_m2p,
  
  output logic probe_out
);

  logic io_exit;

  // リセット回路 (16サイクル)
  logic reset;
  logic [15:0] reset_reg = '1;
  assign reset = reset_reg[0];
  always_ff @(posedge clock) begin
      reset_reg <= {1'b0, reset_reg[15:1]};
  end

  localparam int CLOCK_HZ = 27_000_000;
  localparam int PULSE_COUNTER_BITS = 32;
  localparam int MAX_PULSE_INTERVAL = CLOCK_HZ / 500;

  typedef logic [PULSE_COUNTER_BITS-1:0] pulse_counter_t;

  pulse_counter_t pulse_counter = 0;
  pulse_counter_t max_pulse_counter = MAX_PULSE_INTERVAL - 1;
  logic [2:0] phase = 0;
  logic [3:0] stepper_out = 0;
  logic direction_ccw = 0;

  assign stepper_m1p = stepper_out;
  assign stepper_m2p = stepper_out;

  logic io_uartTx;
  logic io_uartRx;
  logic io_segmentOut_outputEnable;
  logic io_segmentOut_shiftClock;
  logic io_segmentOut_latch;
  logic io_segmentOut_data;
  logic io_digitSelector_outputEnable;
  logic io_digitSelector_shiftClock;
  logic io_digitSelector_latch;
  logic io_digitSelector_data;
  logic [31:0] io_ledOut;
  logic [31:0] io_switchIn;
  logic [31:0] io_stepperCount;
  logic [0:0]  io_stepperCwCcw;
  logic io_probeOut;
  
  always_comb begin
    com_oe    <= io_digitSelector_outputEnable;
    com_srclk <= io_digitSelector_shiftClock;
    com_rclk  <= io_digitSelector_latch;
    com_ser   <= io_digitSelector_data;
    seg_oe    <= io_segmentOut_outputEnable;
    seg_srclk <= io_segmentOut_shiftClock;
    seg_rclk  <= io_segmentOut_latch;
    seg_ser   <= io_segmentOut_data;

    led_out   <= ~io_ledOut[5:0];
    io_switchIn <= {24'd0, switch_in};

    uart_tx   <= io_uartTx;
    io_uartRx <= uart_rx;

    probe_out <= io_probeOut;
  end

  TopWithStepper top(
    .clock(clock),
    .reset(reset),
    .io_debug_pc(),
    .io_success(),
    .io_exit(),
    .*
  );
  
  always_ff @(posedge clock) begin
    if (reset) begin
      pulse_counter <= 0;
      phase <= 0;
      max_pulse_counter <= MAX_PULSE_INTERVAL - 1;
      direction_ccw <= 0;
    end else begin
      if (pulse_counter >= max_pulse_counter) begin
        pulse_counter <= 0;
        if( direction_ccw ) begin
          phase <= phase - 1;
        end else begin
          phase <= phase + 1;
        end
      end else begin
        pulse_counter <= pulse_counter + 1;
      end
      
      direction_ccw <= io_stepperCwCcw;
      max_pulse_counter <= io_stepperCount;

      case(phase)
        0: stepper_out <= 4'b0001;
        1: stepper_out <= 4'b0011;
        2: stepper_out <= 4'b0010;
        3: stepper_out <= 4'b0110;
        4: stepper_out <= 4'b0100;
        5: stepper_out <= 4'b1100;
        6: stepper_out <= 4'b1000;
        7: stepper_out <= 4'b1001;
        default: stepper_out <= 4'b0000;
      endcase
    end
  end
endmodule
`default_nettype wire