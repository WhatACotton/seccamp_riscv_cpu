package cpu

import chisel3._
import chisel3.util._
import common.Consts._
import uart._
import video.TestPatternGenerator
import video.VideoSignalGenerator
import video.VideoConfig
import video.SimpleTestPatternGenerator
import video.VideoController
import video.DviOut

class TopWithHDMI(memoryPathGen: Int => String = i => f"../sw/bootrom_${i}.hex", suppressDebugMessage: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val debug_pc = Output(UInt(WORD_LEN.W))
    val gpio_out = Output(UInt(32.W))
    val uart_tx = Output(Bool())
    val uart_rx = Input(Bool())
    val success = Output(Bool())
    val exit = Output(Bool())

    val pixel_reset = Input(Bool())
    val pixel_clock = Input(Clock())
    val dvi_clock = Output(UInt(10.W))
    val dvi_data0 = Output(UInt(10.W))
    val dvi_data1 = Output(UInt(10.W))
    val dvi_data2 = Output(UInt(10.W))
  })

  val clockFreqHz = 27000000
  val baseAddress = BigInt("00000000", 16)
  val memSize = 8192
  val core = Module(new Core(startAddress = baseAddress.U(WORD_LEN.W), suppressDebugMessage))
  val decoder = Module(new DMemDecoder(Seq(
    (BigInt(0x00000000L), BigInt(memSize)), // メモリ
    (BigInt(0xA0000000L), BigInt(64)),      // GPIO
    (BigInt(0xB0000000L), BigInt(0x20000)), // VRAM
    (BigInt(0xB0020000L), BigInt(0x1000)),  // Video Controller
  )))
  
  val memory = Module(new Memory(Some(memoryPathGen), baseAddress.U(WORD_LEN.W), memSize))
  val gpio = Module(new Gpio)

  core.io.imem <> memory.io.imem
  core.io.dmem <> decoder.io.initiator  // CPUにデコーダを接続
  decoder.io.targets(0) <> memory.io.dmem // 0番ポートにメモリを接続
  decoder.io.targets(1) <> gpio.io.mem    // 1番ポートにGPIOを接続
  io.gpio_out := gpio.io.out  // GPIOの出力を外部ポートに接続
  //io.gpio_out := core.io.gpio_out  // GPIO CSRの出力を外部ポートに接続

  // val uartTx = Module(new UartTx(8, clockFreqHz / 115200))
  // io.uart_tx := uartTx.io.tx
  io.uart_tx := false.B

  core.io.interrupt_in := false.B
  io.success := core.io.success
  io.exit := core.io.exit
  io.debug_pc := core.io.debug_pc

  // 640x480@60Hz CEA-861 timing
  // https://tomverbeure.github.io/video_timings_calculator
  //val videoParams = video.VideoParams(24, 33, 480, 10, 2, 48, 640, 16, 96)
  val videoParams = video.VideoParams(24, 20, 720, 5, 5, 220, 1280, 110, 40)
  val videoController = Module(new VideoController(videoParams, 8, 16, 16, value => Cat(value(7, 6), Fill(6, value(6)), value(5, 3), Fill(5, value(3)), value(2, 0), Fill(5, value(0))) /* BGR233 to BGR888 */ ))
  decoder.io.targets(2) <> videoController.io.mem
  decoder.io.targets(3) <> videoController.io.reg
  videoController.io.videoClock := io.pixel_clock
  videoController.io.videoReset := io.pixel_reset

  withClockAndReset(io.pixel_clock, io.pixel_reset) {
    val dviOut = Module(new DviOut)
    io.dvi_clock := dviOut.io.dviClock
    io.dvi_data0 := dviOut.io.dviData0
    io.dvi_data1 := dviOut.io.dviData1
    io.dvi_data2 := dviOut.io.dviData2

    val useTestPatternGenerator = false
    if( !useTestPatternGenerator ) {
      dviOut.io.video <> videoController.io.video
    } else {
      val tpg = Module(new SimpleTestPatternGenerator(videoParams))
      dviOut.io.video <> tpg.io.video
    }
  }
}