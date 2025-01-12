package cpu

import chisel3._
import chisel3.util._
import common.Consts._
import uart.UartTx
import segled.SegmentLedWithShiftRegs
import segled.ShiftRegisterPort
import display.MatrixLed
import display.MatrixLedConfig
import uart.UartRx

class TopWithStepper(memoryPathGen: Int => String = i => f"../sw/bootrom_${i}.hex", suppressDebugMessage: Boolean = false, memorySize: Int = 8192, enableProbe: Boolean = false, forSimulation: Boolean = false, useTargetPrimitive: Boolean = true) extends Module {
  val io = IO(new Bundle {
    val debug_pc = Output(UInt(WORD_LEN.W))
    val uartTx = Output(Bool())
    val uartRx = Input(Bool())
    val success = Output(Bool())
    val segmentOut = ShiftRegisterPort()
    val digitSelector = ShiftRegisterPort()
    val ledOut = Output(UInt(32.W))
    val switchIn = Input(UInt(32.W))
    val stepperCount = Output(UInt(32.W))
    val stepperCwCcw = Output(UInt(1.W))
    val probeOut = Output(Bool())
    val exit = Output(Bool())
  })

  val clockFreqHz = 27000000
  val baseAddress = BigInt("00000000", 16)
  val core = Module(new Core(startAddress = baseAddress.U(WORD_LEN.W), suppressDebugMessage))

  val memory = Module(new Memory(Some(memoryPathGen), baseAddress.U(WORD_LEN.W), memorySize, forSimulation, useTargetPrimitive = useTargetPrimitive))
  val gpios = Module(new GpioArray((0 until 6).map(_ => BigInt("ffffffff", 16))))  // GPIO Array (6ポート)
  val uartRegs = Module(new IORegister(Seq((0x100ff, 0xff), (0x03, 0x00))))           // UART IOレジスタ

  val decoder = Module(new DMemDecoder(Seq(
    (BigInt(0x00000000L), BigInt(memorySize)),         // メモリ
    (BigInt(0xA0000000L), gpios.ADDRESS_RANGE),     // GPIO Array (6ポート)
    (BigInt(0xA0001000L), uartRegs.ADDRESS_RANGE),  // UART IO
  )))
  core.io.imem <> memory.io.imem
  core.io.dmem <> decoder.io.initiator  // CPUにデコーダを接続

  decoder.io.targets(0) <> memory.io.dmem   // 0番ポートにメモリを接続
  decoder.io.targets(1) <> gpios.io.mem     // 1番ポートにGPIOを接続
  decoder.io.targets(2) <> uartRegs.io.mem  // 2番ポートにUART IOを接続
  
  // GPIO port 0, 1 に8セグメント6桁LED用のドライバを接続
  val segmentLeds = Module(new SegmentLedWithShiftRegs(8, 6, 2, 2700, true, true))
  io.segmentOut <> segmentLeds.io.segmentOut
  io.digitSelector <> segmentLeds.io.digitSelector
  segmentLeds.io.digits := VecInit((0 to 3).map(i => gpios.io.out(0)(8 * i + 7, 8 * i)) ++ (0 to 1).map(i => gpios.io.out(1)(8 * i + 7, 8 * i)))
  gpios.io.in(0) := 0.U
  gpios.io.in(1) := 0.U

  // GPIO port 2, 3 にステッピングモータドライバを接続
  val stepperCount = RegInit((clockFreqHz / 500).U(32.W))
  val stepperCwCcw = RegInit(0.U(1.W))
  stepperCount := gpios.io.out(2)
  stepperCwCcw := gpios.io.out(3)(0)
  gpios.io.in(2) := stepperCount
  gpios.io.in(3) := Cat(0.U(31.W), stepperCwCcw)
  io.stepperCount := stepperCount
  io.stepperCwCcw := stepperCwCcw

  // GPIO port 4にLED用のドライバを接続
  io.ledOut := gpios.io.out(4)
  gpios.io.in(4) := 0.U

  // GPIO port 5にピン入力のドライバを接続
  io.switchIn <> gpios.io.in(5)

  val uartTx = Module(new UartTx(8, clockFreqHz / 115200))
  val uartRx = Module(new UartRx(8, clockFreqHz / 115200, 2))
  val uartTxValidReady = Wire(new DecoupledIO(UInt(8.W)))
  val uartTxQueue = Queue(uartTxValidReady, 16)
  val uartRxQueue = Queue(uartRx.io.out, 16)

  io.uartTx <> uartTx.io.tx
  uartTx.io.in <> uartTxQueue
  uartTxValidReady.valid := uartRegs.io.out(0).valid
  uartTxValidReady.bits := uartRegs.io.out(0).bits
  uartRegs.io.in(1).bits := Cat(0.U(30.W), uartRxQueue.valid, uartTxValidReady.ready)
  uartRegs.io.in(1).valid := true.B
  core.io.interrupt_in := uartRxQueue.valid

  io.uartRx <> uartRx.io.rx
  uartRegs.io.in(0).bits := Cat(0.U(15.W), uartRxQueue.valid, 0.U(8.W), uartRxQueue.bits)
  uartRegs.io.in(0).valid := true.B
  uartRxQueue.ready := uartRegs.io.in(0).ready

  io.success := core.io.success
  io.exit := core.io.exit
  io.debug_pc := core.io.debug_pc

  // 信号観測用プローブを構築
  if( enableProbe ) {
    val probe = Module(new diag.Probe(new diag.ProbeConfig(bufferDepth = 512, triggerPosition = 512 - 16), 65))
    probe.io.in := Cat( core.io.imem.valid, core.io.imem.addr, core.io.imem.inst )
    val noActivityCounter = RegInit(0.U(log2Ceil(256).W))
    when( gpios.io.mem.wen ) {
      noActivityCounter := 0.U
    } .otherwise {
      noActivityCounter := noActivityCounter + 1.U
    }
    probe.io.trigger := (noActivityCounter === 255.U) | !io.switchIn(0)
    val probeFrameAdapter = Module(new diag.ProbeFrameAdapter(probe.width))
    probeFrameAdapter.io.in <> probe.io.out
    val probeUartTx = Module(new UartTx(numberOfBits = 8, baudDivider = clockFreqHz / 115200))
    probeUartTx.io.in <> probeFrameAdapter.io.out
    io.probeOut := probeUartTx.io.tx
  } else {
    io.probeOut := true.B
  }
}