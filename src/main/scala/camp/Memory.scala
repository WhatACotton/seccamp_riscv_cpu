package camp

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import common.Consts._

class ImemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val inst = Output(UInt(WORD_LEN.W))
}
class DmemPortIo extends Bundle {
  val addr = Input(UInt(WORD_LEN.W))
  val rdata = Output(UInt(WORD_LEN.W))
  val wen = Input(Bool())
  val wdata = Input(UInt(WORD_LEN.W))
}

class Memory extends Module {
  val io = IO(new Bundle {
    val imem = new ImemPortIo()
    val dmem = new DmemPortIo()
  })
  // メモリの確保・初期化
  val mem = SyncReadMem(16384, UInt(8.W))
  // メモリの読み込み
  loadMemoryFromFile(mem, "src/hex/ctest.hex")

  // 命令用のメモリアクセス
  // アドレスに対応するデータを読み書き
  // 今のアドレスから4バイト分のデータを読み込む
  io.imem.inst := Cat(
    mem.read(io.imem.addr >> 2).reverse
  )

  // データ用のメモリアクセス
  // 指定されたのアドレスから4バイト分のデータを読み込む
  io.dmem.rdata := Cat(
    mem.read(io.dmem.addr >> 2).reverse
  )

  // データの書き込み
  // データを4バイト分書き込む
  when(io.dmem.wen) {
    mem.write(
      io.dmem.addr >> 2,
      VecInit((0 to 3).map(i => io.dmem.wdata(8 * (i + 1) - 1, 8 * i)).reverse)
    )
  }
}
