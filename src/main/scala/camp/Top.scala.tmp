package camp

import chisel3._
import common.Consts._

class Top extends Module {
  val io = IO(new Bundle {
    // プログラムの終了フラグ
    val exit = Output(Bool())
    // デバッグ用
    val gp = Output(UInt(WORD_LEN.W))
  })
  // モジュールのインスタンス化
  val core = Module(new Core())
  // メモリのインスタンス化
  val memory = Module(new Memory())

  // メモリの接続
  core.io.imem <> memory.io.imem
  core.io.dmem <> memory.io.dmem

  // 終了フラグ
  io.exit := core.io.exit
  // デバッグ用
  io.gp := core.io.gp
}
