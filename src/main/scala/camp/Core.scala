package camp

import chisel3._
import chisel3.util._
import common.Consts._
import common.Instructions._

class Core extends Module {
  val io = IO(new Bundle {
    val imem = Flipped(new ImemPortIo())
    val dmem = Flipped(new DmemPortIo())
    val exit = Output(Bool())
    val gp = Output(UInt(WORD_LEN.W))
  })
  // register
  val regfile = Mem(32, UInt(WORD_LEN.W))
  // csr register
  val csr_regfile = Mem(4096, UInt(WORD_LEN.W))

  // program counter register
  val if_pc_reg = RegInit(START_ADDR)
  // pipeline registers
  // IF/ID stage
  val id_pc_reg = RegInit(0.U(WORD_LEN.W))
  val id_reg_inst = RegInit(0.U(WORD_LEN.W))

  // ID/EX stage
  val exe_reg_pc = RegInit(0.U(WORD_LEN.W))
  val exe_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val exe_reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_op2_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val exe_reg_exe_fun = RegInit(0.U(EXE_FUN_LEN.W))
  val exe_reg_mem_wen = RegInit(0.U(MEN_LEN.W))
  val exe_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val exe_reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  val exe_reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  val exe_reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val exe_reg_imm_i_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_s_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_b_sext = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_u_shifted = RegInit(0.U(WORD_LEN.W))
  val exe_reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))

  // EX/MEM stage
  val mem_reg_pc = RegInit(0.U(WORD_LEN.W))
  val mem_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val mem_reg_op1_data = RegInit(0.U(WORD_LEN.W))
  val mem_reg_rs2_data = RegInit(0.U(WORD_LEN.W))
  val mem_reg_mem_wen = RegInit(0.U(MEN_LEN.W))
  val mem_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val mem_reg_wb_sel = RegInit(0.U(WB_SEL_LEN.W))
  val mem_reg_csr_addr = RegInit(0.U(CSR_ADDR_LEN.W))
  val mem_reg_csr_cmd = RegInit(0.U(CSR_LEN.W))
  val mem_reg_imm_z_uext = RegInit(0.U(WORD_LEN.W))
  val mem_reg_alu_out = RegInit(0.U(WORD_LEN.W))

  val mem_wb_data = Wire(UInt(WORD_LEN.W))

  // MEM/WB stage
  val wb_reg_wb_addr = RegInit(0.U(ADDR_LEN.W))
  val wb_reg_rf_wen = RegInit(0.U(REN_LEN.W))
  val wb_reg_wb_data = RegInit(0.U(WORD_LEN.W))

  // pc_reg connects to imem
  io.imem.addr := if_pc_reg

  // instruction fetch
  val if_inst = io.imem.inst

  // program counter plus4 分岐命令のときに使われる
  val if_pc_plus4 = if_pc_reg + 4.U(WORD_LEN.W)

  // ストールの実装
  val stall_flg = Wire(Bool())

  // 分岐先のカウンタ
  val exe_br_target = Wire(UInt(WORD_LEN.W))
  // 分岐フラグ
  val exe_br_flg = Wire(Bool())

  // 分岐命令かどうか
  val exe_jmp_flg = Wire(Bool())

  // ALUの出力
  val exe_alu_out = Wire(UInt(WORD_LEN.W))

  // pc_next 次のprogram counter
  // br_flgがtrueのときは分岐先のカウンタ
  // jmp_flgがtrueのときはALUの出力
  // ecallのときはcsr_regfile(0x305)の値
  // stall_flgがtrueのときは今のprogram counter
  // どれにも該当しない場合は今のprogram counter+4
  val if_pc_next = MuxCase(
    if_pc_plus4,
    Seq(
      exe_br_flg -> exe_br_target,
      exe_jmp_flg -> exe_alu_out,
      stall_flg -> if_pc_reg // stall
    )
  )

  // program counterの更新
  if_pc_reg := if_pc_next

  id_pc_reg := Mux(stall_flg, id_pc_reg, if_pc_reg)

  // exeステージに来たとき、分岐命令なら、命令フェッチをバブル化させる
  id_reg_inst := MuxCase(
    if_inst,
    Seq(
      // ジャンプ命令とストールが同時発生したときはジャンプ命令を優先する
      (exe_br_flg || exe_jmp_flg) -> BUBBLE,
      stall_flg -> id_reg_inst
    )
  )

  // ifからidに来るとき、分岐命令なら、デコード処理をバブル化させる
  val id_inst =
    Mux((exe_br_flg || exe_jmp_flg || stall_flg), BUBBLE, id_reg_inst)

  // **重要**
  // 命令のデコード
  // ここでrs1, rs2, wb(write back)のアドレスを取り出す
  val id_rs1_addr = id_inst(19, 15)
  val id_rs2_addr = id_inst(24, 20)
  val id_wb_addr = id_inst(11, 7)

  val id_rs1_addr_b = id_reg_inst(19, 15)
  val id_rs2_addr_b = id_reg_inst(24, 20)
  val id_rs1_data_hazard =
    (exe_reg_rf_wen === REN_S) && (id_rs1_addr_b =/= 0.U) && (id_rs1_addr_b === exe_reg_wb_addr)

  val id_rs2_data_hazard =
    (exe_reg_rf_wen === REN_S) && (id_rs2_addr_b =/= 0.U) && (id_rs2_addr_b === exe_reg_wb_addr)

  stall_flg := (id_rs1_data_hazard || id_rs2_data_hazard)

  // レジスタからデータを取り出す
  // rs1_addrが0ではないとき、つまりアドレスが指定されているときはそのアドレスのデータを取り出す
  // 0のとき、つまりアドレスが指定されていないときは0として扱う
  val id_rs1_data = MuxCase(
    regfile(id_rs1_addr),
    Seq(
      // rs1_addrが0のときは0
      (id_rs1_addr === 0.U) -> 0.U(WORD_LEN.W),
      // メモリからのデータを取り出す
      ((id_rs1_addr === mem_reg_wb_addr) && (mem_reg_rf_wen === REN_S)) -> mem_wb_data,
      // レジスタファイルからのデータを取り出す
      ((id_rs1_addr === wb_reg_wb_addr) && (wb_reg_rf_wen === REN_S)) -> wb_reg_wb_data
    )
  )
  // rs2_addrも同様
  val id_rs2_data = MuxCase(
    regfile(id_rs2_addr),
    Seq(
      // rs2_addrが0のときは0
      (id_rs2_addr === 0.U) -> 0.U(WORD_LEN.W),
      // メモリからのデータを取り出す
      ((id_rs2_addr === mem_reg_wb_addr) && (mem_reg_rf_wen === REN_S)) -> mem_wb_data,
      // レジスタファイルからのデータを取り出す
      ((id_rs2_addr === wb_reg_wb_addr) && (wb_reg_rf_wen === REN_S)) -> wb_reg_wb_data
    )
  )

  // 命令のデコード
  // 即値
  // I形式
  //  ↓ここ
  // |31 imm[11:0] 20|19 rs1 15|14 funct3 12|11 rd 7|6 opcode 0|
  // 符号拡張即値
  // |31 --inst[31]-- 11|10 inst[30:25] 5|4 inst[24:21] 1|0 inst[20] 0|
  val id_imm_i = id_inst(31, 20)
  // オフセットの符号拡張
  // imm_iの頭の符号ビットで上の20bitを埋める
  // sext: sign extension
  val id_imm_i_sext = Cat(Fill(20, id_imm_i(11)), id_imm_i)
  // S形式
  //  ↓ここ                                             ↓ここ
  // |31 imm[11:5] 25|24 rs2 20|19 rs1 15|14 funct3 12|11 imm[4:0] 7|6 opcode 0|
  // |31 --inst[31]-- 11|10 inst[30:25] 5|4 inst[11:8] 1|0 inst[7] 0|
  val id_imm_s = Cat(id_inst(31, 25), id_inst(11, 7))
  val id_imm_s_sext = Cat(Fill(20, id_imm_s(11)), id_imm_s)
  // B形式
  //  ↓ここ                                                 ↓ここ
  // |31 imm[12|10:5] 25|24 rs2 20|19 rs1 15|14 funct3 12|11 imm[4:1|11] 7|6 opcode 0|
  // |31 --inst[31]-- 12|11 inst[7] 10|9 inst[30:25] 5|4 inst[11:8] 1|0 0 0|
  // 符号拡張したときに必ず最後が0になるのはRiscVの命令長が必ず2byteの整数倍長だから
  val id_imm_b = Cat(
    id_inst(31),
    id_inst(7),
    id_inst(30, 25),
    id_inst(11, 8)
  )
  val id_imm_b_sext = Cat(Fill(19, id_imm_b(11)), id_imm_b, 0.U(1.U))
  // J形式
  //  ↓ここ
  // |31 imm[20|10:1|11|19:12] 12|11 rd 7|6 opcode 0|
  // |31 --inst[31]-- 20|19 inst[19:12] 12|11 inst[20] 11|10 inst[30:25] 5|4 inst[24:21] 1|0 0 0|
  // 符号拡張したときに必ず最後が0になるのはRiscVの命令長が必ず2byteの整数倍長だから
  val id_imm_j = Cat(
    id_inst(31),
    id_inst(19, 12),
    id_inst(20),
    id_inst(30, 21)
  )
  val id_imm_j_sext = Cat(Fill(11, id_imm_j(19)), id_imm_j, 0.U(1.U))
  // U形式
  //  ↓ここ
  // |31 imm[31:12] 12|11 rd 7|6 opcode 0|
  // |31 inst[31] 31|30 inst[30:20] 20|19 inst[19:12] 12|11 --0-- 0|
  val id_imm_u = id_inst(31, 12)
  val id_imm_u_shifted = Cat(id_imm_u, Fill(12, 0.U))
  // CSR用の即値(I形式)
  // ↓ここ
  // |31 imm[11:0] 20|19 rs1 15|14 funct3 12|11 rd 7|6 opcode 0|
  // |31 --0-- 4|3 inst[19:15] 0|
  val id_imm_z = id_inst(19, 15)
  val id_imm_z_uext = Cat(Fill(27, 0.U), id_imm_z)

  // csignals 制御信号をまとめたもの
  // ListLookup
  // instと各bitPatを比較して、一致するものがあればそれを返す
  // なければデフォルトのものを返す
  val csignals = ListLookup(
    id_inst,
    // どの命令にも一致しないときは全ての信号をXにする
    List(ALU_X, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),
    Array(
      // LW  Load Word命令 I形式
      // 指定したレジスタに即値を加算したアドレスのメモリのデータを書き込む
      // x[rd] = M[x[rs1] + sext(offset)]
      // ALU: ADD
      // OP1: RS1 (rs1_data) 命令のデコードでregfileから取り出したデータ
      // OP2: IMI (imm_i_sext)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: MEM
      // CSR: X(なし)

      LW -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_MEM, CSR_X),

      // SW  Store Word命令 S形式
      // M[x[rs1] + sext(offset)] = x[rs2]
      // ALU: ADD
      // OP1: RS1 (rs1_data)
      // OP2: IMI (imm_s_sext)
      // DMEMとやり取りをするか: S(する)
      // レジスタファイルに書き込むか: X(しない)
      // WriteBack: X(しない)
      // CSR: X(なし)

      SW -> List(ALU_ADD, OP1_RS1, OP2_IMS, MEN_S, REN_X, WB_X, CSR_X),

      // 加減算・論理演算命令 R形式
      // OP1: RS1 (rs1_data)
      // OP2: RS2 (rs2_data)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: ALU
      // CSR: X(なし)

      // Add命令
      // x[rd] = x[rs1] + x[rs2]
      // ALU: ADD
      ADD -> List(ALU_ADD, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Sub命令
      // x[rd] = x[rs1] - x[rs2]
      // ALU: SUB
      SUB -> List(ALU_SUB, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // And命令 R形式
      // x[rd] = x[rs1] & x[rs2]
      // ALU: AND
      AND -> List(ALU_AND, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Or命令 R形式
      // x[rd] = x[rs1] | x[rs2]
      // ALU: OR
      OR -> List(ALU_OR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Xor命令 R形式
      // x[rd] = x[rs1] ^ x[rs2]
      // ALU: XOR
      XOR -> List(ALU_XOR, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // 即値加算・論理演算命令
      // OP1: RS1 (rs1_data)
      // OP2: IMI (imm_i_sext)

      // Addi命令
      // x[rd] = x[rs1] + sext(imm)
      // ALU: ADD
      ADDI -> List(ALU_ADD, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // Andi命令 I形式
      // x[rd] = x[rs1] & sext(imm)
      // ALU: AND
      ANDI -> List(ALU_AND, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // Ori命令 I形式
      // x[rd] = x[rs1] | sext(imm)
      // ALU: OR
      ORI -> List(ALU_OR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // Xori命令 I形式
      // x[rd] = x[rs1] ^ sext(imm)
      // ALU: XOR
      XORI -> List(ALU_XOR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // シフト命令 R形式
      // OP1: RS1 (rs1_data)
      // OP2: RS2 (rs2_data)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: ALU
      // CSR: X(なし)

      // Sll  Shift Left Logical 命令
      // x[rd] = x[rs1] << x[rs2]
      // ALU: SLL
      SLL -> List(ALU_SLL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Srl  Shift Right Logical 命令 論理右シフト
      // x[rd] = x[rs1] >> x[rs2]
      // ALU: SRL
      SRL -> List(ALU_SRL, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Sra  Shift Right Arithmetic 命令 算術右シフト
      // x[rd] = x[rs1] >> x[rs2]
      // ALU: SRA
      SRA -> List(ALU_SRA, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // 即値シフト命令 I形式
      // OP2: IMI (imm_i_sext)

      // Slli  Shift Left Logical Immediate 命令
      // x[rd] = x[rs1] << imm
      // ALU: SLL
      SLLI -> List(ALU_SLL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      // Srli  Shift Right Logical Immediate 命令 論理右シフト
      // x[rd] = x[rs1] >> imm
      // ALU: SRL
      SRLI -> List(ALU_SRL, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),
      // Srai  Shift Right Arithmetic Immediate 命令 算術右シフト
      // x[rd] = x[rs1] >> imm
      // ALU: SRA
      SRAI -> List(ALU_SRA, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // 比較演算命令 R形式
      // OP1: RS1 (rs1_data)
      // OP2: RS2 (rs2_data)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: ALU
      // CSR: X(なし)

      // Slt  Set Less Than 命令
      // x[rd] = (x[rs1] < x[rs2]) ? 1 : 0
      // ALU: SLT
      SLT -> List(ALU_SLT, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // Sltu  Set Less Than Unsigned 命令
      // x[rd] = (x[rs1] < x[rs2]) ? 1 : 0
      // ALU: SLTU
      SLTU -> List(ALU_SLTU, OP1_RS1, OP2_RS2, MEN_X, REN_S, WB_ALU, CSR_X),

      // 即値比較演算命令 I形式
      // OP1: RS1 (rs1_data)
      // OP2: IMI (imm_i_sext)

      // Slti  Set Less Than Immediate 命令
      // x[rd] = (x[rs1] < imm) ? 1 : 0
      // ALU: SLT
      SLTI -> List(ALU_SLT, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // Sltiu  Set Less Than Immediate Unsigned 命令
      // x[rd] = (x[rs1] < imm) ? 1 : 0
      // ALU: SLTU
      SLTIU -> List(ALU_SLTU, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_ALU, CSR_X),

      // 分岐命令 B形式
      // OP1: RS1 (rs1_data)
      // OP2: RS2 (rs2_data)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: X(しない)
      // WriteBack: X(しない)
      // CSR: X(なし)
      // ALU: X(しない)

      // Beq  Branch if Equal 命令
      // if (x[rs1] == x[rs2]) pc += offset(imm_b_sext)
      BEQ -> List(BR_BEQ, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // Bne  Branch if Not Equal 命令
      // if (x[rs1] != x[rs2]) pc += offset(imm_b_sext)
      BNE -> List(BR_BNE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // Bge  Branch if Greater or Equal 命令
      // if (x[rs1] >= x[rs2]) pc += offset(imm_b_sext)
      BGE -> List(BR_BGE, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // Bgeu  Branch if Greater or Equal Unsigned 命令
      // if (x[rs1] >= x[rs2]) pc += offset(imm_b_sext)
      BGEU -> List(BR_BGEU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // Blt  Branch if Less Than 命令
      // if (x[rs1] < x[rs2]) pc += offset(imm_b_sext)
      BLT -> List(BR_BLT, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // Bltu  Branch if Less Than Unsigned 命令
      // if (x[rs1] < x[rs2]) pc += offset(imm_b_sext)
      BLTU -> List(BR_BLTU, OP1_RS1, OP2_RS2, MEN_X, REN_X, WB_X, CSR_X),

      // ジャンプ命令 J形式
      // OP1: PC (pc_reg)
      // OP2: IMJ (imm_j_sext)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: PC
      // CSR: X(なし)

      // Jal  Jump and Link 命令
      // x[rd] = pc + 4; pc += offset(imm_j_sext)
      // ALU: ADD
      JAL -> List(ALU_ADD, OP1_PC, OP2_IMJ, MEN_X, REN_S, WB_PC, CSR_X),

      // Jalr  Jump and Link Register 命令
      // x[rd] = pc + 4; pc = x[rs1] + imm
      // ALU: JALR
      JALR -> List(ALU_JALR, OP1_RS1, OP2_IMI, MEN_X, REN_S, WB_PC, CSR_X),

      // 即値ロード命令 U形式
      // OP1: X(しない)
      // OP2: IMU (imm_u_shifted)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: ALU
      // CSR: X(なし)

      // この命令を使うことで任意の場所に相対的にジャンプできる

      // Lui  Load Upper Immediate 命令
      // x[rd] = imm
      // ALU: ADD
      LUI -> List(ALU_ADD, OP1_X, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),

      // Auipc  Add Upper Immediate to PC 命令
      // x[rd] = pc + imm
      // ALU: ADD
      AUIPC -> List(ALU_ADD, OP1_PC, OP2_IMU, MEN_X, REN_S, WB_ALU, CSR_X),

      // CSR Control and Status Register 命令
      // OP1: RS1 (rs1_data)
      // OP2: X(しない)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: S(書き込む)
      // WriteBack: CSR
      // CSR: W(書き込む), S(セット), C(クリア), E(例外)

      // Csrrw  Control and Status Register Read and Write 命令
      // x[rd] = csr; csr = x[rs1]
      // ALU: COPY1
      CSRRW -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),

      // Csrrwi  Control and Status Register Read and Write Immediate 命令
      // x[rd] = csr; csr = imm_z
      // ALU: COPY1
      CSRRWI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_W),

      // Csrrs  Control and Status Register Read and Set 命令
      // x[rd] = csr; csr = csr | x[rs1]
      // ALU: COPY1
      CSRRS -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),

      // Csrrsi  Control and Status Register Read and Set Immediate 命令
      // x[rd] = csr; csr = csr | imm_z
      // ALU: COPY1
      CSRRSI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_S),

      // Csrrc  Control and Status Register Read and Clear 命令
      // x[rd] = csr; csr = csr & ~x[rs1]
      // ALU: COPY1
      CSRRC -> List(ALU_COPY1, OP1_RS1, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),

      // Csrrci  Control and Status Register Read and Clear Immediate 命令
      // x[rd] = csr; csr = csr & ~imm_z
      // ALU: COPY1
      CSRRCI -> List(ALU_COPY1, OP1_IMZ, OP2_X, MEN_X, REN_S, WB_CSR, CSR_C),

      // Ecall  Environment Call 命令
      // ALU: X(しない)
      // OP1: X(しない)
      // OP2: X(しない)
      // DMEMとやり取りをするか: X(しない)
      // レジスタファイルに書き込むか: X(しない)
      // WriteBack: X(しない)
      // CSR: E(例外)
      ECALL -> List(ALU_X, OP1_X, OP2_X, MEN_X, REN_X, WB_X, CSR_E)
    )
  )

  // csignalsから取り出した制御信号をそれぞれの変数に格納
  val id_exe_fun :: id_op1_sel :: id_op2_sel :: id_mem_wen :: id_rf_wen :: id_wb_sel :: id_csr_cmd :: Nil =
    csignals

  // operand1の選択
  // どれにも当てはまらない場合は0
  val id_op1_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op1_sel === OP1_RS1) -> id_rs1_data,
      (id_op1_sel === OP1_PC) -> id_pc_reg,
      (id_op1_sel === OP1_IMZ) -> id_imm_z_uext
    )
  )

  // operand2の選択
  val id_op2_data = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (id_op2_sel === OP2_RS2) -> id_rs2_data,
      (id_op2_sel === OP2_IMI) -> id_imm_i_sext,
      (id_op2_sel === OP2_IMS) -> id_imm_s_sext,
      (id_op2_sel === OP2_IMJ) -> id_imm_j_sext,
      (id_op2_sel === OP2_IMU) -> id_imm_u_shifted
    )
  )
  // CSRの実装
  // ECALL命令のときは0x305の値を返す
  val id_csr_addr =
    Mux(id_csr_cmd === CSR_E, 0x342.U(CSR_ADDR_LEN.W), id_inst(31, 20))

  // EX stage
  exe_reg_pc := id_pc_reg
  exe_reg_op1_data := id_op1_data
  exe_reg_op2_data := id_op2_data
  exe_reg_rs2_data := id_rs2_data
  exe_reg_wb_addr := id_wb_addr
  exe_reg_rf_wen := id_rf_wen
  exe_reg_exe_fun := id_exe_fun
  exe_reg_wb_sel := id_wb_sel
  exe_reg_imm_i_sext := id_imm_i_sext
  exe_reg_imm_s_sext := id_imm_s_sext
  exe_reg_imm_b_sext := id_imm_b_sext
  exe_reg_imm_u_shifted := id_imm_u_shifted
  exe_reg_imm_z_uext := id_imm_z_uext
  exe_reg_csr_addr := id_csr_addr
  exe_reg_csr_cmd := id_csr_cmd
  exe_reg_mem_wen := id_mem_wen

  // ALUの実装
  // exe_funによって選択された演算を行う
  exe_alu_out := MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      // 加算
      (exe_reg_exe_fun === ALU_ADD) -> (exe_reg_op1_data + exe_reg_op2_data),
      // 減算
      (exe_reg_exe_fun === ALU_SUB) -> (exe_reg_op1_data - exe_reg_op2_data),
      // AND
      (exe_reg_exe_fun === ALU_AND) -> (exe_reg_op1_data & exe_reg_op2_data),
      // OR
      (exe_reg_exe_fun === ALU_OR) -> (exe_reg_op1_data | exe_reg_op2_data),
      // XOR
      (exe_reg_exe_fun === ALU_XOR) -> (exe_reg_op1_data ^ exe_reg_op2_data),
      // SLL 論理左シフト
      // op2_dataの下位5bitを取り出してシフトする
      // シフト後の値の下位32bitを取り出す
      (exe_reg_exe_fun === ALU_SLL) -> (exe_reg_op1_data << exe_reg_op2_data(
        4,
        0
      ))(31, 0),
      // SRL 論理右シフト
      // op2_dataの下位5bitを取り出してシフトする
      // .asUIntを使うことでビット幅をシフト量分だけ拡張する
      (exe_reg_exe_fun === ALU_SRL) -> (exe_reg_op1_data >> exe_reg_op2_data(
        4,
        0
      )).asUInt(),
      // SRA 算術右シフト
      // op2_dataの下位5bitを取り出してシフトする
      // .asSIntを使うこでビット幅をシフト量分だけ拡張する
      (exe_reg_exe_fun === ALU_SRA) -> (exe_reg_op1_data
        .asSInt() >> exe_reg_op2_data(4, 0)).asUInt(),
      // SLT 符号付き比較
      // op1_dataがop2_dataより小さいとき1、そうでないとき0
      // asUIntを使うことでBoolをUIntに変換する
      (exe_reg_exe_fun === ALU_SLT) -> (exe_reg_op1_data
        .asSInt() < exe_reg_op2_data.asSInt()).asUInt(),
      // SLTU 符号なし比較
      (exe_reg_exe_fun === ALU_SLTU) -> (exe_reg_op1_data < exe_reg_op2_data)
        .asUInt(),
      // JALR ジャンプ命令
      // ~1.U(WORD_LEN.W) これは下位ビットが0でその他が1のビット列を作る
      // これを使ってANDを取ると下位ビットが必ず0になるのでジャンプ先のアドレスが確実に命令の先頭になる（意図的に即値に飛ばすこともできるけど...）
      (exe_reg_exe_fun === ALU_JALR) -> ((exe_reg_op1_data + exe_reg_op2_data) & ~1
        .U(WORD_LEN.W)),
      // COPY1
      (exe_reg_exe_fun === ALU_COPY1) -> exe_reg_op1_data
    )
  )

  // 分岐先のカウンタ
  exe_br_target := exe_reg_pc + exe_reg_imm_b_sext

  // 分岐命令の実装
  exe_br_flg := MuxCase(
    // 分岐しないときはfalse
    false.B,
    Seq(
      (exe_reg_exe_fun === BR_BEQ) -> (exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BNE) -> !(exe_reg_op1_data === exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BLT) -> (exe_reg_op1_data
        .asSInt() < exe_reg_op2_data.asSInt()),
      (exe_reg_exe_fun === BR_BGE) -> !(exe_reg_op1_data
        .asSInt() < exe_reg_op2_data.asSInt()),
      (exe_reg_exe_fun === BR_BLTU) -> (exe_reg_op1_data < exe_reg_op2_data),
      (exe_reg_exe_fun === BR_BGEU) -> !(exe_reg_op1_data < exe_reg_op2_data)
    )
  )

  exe_jmp_flg := (exe_reg_wb_sel === WB_PC)

  // MEMレジスタに書き込み
  mem_reg_pc := exe_reg_pc
  mem_reg_op1_data := exe_reg_op1_data
  mem_reg_rs2_data := exe_reg_rs2_data
  mem_reg_wb_addr := exe_reg_wb_addr
  mem_reg_alu_out := exe_alu_out
  mem_reg_rf_wen := exe_reg_rf_wen
  mem_reg_wb_sel := exe_reg_wb_sel
  mem_reg_csr_addr := exe_reg_csr_addr
  mem_reg_csr_cmd := exe_reg_csr_cmd
  mem_reg_imm_z_uext := exe_reg_imm_z_uext
  mem_reg_mem_wen := exe_reg_mem_wen

  // io.exitがtrueのときプログラムを終了する
  // io.exit := (mem_reg_pc === 0x44.U(WORD_LEN.W))
  io.exit := (id_reg_inst === UNIMP)

  // 以降の演算でaluの結果を使用したいので、alu_outをdmemに渡す
  io.dmem.addr := mem_reg_alu_out

  // SW命令のときにしか使われない MEN_Sの命令はSWのみ
  // write enable
  io.dmem.wen := mem_reg_mem_wen
  io.dmem.wdata := mem_reg_rs2_data

  val csr_rdata = csr_regfile(mem_reg_csr_addr)
  val csr_wdata = MuxCase(
    0.U(WORD_LEN.W),
    Seq(
      (mem_reg_csr_cmd === CSR_W) -> mem_reg_op1_data,
      (mem_reg_csr_cmd === CSR_S) -> (csr_rdata | mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_C) -> (csr_rdata & ~mem_reg_op1_data),
      (mem_reg_csr_cmd === CSR_E) -> 11.U(WORD_LEN.W)
    )
  )

  // 命令がCSR命令のとき
  when(mem_reg_csr_cmd > 0.U) {
    csr_regfile(mem_reg_csr_addr) := csr_wdata
  }

  // write backの実装

  mem_wb_data := MuxCase(
    // デフォルトはalu_outが出力される
    mem_reg_alu_out,
    Seq(
      // メモリに書き込むとき
      (mem_reg_wb_sel === WB_MEM) -> io.dmem.rdata,
      // program counterを書き込むとき
      (mem_reg_wb_sel === WB_PC) -> (mem_reg_pc + 4.U(WORD_LEN.W)),
      // csrの値を書き込むとき
      (mem_reg_wb_sel === WB_CSR) -> csr_rdata
    )
  )

  // WBレジスタに書き込み
  wb_reg_wb_addr := mem_reg_wb_addr
  wb_reg_rf_wen := mem_reg_rf_wen
  wb_reg_wb_data := mem_wb_data

  // レジスタファイルへの書き込み
  when(wb_reg_rf_wen === REN_S) {
    regfile(wb_reg_wb_addr) := wb_reg_wb_data
  }

  // デバッグ用
  io.gp := regfile(3)

  // デバッグ用の出力
  printf("pc_reg= %x \n", id_pc_reg)
  printf("inst= %x \n", id_reg_inst)
  printf("rs1= %x \n", id_rs1_addr)
  printf("rs2= %x \n", id_rs2_addr)
  printf("rd= %x \n", id_wb_addr)
  printf("rs1_data= %x \n", id_rs1_data)
  printf("rs2_data= %x \n", id_rs2_data)
  printf("imm_i= %x \n", id_imm_i)
  printf("imm_i_sext= %x \n", id_imm_i_sext)
  printf("imm_s= %x \n", id_imm_s)
  printf("imm_s_sext= %x \n", id_imm_s_sext)
  printf("imm_b= %x \n", id_imm_b)
  printf("imm_b_sext= %x \n", id_imm_b_sext)
  printf("imm_j= %x \n", id_imm_j)
  printf("imm_j_sext= %x \n", id_imm_j_sext)
  printf("imm_u= %x \n", id_imm_u)
  printf("imm_u_shifted= %x \n", id_imm_u_shifted)
  printf("imm_z= %x \n", id_imm_z)
  printf("imm_z_uext= %x \n", id_imm_z_uext)
  printf("exe_fun= %x \n", id_exe_fun)
  printf("op1_sel= %x \n", id_op1_sel)
  printf("op2_sel= %x \n", id_op2_sel)
  printf("mem_wen= %x \n", id_mem_wen)
  printf("rf_wen= %x \n", id_rf_wen)
  printf("wb_sel= %x \n", id_wb_sel)
  printf("csr_cmd= %x \n", id_csr_cmd)
  printf("op1_data= %x \n", id_op1_data)
  printf("op2_data= %x \n", id_op2_data)
  printf("csr_addr= %x \n", id_csr_addr)
  printf("stall_flg= %x \n", stall_flg)
  printf("io.gp= %x \n", regfile(3))

  printf("---------\n")

}
