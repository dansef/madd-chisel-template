package madd

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

class StridePrefetcher(val addressWidth: Int, val pcWidth: Int) extends Module {
  val io = IO(new Bundle {
    // 输入端口
    val pc = Input(UInt(pcWidth.W))
    val address = Input(UInt(addressWidth.W))
    // 输出端口
    val prefetch_address = Output(UInt(addressWidth.W))
    val prefetch_valid = Output(Bool())
  })
  
  val tableSize = 64
  val table = Queue((UInt(pcWidth.W), UInt(addrWidth.W), SInt(16.W)), tableSize)
  
  val matchEntry = table.find{ case (pc, _, _) => pc === io.pc }
  
  val MAX_PREFETCHES = 4
  val prefetchCount = RegInit(0.U(log2Ceil(MAX_PREFETCHES+1).W))
  val stride = RegInit(0.S(16.W))
  val prefetchAddress = RegInit(0.U(addrWidth.W))

  when(prefetchCount > 0.U) {
    prefetchAddress := prefetchAddress + stride
    prefetchCount := prefetchCount - 1.U
  }.otherwise {
    prefetchCount := 0.U
  }

  when(matchEntry != null) {
    stride := matchEntry._3
    prefetchAddress := io.address + stride
    prefetchCount := MAX_PREFETCHES.U
  }
  
  // 更新表格中的步幅信息 
  table.filter{ case (pc, _, _) => pc === io.pc }.foreach{
    case (_, prevAddress, prevStride) =>
    table -= ((io.pc, prevAddress, prevStride))
    table.enqueue((io.pc, io.address, stride)) 
  }
  // 如果表格中没有匹配的条目，则创建新的条目 
  if (table.size == tableSize && matchEntry == null) {
    table.dequeue()
  }
  table.enqueue((io.pc, io.address, stride))
} 
io.prefetch_address := prefetchAddress
io.prefetch_valid := prefetchCount > 0.U }
  

// TODO: update this module to implement stride prefetching.
class MatrixAddition1(M: Int, N: Int) extends Module {
  val io = IO(new MatrixAddition1IO(M, N))

  io.out := DontCare

  for (i <- 0 until M) {
    for (j <- 0 until N) {
      var sum = 0.S(32.W)

      sum = io.a(i * N + j) + io.b(i * N + j)

      io.out(i * N + j) := sum
    }
  }
}

object MatrixAddition1 extends App {
  (new ChiselStage).execute(
    Array("-X", "verilog", "-td", "source/"),
    Seq(
      ChiselGeneratorAnnotation(() => new MatrixAddition1(3, 2))
    )
  )
}
