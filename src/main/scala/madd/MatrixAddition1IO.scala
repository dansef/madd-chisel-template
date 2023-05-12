package madd

import chisel3._
import chisel3.util._

class StridePrefetcher(val pcWidth: Int, val addrWidth: Int) extends Module {
  val io = IO(new Bundle {
    val pc = Input(UInt(pcWidth.W))
    val address = Input(UInt(addrWidth.W))
    val prefetch_address = Output(UInt(addrWidth.W))
    val prefetch_valid = Output(Bool())
  })

  val tableSize = 64
  val table = Queue((UInt(pcWidth.W), UInt(addrWidth.W), SInt(16.W)), tableSize)

  // Find matching entry in the table for the current PC
  val matchEntry = table.find{ case (pc, _, _) => pc === io.pc }

  // Calculate stride and store it in the table
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
    val (_, prevAddress, prevStride) = matchEntry
    stride := io.address.asSInt - prevAddress.asSInt
    table.enqueue((io.pc, io.address, stride))
    prefetchAddress := io.address + stride
    prefetchCount := MAX_PREFETCHES.U
  }.otherwise {
    table.enqueue((io.pc, io.address, 0.S))
  }

  // Output prefetch address and valid signal
  io.prefetch_address := prefetchAddress
  io.prefetch_valid := prefetchCount > 0.U
}

class MatrixAddition1IO(M: Int, N: Int) extends Bundle {
  val a = Input(Vec(M * N, SInt(32.W)))
  val b = Input(Vec(M * N, SInt(32.W)))

  val out = Output(Vec(M * N, SInt(32.W)))

  override def cloneType =
    new MatrixAddition1IO(M, N).asInstanceOf[this.type]
}
