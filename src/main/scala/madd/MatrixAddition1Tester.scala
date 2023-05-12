package madd

import chisel3._
import chisel3.iotesters.PeekPokeTester
import chisel3.util._

class StridePrefetcherSpec extends ChiselFlatSpec { 
  // 在这里指定addressWidth、pcWidth和测试的backend
  private val addressWidth = 32 
  private val pcWidth = 32 
  private val backendName = "firrtl"
  
  "StridePrefetcher" should s"work correctly with $backendName backend" in { 
    Driver(() => new StridePrefetcher(addressWidth, pcWidth), backendName) { c =>
    new StridePrefetcherTester(c)
  } should be(true)
  }
}

class StridePrefetcherTest(dut: StridePrefetcher) extends PeekPokeTester(dut) {

  // Test with a sequence of consecutive memory accesses
  poke(dut.io.pc, 0.U)
  var prevAddress = 0.U
  for (i <- 0 until 10) {
    val address = prevAddress + 4.U
    poke(dut.io.address, address)
    expect(dut.io.prefetch_address, prevAddress + 16.U)
    expect(dut.io.prefetch_valid, i >= 1 && i <= 4)
    step(1)
    prevAddress = address
  }

  // Test with a sequence of non-consecutive memory accesses
  poke(dut.io.pc, 0.U)
  prevAddress = 0.U
  for (i <- 0 until 10) {
    val address = prevAddress + 8.U
    poke(dut.io.address, address)
    expect(dut.io.prefetch_address, prevAddress + 16.U)
    expect(dut.io.prefetch_valid, i == 2 || i == 6)
    step(1)
    prevAddress = address
  }
}

object StridePrefetcherTest extends App {
  chisel3.iotesters.Driver(() => new StridePrefetcher(32, 32)) { dut =>
    new StridePrefetcherTest(dut)
  }
}
