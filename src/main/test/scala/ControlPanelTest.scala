import org.scalatest.funspec.AnyFunSpec
import domain._
import org.scalatest.matchers.should.Matchers._

class ControlPanelTest extends AnyFunSpec {
  import ControlPanelTest._
  describe("A ControlPanel") {
    describe("when no instructions, nothing in output") {
      val controlPanel = new ControlPanel()
      controlPanel.run(List()) shouldBe Right()
      controlPanel.outputFor(0) shouldBe None
    }
    describe("when instructions are not empty") {
      it("should correctly put chips to outputs") {
        val controlPanel = new ControlPanel()
        controlPanel.run(instructions) shouldBe Right()
        controlPanel.outputFor(0) shouldBe Some(List(1))
        controlPanel.outputFor(1) shouldBe Some(List(2))
      }

      it("should return correct results if instructions from the example") {
        //    value 5 goes to bot 2
        //    bot 2 gives low to bot 1 and high to bot 0
        //    value 3 goes to bot 1
        //    bot 1 gives low to output 1 and high to bot 0
        //    bot 0 gives low to output 2 and high to output 0
        //    value 2 goes to bot 2
        //In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip,
        // and output bin 2 contains a value-3 microchip. In this configuration, bot number 2 is responsible
        // for comparing value-5 microchips with value-2 microchips.
        val controlPanel = new ControlPanel()
        controlPanel.run(instructions2) shouldBe Right()
        controlPanel.outputFor(0) shouldBe Some(List(5))
        controlPanel.outputFor(1) shouldBe Some(List(2))
        controlPanel.outputFor(2) shouldBe Some(List(3))
        controlPanel.whichBotCompared(2, 5) shouldBe Some(2)
      }
    }
  }

  object ControlPanelTest {
    val instructions = List(
      MoveChipTo(2, NextBot(1)),
      MoveChipTo(1, NextBot(1)),
      PassChipsToNext(1, Output(0), Output(1))
    )

    val instructions2 = List(
      MoveChipTo(5, NextBot(2)),
      PassChipsToNext(2, NextBot(1), NextBot(0)),
      MoveChipTo(3, NextBot(1)),
      PassChipsToNext(1, Output(1), NextBot(0)),
      PassChipsToNext(0, Output(2), Output(0)),
      MoveChipTo(2, NextBot(2))
    )
  }

}
