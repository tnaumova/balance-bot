package domain

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class BotTest extends AnyFunSpec {
  describe("A Bot") {
    describe("when receives an InputChip event") {
      it("should return correct value for canAcceptChip") {
        val bot = new Bot(1)
        bot.canAcceptChip shouldBe true

        bot.receive(MoveChipTo(1, NextBot(1)))
        bot.canAcceptChip shouldBe true

        bot.receive(MoveChipTo(1, NextBot(1)))
        bot.canAcceptChip shouldBe false
      }
    }
    describe(
      "when receives 2 MoveChipTo, after PassChip command should return list of followup events"
    ) {
      val bot = new Bot(1)
      bot.receive(MoveChipTo(1, NextBot(1))) shouldBe Right(Seq())
      bot.receive(MoveChipTo(10, NextBot(1))) shouldBe Right(Seq())
      bot.receive(PassChipsToNext(1, NextBot(2), NextBot(2))) shouldBe Right(
        List(MoveChipTo(1, NextBot(2)), MoveChipTo(10, NextBot(2)))
      )
      bot.processed(1, 10) shouldBe true
      bot.canAcceptChip shouldBe true
    }

    describe("when receives PassChipsToNext commands") {
      it("should stash it and process when receives 2 MoveChipTo commands") {
        val bot = new Bot(1)
        bot.receive(PassChipsToNext(1, NextBot(2), NextBot(3))) shouldBe Right(Seq())
        bot.receive(MoveChipTo(1, NextBot(1))) shouldBe Right(Seq())
        bot.receive(MoveChipTo(10, NextBot(1))) shouldBe Right(
          List(MoveChipTo(1, NextBot(2)), MoveChipTo(10, NextBot(3)))
        )
        bot.processed(1, 10) shouldBe true
        bot.canAcceptChip shouldBe true
      }
    }

    describe("when receives 2 MoveChiTo") {
      it("should store chows ordered by value") {
        val bot = new Bot(1)
        bot.receive(MoveChipTo(10, NextBot(1)))
        bot.receive(MoveChipTo(1, NextBot(1)))
        bot.receive(PassChipsToNext(1, Output(1), Output(2))) shouldBe Right(
          Seq(MoveChipTo(1, Output(1)), MoveChipTo(10, Output(2)))
        )
      }
    }
  }
}
