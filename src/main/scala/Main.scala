import domain.{Command, Destination, MoveChipTo, NextBot, PassChipsToNext}

object Main extends App {
  private val valuePattern = {
    "value ([0-9]+) goes to bot ([0-9]+)".r
  }
  private val passPattern = {
    "bot ([0-9]+) gives low to ([a-z]+) ([0-9]+) and high to ([a-z]+) ([0-9]+)".r
  }

  //  read input and parse rows
  private def loadInstructions(): List[Command] = {
    val bufferedSource = io.Source.fromFile("src/main/resources/input.txt")
    val cmds = bufferedSource.getLines
      .map {
        case valuePattern(chip, bot) =>
          Some(MoveChipTo(chip.toInt, NextBot(bot.toInt)))
        case passPattern(bot, lowDest, lowInd, highDest, highInd) =>
          val low  = Destination.from(lowDest, lowInd.toInt)
          val high = Destination.from(highDest, highInd.toInt)
          if (low.isSuccess && high.isSuccess) {
            Some(PassChipsToNext(bot.toInt, low.get, high.get))
          } else {
            None //for simplicity skip invalid commands
          }
      }
      .filter(_.isDefined)
      .map(_.get)
      .toList

    bufferedSource.close
    cmds
  }

  val panel = new ControlPanel()
  panel.run(loadInstructions())
//  PART 1:
//    Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?
  println(
    panel.whichBotComparedPretty(61, 17)
  )
  //  PART 2:
  //    What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
  println(s"Output[0] ${panel.outputForPretty(0)}")
  println(s"Output[1] ${panel.outputForPretty(1)}")
  println(s"Output[2] ${panel.outputForPretty(2)}")
}
