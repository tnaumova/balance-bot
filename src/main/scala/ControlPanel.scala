import domain._

import scala.collection.mutable

/**    Control accepts events from bots and tracks output
  */
class ControlPanel {
  private val bots: mutable.Map[BotId, Bot] = mutable.Map()
  private val output                        = mutable.Map[OutputNum, List[Chip]]()

  private def chipToOutput(c: Chip, ind: Chip): Unit = {
    output.updateWith(ind)(maybeList =>
      if (maybeList.isDefined) {
        maybeList.map(_ :+ c)
      } else Some(List(c))
    )
  }

  private def getOrInitiate(botId: BotId): Bot = {
    if (bots.contains(botId)) {
      bots(botId)
    } else {
      val bot = new Bot(botId)
      bots.update(botId, bot)
      bot
    }
  }

  /** Runs instructions from the list.
    */
  def run(instructions: List[Command]): Unit = {
    val inputQueue: mutable.Queue[Command] = mutable.Queue()
    inputQueue.enqueueAll(instructions)

    while (inputQueue.nonEmpty) {
      val next = inputQueue.dequeue()
      next match {
        case move @ MoveChipTo(_, next: NextBot) =>
          sendToBot(next.botNum, move, inputQueue)
        case MoveChipTo(c: Chip, output: Output) => chipToOutput(c, output.ind)
        case pass @ PassChipsToNext(from, _, _) =>
          sendToBot(from, pass, inputQueue)
      }
    }

    def sendToBot(botId: BotId, command: Command, inputQueue: mutable.Queue[Command]): Unit = {
      val newCmds = getOrInitiate(botId).receive(command) match {
        case Right(cmds) => cmds
        case Left(error) =>
          println(
            s" $botId failed to process command $command, cause $error"
          ) // for simplicity just printing errors, without further handling
          Seq()
      }
      inputQueue.enqueueAll(newCmds)
    }
  }

  def outputFor(index: Int): Option[List[Int]] = {
    output.get(index)
  }

  def outputForPretty(index: Int): String = {
    outputFor(index).map(_.mkString(",")).getOrElse("is empty")
  }

  def whichBotCompared(chip1: Chip, chip2: Chip): Option[BotId] = {
    // first sort the input, to make sure it is sorted
    val Array(low, high) = Array(chip1, chip2).sorted
    bots.values
      .find(_.processed(low, high))
      .map(_.id)
  }

  def whichBotComparedPretty(chip1: Chip, chip2: Chip): String = {
    whichBotCompared(chip1, chip2)
      .map(id => s"Bot $id")
      .getOrElse("Such pair is not found")
  }
}
