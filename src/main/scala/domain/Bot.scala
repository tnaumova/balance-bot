package domain

import scala.collection.mutable

class Bot(val id: BotId) {
  private var chips: Array[Option[Chip]]                 = Array(None, None)
  private val compared: mutable.Set[(Chip, Chip)]        = mutable.Set[(Chip, Chip)]()
  private val stashedCmd: mutable.Queue[PassChipsToNext] = mutable.Queue[PassChipsToNext]()
  private val NoFollowUpCmd                              = Right(Seq())

  private def handle(to: PassChipsToNext): Either[BotError, Seq[MoveChipTo]] = {
    val Array(low, high) = chips.map(_.get)
    compared += ((low, high)) // save history
    val followUpCmd = Right(
      Seq(MoveChipTo(low, to.low), MoveChipTo(high, to.high))
    )
    chips = Array(None, None) // clear stored chips
    followUpCmd
  }

  // receives commands puts it to queue, or updates chip state, might return follow up events
  def receive(command: Command): Either[BotError, Seq[MoveChipTo]] = command match {
    case MoveChipTo(chip, _) =>
      if (canAcceptChip) {
        acceptChip(chip)
      } else {
        Left(CannotAccept("Cannot accept chip"))
      }
      if (hasTwo && stashedCmd.nonEmpty) {
        val nextStashedEvent = stashedCmd.dequeue()
        handle(nextStashedEvent)
      } else NoFollowUpCmd
    case to: PassChipsToNext =>
      if (hasTwo) {
        handle(to)
      } else {
        stashedCmd.enqueue(to)
        NoFollowUpCmd
      }

    case _ => Left(UnsupportedBotEvent("Unsupported event type"))
  }

  private[domain] def hasTwo: Boolean        = chips(0).isDefined && chips(1).isDefined
  private[domain] def canAcceptChip: Boolean = chips(0).isEmpty || chips(1).isEmpty

  private[domain] def acceptChip(aChip: Int): Unit = chips match {
    case Array(None, None) => chips.update(0, Some(aChip))
    case Array(Some(low), None) =>
      if (aChip > low) chips.update(1, Some(aChip))
      else {
        chips(0) = Some(aChip)
        chips(1) = Some(low)
      }
  }

  def processed(low: Chip, high: Chip): Boolean = {
    compared.contains((low, high))
  }

}
