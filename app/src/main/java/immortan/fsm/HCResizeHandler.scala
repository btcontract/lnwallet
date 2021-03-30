package immortan.fsm

import immortan.{ChannelHosted, ChannelListener, HostedCommits}
import fr.acinq.eclair.channel.HC_CMD_RESIZE
import immortan.ChannelListener.Transition
import immortan.Channel.SUSPENDED
import fr.acinq.bitcoin.Satoshi


// Successful resize may come from a different handler, client should always re-check if new capacity is OK
abstract class HCResizeHandler(delta: Satoshi, chan: ChannelHosted) extends ChannelListener { me =>
  def onResizingSuccessful(hc1: HostedCommits): Unit
  def onChannelSuspended(hc1: HostedCommits): Unit

  override def onBecome: PartialFunction[Transition, Unit] = {
    case (_, _, hc1: HostedCommits, prevState, SUSPENDED) if SUSPENDED != prevState =>
      // Something went wrong while we were trying to resize a channel
      onChannelSuspended(hc1)
      chan.listeners -= me

    case(_, hc0: HostedCommits, hc1: HostedCommits, _, _) if hc0.resizeProposal.isDefined && hc1.resizeProposal.isEmpty =>
      // Previous state had resizing proposal while new one does not and channel is not suspended, meaning it's all fine
      onResizingSuccessful(hc1)
      chan.listeners -= me
  }

  chan.listeners += me
  chan process HC_CMD_RESIZE(delta)
}
