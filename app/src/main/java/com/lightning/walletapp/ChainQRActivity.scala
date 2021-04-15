package com.lightning.walletapp

import com.azoft.carousellayoutmanager._
import com.lightning.walletapp.R.string._

import android.view.{View, ViewGroup}
import fr.acinq.eclair.blockchain.electrum.ElectrumWallet.Data.MAX_RECEIVE_ADDRESSES
import com.lightning.walletapp.BaseActivity.StringOps
import androidx.recyclerview.widget.RecyclerView
import com.ornach.nobobutton.NoboButton
import android.widget.TextView
import immortan.crypto.Tools
import android.os.Bundle
import immortan.LNParams


class ChainQRActivity extends QRActivity { me =>
  lazy private[this] val chainQrCaption = findViewById(R.id.chainQrCaption).asInstanceOf[TextView]
  lazy private[this] val chainQrCodes = findViewById(R.id.chainQrCodes).asInstanceOf[RecyclerView]
  lazy private[this] val chainQrMore = findViewById(R.id.chainQrMore).asInstanceOf[NoboButton]

  private[this] var allAddresses: List[String] = Nil
  private[this] var addresses: List[String] = Nil

  val adapter: RecyclerView.Adapter[QRViewHolder] = new RecyclerView.Adapter[QRViewHolder] {
    override def onBindViewHolder(holder: QRViewHolder, pos: Int): Unit = updateView(addresses(pos), holder)
    override def getItemId(itemPosition: Int): Long = itemPosition
    override def getItemCount: Int = addresses.size

    override def onCreateViewHolder(parent: ViewGroup, viewType: Int): QRViewHolder = {
      val qrCodeContainer = getLayoutInflater.inflate(R.layout.frag_qr, parent, false)
      new QRViewHolder(qrCodeContainer)
    }

    private def updateView(address: String, holder: QRViewHolder): Unit =
      runInFutureProcessOnUI(QRActivity.get(address, qrSize), onFail) { bitmap =>
        def share: Unit = runInFutureProcessOnUI(shareData(bitmap, address), onFail)(Tools.none)
        holder.qrCopy setOnClickListener onButtonTap(WalletApp.app copy address)
        holder.qrShare setOnClickListener onButtonTap(share)
        holder.qrLabel setText address.humanSix
        holder.qrCode setImageBitmap bitmap
      }
  }

  def INIT(state: Bundle): Unit =
    if (WalletApp.isAlive && LNParams.isOperational) {
      runFutureProcessOnUI(LNParams.chainWallet.wallet.getReceiveAddresses, onFail) { freshAddresses =>
        val layoutManager = new CarouselLayoutManager(CarouselLayoutManager.HORIZONTAL, false)
        layoutManager.setPostLayoutListener(new CarouselZoomPostLayoutListener)
        layoutManager.setMaxVisibleItems(4)

        allAddresses = freshAddresses
        addresses = freshAddresses.take(1)

        chainQrMore setOnClickListener onButtonTap {
          // Show all remaining QR images right away
          addresses = allAddresses

          // Animate list changes and remove a button since it gets useless
          adapter.notifyItemRangeInserted(1, MAX_RECEIVE_ADDRESSES - 1)
          chainQrMore.setVisibility(View.GONE)
        }

        chainQrCodes.addOnScrollListener(new CenterScrollListener)
        chainQrCodes.setLayoutManager(layoutManager)
        chainQrCodes.setHasFixedSize(true)
        chainQrCodes.setAdapter(adapter)
      }

      setContentView(R.layout.activity_qr_chain)
      chainQrCaption.setText(getString(dialog_receive_btc).html)
    } else {
      WalletApp.freePossiblyUsedResouces
      me exitTo ClassNames.mainActivityClass
    }
}
