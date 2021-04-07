package com.lightning.walletapp.sheets

import android.view.{LayoutInflater, View, ViewGroup}
import com.google.android.material.bottomsheet.BottomSheetDialogFragment
import com.lightning.walletapp.utils.OnListItemClickListener
import com.lightning.walletapp.ChoiceReceiver
import android.widget.ListView
import android.os.Bundle


class ChoiceBottomSheet(list: ListView, tag: String, host: ChoiceReceiver) extends BottomSheetDialogFragment { me =>
  override def onCreateView(inflater: LayoutInflater, container: ViewGroup, state: Bundle): View = list

  override def onViewCreated(view: View, state: Bundle): Unit =
    list setOnItemClickListener new OnListItemClickListener {
      def onItemClicked(itemPosition: Int): Unit = {
        host.onChoiceMade(tag, itemPosition)
        dismiss
      }
    }
}
