package com.lightning.walletapp.utils

import androidx.work._
import android.content.{Context, Intent}
import android.app.{Notification, PendingIntent}
import androidx.core.app.{NotificationCompat, NotificationManagerCompat}
import com.lightning.walletapp.ClassNames
import java.util.concurrent.TimeUnit
import fr.acinq.eclair.secureRandom


object DelayedNotification {
  final val WATCH_TOWER_TAG = "watchTower"
  final val IN_FLIGHT_HTLC_TAG = "inFlightHtlc"
  final val CHANNEL_ID = "delayedNotificationChannelId"

  def schedule(context: Context, tag: String, delayMsecs: Long, title: String, body: String): Operation = {
    val constraintBuilder = (new Constraints.Builder).setTriggerContentMaxDelay(1, TimeUnit.MILLISECONDS).build
    val dataBuilder = (new Data.Builder).putString("title", title).putString("body", body)
    val targetClass = classOf[NotificationSchedule]
    val manager = WorkManager.getInstance(context)

    manager.cancelAllWorkByTag(tag)
    manager.enqueue(new OneTimeWorkRequest.Builder(targetClass)
      .setInitialDelay(delayMsecs, TimeUnit.MILLISECONDS)
      .setConstraints(constraintBuilder)
      .setInputData(dataBuilder.build)
      .addTag(tag)
      .build)
  }

  class NotificationSchedule(context: Context, params: WorkerParameters) extends Worker(context, params) {
    private def setNotification(context: Context, notificationTitle: String, notificationBody: String): Unit = {
      val disaplyIntent = PendingIntent.getActivity(context, 0, new Intent(context, ClassNames.mainActivityClass), 0)
      val notificationBuilder = new NotificationCompat.Builder(context, CHANNEL_ID)
      val notificationId = secureRandom.nextInt(1000000)

      NotificationManagerCompat.from(context).notify(notificationId, notificationBuilder
        .setSmallIcon(com.lightning.walletapp.R.drawable.baseline_feedback_24)
        .setPriority(NotificationCompat.PRIORITY_HIGH)
        .setDefaults(Notification.DEFAULT_ALL)
        .setContentTitle(notificationTitle)
        .setContentText(notificationBody)
        .setContentIntent(disaplyIntent)
        .build)
    }

    override def doWork: ListenableWorker.Result = {
      val title = params.getInputData.getString("title")
      val body = params.getInputData.getString("body")
      setNotification(context, title, body)
      ListenableWorker.Result.success
    }
  }
}
